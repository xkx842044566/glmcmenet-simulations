# 01_example2_simulation.R
# Defines the simulation functions for Example 2 (Gaussian and GLM cases).
# This file is sourced by `02_run_simulation.R`; users do not usually call
# these functions directly.
# To reproduce the paper’s results, you only need to run `02_run_simulation.R`,
# which will automatically source this file.

suppressPackageStartupMessages({
  library(MASS)
  library(glmcmenet)
  library(cmenet)
  library(glmnet)
  library(ncvreg)
  library(grpreg)
  library(hierNet)
  library(stringr)
  library(parallel)
  library(foreach)
  library(doParallel)
})

.get_cores <- function(cores = NULL, max_cap = 16L) {
  if (!is.null(cores)) {
    return(max(1L, as.integer(cores)))
  }
  
  slurm_cores <- suppressWarnings(as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", unset = NA)))
  if (!is.na(slurm_cores) && slurm_cores > 0L) {
    return(max(1L, slurm_cores))
  }
  
  dc <- parallel::detectCores(logical = TRUE)
  if (is.na(dc)) dc <- 1L
  
  cores <- max(1L, dc - 1L)
  cores <- min(cores, max_cap)
  cores
}


# Utility: safe family guard for hierNet (binomial only)
.fit_hiernet_binomial <- function(xme, y, tst, family) {
  if (!identical(family, "binomial")) {
    return(list(group = NA, TP = NA, FP = NA, FN = NA, class = NA, loss = NA))
  }
  glmhnp <- hierNet.logistic.path(xme, as.numeric(y))
  cv.glmhn <- hierNet.cv(glmhnp, xme, y, trace = 0)
  l.opt <- which(glmhnp$lamlist == cv.glmhn$lamhat)
  
  me.sel <- (glmhnp$bp - glmhnp$bn)[, l.opt]
  me.idx <- which(me.sel != 0)
  
  pred <- predict(glmhnp, newx = tst[, 1:ncol(xme)])$prob[, l.opt]
  list(me.idx = me.idx, pred = pred)
}

# Helper: tidy score for binomial / poisson
.bin_loss <- function(y, p) mean(-2 * (y * log(p) + (1 - y) * log(1 - p)))
.poi_loss <- function(y, mu) {
  ylogy <- y * log(pmax(y, 1e-12)); ylogy[y == 0] <- 0
  mean(2 * (ylogy - y + mu - y * log(pmax(mu, 1e-12))))
}
.rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))
.mse <- function(y, yhat) mean((y - yhat)^2)



# ---- MAIN FUNCTION FOR GAUSSIAN -----------------------------------------------------------
simulation_glmcmenet_gaussian <- function(
    n = 50, p = 50, num.act = 2, num.grp = c(4, 6, 8, 10, 12),
    warm.str = c("lasso","adaptive_lasso","elastic","NULL"),
    elastic_alpha = NULL,
    iter = 100, rho = 0, seed = 1L, cores = NULL
) {
  warm.str <- match.arg(warm.str)
  
  ## --- helpers ---
  .pick_idx <- function(lambda_vec, target) which.min(abs(lambda_vec - target))
  
  ## RNG reproducibility (works with clusterSetRNGStream)
  set.seed(seed, kind = "L'Ecuyer-CMRG")
  
  ## Correlated ME generator
  ones <- matrix(1, p, p)
  covmtx <- rho * ones + (1 - rho) * diag(p)
  
  ## Cluster (portable)
  #if (is.null(cores)) cores <- max(1L, parallel::detectCores(logical = TRUE) - 1L)
  cores <- .get_cores(cores, max_cap = 32L)
  closeAllConnections()  # <<< add this line
  cl <- parallel::makeCluster(cores)
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
  doParallel::registerDoParallel(cl)
  parallel::clusterSetRNGStream(cl, iseed = seed)
  
  ## Preload pkgs on workers
  parallel::clusterEvalQ(cl, {
    suppressPackageStartupMessages({
      library(MASS); library(glmcmenet); library(cmenet); library(glmnet)
      library(ncvreg); library(grpreg); library(hierNet); library(stringr)
    })
    NULL
  })
  
  ## Method names (8 slots incl. cmenet)
  base_names <- c("cme", "glmcme(baseline)", "glmcme(m1w1)",
                  "Lasso", "adpLasso", "MCP", "GEL", "hiernet")
  
  ## One-iteration worker
  single_iter <- function(k) {
    ## Design
    lat <- MASS::mvrnorm(n, p, mu = rep(0, p), Sigma = covmtx)
    mem <- (lat >= 0) - (lat < 0)
    mm  <- glmcmenet::full.model.mtx(mem)$model.mtx
    glist <- glmcmenet::grouplist(mm)
    
    ## Test set
    ntst <- 20
    lat_t <- MASS::mvrnorm(ntst, p, mu = rep(0, p), Sigma = covmtx)
    mem_t <- (lat_t >= 0) - (lat_t < 0)
    tst   <- glmcmenet::full.model.mtx(mem_t)$model.mtx
    tst_ref <- tst
    
    xme  <- mm[, 1:p]
    xcme <- mm[, (p + 1):ncol(mm)]
    
    make_names <- function() setNames(
      rep(NA_real_, length(num.grp) * 8L),
      unlist(lapply(num.grp, function(g) paste0(base_names, "_G", g, "A", num.act)))
    )
    
    out <- list(
      siblings = list(
        TP = make_names(), FP = make_names(), FN = make_names(),
        group = make_names(), loss = make_names(),
        underlying = setNames(rep(NA_real_, length(num.grp)), paste0("G", num.grp, "A", num.act))
      ),
      cousins = list(
        TP = make_names(), FP = make_names(), FN = make_names(),
        group = make_names(), loss = make_names(),
        underlying = setNames(rep(NA_real_, length(num.grp)), paste0("G", num.grp, "A", num.act))
      )
    )
    
    ## Helper: Fill metrics 
    fill_metrics <- function(gidx, sel_idx, grp_idx, grp_true, pred, ytst) {
      TP <- length(intersect(sel_idx, gidx$ind))
      FP <- length(setdiff(sel_idx, gidx$ind))
      FN <- length(setdiff(gidx$ind, sel_idx))
      GTPR <- length(intersect(grp_idx, grp_true)) / max(1L, length(grp_true))
      list(TP = TP, FP = FP, FN = FN, group = GTPR, loss = .rmse(ytst, pred))
    }
    
    # Helper: build active sets 
    make_active_siblings <- function(ng) {
      cmeind <- integer(0); meind <- seq_len(ng)
      for (ii in seq_len(ng)) {
        pair_ind <- sample.int(p - 1L, num.act, replace = FALSE)
        eff <- sapply(pair_ind, function(idx) {
          base <- 2 * (idx - 1) + 1
          sample(base:(base + 1), 1)
        })
        cmeind <- c(cmeind, p + eff + (ii - 1) * (2 * (p - 1)))
      }
      sort(unique(c(meind, cmeind)))
    }
    
    make_active_cousins <- function(ng) {
      cmeind <- integer(0); meind <- seq_len(ng)
      for (ii in seq_len(ng)) {
        eff <- sample(seq_len(2 * (p - 1)), num.act)
        for (jj in seq_along(eff)) {
          if (ii > ceiling(eff[jj] / 2)) {
            cmeind <- c(cmeind, p + (ceiling(eff[jj] / 2) - 1) * (2 * (p - 1)) +
                          ifelse(eff[jj] %% 2 == 0, 2, 1) + (ii - 2) * 2)
          } else {
            cmeind <- c(cmeind, p + ceiling(eff[jj] / 2) * (2 * (p - 1)) +
                          ifelse(eff[jj] %% 2 == 0, 2, 1) + (ii - 1) * 2)
          }
        }
      }
      sort(unique(c(meind, cmeind)))
    }
    
    ## Scenario loop
    for (mode in c("siblings", "cousins")) {
      for (g in seq_along(num.grp)) {
        ng <- num.grp[g]
        ind <- if (mode == "siblings") make_active_siblings(ng) else make_active_cousins(ng)
        grp_true <- unique(unlist(lapply(ind, function(id) names(glist)[sapply(glist, function(G) id %in% G)])))
        des <- mm[, ind]
        
        ## Truth & outcome (Gaussian)
        intercept <- 12
        betatrue <- c(rep(5, ng), rep(1, 2 * ng))
        xb <- as.numeric(intercept + des %*% betatrue)
        y  <- xb + rnorm(n, sd = 1)
        
        ## Test truth
        xbt <- as.numeric(intercept + tst_ref[, ind] %*% betatrue)
        yt  <- xbt + rnorm(nrow(tst_ref), sd = 1)
        out[[mode]]$underlying[g] <- .rmse(yt, xbt)
        
        ## Ridge weights AFTER y exists
        cv.ridge <- glmnet::cv.glmnet(cbind(xme, xcme), y, family = "gaussian", alpha = 0, standardize = FALSE)
        coefs <- as.numeric(coef(cv.ridge, s = cv.ridge$lambda.min))[-1]
        w  <- 1 / (abs(coefs) + 1 / n);  w[!is.finite(w)] <- 9.999e8
        mg <- sapply(glist, function(idx) 1 / (sum(abs(coefs[idx])) + 1 / n))
        mg[!is.finite(mg)] <- 9.999e8
        
        ## Stage-2 weights (adaptive from lasso with w)
        cv.lw <- glmnet::cv.glmnet(cbind(xme, xcme), y, family = "gaussian",
                                   alpha = 1, type.measure="deviance", penalty.factor = w, nfolds = 10)
        acoef <- as.numeric(coef(cv.lw, s = cv.lw$lambda.min))[-1]
        w1  <- 1 / (abs(acoef) + 1 / n); w1[!is.finite(w1)] <- 9.999e8
        mg1 <- sapply(glist, function(idx) 1 / (sum(abs(acoef[idx])) + 1 / n))
        mg1[!is.finite(mg1)] <- 9.999e8
        
        ## Utility
        safe_groups <- function(sel) unique(unlist(lapply(sel, function(id) names(glist)[sapply(glist, function(G) id %in% G)])))
        
        ## (1) cmenet
        #sel <- integer(0); grp <- character(0); pred <- rep(NA_real_, ntst)
        cv.cme <- cmenet::cv.cmenet(xme, xcme, y, var.names = colnames(mm), nfolds = 10, warm.str = warm.str)
        ks <- .pick_idx(cv.cme$lambda.sib, cv.cme$params[1])
        kc <- .pick_idx(cv.cme$lambda.cou, cv.cme$params[2])
        sel0 <- cv.cme$select.idx
        grp0 <- safe_groups(sel0)
        pred0 <- cmenet::predictcme(cv.cme$cme.fit, newx = tst_ref)[, ks, kc]
        m0 <- fill_metrics(list(ind = ind), sel0, grp0, grp_true, pred0, yt)
        
        ## (2) glmcmenet baseline
        cv.g0 <- glmcmenet::cv.glmcmenet(xme, xcme, y, family = "gaussian",
                                         var.names = colnames(mm),
                                         nfolds = 10, type.measure="deviance",
                                         warm.str = warm.str, elastic_alpha = elastic_alpha, screen_ind = FALSE)
        ks <- .pick_idx(cv.g0$lambda.sib, cv.g0$params[1])
        kc <- .pick_idx(cv.g0$lambda.cou, cv.g0$params[2])
        sel1 <- cv.g0$select.idx
        grp1 <- safe_groups(sel1)
        pred1 <- glmcmenet::predictcme(cv.g0$cme.fit, newx = tst_ref, type = "response")[, ks, kc]
        m1 <- fill_metrics(list(ind = ind), sel1, grp1, grp_true, pred1, yt)
        
        ## (3) glmcmenet with w1 & mg1
        cv.g1 <- glmcmenet::cv.glmcmenet(xme, xcme, y, family = "gaussian",
                                         var.names = colnames(mm),
                                         nfolds = 10, type.measure="deviance",
                                         warm.str = warm.str, elastic_alpha = elastic_alpha, screen_ind = FALSE,
                                         penalty.factor = w1, group.penalty = mg1)
        ks <- .pick_idx(cv.g1$lambda.sib, cv.g1$params[1])
        kc <- .pick_idx(cv.g1$lambda.cou, cv.g1$params[2])
        sel2 <- cv.g1$select.idx
        grp2 <- safe_groups(sel2)
        pred2 <- glmcmenet::predictcme(cv.g1$cme.fit, newx = tst_ref, type = "response")[, ks, kc]
        m2 <- fill_metrics(list(ind = ind), sel2, grp2, grp_true, pred2, yt)
        
        ## (4) Lasso
        cv.las <- glmnet::cv.glmnet(cbind(xme, xcme), y, family = "gaussian", type.measure="deviance", nfolds = 10)
        k <- .pick_idx(cv.las$lambda, cv.las$lambda.min)
        sel3 <- which(cv.las$glmnet.fit$beta[, k, drop = TRUE] != 0)
        grp3 <- safe_groups(sel3)
        pred3 <- as.numeric(predict(cv.las$glmnet.fit, newx = tst_ref, type = "response")[, k, drop = TRUE])
        m3 <- fill_metrics(list(ind = ind), sel3, grp3, grp_true, pred3, yt)
        
        ## (5) Adaptive Lasso (ridge weights)
        cv.al <- glmnet::cv.glmnet(cbind(xme, xcme), y, family = "gaussian", type.measure="deviance",
                                   nfolds = 10, penalty.factor = w)
        k <- .pick_idx(cv.al$lambda, cv.al$lambda.min)
        sel4 <- which(cv.al$glmnet.fit$beta[, k, drop = TRUE] != 0)
        grp4 <- safe_groups(sel4)
        pred4 <- as.numeric(predict(cv.al$glmnet.fit, newx = tst_ref, type = "response")[, k, drop = TRUE])
        m4 <- fill_metrics(list(ind = ind), sel4, grp4, grp_true, pred4, yt)
        
        
        ## (6) ncvreg (MCP)
        cv.ncv <- ncvreg::cv.ncvreg(cbind(xme, xcme), y, nfolds = 10, family = "gaussian")
        k <- .pick_idx(cv.ncv$lambda, cv.ncv$lambda.min)
        sel5 <- which(cv.ncv$fit$beta[, k, drop = TRUE] != 0); sel5 <- sel5[sel5 != 1] - 1
        grp5 <- safe_groups(sel5)
        pred5 <- as.numeric(predict(cv.ncv$fit, X = tst_ref, type = "response")[, k, drop = TRUE])
        m5 <- fill_metrics(list(ind = ind), sel5, grp5, grp_true, pred5, yt)
        
        ## (7) grpreg (GEL → cMCP fallback)
        group_lab <- sub("\\|.*", "", colnames(cbind(xme, xcme)))
        cv.grp <- try({
          grpreg::cv.grpreg(cbind(xme, xcme), y, nfolds = 10, group = group_lab, family = "gaussian", penalty = "gel")
        }, silent = TRUE)
        if (inherits(cv.grp, "try-error")) {
          cv.grp <- try(grpreg::cv.grpreg(cbind(xme, xcme), y, nfolds = 10, group = group_lab, family = "gaussian", penalty = "cMCP"),
                        silent = TRUE)
        }
        if (!inherits(cv.grp, "try-error")) {
          fit.grp <- cv.grp$fit
          lamg_idx <- which(cv.grp$lambda == cv.grp$lambda.min)
          sel_grp <- which(fit.grp$beta[, lamg_idx] != 0); sel_grp <- sel_grp[sel_grp != 1] - 1
          grp_grp <- unique(unlist(lapply(sel_grp, function(id) names(glist)[sapply(glist, function(G) id %in% G)])))
          pred_grp <- as.numeric(predict(fit.grp, X = tst, type = "response")[, lamg_idx])
          m6 <- fill_metrics(list(ind = ind), sel_grp, grp_grp, grp_true, pred_grp, yt)
        } else {
          m6 <- list(TP = NA, FP = NA, FN = NA, group = NA, class = NA, loss = NA)
        }
        
        ## (8) hierNet (Gaussian)
        ## (special handling to compute ME/INT matches)
        glmhnp <- hierNet::hierNet.path(xme, as.numeric(y), trace = 0)
        cv.hn  <- hierNet::hierNet.cv(glmhnp, xme, y, trace = 0)
        k <- .pick_idx(glmhnp$lamlist, cv.hn$lamhat)
        me.sel <- (glmhnp$bp - glmhnp$bn)[, k]
        me.idx <- which(me.sel != 0)
        int.sel <- glmhnp$th[, , k]
        int.idx <- which(int.sel != 0, arr.ind = TRUE)
        int.nm <- character(0)
        if (nrow(int.idx)) {
          int.idx <- t(apply(int.idx, 1, function(xx) sort(xx)))
          int.idx <- unique(int.idx)
          int.nm <- sapply(seq_len(nrow(int.idx)), function(ii) {
            paste0(colnames(xme)[int.idx[ii, 1]], colnames(xme)[int.idx[ii, 2]])
          })
        }
        Mtrue <- matrix(stringr::str_replace_all(unlist(strsplit(colnames(mm)[ind], "|", fixed = TRUE)),
                                                 "[^[:alnum:]]", ""), nrow = 2)
        me.true  <- unique(Mtrue[1,])
        int.true <- paste0(apply(Mtrue, 2, sort)[1,], apply(Mtrue, 2, sort)[2,])
        
        grp_h <- length(intersect(colnames(xme)[me.idx], me.true)) / max(1L, length(me.true))
        TP <- length(intersect(colnames(xme)[me.idx], me.true)) + length(intersect(int.nm, int.true))
        FP <- length(setdiff(colnames(xme)[me.idx], me.true)) + length(setdiff(int.nm, int.true))
        FN <- length(setdiff(me.true, colnames(xme)[me.idx])) + length(setdiff(int.true, int.nm))
        pred_h <- as.numeric(predict(glmhnp, newx = tst_ref[, 1:p])[, k])
        m7 <- list(TP = TP, FP = FP, FN = FN, group = grp_h, loss = .rmse(yt, pred_h))
        
        idx_base <- (g - 1) * 8
        for (j in 0:7) {
          jj <- 1 + idx_base + j
          cur <- switch(j + 1, m0, m1, m2, m3, m4, m5, m6,m7)
          out[[mode]]$TP[jj]    <- cur$TP
          out[[mode]]$FP[jj]    <- cur$FP
          out[[mode]]$FN[jj]    <- cur$FN
          out[[mode]]$group[jj] <- cur$group
          out[[mode]]$loss[jj]  <- cur$loss
        }
        
      } # g
    }   # mode
    
    out
  }
  
  ## Run in parallel
  parallel::clusterExport(
    cl,
    varlist = c("n","p","num.act","num.grp","warm.str","rho","ones","covmtx",
                ".pick_idx",".rmse"),
    envir = environment()
  )
  
  results <- parallel::parLapply(cl, 1:iter, single_iter)
  
  ## Bind results to tidy lists
  bind_mode <- function(mode) {
    nm <- names(results[[1]][[mode]])
    out <- lapply(nm, function(k) do.call(rbind, lapply(results, function(z) z[[mode]][[k]])))
    names(out) <- nm
    out
  }
  sib <- bind_mode("siblings")
  cou <- bind_mode("cousins")
  
  list(
    siblings = sib,
    cousins  = cou,
    meta = list(n = n, p = p, num.act = num.act, num.grp = num.grp,
                family = "gaussian", warm.str = warm.str,
                iter = iter, rho = rho, seed = seed)
  )
}



# ---- MAIN FUNCTION FOR GLM -----------------------------------------------------------
simulation_glmcmenet_glm <- function(
    n = 50, p = 50, num.act = 2, num.grp = c(4, 6, 8, 10, 12),
    family = c("binomial", "poisson"),
    warm.str = c("lasso","adaptive_lasso","elastic","NULL"),
    elastic_alpha = NULL,
    iter = 100, rho = 0, seed = 1L, cores = NULL
) {
  family <- match.arg(family)
  warm.str <- match.arg(warm.str)
  
  # RNG reproducibility (works with clusterSetRNGStream)
  set.seed(seed, kind = "L'Ecuyer-CMRG")
  
  # Correlated ME generator
  ones <- matrix(1, p, p)
  covmtx <- rho * ones + (1 - rho) * diag(p)
  
  # Cluster (portable)
  #if (is.null(cores)) cores <- max(1L, parallel::detectCores(logical = TRUE) - 1L)
  cores <- .get_cores(cores, max_cap = 32L)
  closeAllConnections()  # <<< add this line
  cl <- makeCluster(cores)
  on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
  registerDoParallel(cl)
  parallel::clusterSetRNGStream(cl, iseed = seed)
  
  # Preload pkgs and symbols on workers
  clusterEvalQ(cl, {
    suppressPackageStartupMessages({
      library(MASS); library(glmcmenet); library(cmenet); library(glmnet)
      library(ncvreg); library(grpreg); library(hierNet); library(stringr)
    })
    NULL
  })
  
  # Base names for methods (7 slots)
  base_names <- c("glmcme(baseline)","glmcme(ridge)","Lasso","adpLasso","MCP","GEL","hiernet")
  
  # One iteration work
  single_iter <- function(k) {
    # Design matrices
    latmtx <- MASS::mvrnorm(n, p, mu = rep(0, p), Sigma = covmtx)
    memtx  <- (latmtx >= 0) - (latmtx < 0)
    mm     <- glmcmenet::full.model.mtx(memtx)$model.mtx
    glist  <- glmcmenet::grouplist(mm)
    
    # test set
    ntst <- 20
    lat_t <- MASS::mvrnorm(ntst, p, mu = rep(0, p), Sigma = covmtx)
    mem_t <- (lat_t >= 0) - (lat_t < 0)
    tst   <- glmcmenet::full.model.mtx(mem_t)$model.mtx
    
    xme  <- mm[, 1:p]
    xcme <- mm[, (p + 1):ncol(mm)]
    
    make_names <- function(ng) setNames(rep(NA_real_, length(num.grp) * 7L),
                                        unlist(lapply(num.grp, function(g) paste0(base_names, "_G", g, "A", num.act))))
    
    # containers
    out <- list(
      siblings = list(TP = make_names(num.grp), FP = make_names(num.grp), FN = make_names(num.grp),
                      group = make_names(num.grp), class = make_names(num.grp), loss = make_names(num.grp),
                      underlying = setNames(rep(NA_real_, length(num.grp)),
                                            paste0("G", num.grp, "A", num.act))),
      cousins  = list(TP = make_names(num.grp), FP = make_names(num.grp), FN = make_names(num.grp),
                      group = make_names(num.grp), class = make_names(num.grp), loss = make_names(num.grp),
                      underlying = setNames(rep(NA_real_, length(num.grp)),
                                            paste0("G", num.grp, "A", num.act)))
    )
    
    # --- helper to fill metrics for one scenario (sib/cou) and one ng ------------
    fill_metrics <- function(gidx, sel_idx, grp_idx, grp_true, pred, ytst, fam) {
      TP <- length(intersect(sel_idx, gidx$ind))
      FP <- length(setdiff(sel_idx, gidx$ind))
      FN <- length(setdiff(gidx$ind, sel_idx))
      GTPR <- length(intersect(grp_idx, grp_true)) / max(1L, length(grp_true))
      
      if (fam == "binomial") {
        cls  <- mean((pred > 0.5) != ytst)
        loss <- .bin_loss(ytst, pmin(pmax(pred, 1e-12), 1 - 1e-12)) 
      } else if (fam == "poisson") {
        cls  <- sqrt(mean((pred - ytst)^2))
        loss <- .poi_loss(ytst, pred) 
      } else { # gaussian
        cls  <- NULL
        loss <- .rmse(pred,ytst)
      }
      list(TP = TP, FP = FP, FN = FN, group = GTPR, class = cls, loss = loss)
    }
    
    # Helper: build active sets 
    make_active_siblings <- function(ng) {
      cmeind <- integer(0); meind <- seq_len(ng)
      for (ii in seq_len(ng)) {
        pair_ind <- sample.int(p - 1L, num.act, replace = FALSE)
        eff <- sapply(pair_ind, function(idx) {
          base <- 2 * (idx - 1) + 1
          sample(base:(base + 1), 1)
        })
        cmeind <- c(cmeind, p + eff + (ii - 1) * (2 * (p - 1)))
      }
      sort(unique(c(meind, cmeind)))
    }
    
    make_active_cousins <- function(ng) {
      cmeind <- integer(0); meind <- seq_len(ng)
      for (ii in seq_len(ng)) {
        eff <- sample(seq_len(2 * (p - 1)), num.act)
        for (jj in seq_along(eff)) {
          if (ii > ceiling(eff[jj] / 2)) {
            cmeind <- c(cmeind, p + (ceiling(eff[jj] / 2) - 1) * (2 * (p - 1)) +
                          ifelse(eff[jj] %% 2 == 0, 2, 1) + (ii - 2) * 2)
          } else {
            cmeind <- c(cmeind, p + ceiling(eff[jj] / 2) * (2 * (p - 1)) +
                          ifelse(eff[jj] %% 2 == 0, 2, 1) + (ii - 1) * 2)
          }
        }
      }
      sort(unique(c(meind, cmeind)))
    }
    
    # -------- iterate scenarios: siblings then cousins ---------------------------
    for (mode in c("siblings", "cousins")) {
      for (g in seq_along(num.grp)) {
        ng <- num.grp[g]
        ind <- if (mode == "siblings") make_active_siblings(ng) else make_active_cousins(ng)
        grp_true <- unique(unlist(lapply(ind, function(id) names(glist)[sapply(glist, function(G) id %in% G)])))
        des <- mm[, ind]
        
        # Truth + outcome
        intercept <- 0
        if (family == "binomial") {
          betatrue <- c(rep(5, ng), rep(1, 2 * ng))
          xb <- intercept + des %*% betatrue
          y  <- rbinom(nrow(des), 1, 1 / (1 + exp(-xb)))
        } else if (family == "poisson") {
          betatrue <- c(rep(2 / ng, ng), rep(0.4 / ng, 2 * ng))
          xb <- intercept + des %*% betatrue
          y  <- rpois(nrow(des), exp(xb))
        } else {
          betatrue <- c(rep(2, ng), rep(0.5, 2 * ng))
          xb <- intercept + des %*% betatrue
          y  <- xb + rnorm(nrow(des))
        }
        print(y)
        # test truth
        xbt <- intercept + tst[, ind] %*% betatrue
        if (family == "binomial") {
          yt  <- rbinom(length(xbt), 1, 1 / (1 + exp(-xbt)))
          out[[mode]]$underlying[g] <- mean((1 / (1 + exp(-xbt)) > 0.5) != yt)
        } else if (family == "poisson") {
          yt  <- rpois(length(xbt), exp(xbt))
          out[[mode]]$underlying[g] <- .rmse(yt, exp(xbt))
        } else {
          yt  <- xbt + rnorm(length(xbt))
          out[[mode]]$underlying[g] <- .rmse(yt, xbt)
        }
        
        # weights from ridge fit (recompute with current y)
        cv.ridge <- glmnet::cv.glmnet(cbind(xme, xcme), y, family = family, alpha = 0, standardize = FALSE)
        coefs <- as.numeric(coef(cv.ridge, s = cv.ridge$lambda.min))[-1]
        w  <- 1 / (abs(coefs) + 1 / n)       # element-wise
        w[!is.finite(w)] <- 9.999e8
        mg <- sapply(glist, function(idx) 1 / (sum(abs(coefs[idx])) + 1 / n))
        mg[!is.finite(mg)] <- 9.999e8
        
        # --- (1) glmcmenet baseline
        cv.g0 <- glmcmenet::cv.glmcmenet(xme, xcme, y, family = family,
                                         var.names = colnames(mm),
                                         nfolds = 10, type.measure = "deviance",
                                         warm.str = warm.str, elastic_alpha = elastic_alpha, screen_ind = FALSE)
        sel0 <- cv.g0$select.idx
        grp0 <- unique(unlist(lapply(sel0, function(id) names(glist)[sapply(glist, function(G) id %in% G)])))
        pred0 <- glmcmenet::predictcme(cv.g0$cme.fit, newx = tst, type = "response")[,
                                                                                     which(cv.g0$lambda.sib == cv.g0$params[1]),
                                                                                     which(cv.g0$lambda.cou == cv.g0$params[2])]
        m0 <- fill_metrics(list(ind = ind), sel0, grp0, grp_true, pred0, yt, family)
        
        # --- (2) glmcmenet adaptive (ridge mg & w)
        cv.g1 <- glmcmenet::cv.glmcmenet(xme, xcme, y, family = family,
                                         var.names = colnames(mm),
                                         nfolds = 10, type.measure = "deviance",
                                         warm.str = warm.str, elastic_alpha = elastic_alpha, screen_ind = FALSE,
                                         group.penalty = mg, penalty.factor = w)
        sel1 <- cv.g1$select.idx
        grp1 <- unique(unlist(lapply(sel1, function(id) names(glist)[sapply(glist, function(G) id %in% G)])))
        pred1 <- glmcmenet::predictcme(cv.g1$cme.fit, newx = tst, type = "response")[,
                                                                                     which(cv.g1$lambda.sib == cv.g1$params[1]),
                                                                                     which(cv.g1$lambda.cou == cv.g1$params[2])]
        m1 <- fill_metrics(list(ind = ind), sel1, grp1, grp_true, pred1, yt, family)
        
        # --- (3) Lasso
        cv.las <- glmnet::cv.glmnet(cbind(xme, xcme), y, family = family, type.measure = "deviance", nfolds = 10)
        b_las  <- cv.las$glmnet.fit$beta[, which(cv.las$lambda == cv.las$lambda.min)]
        sel_las <- which(as.numeric(b_las) != 0)
        grp_las <- unique(unlist(lapply(sel_las, function(id) names(glist)[sapply(glist, function(G) id %in% G)])))
        pred_las <- as.numeric(predict(cv.las$glmnet.fit, newx = tst, type = "response")[, which(cv.las$lambda == cv.las$lambda.min)])
        m2 <- fill_metrics(list(ind = ind), sel_las, grp_las, grp_true, pred_las, yt, family)
        
        # --- (4) Adaptive Lasso
        cv.al <- glmnet::cv.glmnet(cbind(xme, xcme), y, family = family, type.measure = "deviance",
                                   nfolds = 10, penalty.factor = w)
        b_al  <- cv.al$glmnet.fit$beta[, which(cv.al$lambda == cv.al$lambda.min)]
        sel_al <- which(as.numeric(b_al) != 0)
        grp_al <- unique(unlist(lapply(sel_al, function(id) names(glist)[sapply(glist, function(G) id %in% G)])))
        pred_al <- as.numeric(predict(cv.al$glmnet.fit, newx = tst, type = "response")[, which(cv.al$lambda == cv.al$lambda.min)])
        m3 <- fill_metrics(list(ind = ind), sel_al, grp_al, grp_true, pred_al, yt, family)
        
        # --- (5) ncvreg (MCP default)
        cv.ncv <- ncvreg::cv.ncvreg(cbind(xme, xcme), y, nfolds = 10, family = family)
        fit.ncv <- cv.ncv$fit
        lam_idx <- which(cv.ncv$lambda == cv.ncv$lambda.min)
        sel_ncv <- which(fit.ncv$beta[, lam_idx] != 0); sel_ncv <- sel_ncv[sel_ncv != 1] - 1
        grp_ncv <- unique(unlist(lapply(sel_ncv, function(id) names(glist)[sapply(glist, function(G) id %in% G)])))
        pred_ncv <- as.numeric(predict(fit.ncv, X = tst, type = "response")[, lam_idx])
        m4 <- fill_metrics(list(ind = ind), sel_ncv, grp_ncv, grp_true, pred_ncv, yt, family)
        
        # --- (6) grpreg (try GEL then cMCP)
        group_lab <- sub("\\|.*", "", colnames(cbind(xme, xcme)))
        cv.grp <- try({
          grpreg::cv.grpreg(cbind(xme, xcme), y, nfolds = 10, group = group_lab, family = family, penalty = "gel")
        }, silent = TRUE)
        if (inherits(cv.grp, "try-error")) {
          cv.grp <- try(grpreg::cv.grpreg(cbind(xme, xcme), y, nfolds = 10, group = group_lab, family = family, penalty = "cMCP"),
                        silent = TRUE)
        }
        if (!inherits(cv.grp, "try-error")) {
          fit.grp <- cv.grp$fit
          lamg_idx <- which(cv.grp$lambda == cv.grp$lambda.min)
          sel_grp <- which(fit.grp$beta[, lamg_idx] != 0); sel_grp <- sel_grp[sel_grp != 1] - 1
          grp_grp <- unique(unlist(lapply(sel_grp, function(id) names(glist)[sapply(glist, function(G) id %in% G)])))
          pred_grp <- as.numeric(predict(fit.grp, X = tst, type = "response")[, lamg_idx])
          m5 <- fill_metrics(list(ind = ind), sel_grp, grp_grp, grp_true, pred_grp, yt, family)
        } else {
          m5 <- list(TP = NA, FP = NA, FN = NA, group = NA, class = NA, loss = NA)
        }
        
        # --- (7) hierNet (binomial only)
        h <- .fit_hiernet_binomial(xme, y, tst, family)
        if (!any(is.na(h))) {
          # compute true sets for hierNet output
          Mtrue <- matrix(stringr::str_replace_all(unlist(strsplit(colnames(mm)[ind], "|", fixed = TRUE)), "[^[:alnum:]]", ""), nrow = 2)
          me.true <- unique(Mtrue[1,])
          int.true <- paste0(apply(Mtrue, 2, sort)[1,], apply(Mtrue, 2, sort)[2,])
          
          me.idx <- h$me.idx
          # For interactions, hierNet returns th; skipped here for brevity—treat as ME-only proxy
          grp_h <- length(intersect(colnames(xme)[me.idx], me.true)) / max(1L, length(me.true))
          TP <- length(intersect(colnames(xme)[me.idx], me.true))
          FP <- length(setdiff(colnames(xme)[me.idx], me.true))
          FN <- length(setdiff(me.true, colnames(xme)[me.idx]))
          pred_h <- h$pred
          if (family == "binomial") {
            cls  <- mean((pred_h > 0.5) != yt)
            loss <- .bin_loss(yt, pmin(pmax(pred_h, 1e-12), 1 - 1e-12))
          } else { cls <- loss <- NA }
          m6 <- list(TP = TP, FP = FP, FN = FN, group = grp_h, class = cls, loss = loss)
        } else {
          m6 <- list(TP = NA, FP = NA, FN = NA, group = NA, class = NA, loss = NA)
        }
        
        # write into the 7 slots for this g
        idx_base <- (g - 1) * 7
        for (j in 0:6) {
          jj <- 1 + idx_base + j
          cur <- switch(j + 1, m0, m1, m2, m3, m4, m5, m6)
          out[[mode]]$TP[jj]    <- cur$TP
          out[[mode]]$FP[jj]    <- cur$FP
          out[[mode]]$FN[jj]    <- cur$FN
          out[[mode]]$group[jj] <- cur$group
          out[[mode]]$class[jj] <- cur$class
          out[[mode]]$loss[jj]  <- cur$loss
        }
      } # g
    }   # mode
    
    out
  }
  
  # -------- run in parallel deterministically -----------------------------------
  clusterExport(cl, varlist = c("n","p","num.act","num.grp","family","warm.str","elastic_alpha", "rho",
                                "ones","covmtx",".bin_loss",".poi_loss",".rmse",
                                ".fit_hiernet_binomial"), envir = environment())
  
  results <- parLapply(cl, 1:iter, single_iter)
  
  # ---- bind outputs to tidy data.frames ----------------------------------------
  bind_mode <- function(mode) {
    nm <- names(results[[1]][[mode]])
    out <- lapply(nm, function(k) do.call(rbind, lapply(results, function(z) z[[mode]][[k]])))
    names(out) <- nm
    out
  }
  sib <- bind_mode("siblings")
  cou <- bind_mode("cousins")
  
  # Return tidy list
  list(
    siblings = sib,
    cousins  = cou,
    meta = list(n = n, p = p, num.act = num.act, num.grp = num.grp,
                family = family, warm.str = warm.str, elastic_alpha = elastic_alpha, iter = iter, rho = rho, seed = seed)
  )
}



