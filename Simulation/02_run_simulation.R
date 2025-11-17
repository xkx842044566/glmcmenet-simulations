#!/usr/bin/env Rscript
# 02_run_simulation.R  — main driver for Example 2 simulations
#
# To reproduce the simulation results:
#   1. Open an R session in the project root (the folder containing
#      `Rcode.Rproj` and `renv.lock`).
#   2. Once per machine, install and restore the renv environment:
#        install.packages("renv")
#        renv::restore()
#   3. Run this script, e.g.
#        source("02_run_simulation.R")
#      or from the command line
#        Rscript 02_run_simulation.R
#
# The script will call `simulation_glmcmenet_gaussian()` (and/or the GLM
# version), run the simulations, and save the results as
# `.rds` files in the `results/` directory, which are then used by
# `03_draw_figure6.R`.

suppressPackageStartupMessages({
  if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv", repos="https://cloud.r-project.org")
  renv::activate()
  # optional one-time healing on a new machine:
  #try(renv::restore(prompt = FALSE), silent = TRUE)
})
source("01_example2_simulation.R")

# Parameters
n <- 50
p <- 20
num.act = 2
num.grp = c(4, 6, 8, 10)
iter <- 2
seed <- 123

for (rho in c(0, 1/sqrt(2))){
  family <- "gaussian"   # or "poisson", "gaussian"
  warm.str <- "NULL"
  res_gaussian <- simulation_glmcmenet_gaussian(n = n, p = p, num.act=num.act, num.grp=num.grp,
                                       warm.str = warm.str, elastic_alpha = elastic_alpha, iter = iter, rho = rho,
                                       seed = seed)
  
  # Save deterministically
  outdir <- "results"
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  outfile <- sprintf("%s/example2_%s_%s_n%dp%drho%.3f_seed%d.rds",
                     outdir, family, warm.str, n, p, rho, seed)
  saveRDS(res_gaussian, file = outfile, compress = "xz")
  cat("Saved:", outfile, "\n")
}


for (rho in c(0, 1/sqrt(2))){
  family <- "binomial"   # or "poisson", "gaussian"
  warm.str <- "elastic"
  res_glm <- simulation_glmcmenet_glm(n = n, p = p, family = family, num.act=num.act, num.grp=num.grp,
                                    warm.str = warm.str, elastic_alpha = elastic_alpha, iter = iter, rho = rho,
                                    seed = seed)
  # Save deterministically
  outdir <- "results"
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  outfile <- sprintf("%s/example2_%s_%s_n%dp%drho%.3f_seed%d.rds",
                     outdir, family, warm.str, n, p, rho, seed)
  saveRDS(res_glm, file = outfile, compress = "xz")
  cat("Saved:", outfile, "\n")
}


