# 03_draw_figure6.R  — plotting for Example 2
#
# This script reads the `.rds` files in `results/` produced by
# `02_run_simulation.R`, computes additional metrics (precision, TPR, F1),
# and creates Figure 6 used in the paper.
#
# To reproduce the figure:
#   1. Make sure `02_run_simulation.R` has been run and `results/` contains
#      the simulation output.
#   2. Run this script (e.g. `source("03_draw_figure6.R")`).
#      The figure object and any saved graphics files will be created in
#      the working directory.


if(!require(dplyr)) install.packages("dplyr",repos = "http://cran.r-project.org")
library(dplyr)
if(!require(tidyr)) install.packages("tidyr",repos = "http://cran.r-project.org")
library(tidyr)
if(!require(ggplot2)) install.packages("ggplot2",repos = "http://cran.r-project.org")
library(ggplot2)
if(!require(MASS)) install.packages("MASS",repos = "http://cran.r-project.org")
library(MASS)
if(!require(gridExtra)) install.packages("gridExtra",repos = "http://cran.r-project.org")
library(gridExtra)
if(!require(stringr)) install.packages("stringr",repos = "http://cran.r-project.org")
library(stringr)

outdir="results"

#### help function ####
add_metrics <- function(part) {
  TP <- part$TP
  FP <- part$FP
  FN <- part$FN
  
  # Precision = TP / (TP + FP)
  precision <- TP / (TP + FP)
  precision[TP + FP == 0] <- NA  
  
  # TPR (recall, sensitivity) = TP / (TP + FN)
  TPR <- TP / (TP + FN)
  TPR[TP + FN == 0] <- NA
  
  # F1 = 2 * P * TPR / (P + TPR)
  F1 <- 2 * precision * TPR / (precision + TPR)
  F1[(precision + TPR) == 0 | is.na(precision) | is.na(TPR)] <- NA
  
  part$precision <- precision
  part$TPR <- TPR
  part$F1 <- F1
  
  part
}

make_metric_df <- function(plotdat, metric_name) {
  out_list <- vector("list", length(plotdat))
  i <- 1
  
  for (effect in c("siblings","cousins")) {
    mat <- plotdat[[effect]][[metric_name]]
    storage.mode(mat) <- "numeric"
    vals <- colMeans(mat, na.rm = TRUE)
    
    if (metric_name == "underlying") {
      method <- rep("underlying", length(vals))
      GxAy  <- names(vals)
    } else {
      full   <- names(vals)
      method <- sub("_([^_]*)$", "", full)
      GxAy   <- sub(".*_", "", full)
    }
    
    out_list[[i]] <- data.frame(
      effect = effect,
      method = method,
      GxAy   = GxAy,
      num    = as.numeric(vals),
      stringsAsFactors = FALSE
    )
    i <- i + 1
  }
  
  do.call(rbind, out_list)
}
#### gaussian data ####
n=50
p=20
rho=0
family= "gaussian" #"poisson"  #"binomial" "gaussian"

warm.str="NULL" #elastic NULL

filename1 <- sprintf("%s/example2_%s_%s_n%dp%drho%.3f_seed%d.rds",
                     outdir, family, warm.str, n, p, rho, seed)
plotdat1<-readRDS(filename1)

plotdat1$siblings <- add_metrics(plotdat1$siblings)
plotdat1$cousins  <- add_metrics(plotdat1$cousins)

metrics <- names(plotdat1[[1]]) 

plotdat1 <- do.call(rbind,lapply(metrics, function(m) {
    df <- make_metric_df(plotdat1, m)
    df$metric <- m
    df
  })
)

rho=0.707

filename2 = sprintf("%s/example2_%s_%s_n%dp%drho%.3f_seed%d.rds",
                   outdir, family, warm.str, n, p, rho, seed)
plotdat2<-readRDS(filename2)

plotdat2$siblings <- add_metrics(plotdat2$siblings)
plotdat2$cousins  <- add_metrics(plotdat2$cousins)

metrics <- names(plotdat2[[1]]) 

plotdat2 <- do.call(rbind,lapply(metrics, function(m) {
  df <- make_metric_df(plotdat2, m)
  df$metric <- m
  df
})
)

#### draw lineplot for gaussian ####
target_method <- c("cme","glmcme(m1w1)","Lasso","adpLasso","MCP","GEL")
p_TPR_1<-as.data.frame(plotdat1) %>% filter(metric=="TPR") %>%
  group_by(effect, method, GxAy) %>%
  filter(method %in% target_method) %>% 
  summarise(mean_num = mean(as.numeric(num),na.rm=TRUE),
            sd_num= sd(as.numeric(num),na.rm=TRUE)) %>%
  ggplot(aes(x = factor(GxAy, levels=c("G4A2", "G6A2", "G8A2", "G10A2", "G12A2")), y = mean_num, colour = method, group = method)) +
  facet_wrap(~effect,scales="free_y") +
  geom_line() +
  geom_point() +
  # geom_errorbar(aes(ymin=mean_num-sd_num, ymax=mean_num+sd_num), width=0.5,
  #               position=position_dodge(0.05))+
  scale_x_discrete(labels=c('4', '6', '8', '10','12'))+
  scale_color_manual(
    values = c("black","red","blue", "lightblue","#0CB702","purple"),
    breaks=target_method,
    labels = c("cmenet","adaptive cmenet","Lasso","adptive Lasso","MCP","GEL")
  ) +
  labs(title = "", y = "TPR", x="# of active group") +
  theme_bw()

p_Precision_1<-as.data.frame(plotdat1) %>% filter(metric=="precision") %>%
  group_by(effect, method, GxAy) %>%
  filter(method %in% target_method) %>% 
  summarise(mean_num = mean(as.numeric(num),na.rm=TRUE),
            sd_num= sd(as.numeric(num),na.rm=TRUE)) %>%
  ggplot(aes(x = factor(GxAy, levels=c("G4A2", "G6A2", "G8A2", "G10A2", "G12A2")), y = mean_num, colour = method, group = method)) +
  facet_wrap(~effect,scales="free_y") +
  geom_line() +
  geom_point() +
  # geom_errorbar(aes(ymin=mean_num-sd_num, ymax=mean_num+sd_num), width=0.5,
  #               position=position_dodge(0.05))+
  scale_x_discrete(labels=c('4', '6', '8', '10','12'))+
  scale_color_manual(
    values = c("black","red","blue", "lightblue","#0CB702","purple"),
    breaks=target_method,
    labels = c("cmenet","adaptive cmenet","Lasso","adptive Lasso","MCP","GEL")
  ) +
  labs(title = "", y = "Precision", x="# of active group") +
  theme_bw()

p_F1_1<-as.data.frame(plotdat1) %>% filter(metric=="F1") %>%
  group_by(effect, method, GxAy) %>% 
  filter(method %in% target_method) %>% 
  summarise(mean_num = mean(as.numeric(num),na.rm=TRUE),
            sd_num= sd(as.numeric(num),na.rm=TRUE)) %>%
  ggplot(aes(x = factor(GxAy, levels=c("G4A2", "G6A2", "G8A2", "G10A2", "G12A2")), y = mean_num, colour = method, group = method)) +
  facet_wrap(~effect,scales="free_y") +
  geom_line() +
  geom_point() +
  # geom_errorbar(aes(ymin=mean_num-sd_num, ymax=mean_num+sd_num), width=0.5,
  #               position=position_dodge(0.05))+
  scale_x_discrete(labels=c('4', '6', '8', '10','12'))+
  scale_color_manual(
    values = c("black","red","blue", "lightblue","#0CB702","purple"),
    breaks=target_method,
    labels = c("cmenet","adaptive cmenet","Lasso","adptive Lasso","MCP","GEL")
  ) +
  labs(title="",y="F1 score",x="# of active group") +
  theme_bw()

p_deviance_1<-as.data.frame(plotdat1) %>% filter(metric=="loss") %>%
  group_by(effect, method, GxAy) %>% 
  filter(method %in% target_method) %>% 
  summarise(mean_num = mean(as.numeric(num),na.rm=TRUE),
            sd_num= sd(as.numeric(num),na.rm=TRUE)) %>%
  ggplot(aes(x = factor(GxAy, levels=c("G4A2", "G6A2", "G8A2", "G10A2", "G12A2")), y = mean_num, colour = method, group = method)) +
  facet_wrap(~effect,scales="free_y") +
  geom_line() +
  geom_point() +
  # geom_errorbar(aes(ymin=mean_num-sd_num, ymax=mean_num+sd_num), width=0.5,
  #               position=position_dodge(0.05))+
  scale_x_discrete(labels=c('4', '6', '8', '10','12'))+
  scale_color_manual(
    values = c("black","red","blue", "lightblue","#0CB702","purple"),
    breaks=target_method,
    labels = c("cmenet","adaptive cmenet","Lasso","adptive Lasso","MCP","GEL")
  ) +
  labs(title="",y="MSPE", x="# of active group") +
  theme_bw()

p_TPR_2<-as.data.frame(plotdat2) %>% filter(metric=="TPR") %>%
  group_by(effect, method, GxAy) %>%
  filter(method %in% target_method) %>% 
  summarise(mean_num = mean(as.numeric(num),na.rm=TRUE),
            sd_num= sd(as.numeric(num),na.rm=TRUE)) %>%
  ggplot(aes(x = factor(GxAy, levels=c("G4A2", "G6A2", "G8A2", "G10A2", "G12A2")), y = mean_num, colour = method, group = method)) +
  facet_wrap(~effect,scales="free_y") +
  geom_line() +
  geom_point() +
  # geom_errorbar(aes(ymin=mean_num-sd_num, ymax=mean_num+sd_num), width=0.5,
  #               position=position_dodge(0.05))+
  scale_x_discrete(labels=c('4', '6', '8', '10','12'))+
  scale_color_manual(
    values = c("black","red","blue", "lightblue","#0CB702","purple"),
    breaks=target_method,
    labels = c("cmenet","adaptive cmenet","Lasso","adptive Lasso","MCP","GEL")
  ) +
  labs(title = "", y = "TPR", x="# of active group") +
  theme_bw()

p_Precision_2<-as.data.frame(plotdat2) %>% filter(metric=="precision") %>%
  group_by(effect, method, GxAy) %>%
  filter(method %in% target_method) %>% 
  summarise(mean_num = mean(as.numeric(num),na.rm=TRUE),
            sd_num= sd(as.numeric(num),na.rm=TRUE)) %>%
  ggplot(aes(x = factor(GxAy, levels=c("G4A2", "G6A2", "G8A2", "G10A2", "G12A2")), y = mean_num, colour = method, group = method)) +
  facet_wrap(~effect,scales="free_y") +
  geom_line() +
  geom_point() +
  # geom_errorbar(aes(ymin=mean_num-sd_num, ymax=mean_num+sd_num), width=0.5,
  #               position=position_dodge(0.05))+
  scale_x_discrete(labels=c('4', '6', '8', '10','12'))+
  scale_color_manual(
    values = c("black","red","blue", "lightblue","#0CB702","purple"),
    breaks=target_method,
    labels = c("cmenet","adaptive cmenet","Lasso","adptive Lasso","MCP","GEL")
  ) +
  labs(title = "", y = "Precision", x="# of active group") +
  theme_bw()

p_F1_2<-as.data.frame(plotdat2) %>% filter(metric=="F1") %>%
  group_by(effect, method, GxAy) %>% 
  filter(method %in% target_method) %>% 
  summarise(mean_num = mean(as.numeric(num),na.rm=TRUE),
            sd_num= sd(as.numeric(num),na.rm=TRUE)) %>%
  ggplot(aes(x = factor(GxAy, levels=c("G4A2", "G6A2", "G8A2", "G10A2", "G12A2")), y = mean_num, colour = method, group = method)) +
  facet_wrap(~effect,scales="free_y") +
  geom_line() +
  geom_point() +
  # geom_errorbar(aes(ymin=mean_num-sd_num, ymax=mean_num+sd_num), width=0.5,
  #               position=position_dodge(0.05))+
  scale_x_discrete(labels=c('4', '6', '8', '10','12'))+
  scale_color_manual(
    values = c("black","red","blue", "lightblue","#0CB702","purple"),
    breaks=target_method,
    labels = c("cmenet","adaptive cmenet","Lasso","adptive Lasso","MCP","GEL")
  ) +
  labs(title="",y="F1 score",x="# of active group") +
  theme_bw()

p_deviance_2<-as.data.frame(plotdat2) %>% filter(metric=="loss") %>%
  group_by(effect, method, GxAy) %>% 
  filter(method %in% target_method) %>% 
  summarise(mean_num = mean(as.numeric(num),na.rm=TRUE),
            sd_num= sd(as.numeric(num),na.rm=TRUE)) %>%
  ggplot(aes(x = factor(GxAy, levels=c("G4A2", "G6A2", "G8A2", "G10A2", "G12A2")), y = mean_num, colour = method, group = method)) +
  facet_wrap(~effect,scales="free_y") +
  geom_line() +
  geom_point() +
  # geom_errorbar(aes(ymin=mean_num-sd_num, ymax=mean_num+sd_num), width=0.5,
  #               position=position_dodge(0.05))+
  scale_x_discrete(labels=c('4', '6', '8', '10','12'))+
  scale_color_manual(
    values = c("black","red","blue", "lightblue","#0CB702","purple"),
    breaks=target_method,
    labels = c("cmenet","adaptive cmenet","Lasso","adptive Lasso","MCP","GEL")
  ) +
  labs(title="",y="MSPE", x="# of active group") +
  theme_bw()


gaussian_line <- ggpubr::ggarrange(p_F1_1+theme(axis.title.x = element_blank()),
                                   p_Precision_1+theme(axis.title.x = element_blank()),
                                   p_TPR_1+theme(axis.title.x = element_blank()),
                                   p_deviance_1+theme(axis.title.x = element_blank()),
                                   p_F1_2+theme(axis.title.x = element_blank()),
                                   p_Precision_2+theme(axis.title.x = element_blank()),
                                   p_TPR_2,p_deviance_2,
                                   nrow=4,ncol=2, align = "hv",
                                   common.legend = TRUE,legend="bottom")
#gaussian_line

#### binomial data ####
n=50
p=20
rho=0
family= "binomial" 

warm.str="elastic" 

filename1 = sprintf("%s/example2_%s_%s_n%dp%drho%.3f_seed%d.rds",
                   outdir, family, warm.str, n, p, rho, seed)
plotdat1<-readRDS(filename1)

plotdat1$siblings <- add_metrics(plotdat1$siblings)
plotdat1$cousins  <- add_metrics(plotdat1$cousins)

metrics <- names(plotdat1[[1]]) 

plotdat1 <- do.call(rbind,lapply(metrics, function(m) {
  df <- make_metric_df(plotdat1, m)
  df$metric <- m
  df
})
)


rho=0.707

filename2 = sprintf("%s/example2_%s_%s_n%dp%drho%.3f_seed%d.rds",
                   outdir, family, warm.str, n, p, rho, seed)
plotdat2<-readRDS(filename2)

plotdat2$siblings <- add_metrics(plotdat2$siblings)
plotdat2$cousins  <- add_metrics(plotdat2$cousins)

metrics <- names(plotdat2[[1]]) 

plotdat2 <- do.call(rbind,lapply(metrics, function(m) {
  df <- make_metric_df(plotdat2, m)
  df$metric <- m
  df
})
)


#### draw lineplot for binomial ####
target_method <- c("glmcme(baseline)","glmcme(ridge)","Lasso","adpLasso","MCP","GEL")
pbino_TPR_1<-as.data.frame(plotdat1) %>% filter(metric=="TPR") %>%
  group_by(effect, method, GxAy) %>%
  filter(method %in% target_method) %>% 
  summarise(mean_num = mean(as.numeric(num),na.rm=TRUE)) %>%
  ggplot(aes(x = factor(GxAy, levels=c("G4A2", "G6A2", "G8A2", "G10A2", "G12A2")), y = mean_num, colour = method, group = method)) +
  facet_wrap(~effect,scales="free_y") +
  geom_line() +
  geom_point() +
  scale_x_discrete(labels=c('4', '6', '8', '10','12'))+
  scale_color_manual(
    values = c("black","red","blue", "lightblue","#0CB702","purple"),
    breaks=target_method,
    labels = c("glmcme(baseline)","adaptive glmcme","Lasso","adaptive Lasso","MCP","GEL")
  ) +
  labs(title = "", y = "TPR", x="# of active group") +
  theme_bw()

pbino_Precision_1<-as.data.frame(plotdat1) %>% filter(metric=="precision") %>%
  group_by(effect, method, GxAy) %>%
  filter(method %in% target_method) %>% 
  summarise(mean_num = mean(as.numeric(num),na.rm=TRUE)) %>%
  ggplot(aes(x = factor(GxAy, levels=c("G4A2", "G6A2", "G8A2", "G10A2", "G12A2")), y = mean_num, colour = method, group = method)) +
  facet_wrap(~effect,scales="free_y") +
  geom_line() +
  geom_point() +
  scale_x_discrete(labels=c('4', '6', '8', '10','12'))+
  scale_color_manual(
    values = c("black","red","blue", "lightblue","#0CB702","purple"),
    breaks=target_method,
    labels = c("glmcme(baseline)","adaptive glmcme","Lasso","adaptive Lasso","MCP","GEL")
  ) +
  labs(title = "", y = "Precision", x="# of active group") +
  theme_bw()

pbino_F1_1<-as.data.frame(plotdat1) %>% filter(metric=="F1") %>%
  group_by(effect, method, GxAy) %>% filter(method %in% target_method) %>% summarise(mean_num = mean(as.numeric(num),na.rm=TRUE)) %>%
  ggplot(aes(x = factor(GxAy, levels=c("G4A2", "G6A2", "G8A2", "G10A2", "G12A2")), y = mean_num, colour = method, group = method)) +
  facet_wrap(~effect,scales="free_y")+
  geom_line() +
  geom_point() + 
  scale_x_discrete(labels=c('4', '6', '8', '10','12'))+
  scale_color_manual(
    values = c("black","red","blue", "lightblue","#0CB702","purple"),
    breaks=target_method,
    labels = c("glmcme(baseline)","adaptive glmcme","Lasso","adaptive Lasso","MCP","GEL")
  ) +
  labs(title="",y="F1 score",x="# of active group") +
  theme_bw()

pbino_misclassific_1<-as.data.frame(plotdat1) %>% filter(metric=="class") %>%
  group_by(effect, method, GxAy) %>% filter(method %in% target_method) %>% summarise(mean_num = mean(as.numeric(num),na.rm=TRUE)) %>%
  ggplot(aes(x = factor(GxAy, levels=c("G4A2", "G6A2", "G8A2", "G10A2", "G12A2")), y = mean_num, colour = method, group = method)) +
  facet_wrap(~effect,scales="free_y")+
  geom_line() +
  geom_point()  + 
  scale_x_discrete(labels=c('4', '6', '8', '10','12'))+
  scale_color_manual(
    values = c("black","red","blue", "lightblue","#0CB702","purple"),
    breaks=target_method,
    labels = c("glmcme(baseline)","adaptive glmcme","Lasso","adaptive Lasso","MCP","GEL")
  ) +
  labs(title="",y="Misclassific", x="# of active group") +
  theme_bw()

pbino_TPR_2<-as.data.frame(plotdat2) %>% filter(metric=="TPR") %>%
  group_by(effect, method, GxAy) %>%
  filter(method %in% target_method) %>% 
  summarise(mean_num = mean(as.numeric(num),na.rm=TRUE)) %>%
  ggplot(aes(x = factor(GxAy, levels=c("G4A2", "G6A2", "G8A2", "G10A2", "G12A2")), y = mean_num, colour = method, group = method)) +
  facet_wrap(~effect,scales="free_y") +
  geom_line() +
  geom_point() +
  scale_x_discrete(labels=c('4', '6', '8', '10','12'))+
  scale_color_manual(
    values = c("black","red","blue", "lightblue","#0CB702","purple"),
    breaks=target_method,
    labels = c("glmcme(baseline)","adaptive glmcme","Lasso","adaptive Lasso","MCP","GEL")
  ) +
  labs(title = "", y = "TPR", x="# of active group") +
  theme_bw()

pbino_Precision_2<-as.data.frame(plotdat2) %>% filter(metric=="precision") %>%
  group_by(effect, method, GxAy) %>%
  filter(method %in% target_method) %>% 
  summarise(mean_num = mean(as.numeric(num),na.rm=TRUE)) %>%
  ggplot(aes(x = factor(GxAy, levels=c("G4A2", "G6A2", "G8A2", "G10A2", "G12A2")), y = mean_num, colour = method, group = method)) +
  facet_wrap(~effect,scales="free_y") +
  geom_line() +
  geom_point() +
  scale_x_discrete(labels=c('4', '6', '8', '10','12'))+
  scale_color_manual(
    values = c("black","red","blue", "lightblue","#0CB702","purple"),
    breaks=target_method,
    labels = c("glmcme(baseline)","adaptive glmcme","Lasso","adaptive Lasso","MCP","GEL")
  ) +
  labs(title = "", y = "Precision", x="# of active group") +
  theme_bw()

pbino_F1_2<-as.data.frame(plotdat2) %>% filter(metric=="F1") %>%
  group_by(effect, method, GxAy) %>% filter(method %in% target_method) %>% summarise(mean_num = mean(as.numeric(num),na.rm=TRUE)) %>%
  ggplot(aes(x = factor(GxAy, levels=c("G4A2", "G6A2", "G8A2", "G10A2", "G12A2")), y = mean_num, colour = method, group = method)) +
  facet_wrap(~effect,scales="free_y")+
  geom_line() +
  geom_point() + 
  scale_x_discrete(labels=c('4', '6', '8', '10','12'))+
  scale_color_manual(
    values = c("black","red","blue", "lightblue","#0CB702","purple"),
    breaks=target_method,
    labels = c("glmcme(baseline)","adaptive glmcme","Lasso","adaptive Lasso","MCP","GEL")
  ) +
  labs(title="",y="F1 score",x="# of active group") +
  theme_bw()

pbino_misclassific_2<-as.data.frame(plotdat2) %>% filter(metric=="class") %>%
  group_by(effect, method, GxAy) %>% filter(method %in% target_method) %>% summarise(mean_num = mean(as.numeric(num),na.rm=TRUE)) %>%
  ggplot(aes(x = factor(GxAy, levels=c("G4A2", "G6A2", "G8A2", "G10A2", "G12A2")), y = mean_num, colour = method, group = method)) +
  facet_wrap(~effect,scales="free_y")+
  geom_line() +
  geom_point()  + 
  scale_x_discrete(labels=c('4', '6', '8', '10','12'))+
  scale_color_manual(
    values = c("black","red","blue", "lightblue","#0CB702","purple"),
    breaks=target_method,
    labels = c("glmcme(baseline)","adaptive glmcme","Lasso","adaptive Lasso","MCP","GEL")
  ) +
  labs(title="",y="Misclassific", x="# of active group") +
  theme_bw()

binomial_line <- ggpubr::ggarrange(pbino_F1_1+theme(axis.title.x = element_blank()),
                                   pbino_Precision_1+theme(axis.title.x = element_blank()),
                                   pbino_TPR_1+theme(axis.title.x = element_blank()),
                                   pbino_misclassific_1+theme(axis.title.x = element_blank()),
                                   pbino_F1_2+theme(axis.title.x = element_blank()),
                                   pbino_Precision_2+theme(axis.title.x = element_blank()),
                                   pbino_TPR_2,pbino_misclassific_2,
                                   nrow=4,ncol=2, align = "hv",
                                   common.legend = TRUE,legend="bottom")
#binomial_line

figure6 <- ggpubr::ggarrange(gaussian_line,binomial_line,nrow=1,ncol=2, align = "hv",
                             common.legend = TRUE,legend="bottom")
#figure6
pdf(file=sprintf("%s/figure6_example2_seed%d.pdf",outdir, seed),width = 15, height = 8)
print(figure6)
dev.off()
