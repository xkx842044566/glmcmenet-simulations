# 00_renv_setup.R
# One-time script used by the authors to create the project-local renv environment.
# You normally do NOT need to run this to reproduce the results.
# To install the exact package versions, simply run `renv::restore()` in the
# project root (where `renv.lock` lives). This script is kept only for transparency.

if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv", repos = "https://cloud.r-project.org")
renv::init(bare = TRUE)

# CRAN repos (stable mirror)
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install CRAN deps
pkgs <- c("MASS","glmnet","dplyr","tidyr","stringr","ggplot2","ggpubr",
          "gridExtra","grpreg","ncvreg","hierNet","parallel",
          "foreach","doParallel")
install.packages(pkgs)

# GitHub deps (pin to a commit if possible)
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("xkx842044566/glmcmenet@main", upgrade = "never", force = TRUE)
install.packages("cmenet")  # CRAN

renv::snapshot(prompt = FALSE)