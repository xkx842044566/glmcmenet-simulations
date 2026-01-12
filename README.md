
# Simulation code for Conditional Main Effects (Figure 6: Example 2)

This directory contains the R code to reproduce the Example 2 simulations
and Figure 6 in the conditional main effects (CME) manuscript.

The workflow is:

1. Set up the R package environment with **renv**.  
2. Run the simulation driver to generate `.rds` result files.  
3. Run the plotting script to compute metrics and draw Figure 6.  

---

## Directory structure

- `Rcode.Rproj`               – RStudio project file (optional but convenient)
- `00_renv_setup.R`           – one-time script used by the authors to create `renv.lock`
- `01_example2_simulation.R`  – functions implementing the CME simulation models
- `02_run_simulation.R`       – **main driver** that runs the simulations and saves results
- `03_draw_figure6.R`         – post-processing and plotting (precision, TPR, F1, Figure 6)
- `renv.lock`                 – frozen package versions for reproducibility
- `renv/`                     – renv project library metadata
- `results/`                  – output directory for `.rds` simulation results (created automatically)

---

## Requirements

- R (version ≥ 4.1 recommended)
- Internet access the first time you install packages
- On multi-core machines, the scripts use `parallel`, `foreach`, and `doParallel`,
  but they will also run on a single core.
- Platform note (for packages compiled from source):
  - Windows users may need **Rtools**
  - macOS users may need **Xcode Command Line Tools** (`xcode-select --install`)

All package versions used in the paper are recorded in `renv.lock`.

---

## 1. Set up the environment with renv

From the project root (the folder containing `Rcode.Rproj` and `renv.lock`), run:

```r
install.packages("renv")   # once per machine
renv::restore()            # installs the versions recorded in renv.lock
```

This step may take several minutes the first time. You **do not** need to run
`00_renv_setup.R`; that file was only used to create `renv.lock`.

### Installing glmcmenet (GitHub dependency)

The simulations depend on the `glmcmenet` package, which is installed from GitHub and recorded in renv.lock.

In most cases, glmcmenet will be installed automatically when you run:

renv::restore()

If glmcmenet is not installed correctly (e.g., due to network or compilation issues), you can install it manually before or after running renv::restore():

```r
install.packages("remotes")
remotes::install_github("xkx842044566/glmcmenet")
```

To verify installation:

```r
library(glmcmenet)
```

---


## 2. Run the simulations

To reproduce the Example 2 simulation results:

```r
source("02_run_simulation.R")
```

or from a terminal:

```bash
Rscript 02_run_simulation.R
```

The script:

- sources `01_example2_simulation.R`
- sets the simulation parameters.
- runs the Gaussian and GLM simulation
- saves the output as `.rds` files in the `results/` directory.

```text
results/example2_<family>_<warm.str>_n<n>p<p>rho<rho>_seed<seed>.rds
```

You can edit `02_run_simulation.R` to change these settings.

---

## 3. Create summary metrics and Figure 6

Once the `.rds` files are available in `results/`, run:

```r
source("03_draw_figure6.R")
```

This script:

- loads the simulation results,
- computes summary metrics (precision, TPR, F1, and losses) for each method,
  effect type (siblings/cousins), and configuration `GxAy`,
- generates Figure 6 using `ggplot2`.

The figure is saved according to the file name specified near the bottom of
`03_draw_figure6.R` (e.g., `figure6.pdf`).

---

## 4. (Optional) Running on a cluster

If you are using a Slurm cluster, you can run `02_run_simulation.R` via a batch
script that calls:

```bash
Rscript --vanilla 02_run_simulation.R
```

Make sure the working directory for the job is the project root so that the
relative paths (e.g., `results/`) are correct, and export `OMP_NUM_THREADS` and
related variables to match `SLURM_CPUS_PER_TASK` if you want controlled
multi-core behavior.

---

## Troubleshooting

If you encounter issues, please check that:

- you are running R from the project root (so `results/` is found correctly),
- `renv::restore()` completed without errors, and
- the `.rds` files exist in `results/` before running `03_draw_figure6.R`.

If problems persist, double-check package versions with `renv::status()`
and confirm that you are using the same R version (or a compatible one).
