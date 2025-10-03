# glmcmenet-paper-repro

Reproduction code for the paper  
**“Adaptive Bi-Level Variable Selection of Conditional Main Effects for Generalized Linear Models”**  
by **Kexin Xie** and **Xinwei Deng**.

This repository contains self-contained R scripts that regenerate the simulation results used in the paper.  
The code fixes random seeds, restores an isolated R environment with **renv**, and writes outputs to `results/`.

---

## Repository layout
Simulation/
├── 00_renv_setup.R          # one-time: restore the exact package environment via renv
├── 01_example2_simulation.R # reproduces the Example 2 experiment
├── 02_run_simulation.R      # full pipeline: runs all sims and writes results/
├── renv/                    # renv infrastructure (do not edit)
├── renv.lock                # locked package versions for reproducibility
├── results/                 # generated RDS file after running scripts
└── Rcode.Rproj              # optional RStudio project file

---

## Prerequisites

- **R ≥ 4.2** (tested on macOS and Linux; Windows should also work)
- System toolchain to build packages from source  
  - macOS: Xcode Command Line Tools (`xcode-select --install`)  
  - Linux: build essentials (e.g., `gcc`, `gfortran`, `make`)
- Internet access for the first run to restore the environment

You also need the companion package **`glmcmenet`** (the implementation of the method):

```r
install.packages("remotes")
remotes::install_github("xkx842044566/glmcmenet")
```

---

## Quick start (one command)

From the repo root (or the `Simulation/` folder), run:

```bash
Rscript Simulation/02_run_simulation.R
```

This will:

1. Ensure **renv** is installed and restore the locked package versions (`renv.lock`).
2. Load `glmcmenet` and other dependencies.
3. Run the simulation(s) with fixed random seeds.
4. Save outputs to Simulation/results/.

---

## Step-by-step

If you want to run scripts individually:

### 1) Restore the environment once

```r
# working directory = Simulation/
source("00_renv_setup.R")
```

This installs/activates renv and restores all package versions listed in renv.lock.

### 2) Simulation codes

```r
source("01_example2_simulation.R")
```

### 3) Full pipeline

```r
source("02_run_simulation.R")
```

This script runs all configured experiments and writes data into `results/`.

---

## Reproducibility notes

- Each script sets a fixed seed (`set.seed(...)`) and, where relevant, fixes CV folds to ensure identical runs across machines.

- All R package versions are locked in `renv.lock`. To rebuild precisely:

```r
renv::restore()  # executed automatically by 00_renv_setup.R or 02_run_simulation.R
```

- If you upgrade R or packages intentionally, do not commit the modified `renv.lock` unless you intend to update the official reproducible environment.

---

## Expected outputs

After a successful run you should find RDS files in `Simulation/results/`. Exact file names are commented at the top of each script.

---

## Citation

If you use this code or the package, please cite the paper:

> Xie, K. & Deng, X. (2025). Adaptive Bi-Level Variable Selection of Conditional Main Effects for Generalized Linear Models. 

---

## License

Code in this repository is released under the **MIT License**. See `LICENSE` if included, otherwise the same terms as the companion package.

---

## Contact

Questions or issues? Please open a GitHub issue or contact **Kexin Xie** at <kexinx@vt.edu>.

