# Installs the R packages used by analysis.R (run once).
pkgs <- c("readxl", "dplyr", "tidyr", "stringr", "lme4", "lmerTest",
          "emmeans", "effectsize", "ggplot2", "ggeffects")
need <- setdiff(pkgs, rownames(installed.packages()))
if (length(need)) install.packages(need)
