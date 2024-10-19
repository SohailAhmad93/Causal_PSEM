#Install from GitHub

install.packages("remotes")
remotes::install_github("stan-dev/cmdstanr")



ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[,"Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
    cat("Not Installed:", paste(new.pkg, collapse = ", "), "\n")
  } else {
    cat("All Packages Installed :) \n")
  }
  invisible(sapply(pkg, function(x) {
    if (!require(x, character.only = TRUE)) {
      cat("Failed to load package:", x, "\n")
    }
  }))
}

packages <- c("tidyverse", "readr", "Matrix", "car", "lme4", "effects", "writexl", "httr", "blavaan",
              "performance", "glue", "readxl", "modelr", "DHARMa", "brms", "simDAG", "CausalImpact",
              "lubridate", "dagitty", "ggdag", "stringr", "kableExtra", "tidymodels",
              "bayesplot", "emmeans", "tidybayes", "bayestestR", "future",
              "GGally", "cmdstanr", "here", "janitor", "RColorBrewer", "piecewiseSEM")

bayesplot::color_scheme_set("blue")                                    # Set color scheme
options(mc.cores = parallel::detectCores())                            # Parallel Chains
cmdstanr::set_cmdstan_path(path = "C:/Program Files/R/cmdstan-2.35.0") # R-Stan
ipak(packages)                                                         # Install packages
packages



df <- read.csv(file.choose())


head(df)
str(df)
GGally::ggpairs(df)


df %>%
  ggplot(aes(x = as.factor(treatment), y = y_factual, color= as.factor(treatment))) +
  geom_boxplot() +
  geom_jitter(pch = 21, size = 2, width = 0.3, alpha = 0.2) +
  theme_bw()

simple_dag <- dagify(
  treat ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 + x23 + x24 + x25,
  y_fact ~ treat + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 + x23 + x24 + x25,
  exposure = "treat",
  outcome = "y_fact",
  coords = list(
    x = c(
      treat = 13, y_fact = 13, 
      x1 = 1, x2 = 2, x3 = 3, x4 = 4, x5 = 5, x6 = 6, x7 = 7, x8 = 8, 
      x9 = 9, x10 = 10, x11 = 11, x12 = 12, x13 = 13, x14 = 14, x15 = 15, 
      x16 = 16, x17 = 17, x18 = 18, x19 = 19, x20 = 20, x21 = 21, x22 = 22, x23 = 23, x24 = 24, x25 = 25
    ),
    y = c(
      treat = 2, y_fact = 1, 
      x1 = 3, x2 = 3, x3 = 3, x4 = 3, x5 = 3, x6 = 3, x7 = 3, x8 = 3, 
      x9 = 3, x10 = 3, x11 = 3, x12 = 3, x13 = 3, x14 = 3, x15 = 3, 
      x16 = 3, x17 = 3, x18 = 3, x19 = 3, x20 = 3, x21 = 3, x22 = 3, x23 = 3, x24 = 3, x25 = 3
    )
  )
)

ggdag_status(simple_dag) + theme_dag()

v1.sem <-psem(
  glm(treatment ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + X21 + X22 + X23 + X24 + X25, 
      data = df),
  glm(y_factual ~ treatment + X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + X21 + X22 + X23 + X24 + X25,
      data = df)
)

summary(v1.sem)




table <- data.frame(summary(v1.sem)$coefficients) %>% 
  rename(P.astrix = Var.9)

table %>%
  kable(format = "html", caption = "Summary of SEM Coefficients") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Save the table as a CSV file
write.csv(table, file = "summary_IHDP.csv", row.names = FALSE)
