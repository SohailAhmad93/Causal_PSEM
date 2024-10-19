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
df <- df %>% clean_names() # tidy variable names 
df$race <- make_clean_names(df$race, allow_dupes = TRUE) # tidy variable levels
df$race <- as.factor(df$race) # Coerce variable to factor


head(df)
str(df)

GGally::ggpairs(df)



df %>%
  mutate(married = ifelse(married >= 1, "Married", "Not Married"),
         nodegree = ifelse(nodegree >= 1, "No Degree", "Degree")) %>%
  ggplot(aes(x = age^2, y = educ, color = race)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  geom_point(pch = 21, size = 2) +
  xlab("Age") +
  theme_bw() +
  ggtitle("Relationship between age and education split by marrage status, degree, \nand treatment. Colored points indicate race.") +
  facet_grid(treat ~ married + nodegree)


df %>%
  mutate(married = ifelse(married >= 1, "Married", "Not Married"),
         nodegree = ifelse(nodegree >= 1, "No Degree", "Degree")) %>%
  ggplot(aes(y = re78^2, x = age^2, color = race)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  geom_point(pch = 21, size = 2) +
  xlab("Age") +
  ylab("Income (re78)") +
  ggtitle("Relationship between age and income split by marrage status, degree, \nand treatment. Colored points indicate race") +
  facet_grid(treat ~ married + nodegree)



simple_dag <- dagify(
  re78 ~ trt + age + educ + race + mar + degree,
  trt ~ race,
  educ ~ age,
  mar ~ age,
  degree ~ educ,
  educ ~ race,
  exposure = "trt",
  outcome = "re78",
  coords = list(x = c(trt = 1, age = 1.5, educ = 2, re78 = 3, race = 1.5, mar = 2.5, degree = 2.5),
                y = c(trt = 2, age = 1, educ = 1.7, re78 = 2, race = 2.5, mar = 1, degree = 2.5))
)

ggdag_status(simple_dag, layout = "nicely") + theme_dag()

# Model
v1.sem <-psem(
  glm(re78 ~ treat + age + educ + race + married + nodegree, 
      data = df),
  glm(treat ~ race, 
      data = df),
  glm(educ ~ age + race, 
      data = df),
  glm(married ~ age, 
      data = df),
  glm(nodegree ~ educ, 
      data = df)
)


summary(v1.sem)

dSep_results <- dSep(v1.sem)

# Convert the dSep results to a data frame
dSep_df <- as.data.frame(dSep_results)

# Save the results to a CSV file
write.csv(dSep_df, "dSepV1_results.csv", row.names = FALSE)


table <- data.frame(summary(v1.sem)$coefficients) %>% 
  rename(P.astrix = Var.9)


# Save the table as a CSV file
write.csv(table, file = "initial.csv", row.names = FALSE)


# Create super basic DAG
simple_dag <- dagify(
  re78 ~ trt + age + educ + race + mar + degree,
  trt ~ race + degree + mar + educ,
  educ ~ age + race,
  mar ~ age + race,
  degree ~ educ + age + race,
  educ ~ race,
  exposure = "trt",
  outcome = "re78",
  coords = list(x = c(trt = 1, age = 1.5, educ = 2.5, re78 = 3, race = 1.5, mar = 2.5, degree = 2.5),
                y = c(trt = 2, age = 1, educ = 1.85, re78 = 2, race = 2.5, mar = 1, degree = 2.5))
)

ggdag_status(simple_dag, layout = "nicely") + theme_dag()


v2.sem <- psem(
  glm(re78 ~ treat + age + educ + race + married + nodegree, 
      data = df),
  glm(treat ~ race + married + nodegree + educ, 
      data = df),
  glm(educ ~ age + race, 
      data = df),
  glm(married ~ age + race, 
      data = df),
  glm(nodegree ~ educ + age + race, 
      data = df)
)


dSep(v2.sem)

summary(v2.sem)



dSep_results <- dSep(v2.sem)

# Convert the dSep results to a data frame
dSep_df <- as.data.frame(dSep_results)

# Save the results to a CSV file
write.csv(dSep_df, "dSep_results.csv", row.names = FALSE)


table <- data.frame(summary(v2.sem)$coefficients) %>% 
  rename(P.astrix = Var.9)

table_2<- data.frame(dSep(v2.sem))

table %>%
  kable(format = "html", caption = "Summary of SEM Coefficients") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Save the table as a CSV file
write.csv(table, file = "summary_JOB2.csv", row.names = FALSE)



