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

# Load necessary libraries
library(simcausal)
library(dplyr)
library(ggplot2)
library(piecewiseSEM)  # For the psem model
library(ggdag)         # For DAG visualization

# Step 1: Define the Further Revised DAG

# Initialize an empty DAG using simcausal
dag <- DAG.empty()

# Define the updated structure of the DAG with direct links between 'degree' and 'mar', and 'educ' and 'mar'
dag <- dag + node("race", distr = "rbern", prob = 0.5)  # 50% chance of being black or white
dag <- dag + node("age", distr = "rnorm", mean = 30, sd = 10)  # Normal distribution for age

dag <- dag + node("educ", distr = "rnorm",
                  mean = 12 + 0.4 * age + 0.2 * race,
                  sd = 2)  # Variability in education influenced by age and race

dag <- dag + node("mar", distr = "rbern",
                  prob = plogis(-4 + 0.3 * age + 0.1 * race + 0.1 * educ))  # Lower the probability for more variability in marriage

dag <- dag + node("degree", distr = "rbern",
                  prob = plogis(-5 + 0.3 * educ + 0.1 * age + 0.1 * race + 0.2 * mar))  # Adjusted to include 'mar'

dag <- dag + node("trt", distr = "rbern",
                  prob = plogis(-4 + 0.2 * race + 0.3 * degree + 0.15 * mar + 0.3 * educ))  # Treatment influenced by race, degree, and education

dag <- dag + node("re78", distr = "rnorm",
                  mean = 15 + 1.5 * trt + 0.4 * age + 0.8 * educ + 0.2 * race + 0.3 * mar + 0.6 * degree,
                  sd = 7)  # Income influenced by treatment, education, age, race, etc.

# Set the DAG structure
D <- set.DAG(dag)

# Step 2: Define a function to simulate data, apply psem model, and extract independence claims

simulate_and_apply_psem <- function(n) {
  # Simulate data for a given sample size
  set.seed(42)
  sim_data <- sim(DAG = D, n = n)
  
  # Convert race to a factor variable for the model
  sim_data <- sim_data %>%
    mutate(race = factor(ifelse(race == 1, "black", "white")))
  
  # Apply the psem model
  psem_model <- psem(
    glm(re78 ~ trt + age + educ + race + mar + degree, data = sim_data),
    glm(trt ~ race + degree + mar + educ, data = sim_data),
    glm(educ ~ age + race, data = sim_data),
    glm(mar ~ age + race + educ, data = sim_data),  # Updated to include 'educ'
    glm(degree ~ educ + age + race + mar, data = sim_data)  # Updated to include 'mar'
  )
  
  # Calculate AIC
  aic_total <- AIC(psem_model)
  
  # Capture summary statistics from the simulated data
  sim_summary <- sim_data %>%
    summarize(
      mean_re78 = mean(re78),
      sd_re78 = sd(re78),
      mean_age = mean(age),
      sd_age = sd(age),
      prop_treated = mean(trt),
      prop_married = mean(mar),
      mean_educ = mean(educ),
      mean_degree = mean(degree)
    )
  
  # Extract directed separation (d-sep) tests for independence claims
  d_sep_tests <- summary(psem_model)$dTable
  
  # Add sample size information to d-sep tests
  d_sep_tests$Sample_Size <- n
  
  # Return the summary statistics, AIC value, and d-separation results
  return(list(summary = cbind(sim_summary, AIC = aic_total), d_sep_tests = d_sep_tests))
}

# Step 3: Run the simulations for different sample sizes and collect results into one data frame

sample_sizes <- c(500, 1000, 1500, 2000)
results <- data.frame()
d_sep_results <- data.frame()

for (n in sample_sizes) {
  res <- simulate_and_apply_psem(n)
  
  # Append summary statistics and AIC to the results table
  results <- rbind(results, cbind(Sample_Size = n, res$summary))
  
  # Append d-separation test results to the d-separation results table
  d_sep_results <- rbind(d_sep_results, res$d_sep_tests)
}

# Step 4: Display the combined results table
print(results)
print(d_sep_results)

# Save the tables as CSV files
write.csv(results, file = "NNimproved_simulation_JOB_summary.csv", row.names = FALSE)
write.csv(d_sep_results, file = "NNimproved_simulation_JOB_d_sep.csv", row.names = FALSE)

# Step 5: Visualize the Revised DAG

# Define the revised DAG using ggdag (for visualization)
dag_graph <- dagify(
  re78 ~ trt + age + educ + race + mar + degree,
  trt ~ race + degree + mar + educ,
  educ ~ age + race,
  mar ~ age + race + educ,  # Added direct link from educ to mar
  degree ~ educ + age + race + mar,  # Added direct link between degree and mar
  exposure = "trt",
  outcome = "re78"
)

# Plot the further revised DAG
ggdag(dag_graph, text = TRUE) +
  ggtitle("") +
  theme_dag()


# Adjust the plot to rescale AIC values for better visualization
plot1 <- ggplot(results, aes(x = Sample_Size)) +
  geom_line(aes(y = as.numeric(mean_re78), color = "Mean re78"), size = 1.2) +
  geom_point(aes(y = as.numeric(mean_re78), color = "Mean re78"), size = 3) +
  geom_line(aes(y = as.numeric(AIC.AIC) / 500, color = "AIC"), size = 1.2, linetype = "dashed") +  # Rescale AIC
  geom_point(aes(y = as.numeric(AIC.AIC) / 500, color = "AIC"), size = 3) +  # Rescale AIC
  scale_y_continuous(
    name = "Mean re78",
    sec.axis = sec_axis(~ . * 500, name = "AIC")  # Reverse the scaling on the secondary axis
  ) +
  scale_color_manual(values = c("Mean re78" = "blue", "AIC" = "green")) +
  labs(title = "Visualization: Mean re78 and AIC Across Sample Sizes") +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "blue", size = 12),
    axis.title.y.right = element_text(color = "green", size = 12),
    legend.position = "top"
  )

# Display the first plot
print(plot1)



# Second advanced plot: Bar plot with error bars for sd_re78
plot2 <- ggplot(results, aes(x = factor(Sample_Size), y = mean_re78)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = mean_re78 - sd_re78, ymax = mean_re78 + sd_re78), width = 0.2, color = "red") +
  labs(title = "Mean re78 with Standard Deviation Across Sample Sizes", x = "Sample Size", y = "Mean re78") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

print(plot2)



