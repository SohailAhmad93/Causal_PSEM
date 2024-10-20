# Load necessary libraries
library(simcausal)
library(dplyr)
library(ggplot2)
library(piecewiseSEM)  # For the psem model
library(ggdag)  # For DAG visualization

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

# Step 1: Define the DAG for 19 binary and 6 continuous variables

# Initialize an empty DAG using simcausal
dag <- DAG.empty()

# 19 binary variables (b1 to b19)
for (i in 1:19) {
  dag <- dag + node(paste0("b", i), distr = "rbern", prob = 0.5)  # Binary variable with 50% chance
}

# 6 continuous variables (c1 to c6)
for (i in 1:6) {
  dag <- dag + node(paste0("c", i), distr = "rnorm", mean = 0, sd = 1)  # Continuous variable, standard normal distribution
}

# Treatment influenced by all binary and continuous variables
dag <- dag + node("treat", distr = "rbern",
                  prob = plogis(-1 + 0.2 * b1 + 0.3 * b2 + 0.1 * b3 + 0.4 * c1 + 0.2 * c2 + 
                                  0.3 * b4 + 0.2 * b5 + 0.1 * c3 + 0.3 * b6 + 0.2 * b7 + 
                                  0.1 * b8 + 0.3 * c4 + 0.4 * b9 + 0.2 * c5 + 0.5 * b10 + 
                                  0.2 * b11 + 0.3 * b12 + 0.4 * c6 + 0.1 * b13 + 0.2 * b14 +
                                  0.3 * b15 + 0.2 * b16 + 0.4 * b17 + 0.1 * b18 + 0.5 * b19))

# Outcome influenced by treatment and all binary and continuous variables
dag <- dag + node("yfact", distr = "rnorm",
                  mean = 5 + 2 * treat + 0.3 * b1 + 0.2 * b2 + 0.4 * b3 + 0.1 * c1 + 0.5 * c2 +
                    0.2 * b4 + 0.3 * b5 + 0.4 * c3 + 0.1 * b6 + 0.2 * b7 +
                    0.3 * b8 + 0.2 * b9 + 0.4 * c4 + 0.5 * b10 + 0.3 * c5 +
                    0.1 * b11 + 0.4 * b12 + 0.2 * b13 + 0.3 * b14 + 0.1 * b15 +
                    0.5 * c6 + 0.2 * b16 + 0.3 * b17 + 0.4 * b18 + 0.2 * b19,
                  sd = 3)

# Set the DAG structure
D <- set.DAG(dag)

# Step 2: Define a function to simulate data and apply the psem model

simulate_and_apply_psem <- function(n) {
  # Simulate data for a given sample size
  set.seed(42)  # For reproducibility
  sim_data <- sim(DAG = D, n = n)
  
  # Apply the psem model
  psem_model <- psem(
    glm(yfact ~ treat + b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 + b9 + 
          b10 + b11 + b12 + b13 + b14 + b15 + b16 + b17 + b18 + b19 + 
          c1 + c2 + c3 + c4 + c5 + c6, data = sim_data),
    glm(treat ~ b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 + b9 + b10 + 
          b11 + b12 + b13 + b14 + b15 + b16 + b17 + b18 + b19 + 
          c1 + c2 + c3 + c4 + c5 + c6, data = sim_data)
  )

  # Calculate AIC
  aic_total <- AIC(psem_model)
  
  # Capture summary statistics from the simulated data
  sim_summary <- sim_data %>%
    summarize(
      mean_yfact = mean(yfact),
      sd_yfact = sd(yfact),
      prop_treated = mean(treat),
      mean_b1 = mean(b1),  # Example summary for b1, you can add more summaries for b2 to b19 and c1 to c6
      mean_c1 = mean(c1)
    )
  
  # Return the summary statistics and AIC value
  return(cbind(sim_summary, AIC = aic_total))
}

# Step 3: Run the simulations for different sample sizes and collect results into one data frame

sample_sizes <- c(500, 1000, 1500, 2000)
results <- data.frame()

for (n in sample_sizes) {
  result <- simulate_and_apply_psem(n)
  result <- cbind(Sample_Size = n, result)  # Add the sample size column
  results <- rbind(results, result)  # Append the result to the results table
}

# Step 4: Display the combined results table
result=print(results)

result_df <- as.data.frame(result)

# Save the table as a CSV file
write.csv(result_df, file = "simulation_IHDP.csv", row.names = FALSE)


# Load necessary libraries
library(simcausal)
library(ggdag)
library(ggplot2)

# Define the DAG with custom coordinates for better spacing between treat and yfact
dag_graph <- dagify(
  yfact ~ treat + b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 + b9 + b10 + 
    b11 + b12 + b13 + b14 + b15 + b16 + b17 + b18 + b19 + 
    c1 + c2 + c3 + c4 + c5 + c6,
  treat ~ b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 + b9 + b10 + 
    b11 + b12 + b13 + b14 + b15 + b16 + b17 + b18 + b19 + 
    c1 + c2 + c3 + c4 + c5 + c6,
  exposure = "treat",
  outcome = "yfact",
  coords = list(
    x = c(treat = 5, yfact = 10, 
          b1 = 1, b2 = 2, b3 = 3, b4 = 4, b5 = 5, b6 = 6, b7 = 7, b8 = 8, b9 = 9, b10 = 10,
          b11 = 1, b12 = 2, b13 = 3, b14 = 4, b15 = 5, b16 = 6, b17 = 7, b18 = 8, b19 = 9,
          c1 = 5, c2 = 6, c3 = 7, c4 = 8, c5 = 9, c6 = 10),
    y = c(treat = 5, yfact = 2, 
          b1 = 6, b2 = 6, b3 = 6, b4 = 6, b5 = 6, b6 = 6, b7 = 6, b8 = 6, b9 = 6, b10 = 6,
          b11 = 7, b12 = 7, b13 = 7, b14 = 7, b15 = 7, b16 = 7, b17 = 7, b18 = 7, b19 = 7,
          c1 = 4, c2 = 4, c3 = 4, c4 = 4, c5 = 4, c6 = 4)
  )
)

# Simplify the DAG by reducing node size and edge thickness
ggdag(dag_graph, text = TRUE) +  
  geom_dag_node(size = 3, color = "black", alpha = 0) +  # Smaller, transparent nodes
  geom_dag_edges(edge_width = 0.5, color = "black") +  # Thinner, simple edges
  theme_dag(base_size = 8) +  # Smaller text to fit more variables
  ggtitle("") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),  # Centered title
    legend.position = "none",  # No legend for simplicity
    panel.background = element_rect(fill = "white", colour = "white"),  # White background
    panel.grid.major = element_blank(),  # No grid lines
    panel.grid.minor = element_blank()   # No minor grid lines
  )

library(ggplot2)

# Plot mean_yfact and AIC across sample sizes
plot1_sim2 <- ggplot(results, aes(x = Sample_Size)) +
  geom_line(aes(y = as.numeric(mean_yfact), color = "Mean yfact"), size = 1.2) +
  geom_point(aes(y = as.numeric(mean_yfact), color = "Mean yfact"), size = 3) +
  geom_line(aes(y = as.numeric(AIC.AIC) / 200, color = "AIC"), size = 1.2, linetype = "dashed") +  # Rescale AIC
  geom_point(aes(y = as.numeric(AIC.AIC) /200, color = "AIC"), size = 3) +  # Rescale AIC
  scale_y_continuous(
    name = "Mean yfact",
    sec.axis = sec_axis(~ . * 200, name = "AIC")  # Reverse the scaling on the secondary axis
  ) +
  scale_color_manual(values = c("Mean yfact" = "blue", "AIC" = "green")) +
  labs(title = "Simulation 2: Mean yfact and AIC Across Sample Sizes") +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "blue", size = 12),
    axis.title.y.right = element_text(color = "green", size = 12),
    legend.position = "top"
  )

# Display the plot
print(plot1_sim2)


# Bar plot for mean_yfact with error bars
plot2_sim2 <- ggplot(results, aes(x = factor(Sample_Size), y = mean_yfact)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = mean_yfact - sd_yfact, ymax = mean_yfact + sd_yfact), width = 0.2, color = "red") +
  labs(title = "Simulation 2: Mean yfact with Standard Deviation Across Sample Sizes", 
       x = "Sample Size", y = "Mean yfact") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

# Display the second plot
print(plot2_sim2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         