# Load necessary libraries
library(boot)
library(ppcor)
library(openxlsx)
library(tidyverse)

# Define the correlation function to return only the required CI information
compute_correlations <- function(XYZ, method) {
  
  methods <- c("pearson", "spearman", "kendall")
  alpha <- 0.05
  n <- nrow(XYZ)  # Sample size
  
  results <- data.frame(
    Method = character(),
    Type = character(),
    CI.Lower = numeric(),
    CI.Upper = numeric(),
    Zero.Within.CI = logical(),
    stringsAsFactors = FALSE
  )
  
  # UNADJUSTED CORRELATION COEFFICIENTS # ------------------------------------ #
  
    # Unadjusted Parametric approach
    param_corr <- cor.test(XYZ[, 1], XYZ[, 2], method = method, exact = FALSE)
    param_est <- round(param_corr$estimate, 4)
    
    z_r <- 0.5 * log((1 + param_est) / (1 - param_est))
    z_crit <- qnorm(1 - alpha / 2)
    
    if (method == 'pearson') {
      se <- sqrt(1 / (n - 3))
    } else if (method == 'spearman') {
      se <- sqrt((1 + (param_est^2)/2) / (n - 3))
    } else if (method == 'kendall') {
      se <- sqrt(0.437 / (n - 4))
    }
    
    z_l <- z_r - (z_crit * se)
    z_u <- z_r + (z_crit * se)
    
    param_ci_lower <- round((exp(2 * z_l) - 1) / (exp(2 * z_l) + 1), 4)
    param_ci_upper <- round((exp(2 * z_u) - 1) / (exp(2 * z_u) + 1), 4)
    zero_within_ci <- param_ci_lower <= 0 && param_ci_upper >= 0
    
    # Append unadjusted parametric CI results to dataframe
    results <- rbind(
      results,
      data.frame(
        Method = method,
        Type = "Unadjusted Parametric",
        CI.Lower = param_ci_lower,
        CI.Upper = param_ci_upper,
        Zero.Within.CI = zero_within_ci
      )
    )
    
    # Unadjusted Bootstrap approach
    custom_boot_unadjusted <- function(data, indices, method) {
      sample_data <- data[indices, ]
      test_result <- cor.test(sample_data[, 1], sample_data[, 2], method = method)
      return(test_result$estimate)
    }
    
    boot_results_unadjusted <- boot(XYZ, statistic = custom_boot_unadjusted, R = 1000, method = method)
    boot_ci_unadjusted <- boot.ci(boot_results_unadjusted, type = "bca")
    
    boot_ci_lower_unadjusted <- round(boot_ci_unadjusted$bca[4], 4)
    boot_ci_upper_unadjusted <- round(boot_ci_unadjusted$bca[5], 4)
    zero_within_ci <- boot_ci_lower_unadjusted <= 0 && boot_ci_upper_unadjusted >= 0
    
    # Append unadjusted bootstrap CI results to dataframe
    results <- rbind(
      results,
      data.frame(
        Method = method,
        Type = "Unadjusted Bootstrap",
        CI.Lower = boot_ci_lower_unadjusted,
        CI.Upper = boot_ci_upper_unadjusted,
        Zero.Within.CI = zero_within_ci
      )
    )
    
  # PARTIAL CORRELATION COEFFICIENTS # --------------------------------------- #
    
    # Partial correlation (parametric)
    partial_corr <- pcor.test(
      x = XYZ[, 1], 
      y = XYZ[, 2], 
      z = XYZ[, 3], 
      method = method
    )
    
    p_corr_est <- round(partial_corr$estimate, 4)
    
    z_r <- 0.5 * log((1 + p_corr_est) / (1 - p_corr_est))
    
    if (method == 'pearson') {
      se <- sqrt(1 / (n - 3))
    } else if (method == 'spearman') {
      se <- sqrt((1 + (p_corr_est^2)/2) / (n - 3))
    } else if (method == 'kendall') {
      se <- sqrt(0.437 / (n - 4))
    }
    
    z_l <- z_r - (z_crit * se)
    z_u <- z_r + (z_crit * se)
    
    p_corr_ci_lower <- round((exp(2 * z_l) - 1) / (exp(2 * z_l) + 1), 4)
    p_corr_ci_upper <- round((exp(2 * z_u) - 1) / (exp(2 * z_u) + 1), 4)
    zero_within_ci <- p_corr_ci_lower <= 0 && p_corr_ci_upper >= 0
    
    # Append partial parametric CI results to dataframe
    results <- rbind(
      results,
      data.frame(
        Method = method,
        Type = "Partial Parametric",
        CI.Lower = p_corr_ci_lower,
        CI.Upper = p_corr_ci_upper,
        Zero.Within.CI = zero_within_ci
      )
    )
    
    # Bootstrap approach for partial correlation
    custom_boot_partial <- function(data, indices, method) {
      sample_data <- data[indices, ]
      boot_partial_corr <- pcor.test(
        x = sample_data[, 1],
        y = sample_data[, 2],
        z = sample_data[, 3],
        method = method
      )
      return(boot_partial_corr$estimate)
    }
    
    set.seed(0)
    boot_results_partial <- boot(XYZ, statistic = custom_boot_partial, R = 1000, method = method)
    boot_ci_partial <- boot.ci(boot_results_partial, type = "bca")
    
    boot_ci_lower_partial <- round(boot_ci_partial$bca[4], 4)
    boot_ci_upper_partial <- round(boot_ci_partial$bca[5], 4)
    zero_within_ci <- boot_ci_lower_partial <= 0 && boot_ci_upper_partial >= 0
    
    # Append partial bootstrap CI results to dataframe
    results <- rbind(
      results,
      data.frame(
        Method = method,
        Type = "Partial Bootstrap",
        CI.Lower = boot_ci_lower_partial,
        CI.Upper = boot_ci_upper_partial,
        Zero.Within.CI = zero_within_ci
      )
    )
  
  return(results)
    
}

#####################

# Loop for normal distribution results
norm_dist_results <- list()
for (i in 1:10000) {
  
  # Set a different seed each time
  set.seed(i)
  
  # output of iteration
  print(paste("Iteration:", i))
  
  # Experiment parameters
  n <- 100
  method <- 'pearson'
  
  # Generate constants
  a <- round(runif(1, min = 1, max = 5), 2)
  b <- round(runif(1, min = 1, max = 5), 2)
  
  # Generate synthetic data
  Z <- rnorm(n, mean = 0, sd = 1)
  X_noise <- rnorm(n, mean = 0, sd = 1)  # Independent noise for X
  Y_noise <- rnorm(n, mean = 0, sd = 1)  # Independent noise for Y
  
  X <- a * Z + X_noise
  Y <- b * Z + Y_noise
  XYZ <- round(data.frame(X = X, Y = Y, Z = Z), 2)
  
  # Save the correlation result to the list
  norm_dist_results[[i]] <- compute_correlations(XYZ, method = method)
  
}

# view results
norm_combined_df <- do.call(rbind, norm_dist_results)

# Calculate the proportion for Partial Parametric
norm_proportion_partial_parametric <- norm_combined_df %>%
  filter(Type == "Partial Parametric" & Zero.Within.CI == TRUE) %>%
  summarise(Proportion = n() / sum(norm_combined_df$Type == "Partial Parametric")) %>%
  pull(Proportion)

# Calculate the proportion for Partial Bootstrap
norm_proportion_partial_bootstrap <- norm_combined_df %>%
  filter(Type == "Partial Bootstrap" & Zero.Within.CI == TRUE) %>%
  summarise(Proportion = n() / sum(norm_combined_df$Type == "Partial Bootstrap")) %>%
  pull(Proportion)

# Output the results
list(norm_Partial_Parametric = norm_proportion_partial_parametric,
     norm_artial_Bootstrap = norm_proportion_partial_bootstrap)

#####################

# Loop for t distribution results
t_dist_results <- list()
for (i in 1:10000) {
  
  # Set a different seed each time
  set.seed(i)
  
  # output of iteration
  print(paste("Iteration:", i))
  
  # Experiment parameters
  n <- 100
  method <- 'pearson'
  
  # Generate constants
  a <- round(runif(1, min = 1, max = 5), 2)
  b <- round(runif(1, min = 1, max = 5), 2)
  
  # Generate synthetic data
  Z <- rt(n, df = n - 1, ncp = 2)
  X_noise <- rnorm(n, mean = 0, sd = 1)  # Independent noise for X
  Y_noise <- rnorm(n, mean = 0, sd = 1)  # Independent noise for Y
  
  X <- a * Z + X_noise
  Y <- b * Z + Y_noise
  XYZ <- round(data.frame(X = X, Y = Y, Z = Z), 2)
  
  # Save the correlation result to the list
  t_dist_results[[i]] <- compute_correlations(XYZ, method = method)
  
}

# view results
combined_df <- do.call(rbind, t_dist_results)

# Calculate the proportion for Partial Parametric
proportion_partial_parametric <- combined_df %>%
  filter(Type == "Partial Parametric" & Zero.Within.CI == TRUE) %>%
  summarise(Proportion = n() / sum(combined_df$Type == "Partial Parametric")) %>%
  pull(Proportion)

# Calculate the proportion for Partial Bootstrap
proportion_partial_bootstrap <- combined_df %>%
  filter(Type == "Partial Bootstrap" & Zero.Within.CI == TRUE) %>%
  summarise(Proportion = n() / sum(combined_df$Type == "Partial Bootstrap")) %>%
  pull(Proportion)

# Output the results
list(Partial_Parametric = proportion_partial_parametric,
     Partial_Bootstrap = proportion_partial_bootstrap)

#####################

# Loop for f distribution results
f_dist_results <- list()
for (i in 1:10000) {
  
  # Set a different seed each time
  set.seed(i)
  
  # output of iteration
  print(paste("Iteration:", i))
  
  # Experiment parameters
  n <- 100
  method <- 'pearson'
  
  # Generate constants
  a <- round(runif(1, min = 1, max = 5), 2)
  b <- round(runif(1, min = 1, max = 5), 2)
  
  # Generate synthetic data
  Z <- rf(n, df1 = 1, df2 = n - 1)
  X_noise <- rnorm(n, mean = 0, sd = 1)  # Independent noise for X
  Y_noise <- rnorm(n, mean = 0, sd = 1)  # Independent noise for Y
  
  X <- a * Z + X_noise
  Y <- b * Z + Y_noise
  XYZ <- round(data.frame(X = X, Y = Y, Z = Z), 2)
  
  # Save the correlation result to the list
  f_dist_results[[i]] <- compute_correlations(XYZ, method = method)
  
}

# view results
combined_df <- do.call(rbind, f_dist_results)

# Calculate the proportion for Partial Parametric
proportion_partial_parametric <- combined_df %>%
  filter(Type == "Partial Parametric" & Zero.Within.CI == TRUE) %>%
  summarise(Proportion = n() / sum(combined_df$Type == "Partial Parametric")) %>%
  pull(Proportion)

# Calculate the proportion for Partial Bootstrap
proportion_partial_bootstrap <- combined_df %>%
  filter(Type == "Partial Bootstrap" & Zero.Within.CI == TRUE) %>%
  summarise(Proportion = n() / sum(combined_df$Type == "Partial Bootstrap")) %>%
  pull(Proportion)

# Output the results
list(Partial_Parametric = proportion_partial_parametric,
     Partial_Bootstrap = proportion_partial_bootstrap)
