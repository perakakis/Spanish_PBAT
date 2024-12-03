# Load required libraries
library(ggplot2)
library(Hmisc)

# Function to summarize and plot a categorical variable
summarize_and_plot <- function(df, var_name) {
  # Convert the variable to a factor if it isn't already
  df[[var_name]] <- as.factor(df[[var_name]])
  
  # Count the occurrences of each category and compute the percentages
  var_counts <- table(df[[var_name]])
  var_summary <- as.data.frame(var_counts)
  names(var_summary) <- c("Category", "Count")
  total_count <- sum(var_summary$Count)
  var_summary$Percentage <- (var_summary$Count / total_count) * 100
  var_summary$Percentage <- round(var_summary$Percentage, 2)
  
  # Plot the distribution as a bar plot with percentages
  plot <- ggplot(var_summary, aes(x = Category, y = Percentage)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    labs(title = paste("Distribution of", var_name), x = var_name, y = "Percentage (%)") +
    theme_minimal()
  
  # Print the summary and display the plot
  print(var_summary)
  print(plot)
}

# Function to calculate means, standard deviations, and t-tests with t_diff
calculate_group_stats <- function(df, group_var, measurement_vars) {
  # Initialize lists to hold results
  stats_list <- list()
  t_tests_list <- list()
  
  # Calculate means and standard deviations by group
  for (var in measurement_vars) {
    group_means <- tapply(df[[var]], df[[group_var]], mean, na.rm = TRUE)
    group_sds <- tapply(df[[var]], df[[group_var]], sd, na.rm = TRUE)
    stats_list[[var]] <- c(group_means, group_sds)
  }
  
  # Convert to a dataframe
  stats_df <- as.data.frame(do.call(rbind, stats_list))
  colnames(stats_df) <- c("Female_M", "Male_M", "Female_SD", "Male_SD")
  
  # Perform t-tests for each variable and store results
  for (var in measurement_vars) {
    t_result <- t.test(df[[var]] ~ df[[group_var]], na.rm = TRUE)
    t_tests_list[[var]] <- c(t_result$statistic, t_result$p.value)
  }
  
  # Convert t-test results to a dataframe
  t_tests_df <- as.data.frame(do.call(rbind, t_tests_list))
  colnames(t_tests_df) <- c("t_diff", "p_value")
  
  # Combine the descriptive statistics and t-test results
  final_stats <- cbind(stats_df[, c("Female_M", "Female_SD", "Male_M", "Male_SD")], t_tests_df)
  rownames(final_stats) <- measurement_vars
  
  return(final_stats)
}

# Function to add asterisks for significance levels
add_significance_stars <- function(p_values) {
  stars <- ifelse(p_values < 0.001, "***",
                  ifelse(p_values < 0.01, "**",
                         ifelse(p_values < 0.05, "*", "")))
  return(stars)
}

# Function to generate the correlation matrix and formatted table with asterisks
correlation_with_asterisks <- function(df, var_names) {
  # Subset the dataframe to include only the specified variables
  df_subset <- df[, var_names, drop = FALSE]
  
  # Calculate correlation matrix and p-values
  corr_result <- rcorr(as.matrix(df_subset))
  corr_matrix <- corr_result$r
  p_matrix <- corr_result$P
  
  # Apply asterisks to each correlation value
  stars <- matrix(add_significance_stars(p_matrix), nrow = nrow(p_matrix), ncol = ncol(p_matrix))
  
  # Combine correlation values and significance stars
  formatted_corr <- matrix(paste0(round(corr_matrix, 2), stars), nrow = nrow(corr_matrix))
  colnames(formatted_corr) <- colnames(corr_matrix)
  rownames(formatted_corr) <- rownames(corr_matrix)
  
  # Get only the lower triangular part
  corr_matrix[upper.tri(corr_matrix, diag = TRUE)] <- NA
  formatted_corr[upper.tri(formatted_corr, diag = TRUE)] <- ""
  
  # Convert to a dataframe for a cleaner output
  formatted_corr_df <- as.data.frame(formatted_corr)
  
  return(list(correlation_matrix = as.data.frame(round(corr_matrix, 2)), formatted_table = formatted_corr_df))
}

# Function to generate a correlation table between two sets of variables (excluding within-set correlations)
correlation_between_sets <- function(df, set1_vars, set2_vars) {
  
  # Subset the dataframe to include only the specified variables
  set1_subset <- df[, set1_vars, drop = FALSE]
  set2_subset <- df[, set2_vars, drop = FALSE]
  
  # Calculate correlation matrix and p-values between the two sets
  corr_result <- rcorr(as.matrix(set1_subset), as.matrix(set2_subset))
  corr_matrix <- corr_result$r
  p_matrix <- round(corr_result$P,6)
  
  # Extract only the correlations between set1 (rows) and set2 (columns)
  corr_between <- corr_matrix[1:length(set1_vars), (length(set1_vars) + 1):ncol(corr_matrix)]
  p_between <- p_matrix[1:length(set1_vars), (length(set1_vars) + 1):ncol(p_matrix)]
  
  # Apply asterisks to each correlation value
  stars <- matrix(add_significance_stars(p_between), nrow = nrow(p_between), ncol = ncol(p_between))
  
  # Combine correlation values and significance stars
  formatted_corr <- matrix(paste0(round(corr_between, 2), stars), nrow = nrow(corr_between), ncol = ncol(corr_between))
  colnames(formatted_corr) <- set2_vars
  rownames(formatted_corr) <- set1_vars
  
  # Convert to a dataframe for better output
  formatted_corr_df <- as.data.frame(formatted_corr)
  
  return(formatted_corr_df)
}

# Function to calculate consensus communities
calculate_consensus_communities <- function(net, num_trials = 100) {
  louvain_memberships <- matrix(nrow = vcount(net), ncol = num_trials)
  for (i in 1:num_trials) {
    set.seed(i)
    communities <- cluster_louvain(net,resolution = 1)
    louvain_memberships[, i] <- communities$membership
  }
  apply(louvain_memberships, 1, function(x) {
    as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
  })
}