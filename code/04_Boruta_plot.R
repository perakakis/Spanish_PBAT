library(reshape2)

source("./code/02_Descriptives.R")
load("./data/Boruta_results.RData")

# Define the predictors vector: you could make these subvectors, 
#like selection, retention and variation
Predictors <- PBAT_labels
boruta_outcomes <- c(STOPD_labels, "VITAL", "HEALTH")

# Combine all results into one dataframe
all_results <- do.call(rbind, results)

# Reshape for plotting
wide_data <- dcast(all_results, Predictor ~ outcome, value.var = "Rank")

# Melt the data for ggplot2
plot_data <- melt(wide_data, id.vars = "Predictor")
plot_data$Predictor <- factor(plot_data$Predictor, levels = rev(PBAT_labels))
plot_data$variable <- factor(plot_data$variable, levels = boruta_outcomes)

# Plotting with ggplot2 using a grey color scale
p <- ggplot(plot_data, aes(x = variable, y = Predictor, fill = value)) +
  geom_tile(color = "white") +  # Adds borders to the tiles
  geom_text(aes(label = ifelse(is.na(value), "NA", as.character(value))), size = 3, color = "black") +  # Adds text labels
  scale_fill_gradient(low = "grey", high = "white", na.value = "white", guide = "none") +  # Color gradient with white for NA
  labs(title = "Predictor Rank Across Outcomes", x = "Outcome", y = "Predictor") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for better visibility

# ggsave(filename = "./figures/Boruta.pdf", plot = p, width = 10, height = 6, dpi = 300)

# Export to csv
wide_data$Predictor <- factor(wide_data$Predictor, levels = PBAT_labels)
wide_data <- wide_data[order(wide_data$Predictor), ]
wide_data <- wide_data[, c("Predictor", boruta_outcomes)]

# write.csv(wide_data, "./tables/Boruta_results.csv", row.names = FALSE)
