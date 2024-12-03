library(Boruta)
library(ggplot2)
library(reshape2)

source("./code/02_Descriptives.R")
set.seed(1)

# Define the predictors vector
Predictors <- PBAT_labels

boruta_outcomes <- c(STOPD_labels, "VITAL", "HEALTH")

results <- list()  # Store results for each outcome
importance_results <- list()  # To keep track of actual importance scores

for(outcome in boruta_outcomes) {
  test.set <- df[, c(outcome, Predictors)]
  
  boruta.train <- Boruta(as.formula(paste(outcome, "~.")), data = test.set, doTrace = 0)
  final.boruta <- TentativeRoughFix(boruta.train)
  boruta.df <- attStats(final.boruta)
  boruta.df$Predictor <- rownames(boruta.df)
  
  # Store full importance data for reference
  importance_results[[outcome]] <- boruta.df
  
  # Mark "Rejected" predictors' ranks as NA
  boruta.df$Rank <- ifelse(boruta.df$decision == "Rejected", NA, 
                           rank(-boruta.df$meanImp, na.last = "keep"))
  boruta.df$outcome <- outcome
  results[[outcome]] <- boruta.df[, c("Predictor", "Rank", "outcome")]
}

# Save results
# save(results, file = "./data/Boruta_results_short_labels.RData")
# save(importance_results, file = "./data/Boruta_results_importance_short_labels.RData")