source("./code/auxFunctions.R")

set.seed(1)

PBAT_labels <- c("Chose to do personally important things",
  "Helped My Health",
  "Paid attention to important things in daily life",
  "Connected with important people",
  "Experience range emotions approp. to moment",
  "Found ways to challenge self",
  "Used thinking to live better",
  "Did things only to comply to others",
  "Hurt my health",
  "Struggled to connect with moments of day",
  "Hurt social connections",
  "Found no appropriate outlet for feelings",
  "Found no meaningful challenge",
  "My thinking got in the way of important things",
  "Stuck & unable to change ineffective behavior",
  "Able to change behavior, when changing helped",
  "Struggled to keep doing what was important",
  "Stuck to Strategies that worked")

# Short labels
PBAT_labels <- c(
 "Motivation+", "Health+", "Attention+", "Social+", "Affect+", "Challenge+", "Cognition+",
 "Motivation-", "Health-", "Attention-", "Social-", "Affect-", "Challenge-", "Cognition-",
 "Variation-", "Variation+", "Retention-", "Retention+"
)

PBAT_order <- c(16,11,5,17,13,2,14,3,8,18,6,15,7,10,4,1,9,12)

STOPD_labels <- c("SAD",
                 "ANXIOUS",
                 "STRESSED",
                 "ANGRY",
                 "NOSUPP")

NEED_labels <- c("Autonomy Satisfaction",
                 "Autonomy Frustration",
                 "Connection Satisfaction",
                 "Connection Frustration",
                 "Competence Satisfaction",
                 "Competence Frustration")

NEED_order <- c(1,2,5,6,3,4)

# Load data
df <- read.csv(file = "./data/data.csv")

# Rename PBAT_labels
reordered_PBAT_labels <- PBAT_labels[PBAT_order]
colnames(df)[4:21] <- reordered_PBAT_labels

# Rename STOPD_labels
colnames(df)[22:26] <- STOPD_labels

# Rename NEED_labels
reordered_NEED_labels <- NEED_labels[NEED_order]
colnames(df)[31:36] <- reordered_NEED_labels

# Average Vital
df$VITAL <- rowMeans(df[, c("VITAL1", "VITAL2", "VITAL3")], na.rm = TRUE)

# Convert Health variable
health_mapping <- c("Mala" = 1, "Regular" = 2, "Buena" = 3, "Muy buena" = 4, "Excelente" = 5)
df$HEALTH <- as.numeric(factor(df$HEALTH, levels = names(health_mapping), labels = health_mapping))

# Table 2: Means, standard deviations, and sex differences for PBAT items
PBAT_stats <- round(calculate_group_stats(df, "Sexo", PBAT_labels),3)
#write.csv(PBAT_stats, "./tables/table2.csv")

# Table 3: Means, standard deviations, and sex differences for Need Satisfaction and outcome variables
outcomes <- c(STOPD_labels, "VITAL", "HEALTH", NEED_labels)
outcome_stats <- round(calculate_group_stats(df, "Sexo", outcomes),3)
#write.csv(outcome_stats, "./tables/table3.csv")

# Table 4: Link between positive and negative selection behavior
# Select the first 14 PBAT labels
selected_PBAT_labels <- PBAT_labels[1:14]
result <- correlation_with_asterisks(df, selected_PBAT_labels)
corr_matrix <- result$correlation_matrix
tbl_4 <- result$formatted_table
colnames(tbl_4) <- as.character(1:14)
#write.csv(tbl_4, "./tables/table4.csv")

# Table 5: Relationship between variation and retention items
selected_PBAT_labels <- PBAT_labels[15:18]
result <- correlation_with_asterisks(df, selected_PBAT_labels)
corr_matrix <- result$correlation_matrix
tbl_5 <- result$formatted_table
colnames(tbl_5) <- as.character(1:4)
#write.csv(tbl_5, "./tables/table5.csv")

# Table 6: Link of PBAT items to satisfaction and frustration of the need for autonomy competence, and connection
tbl_6 <- correlation_between_sets(df, PBAT_labels, NEED_labels)
#write.csv(tbl_6, "./tables/table6.csv")

# Table 7: Link between PBAT and clinically relevant outcome
tbl_7 <- correlation_between_sets(df, PBAT_labels, c(STOPD_labels,"HEALTH","VITAL"))
#write.csv(tbl_7, "./tables/table7.csv")