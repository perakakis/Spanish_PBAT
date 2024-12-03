# Load necessary libraries and functions
library(igraph)  # For network analysis and visualization
library(reshape2)  # For reshaping data
library(ggplot2)
library(graphlayouts)
library(visNetwork)
library(gt)

source("./code/auxFunctions.R")

# Set seed for reproducibility
set.seed(1)

# Define thresholds
binary_rank_threshold <- 5
min_community_size <- 2

# Load Boruta results
load("./data/Boruta_results_short_labels.RData")

# Combine all results into one dataframe
all_results <- do.call(rbind, results)

# Step 1: Convert Boruta results into a binary adjacency matrix
all_results$BinaryRank <- ifelse(all_results$Rank <= binary_rank_threshold, 1, 0)  # 1 = important, 0 = not important
wide_binary <- dcast(all_results, Predictor ~ outcome, value.var = "BinaryRank", fill = 0)
rownames(wide_binary) <- wide_binary$Predictor
wide_binary <- wide_binary[, -1]  # Remove 'Predictor' column as it's now rownames

# Step 2: Create a bipartite network
net <- graph_from_incidence_matrix(wide_binary)

# Step 3: Calculate consensus communities
consensus_communities <- calculate_consensus_communities(net)
V(net)$community <- consensus_communities

# Calculate modularity
# modularity_value <- modularity(net, V(net)$community)
# print(modularity_value)

# Step 4: Assign human-readable community names based on outcomes
# Extract outcomes and their assigned communities
outcome_nodes <- V(net)$name[V(net)$type == TRUE]
outcome_communities <- V(net)$community[V(net)$type == TRUE]

# Group outcomes by community
community_outcomes <- data.frame(Community = outcome_communities, Outcomes = outcome_nodes)
community_outcomes <- aggregate(Outcomes ~ Community, data = community_outcomes, FUN = paste, collapse = ", ")

# Add missing communities with "No Outcomes"
all_communities <- data.frame(Community = unique(V(net)$community))
community_outcomes <- merge(all_communities, community_outcomes, by = "Community", all.x = TRUE)
community_outcomes$Outcomes[is.na(community_outcomes$Outcomes)] <- "No Outcomes"

# Map community names to predictors
all_results$Community <- V(net)$community[match(all_results$Predictor, V(net)$name)]
all_results$CommunityName <- community_outcomes$Outcomes[match(all_results$Community, community_outcomes$Community)]

# Step 5: Rank predictors by importance
community_importance <- aggregate(Rank ~ Predictor + CommunityName, data = all_results, FUN = function(x) -mean(x))
community_importance$ImportanceIndex <- abs(min(community_importance$Rank)) - abs(community_importance$Rank)

# Step 6: Filter for communities with at least 2 members
community_sizes <- table(community_importance$CommunityName)
valid_communities <- names(community_sizes[community_sizes >= min_community_size])
top_predictors_filtered <- community_importance[
  community_importance$CommunityName %in% valid_communities & 
    community_importance$CommunityName != "No Outcomes", 
]

# Step 7: Visualizations
# Create the network plot
#pdf("./figures/BipartiteNetwork.pdf", width = 7, height = 5)  # Adjust width and height as needed
plot(net,
     vertex.label = V(net)$name,       # Node labels
     vertex.color = rainbow(length(unique(consensus_communities)))[V(net)$community],
     layout = layout_with_graphopt(net, charge = 0.1),
     # layout = layout_with_fr(net),
     main = "Bipartite Network with Consensus Communities")
#dev.off()

# Create the dot plot
p <- ggplot(top_predictors_filtered, aes(x = factor(CommunityName), y = ImportanceIndex, label = Predictor)) +
  geom_point(aes(color = factor(CommunityName)), size = 3) +
  geom_text(hjust = -0.2, vjust = 0, size = 3) +  # Add predictor labels
  labs(title = "Predictor Importance by Community",
       x = "Community",
       y = "Importance Index",
       color = "Community") +
  theme_minimal()
# ggsave(filename = "./figures/PredictorImportance.pdf", plot = p, width = 10, height = 6, dpi = 300)

# Create interactive network
vis_data <- toVisNetworkData(net)

# Assign colors based on community
community_colors <- rainbow(length(unique(V(net)$community)))  # Generate colors for communities
vis_data$nodes$color <- community_colors[V(net)$community]    # Map community to colors

# Create the interactive plot
vis_network <- visNetwork(nodes = vis_data$nodes, edges = vis_data$edges)
vis_network <- visOptions(vis_network, highlightNearest = TRUE, nodesIdSelection = TRUE)
vis_network <- visLayout(vis_network, randomSeed = 123)

# Render and save the network
vis_network
# visSave(vis_network, file = "./figures/InteractiveNetwork.html")

# Step 8: Centrality measures
# Calculate Degree Centrality
degree_centrality <- degree(net, mode = "all")  # "all" for undirected network
V(net)$degree <- degree_centrality

# Calculate Betweenness Centrality
betweenness_centrality <- betweenness(net, directed = FALSE, normalized = TRUE)
V(net)$betweenness <- betweenness_centrality

# Calculate Closeness Centrality
closeness_centrality <- closeness(net, normalized = TRUE)
V(net)$closeness <- closeness_centrality

# Calculate Eigenvector Centrality
eigenvector_centrality <- eigen_centrality(net, directed = FALSE)$vector
V(net)$eigenvector <- eigenvector_centrality

# Combine Results into a Data Frame
centrality_measures <- data.frame(
  Node = V(net)$name,
  Degree = V(net)$degree,
  Betweenness = V(net)$betweenness,
  Closeness = V(net)$closeness,
  Eigenvector = V(net)$eigenvector
)

centrality_measures <- centrality_measures[1:18,]

# Display Results
#print(centrality_measures)