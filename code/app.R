library(shiny)
library(ggplot2)
library(igraph)
library(reshape2)

# Load Boruta results
load("./data/Boruta_results_short_labels.RData")

# Combine all results into one dataframe
all_results <- do.call(rbind, results)

ui <- fluidPage(
  titlePanel("Interactive Predictor Importance by Community"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "rank_threshold",
        "Rank Threshold for BinaryRank:",
        min = 1,
        max = max(all_results$Rank, na.rm = TRUE),
        value = 5,
        step = 1
      )
    ),
    
    mainPanel(
      plotOutput("dot_plot")
    )
  )
)

server <- function(input, output, session) {
  # Reactive expression to update BinaryRank based on user input
  reactive_data <- reactive({
    # Step 1: Update BinaryRank based on the threshold
    all_results$BinaryRank <- ifelse(all_results$Rank <= input$rank_threshold, 1, 0)
    
    # Reshape into a wide format (rows = Predictors, columns = Outcomes)
    wide_binary <- dcast(all_results, Predictor ~ outcome, value.var = "BinaryRank", fill = 0)
    rownames(wide_binary) <- wide_binary$Predictor
    wide_binary <- wide_binary[, -1]
    
    # Step 2: Create a bipartite network
    net <- graph_from_incidence_matrix(wide_binary)
    
    # Step 3: Run multiple Louvain detections
    num_trials <- 100
    louvain_memberships <- matrix(nrow = vcount(net), ncol = num_trials)
    for (i in 1:num_trials) {
      set.seed(i)
      communities <- cluster_louvain(net)
      louvain_memberships[, i] <- communities$membership
    }
    
    # Calculate consensus communities
    consensus_communities <- apply(louvain_memberships, 1, function(x) {
      as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
    })
    V(net)$community <- consensus_communities
    
    # Assign human-readable community names
    predictor_nodes <- rownames(wide_binary)
    outcome_nodes <- colnames(wide_binary)
    community_outcomes <- aggregate(V(net)$name[V(net)$name %in% outcome_nodes] ~ V(net)$community[V(net)$name %in% outcome_nodes],
                                    FUN = paste, collapse = ", ")
    colnames(community_outcomes) <- c("Community", "Outcomes")
    all_results$Community <- ifelse(
      is.na(match(all_results$Predictor, V(net)$name)),
      "Isolated",
      V(net)$community[match(all_results$Predictor, V(net)$name)]
    )
    community_names <- merge(community_outcomes, data.frame(Community = unique(V(net)$community)), by = "Community", all.y = TRUE)
    community_names$Outcomes <- ifelse(is.na(community_names$Outcomes), "No Outcomes", community_names$Outcomes)
    all_results$CommunityName <- community_names$Outcomes[match(all_results$Community, community_names$Community)]
    
    # Rank predictors by importance
    community_importance <- aggregate(Rank ~ Predictor + Community, data = all_results,
                                      FUN = function(x) -mean(x))
    community_importance$ImportanceIndex <- abs(min(community_importance$Rank)) - abs(community_importance$Rank)
    community_importance <- community_importance[order(community_importance$Community, -community_importance$ImportanceIndex), ]
    
    # Exclude "No Outcomes" community
    community_importance <- community_importance[community_importance$Community != "No Outcomes", ]
    
    return(community_importance)
  })
  
  # Render dot plot
  output$dot_plot <- renderPlot({
    data <- reactive_data()
    
    ggplot(data, aes(x = factor(Community), y = ImportanceIndex, label = Predictor)) +
      geom_point(aes(color = factor(Community)), size = 3) +
      labs(
        title = "Predictor Importance by Community",
        x = "Community",
        y = "Importance Index (Higher = More Important)",
        color = "Community"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "bottom")
  })
}

shinyApp(ui, server)
