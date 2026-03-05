### Libraries ###
{
  library(shiny)
  library(shinydashboard)
  library(visNetwork)
  library(recommenderlab)
  library(tidyverse)
  library(bslib)
  library(rsconnect)
}

### Load data
apri_rec = readRDS("apri_rec.rds")
rules = readRDS("rules.rds")
apri_matrix = readRDS("apri_matrix.rds")

ui = page_sidebar(
  title = "Spotify Analytics Hub",
  # Set the theme (Bootswatch "darkly" is great for Spotify vibes)
  theme = bs_theme(
    version = 5, 
    primary = "#1DB954", # Your Spotify Green
    bg = "#121212", 
    fg = "#FFFFFF"
  ),
  
  # Sidebar (Your Step 1 controls)
  sidebar = sidebar(
    title = "Controls",
    width = 300,
    selectInput("artist_filter", "Step 1: Select Artist:", 
                choices = c("Select Artist" = "", sort(unique(rules$from_artist)))),
    uiOutput("prediction_panel")
  ),
  
  # Main Content (Your network plot and results)
  layout_columns(
    col_widths = c(12),
    card(
      card_header("Recommendation Network"),
      uiOutput("map_placeholder"),
      visNetworkOutput("network_plot", height = "800px")
    )
  )
)

server = function(input, output, session) {
  
  # 1. Centralize the data processing
  get_network_data = reactive({
    req(input$artist_filter != "")
    
    rules_filtered = rules %>% 
      filter(from_artist == input$artist_filter | to_artist == input$artist_filter)
    
    nodes_data = rules_filtered %>% 
      select(from, to) %>% 
      pivot_longer(cols = c(from, to)) %>%
      select(value) %>% 
      unique() %>% 
      rename(id = value) %>%
      mutate(group = str_split_i(id, "::", 1)) # Helper to keep group
    
    edges_data = rules_filtered %>% select(from, to)
    
    # Return both as a list
    return(list(nodes = nodes_data, edges = edges_data))
  })
  
  # 2. Update the input using the reactive data
  observe({
    req(get_network_data())
    data = get_network_data()
    
    song_choices = data$nodes %>% 
      filter(group == input$artist_filter) %>% 
      pull(id) %>% 
      sort()
    
    updateSelectInput(session, "user_song", choices = song_choices)
  })
  
  # --- Logic: Conditional Prediction Panel ---
  output$prediction_panel = renderUI({
    req(input$artist_filter != "") # Only show if an artist is selected
    
    tagList(
      card(
        card_header("Step 2: Predict"),
        selectInput("user_song", "Choose a Song:", choices = NULL),
        actionButton("predict_btn", "Generate Recommendations", 
                       class = "btn-success", width = "100%")
      ),
      card(
        card_header("Top Recommendations"),
        tableOutput("rec_results")
      )
    )
  })
  
  # Logic for Recommendations
  output$rec_results = renderTable({
    req(input$predict_btn)
    isolate({
      # 1. Create the binary sparse matrix for the input
      req(input$user_song)
      
      # Reference your original training columns
      new_user_data = (colnames(apri_matrix) %in% input$user_song) %>% matrix(nrow=1)
      colnames(new_user_data) = colnames(apri_matrix)
      new_user_data = as(new_user_data, "binaryRatingMatrix")
      
      # 2. Predict
      pred = predict(apri_rec, new_user_data, n = 5)
      data.frame(Suggested_Songs = as(pred, "list")[[1]])
    })
  })
  
  output$map_placeholder = renderUI({
    if(input$artist_filter == "") {
      h4("Please select an artist on the left.", 
         style = "text-align: center; margin-top: 100px; color: gray;")
    } else {
      NULL
    }
  })
  
  # Logic for Network Plot
  output$network_plot = renderVisNetwork({
    req(get_network_data())
    data = get_network_data()
    
    visNetwork(data$nodes, data$edges) %>%
      visNodes(font = list(color = "white")) %>% 
      visGroups(groupname = input$artist_filter, color = "#1DB954") %>%
      visEdges(arrows = "to") %>%
      visOptions(selectedBy = list(variable = "group", selected = input$artist_filter), 
                 highlightNearest = list(enabled = TRUE, degree = 1),
                 nodesIdSelection = TRUE) %>%
      visPhysics(solver = "forceAtlas2Based", stabilization = TRUE)
  })
}

shinyApp(ui, server)