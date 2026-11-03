# Assignment 10 - Shiny App
# Faith Stamm
# Vehicle Types & Fatal Traffic Accidents in 2024


library(shiny)

# ------------------------------
# Load cleaned vehicle data
# ------------------------------

vehicles <- readRDS("vehicles_clean.rds")

# Find valid vehicle model years
valid_years <- sort(
  unique(vehicles$MOD_YEAR[
    !is.na(vehicles$MOD_YEAR) &
      vehicles$MOD_YEAR >= 1900 &
      vehicles$MOD_YEAR <= 2025
  ]),
  decreasing = TRUE
)

# Vehicle body type categories
body_categories <- c(
  "Sedans",
  "Coupes",
  "SUVs",
  "Trucks",
  "Vans",
  "Motorcycles",
  "Large Trucks",
  "Buses",
  "Other Vehicles"
)

# ---------------
# User Interface
# ---------------

ui <- fluidPage(
  
  # Custom styling
  tags$head(
    tags$style(HTML("

      /* Overall page */
      body {
        background-color: #FFFFFF;
        color: #30343B;
        font-family: Arial, sans-serif;
      }

      .container-fluid {
        padding: 20px 30px;
      }

      /* Main title */
      h2 {
        font-size: 32px;
        font-weight: 600;
        color: #30343B;
        margin-bottom: 30px;
      }

      /* Sidebar */
      .well {
        background-color: #FAFAFA;
        border: 1px solid #DDDDDD;
        border-radius: 10px;
        box-shadow: none;
        padding: 25px;
      }

      /* Dropdown labels */
      .control-label {
        font-size: 17px;
        font-weight: 600;
        color: #30343B;
        margin-bottom: 8px;
      }

      /* Dropdown selections */
      .selectize-input {
        font-size: 16px;
        border-radius: 6px;
        border: 1px solid #CCCCCC;
        padding: 10px 12px;
        min-height: 42px;
      }

      .selectize-dropdown {
        font-size: 16px;
      }

      /* Space between filters */
      .form-group {
        margin-bottom: 25px;
      }

      /* Reset Filters button */
      #reset {
        background-color: #4B83B5;
        color: white;
        border: none;
        border-radius: 6px;
        font-size: 16px;
        font-weight: 600;
        padding: 11px 20px;
        width: 100%;
        margin-top: 10px;
      }

      #reset:hover {
        background-color: #3C719F;
      }

      /* Graph panel */
      .graph-card {
        background-color: #FFFFFF;
        border: 1px solid #DDDDDD;
        border-radius: 10px;
        padding: 20px;
      }

      /* Centered graph title */
      .graph-title {
        font-size: 23px;
        font-weight: 600;
        color: #30343B;
        text-align: center;
        margin-top: 5px;
        margin-bottom: 20px;
      }

    "))
  ),
  
  # Main title
  titlePanel(
    "Vehicle Types & Fatal Traffic Accidents in 2024"
  ),
  
  sidebarLayout(
    
    # ----------------
    # Filters
    # ----------------
    
    sidebarPanel(
      
      # State dropdown
      selectInput(
        "state",
        "Select a State:",
        choices = c(
          "All States",
          sort(unique(vehicles$STATENAME))
        )
      ),
      
      # Vehicle Make dropdown
      selectInput(
        "make",
        "Select a Vehicle Make:",
        choices = c(
          "All Makes",
          sort(unique(vehicles$MAKENAME))
        )
      ),
      
      # Model Year dropdown
      selectInput(
        "year",
        "Select a Model Year:",
        choices = c(
          "All Years",
          valid_years
        )
      ),
      
      # Reset Filters button
      actionButton(
        "reset",
        "Reset Filters"
      )
      
    ),
    
    # -----------
    # Main Graph
    # -----------
    
    mainPanel(
      
      div(
        class = "graph-card",
        
        # Centered graph title
        div(
          class = "graph-title",
          "Vehicles Involved in Fatal Crashes"
        ),
        
        # Ranked dot plot
        plotOutput(
          "vehiclePlot",
          height = "500px"
        )
        
      )
      
    )
  )
)

# -------
# Server
# -------

server <- function(input, output, session) {
  
  # ---------------------
  # Reset Filters Button
  # ---------------------
  
  observeEvent(input$reset, {
    
    updateSelectInput(
      session,
      "state",
      selected = "All States"
    )
    
    updateSelectInput(
      session,
      "make",
      selected = "All Makes"
    )
    
    updateSelectInput(
      session,
      "year",
      selected = "All Years"
    )
    
  })
  
  # --------------------
  # Filter Vehicle Data
  # --------------------
  
  filtered_vehicles <- reactive({
    
    selected_data <- vehicles
    
    # Filter by state
    if (input$state != "All States") {
      
      selected_data <- selected_data[
        !is.na(selected_data$STATENAME) &
          selected_data$STATENAME == input$state,
        ,
        drop = FALSE
      ]
      
    }
    
    # Filter by vehicle make
    if (input$make != "All Makes") {
      
      selected_data <- selected_data[
        !is.na(selected_data$MAKENAME) &
          selected_data$MAKENAME == input$make,
        ,
        drop = FALSE
      ]
      
    }
    
    # Filter by model year
    if (input$year != "All Years") {
      
      selected_data <- selected_data[
        !is.na(selected_data$MOD_YEAR) &
          selected_data$MOD_YEAR == as.numeric(input$year),
        ,
        drop = FALSE
      ]
      
    }
    
    return(selected_data)
    
  })
  
  # ----------------------------
  # Count Vehicles by Body Type
  # ----------------------------
  
  vehicle_counts <- reactive({
    
    counts <- table(
      factor(
        filtered_vehicles()$BODY_GROUP,
        levels = body_categories
      )
    )
    
    # Rank categories from highest to lowest
    sort(counts, decreasing = TRUE)
    
  })

  # ----------------
  # Ranked Dot Plot
  # ----------------
  output$vehiclePlot <- renderPlot({
    
    counts <- vehicle_counts()
    
    # Leave room for category names
    par(
      mar = c(5, 11, 2, 5),
      family = "sans"
    )
    
    # Handle filters with no matching vehicles
    if (sum(counts) == 0) {
      
      plot.new()
      
      text(
        0.5,
        0.5,
        "No vehicles match your selected filters.",
        cex = 1.2,
        col = "#30343B"
      )
      
      return()
    }
    
    # Convert counts to numeric values
    values <- as.numeric(counts)
    
    # Highest category appears at the top
    positions <- rev(seq_along(values))
    
    # Find the highest count
    max_value <- max(values)
    
    # Create an empty plot
    plot(
      values,
      positions,
      type = "n",
      
      xlim = c(0, max_value * 1.28),
      ylim = c(0.5, length(values) + 0.5),
      
      xaxt = "n",
      yaxt = "n",
      
      xlab = "",
      ylab = "",
      
      main = "",
      bty = "n",
      
      cex.lab = 1.2
    )
    
    # Add light horizontal grid lines
    abline(
      h = positions,
      col = "#E5E5E5",
      lty = "dotted"
    )
    
    # Add x-axis
    axis(
      1,
      at = pretty(c(0, max_value)),
      labels = format(
        pretty(c(0, max_value)),
        big.mark = ",",
        trim = TRUE
      ),
      cex.axis = 1.05
    )
    
    # Add vehicle category names
    axis(
      2,
      at = positions,
      labels = names(counts),
      las = 1,
      tick = FALSE,
      cex.axis = 1.15
    )
    
    # Add blue dots
    points(
      values,
      positions,
      pch = 19,
      cex = 1.8,
      col = "#3478B8"
    )
    
    # Add counts beside the dots
    text(
      values,
      positions,
      labels = format(
        values,
        big.mark = ",",
        trim = TRUE
      ),
      pos = 4,
      offset = 0.7,
      cex = 1.05,
      col = "#30343B"
    )
    
  })
  
}

# ------------------
# Run Shiny App
# ------------------
shinyApp(
  ui = ui,
  server = server
)