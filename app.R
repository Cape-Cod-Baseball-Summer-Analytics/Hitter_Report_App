library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(GeomMLBStadiums)
library(readxl)
library(DT)
library(glue)
library(hexbin)


data <- readRDS("app.rds")

hawks_players <- data |> 
  filter(Team == "Hyannis Harbor Hawks")

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Hyannis Analytics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home Page", tabName = "dashboard", icon = icon("home")),
      menuItem("Post Game Report", tabName = "post", icon = icon("file-alt")),
      menuItem("Spray Charts", tabName = "spray", icon = icon("baseball-ball")),
      menuItem("Heat Maps", tabName = "heat", icon = icon("chart-area"),
               menuSubItem("Whiff/Chase Heat",
                           tabName = "whiff_heat",
                           icon = icon("ban")),
               menuSubItem("Exit Velo Heat",
                           tabName = "ev_heat",
                           icon = icon("tachometer-alt"))
               ),
      menuItem("Exit Velo & LA Table", tabName = "ev", icon = icon("table")),
      menuItem("Blast Data", tabName = "blast", icon = icon("rocket")),
      menuItem("Run Expectancy Matrix", tabName = "re", icon = icon("ruler"))
    )
  ),
  dashboardBody(
    tags$style(HTML("body {background-color: white;} .content-wrapper {background-color: white;}")),
    tabItems(
      tabItem(tabName = "dashboard",
              tags$div(
                style = "text-align: center;",
                tags$h1("Cape League Hitter Report App"),
                tags$img(src = "hawk.png", style = "width: 40%; height: auto;"),
                tags$p("Analysts: Aidan Beilke, Gabriel Appelbaum, Quinn Booth, Richard Legler, Tyler Warren", style = "font-size: 12px; margin-top: 5px;")
              )
      ),
      tabItem(tabName = "post",
              fluidRow(
                column(3, offset = 1, selectInput("playerNamePost", "Type a Player (Last, First):", 
                                                  choices = unique(hawks_players$Batter), 
                                                  selected = "White, Mason", 
                                                  multiple = FALSE, 
                                                  selectize = TRUE),
                       style = "padding-bottom: 20px;"),
                column(3, offset = 1, dateRangeInput("date",
                                                     label = "Select a Game or Multiple",
                                                     start = "2024-05-01",
                                                     end = "2024-06-06",
                                                     min = "2022-01-01",
                                                     max = Sys.Date()),
                       style = "padding-bottom: 20px;"),
                
                
                
              ),
              fluidRow(
                column(6, plotOutput("Pitchchart"), style = "padding-bottom: 20px;"),  # Padding below the first chart
                column(6, plotOutput("Pitchchartwhiff"), style = "padding-bottom: 20px;")  # Padding below the second chart
              ),
              fluidRow(
                column(6, plotOutput("Pitchchartswings")),
                column(6, plotOutput("Pitchcharttakes"))
                
              ),
              fluidRow(
                column(12, DTOutput("tableOutput"))  # Adding the DataTable output here
              )
      ),
      tabItem(tabName = "spray",
              fluidRow(
                column(3, offset = 4,selectInput("playerNameSpray", "Type a Player (Last, First):", choices = unique(hawks_players$Batter), selected = "Gazdar, Jon Jon", multiple = FALSE, selectize = TRUE))
              ),
              fluidRow(
                column(6, plotOutput("sprayChart")),
                column(6, plotOutput("hitTypeDistribution"))
              )
      ),
      tabItem(tabName = "whiff_heat",
              fluidRow(
                column(12, style = "padding-bottom: 20px;",
                       tags$h4(style = "text-align: center;", 
                               "All zone views are from the ", 
                               tags$strong("Pitcher's Perspective."))
                )
              ),
              fluidRow(
                column(3, offset = 3, selectInput("playerNameHeat", "Type a Player (Last, First):", 
                                                  choices = unique(hawks_players$Batter), 
                                                  selected = "White, Mason", 
                                                  multiple = FALSE, 
                                                  selectize = TRUE),
                       style = "padding-bottom: 20px;"),
                column(3, selectInput("pitchType", "Select Pitch Type:",
                                                  choices = c("All" = "",
                                                              "Fastball" = "Fastball",
                                                              "Sinker" = "Sinker",
                                                              "Cutter" = "Cutter",
                                                              "Slider" = "Slider",
                                                              "Curveball" = "Curveball",
                                                              "Changeup" = "ChangeUp",
                                                              "Splitter" = "Splitter")),
                       style = "padding-bottom: 20px;"
                       
              ),
              fluidRow(
                column(6, plotOutput("whiff_heat")),
                column(6, plotOutput("chase_heat"))
              )
            )
      ),
      tabItem(tabName = "ev_heat",
              fluidRow(
                column(12, style = "padding-bottom: 20px;",
                       tags$h4(style = "text-align: center;", 
                               "All zone views are from the ", 
                               tags$strong("Pitcher's Perspective."))
                )
              ),
              fluidRow(
                column(3, offset = 4, selectInput("playerNameHeat2", "Type a Player (Last, First):", 
                                                  choices = unique(hawks_players$Batter), 
                                                  selected = "White, Mason", 
                                                  multiple = FALSE, 
                                                  selectize = TRUE),
                       style = "padding-bottom: 20px;"),
              ),
              fluidRow(
                column(6, plotOutput("ev_heat")),
                column(6, plotOutput("ev_hex"))
              )
      ),
      
      
      tabItem(tabName = "ev",
              DTOutput("ev_table")  # Changed description to match the tab
      ),
      tabItem(tabName = "blast",
              h2("Blast Metrics Overview")  # Changed description to match the tab
      ),
      tabItem(tabName = "re",
              h2("Run Expectancy Matrix"))
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  home_plate_segments <- data.frame(
    x = c(0, 0.71, 0.71, 0, -0.71, -0.71),
    y = c(0.15, 0.15, 0.3, 0.5, 0.3, 0.15),
    xend = c(0.71, 0.71, 0, -0.71, -0.71, 0),
    yend = c(0.15, 0.3, 0.5, 0.3, 0.15, 0.15)
  )
  
  pitch_types <- c("Fastball", "Sinker", "Cutter", "Slider", "Curveball", "Changeup", "Splitter")
  
  observe({
    # Define all pitch types you want to include as options
    all_pitch_types <- c("Fastball", "Sinker", "Cutter", "Slider", "Curveball", "ChangeUp", "Splitter")
    
    # Update the select input with these pitch types statically
    updateSelectInput(session, "pitchType", 
                      choices = c("All" = "", setNames(all_pitch_types, all_pitch_types)))
  })
  
  
  # post game pitch chart
  output$Pitchchart <- renderPlot({
    req(input$playerNamePost)
    
    start_date <- as.Date(input$date[1], format = "%Y-%m-%d")
    end_date <- as.Date(input$date[2], format = "%Y-%m-%d")
    
    post_data <- data |> 
      filter(Batter == input$playerNamePost,
             Date >= start_date,
             Date <= end_date)
    
    pitches <- nrow(post_data)
    
    ggplot(post_data, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
      geom_point(size = 4) +
      geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black") +
      annotate('rect', xmin = -0.71, xmax = 0.71, ymin = 1.6, ymax = 3.5, fill = 'white', 
               color = 'red', alpha = 0.0001, size = 1) +
      labs(title = paste(input$playerNamePost, "-- All Pitches"),
           subtitle = paste("Num Pitches: ", pitches)) +
      xlim(-3, 3) +
      ylim(0, 4) +
      theme_bw() +
      theme(legend.position = "right",
            legend.text = element_text(size = 9),  # Adjust text size
            legend.title = element_text(size = 8), # Adjust title size
            legend.key.size = unit(0.5, "cm"),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
    
  })
  
  # post game whiff chart
  output$Pitchchartwhiff <- renderPlot({
    req(input$playerNamePost)
    
    start_date <- as.Date(input$date[1], format = "%Y-%m-%d")
    end_date <- as.Date(input$date[2], format = "%Y-%m-%d")
    
    post_data <- data |> 
      filter(Batter == input$playerNamePost,
             Date >= start_date,
             Date <= end_date,
             PitchCall == "StrikeSwinging")
    
    pitches <- nrow(post_data)
  
    
    ggplot(post_data, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
      geom_point(size = 4) +
      geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black") +
      annotate('rect', xmin = -0.71, xmax = 0.71, ymin = 1.6, ymax = 3.5, fill = 'white', 
               color = 'red', alpha = 0.0001, size = 1) +
      labs(title = paste(input$playerNamePost, "-- Swing & Misses"),
           subtitle = paste("Num Pitches: ", pitches)) +
      xlim(-3, 3) +
      ylim(0, 4) +
      theme_bw() +
      theme(legend.position = "right",
            legend.text = element_text(size = 9),  # Adjust text size
            legend.title = element_text(size = 8), # Adjust title size
            legend.key.size = unit(0.5, "cm"),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
    
  })
  
  # post game swings
  output$Pitchchartswings <- renderPlot({
    req(input$playerNamePost)
    
    start_date <- as.Date(input$date[1], format = "%Y-%m-%d")
    end_date <- as.Date(input$date[2], format = "%Y-%m-%d")
    
    post_data <- data |> 
      filter(Batter == input$playerNamePost,
             Date >= start_date,
             Date <= end_date,
             PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall"))
    
    pitches <- nrow(post_data)
    
    ggplot(post_data, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
      geom_point(size = 4) +
      geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black") +
      annotate('rect', xmin = -0.71, xmax = 0.71, ymin = 1.6, ymax = 3.5, fill = 'white', 
               color = 'red', alpha = 0.0001, size = 1) +
      labs(title = paste(input$playerNamePost, "-- Swings"),
           subtitle = paste("Num Pitches: ", pitches)) +
      xlim(-3, 3) +
      ylim(0, 4) +
      theme_bw() +
      theme(legend.position = "right",
            legend.text = element_text(size = 9),  # Adjust text size
            legend.title = element_text(size = 8), # Adjust title size
            legend.key.size = unit(0.5, "cm"),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
    
  })
  
  # post game takes
  output$Pitchcharttakes <- renderPlot({
    req(input$playerNamePost)
    
    start_date <- as.Date(input$date[1], format = "%Y-%m-%d")
    end_date <- as.Date(input$date[2], format = "%Y-%m-%d")
    
    post_data <- data |> 
      filter(Batter == input$playerNamePost,
             Date >= start_date,
             Date <= end_date,
             PitchCall %in% c("BallCalled", "BallinDirt", "HitByPitch", "StrikeCalled"))
    
    pitches <- nrow(post_data)
    
    ggplot(post_data, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
      geom_point(size = 4) +
      geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black") +
      annotate('rect', xmin = -0.71, xmax = 0.71, ymin = 1.6, ymax = 3.5, fill = 'white', 
               color = 'red', alpha = 0.0001, size = 1) +
      labs(title = paste(input$playerNamePost, "-- Takes"),
           subtitle = paste("Num Pitches: ", pitches)) +
      xlim(-3, 3) +
      ylim(0, 4) +
      theme_bw() +
      theme(legend.position = "right",
            legend.text = element_text(size = 9),  # Adjust text size
            legend.title = element_text(size = 8), # Adjust title size
            legend.key.size = unit(0.5, "cm"),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
    
  })
  
  
  
  
  output$tableOutput <- renderDT({
    req(input$playerNamePost)
    
    start_date <- as.Date(input$date[1], format = "%Y-%m-%d")
    end_date <- as.Date(input$date[2], format = "%Y-%m-%d")
    
    xba <- data |> 
      filter(Batter == input$playerNamePost,
             Date >= start_date,
             Date <= end_date,
             PitchCall == "InPlay") |> 
      mutate(count = paste(Balls, Strikes, sep = "-"),
             inning = paste(Top.Bottom, Inning, sep = " "),
             ExitSpeed = round(ExitSpeed, 2),
             Angle = round(Angle, 2)) |> 
      select(xba, ExitSpeed, Angle, TaggedPitchType, inning, count, Outs, PlayResult) |> 
      arrange(desc(ExitSpeed))
    
    
    datatable(xba, options = list(pageLength = 10, autoWidth = TRUE),
              colnames = c("Expected BA", "Exit Velocity", "Launch Angle",
                           "Pitch Type", "Inning", "Count", "Outs",
                           "Result"))
  })
  
  
  
  # spray chart
  output$sprayChart <- renderPlot({
    req(input$playerNameSpray)
    player_data <- data %>%
      filter(Batter == input$playerNameSpray,
             AutoHitType %in% c("FlyBall", "GroundBall", "LineDrive", "Popup"),
             PitchCall == "InPlay",
             PlayResult != "Sacrifice")
    
    ggplot(player_data, aes(x = hc_x, y = hc_y, color = AutoHitType)) +
      geom_spraychart(stadium_transform_coords = TRUE, stadium_segments = "all") +
      coord_fixed() +
      theme_void()
  })
  
  # pie chart
  output$hitTypeDistribution <- renderPlot({
    req(input$playerNameSpray)
    player_data <- data %>%
      filter(Batter == input$playerNameSpray,
             AutoHitType %in% c("FlyBall", "GroundBall", "LineDrive", "Popup")) 
    
    hit_types <- player_data %>%
      group_by(AutoHitType) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Percentage = Count / sum(Count) * 100)
    
    ggplot(hit_types, aes(x = "", y = Count, fill = AutoHitType)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5)) +
      theme_void() +
      labs(title = "Hit Type Distribution", fill = "Hit Type") +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5))
  })
  
  # whiff chart
  output$whiff_heat <- renderPlot({
    req(input$playerNameHeat)
    
    filtered_data <- data %>%
      filter(Batter == input$playerNameHeat, is_whiff == 1) %>%
      {if (input$pitchType != "") filter(., TaggedPitchType == input$pitchType) else .}
    
    if (nrow(filtered_data) == 0) {
      return()
    }
    
    # Name formatting
    names <- str_split(input$playerNameHeat, pattern = ", ")[[1]]
    first_name <- names[2]
    last_name <- names[1]
    
    num_pitches <- nrow(filtered_data)
    
    data |> 
      filter(Batter == input$playerNameHeat, 
             is_whiff == 1) |> 
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(geom = "polygon", aes(fill = after_stat(nlevel))) +
      xlim(c(2, -2)) +
      ylim(c(0, 5)) +
      theme_classic() +
      annotate('rect', xmin = -0.71, xmax = 0.71, ymin = 1.6, ymax = 3.5, fill = 'white', 
               color = 'red', alpha = 0.0001, size = 1) +
      geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black") +
      theme(legend.position = "none") +
      labs(title = paste(str_split(input$playerNameHeat, pattern = ", ")[[1]][2], 
                         str_split(input$playerNameHeat, pattern = ", ")[[1]][1], "Whiff Heat Map"),
           subtitle = paste("Num Pitches:", num_pitches)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
  })

  # chase map
  output$chase_heat <- renderPlot({
    req(input$playerNameHeat)
    
    # Filter data
    filtered_data <- data %>%
      filter(Batter == input$playerNameHeat, Zone %in% c(0, 11, 12, 13, 14)) %>%
      {if (input$pitchType != "") filter(., TaggedPitchType == input$pitchType) else .}
    
    # Early return if data is empty
    if (nrow(filtered_data) == 0) {
      return()
    }
    
    # Extract first and last name only once
    names <- str_split(input$playerNameHeat, pattern = ", ")[[1]]
    first_name <- names[2]
    last_name <- names[1]
    
    # Number of pitches
    num_pitches <- nrow(filtered_data)
    
    # Create the plot
    ggplot(filtered_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(geom = "polygon", aes(fill = after_stat(nlevel))) + 
      xlim(c(-2, 2)) +
      ylim(c(0, 5)) +
      theme_classic() +
      annotate('rect', xmin = -0.71, xmax = 0.71, ymin = 1.6, ymax = 3.5, fill = 'white', 
               color = 'red', alpha = 0, size = 1) +
      geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black") +
      theme(legend.position = "none") +
      labs(title = paste(first_name, last_name, "Chase Heat Map"),
           subtitle = paste("Num Pitches:", num_pitches)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
  })
  
  output$ev_heat <- renderPlot({
    req(input$playerNameHeat2) 
    
    zones <- data.frame(
      xmin = c(-0.83, -0.27, 0.27, -0.83, -0.27, 0.27, -0.83, -0.27, 0.27),
      xmax = c(-0.27, 0.27, 0.83, -0.27, 0.27, 0.83, -0.27, 0.27, 0.83),
      ymin = c(2.867, 2.867, 2.867, 2.234, 2.234, 2.234, 1.6, 1.6, 1.6),
      ymax = c(3.5, 3.5, 3.5, 2.867, 2.867, 2.867, 2.234, 2.234, 2.234),
      Zone = c(1:9)
    )
    
    segments <- data.frame(
      x = c(0, 0, -1.13, -1.13, -1.13, -1.13, 0, 0, 0, 1.13, 1.13, 1.13, 1.13, 0),
      y = c(3.5, 3.8, 3.8, 2.55, 2.55, 1.3, 1.3, 3.5, 3.8, 3.8, 2.55, 2.55, 1.3, 1.3),
      xend = c(0, -1.13, -1.13, -0.85, -1.13, 0, 0, 0, 1.13, 1.13, 0.85, 1.13, 0, 0),
      yend = c(3.8, 3.8, 2.55, 2.55, 1.3, 1.3, 1.6, 3.8, 3.8, 2.55, 2.55, 1.3, 1.3, 1.6)
    )
    
    avg_ev <- data |> 
      filter(Batter == input$playerNameHeat2) |> 
      group_by(Zone) |> 
      summarise(avg_ev = mean(ExitSpeed, na.rm = T)) |> 
      filter(Zone != 0) |> 
      mutate(x = c(-0.55, 0, 0.55, -0.55, 0, 0.55, -0.55, 0, 0.55, -1, 1, -1, 1),
             y = c(3.2, 3.2, 3.2, 2.57, 2.57, 2.57, 1.92, 1.92, 1.92, 3.7, 3.7, 1.4, 1.4))
    
    
    zones <- merge(zones, avg_ev, by = "Zone", all.x = TRUE)
    
    
    ggplot() +
      geom_rect(data = zones, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = avg_ev), alpha = 0.5) +
      geom_segment(data = segments, aes(x = x, y = y, xend = xend, yend = yend), color = "black", size = 1) +
      geom_text(data = avg_ev, aes(x = x, y = y, label = round(avg_ev, 1)), color = "black", size = 5) +
      geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black") +
      scale_fill_gradient2(low = "blue",mid = "white", high = "red", midpoint = median(data$ExitSpeed, na.rm = TRUE)) +  
      xlim(c(-1.5, 1.5)) +
      ylim(c(0, 5)) +
      labs(title = "Exit Velocity Distribution by Zone",
           x = "Plate Location Side",
           y = "Plate Location Height") +
      theme_classic() +
      theme(legend.position = "None",
            plot.title = element_text(hjust = 0.5, face = "bold"))
    
  })
  
  output$ev_hex <- renderPlot({
    req(input$playerNameHeat2) 
    
    data |> 
      filter(Batter == input$playerNameHeat2) |> 
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight, z = ExitSpeed))+
      stat_summary_hex(bins = 20) +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = median(data$ExitSpeed, na.rm = TRUE)) +
      annotate('rect', xmin = -0.71, xmax = 0.71, ymin = 1.6, ymax = 3.5, fill = 'black', 
               color = 'red', alpha = 0.0001, size = 1) + 
      coord_equal() + 
      xlim(c(-3, 3)) +
      ylim(c(0, 5)) +
      labs(fill = "Exit Velocity",
           title = "Exit Velocity Distribution By Bins") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "None")
    
  })
  
  output$ev_table <- renderDataTable({
    ev_table <- data |> 
      filter(Team == "Hyannis Harbor Hawks",
             PitchCall == "InPlay") |> 
      mutate(hard_hit = ifelse(ExitSpeed >= 95, 1, 0),
             is_sweet_spot = ifelse(Angle >= 8 & Angle <= 32, 1, 0)) |> 
      group_by(Batter) |> 
      summarise(
        BBE = n(),
        `LA(°)` = round(mean(Angle, na.rm = TRUE), 1),
        `LA SwSp%` = round(mean(is_sweet_spot, na.rm = TRUE) * 100, 1),
        max_ev = ifelse(all(is.na(ExitSpeed)), NA, round(max(ExitSpeed, na.rm = TRUE), 1)),
        avg_ev = round(mean(ExitSpeed, na.rm = TRUE), 1),
        max_distance = round(max(Distance, na.rm = TRUE)),
        `95+` = sum(hard_hit, na.rm = TRUE),
        hard_hit_rate = round(mean(hard_hit, na.rm = TRUE) * 100, 1),
        barrels = sum(barrel, na.rm = TRUE),
        barrels_per_bbe = round((barrels / BBE) * 100, 1)
      ) |> 
      filter(BBE >= 15) |> 
      arrange(desc(barrels_per_bbe))
    
    datatable(ev_table, options = list(
      pageLength = -1,  # Show all rows in one page
      searching = FALSE, # Disable search box
      paging = FALSE,    # Disable pagination
      info = FALSE,      # Hide the table info (e.g., "Showing 1 to 10 of 50 entries")
      ordering = TRUE    # Enable column sorting
    ), 
    class = "display compact cell-border stripe", 
    rownames = FALSE,
    colnames = c("Batter", "BBE", "LA(°)", "LA SwSp%",
                 "Max EV", "Average EV", "Max Distance",
                 "95+", "Hard Hit Rate", "Barrels", "Barrels/BBE%")
    )
    
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
