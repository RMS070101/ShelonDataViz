#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#



# Load necessary libraries
library(shiny)
library(ggplot2)
library(ggrain)
library(dplyr)
football_data = read.csv("passing_cleaned.csv", header = TRUE)
rushing_data = read.csv("rushing_cleaned.csv", header = TRUE)
football_data$Sufficient_Attempts <- ifelse(football_data$Att >= 20, "yes", "no")
filtered_data <- football_data[football_data$Att > 20 & football_data$GS > 12,]
library(GGally)
football_data$Yards <- ifelse(football_data$Yds < 2000, "low",
                              ifelse(football_data$Yds >= 2000 & football_data$Yds < 4000, "medium", "high"))
football_data$Yards <- factor(football_data$Yards, levels = c("low", "medium", "high"))
filtered_data$Yards <- ifelse(filtered_data$Yds < 2000, "low",
                              ifelse(filtered_data$Yds >= 2000 & filtered_data$Yds < 4000, "medium", "high"))
filtered_data$Yards <- factor(filtered_data$Yards, levels = c("low", "medium", "high"))
football_data$incompletions <- football_data$Att - football_data$Cmp
filtered_data$incompletions <- filtered_data$Att - filtered_data$Cmp
football_data$completion_rate <- football_data$Cmp / football_data$Att
filtered_data$completion_rate <- filtered_data$Cmp / filtered_data$Att
# Calculate highest passing yards per year
max_yards_per_year <- football_data %>%
  group_by(Year) %>%
  summarise(max_yards = max(Yds, na.rm = TRUE))
max_values <- football_data %>%
  group_by(Year) %>%
  summarise(max_TD = max(TD, na.rm = TRUE),
            max_Int = max(Int, na.rm = TRUE),
            .groups = "drop")  # Use .groups = "drop" to avoid grouped data
max_values_long <- max_values %>%
  tidyr::pivot_longer(cols = c(max_TD, max_Int), names_to = "Variable", values_to = "Max_Value")
# Calculate average passing touchdowns (TDs) by Year
avg_passing_tds <- football_data %>%
  group_by(Year) %>%
  summarise(avg_TD = mean(TD, na.rm = TRUE),
            .groups = "drop")  # Use .groups = "drop" to avoid grouped data
football_data <- football_data %>%
  mutate(age_group = case_when(
    Age <= 28 ~ "young",
    Age > 28 & Age <= 34 ~ "medium",
    Age > 34 ~ "old"
  ))
top_passers <- football_data %>%
  group_by(Year) %>%
  top_n(1, Yds) %>%
  ungroup()
top_passers <- top_passers %>%
  mutate(age_group = case_when(
    Age <= 28 ~ "young",
    Age > 28 & Age <= 34 ~ "medium",
    Age > 34 ~ "old"
  ))
age_group_counts <- top_passers %>%
  count(age_group)
football_data$FantasyPPG = ((football_data$Yds * .04) + (football_data$TD * 4) - (football_data$Int * 2)) / (football_data$G)
filtered_data$FantasyPPG = ((filtered_data$Yds * .04) + (filtered_data$TD * 4) - (filtered_data$Int * 2)) / (filtered_data$G)


# Merge rushing touchdowns (rTD) into filtered_data for matching players and years
shelon_set <- filtered_data %>%
  inner_join(rushing_data, by = c("Player", "Year"))
# Update QB_Type based on conditions
shelon_set <- shelon_set %>%
  mutate(QB_Type = ifelse(rYds > 400 & rTD > 4, "Mobile QB", "Pocket Passer"))

# Define UI
ui <- fluidPage(
  titlePanel("Quarterback Passing and Rushing Yards Over Years"),
  sidebarLayout(
    sidebarPanel(
      textInput("quarterbackInput", "Enter Quarterback's Name (2001-2023):"),
      actionButton("submitBtn", "Submit"),
      actionButton("resetBtn", "Reset")
    ),
    mainPanel(
      plotOutput("passingYardsPlot"),
      plotOutput("rushingYardsPlot")
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Initialize list to store player data
  player_data <- reactiveValues(data_list = list())
  
  observeEvent(input$submitBtn, {
    # Get quarterback's name from input
    quarterback_name <- input$quarterbackInput
    
    # Filter data for the selected quarterback
    qb_data <- filter(shelon_set, Player == quarterback_name) %>%
      arrange(Year)  # Sort data by year
    
    # Store or update player data in reactive list
    if (quarterback_name %in% names(player_data$data_list)) {
      player_data$data_list[[quarterback_name]] <- qb_data
    } else {
      player_data$data_list[[quarterback_name]] <- qb_data
    }
    
    # Update passing yards plot
    output$passingYardsPlot <- renderPlot({
      plotPassingYards(player_data$data_list)
    })
    
    # Update rushing yards plot
    output$rushingYardsPlot <- renderPlot({
      plotRushingYards(player_data$data_list)
    })
  })
  
  observeEvent(input$resetBtn, {
    # Clear all player data
    player_data$data_list <- list()
  })
  
  # Function to plot passing yards
  plotPassingYards <- function(player_data_list) {
    unique_players <- names(player_data_list)
    num_players <- length(unique_players)
    color_palette <- rainbow(num_players)
    
    plot_data <- lapply(seq_along(unique_players), function(i) {
      player <- unique_players[i]
      color <- color_palette[i]
      ggplot(player_data_list[[player]], aes(x = Year, y = Yds)) +
        geom_line(color = color) +
        labs(title = paste("Passing Yards Over Years -", player),
             x = "Year", y = "Passing Yards") +
        theme_minimal()
    })
    do.call(gridExtra::grid.arrange, plot_data)
  }
  
  # Function to plot rushing yards
  plotRushingYards <- function(player_data_list) {
    unique_players <- names(player_data_list)
    num_players <- length(unique_players)
    color_palette <- rainbow(num_players)
    
    plot_data <- lapply(seq_along(unique_players), function(i) {
      player <- unique_players[i]
      color <- color_palette[i]
      ggplot(player_data_list[[player]], aes(x = Year, y = rYds)) +
        geom_line(color = color) +
        labs(title = paste("Rushing Yards Over Years -", player),
             x = "Year", y = "Rushing Yards") +
        theme_minimal()
    })
    do.call(gridExtra::grid.arrange, plot_data)
  }
}

# Run Shiny app
shinyApp(ui = ui, server = server)

