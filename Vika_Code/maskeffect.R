library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# Define UI
ui <- fluidPage(
  titlePanel("Effect of Mask Mandates: County-Level Scatter Plot"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_filter", 
                     "Select Date Range:",
                     start = as.Date("2020-06-01"),  # Default start date
                     end = as.Date("2020-07-31"),    # Default end date
                     min = min(merged_data$date),
                     max = max(merged_data$date)),
      checkboxInput("show_trend", "Show Trend Line", value = TRUE)
    ),
    
    mainPanel(
      plotlyOutput("scatter_plot")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Filter and normalize data
  normalized_data <- reactive({
    req(input$date_filter)  # Ensure the input is available
    
    # Filter data based on date range
    data <- merged_data %>%
      filter(
        date >= as.Date(input$date_filter[1]),
        date <= as.Date(input$date_filter[2]),
        mandate_status %in% c("Always Mandated", "Never Mandated")
      )
    
    # Calculate Z-scores (standardize within each group)
    data <- data %>%
      group_by(mandate_status) %>%
      mutate(
        z_cumulative_cases = scale(cumulative_cases_per_100k),
        z_cumulative_deaths = scale(cumulative_deaths_per_100k)
      ) %>%
      ungroup()
    
    data
  })
  
  # Render interactive scatter plot
  output$scatter_plot <- renderPlotly({
    data <- normalized_data()
    p <- ggplot(data, aes(x = z_cumulative_cases, y = z_cumulative_deaths, color = mandate_status,
                          text = paste("County:", county,
                                       "<br>Cumulative Cases per 100k:", round(cumulative_cases_per_100k, 2),
                                       "<br>Cumulative Deaths per 100k:", round(cumulative_deaths_per_100k, 2),
                                       "<br>Cases Z-Score:", round(z_cumulative_cases, 2),
                                       "<br>Deaths Z-Score:", round(z_cumulative_deaths, 2)))) +
      geom_point(alpha = 0.5, size = 2) +  # Smaller points for less clutter
      labs(
        title = "Scatter Plot: Cases vs. Deaths (Z-Scores)",
        x = "Normalized Cumulative Cases (Z-Score)",
        y = "Normalized Cumulative Deaths (Z-Score)",
        color = "Mandate Status"
      ) +
      theme_minimal()
    
    # Optionally add a trend line
    if (input$show_trend) {
      p <- p + geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1)
    }
    
    # Convert to interactive plot
    ggplotly(p, tooltip = "text")
  })
}

# Run the application
shinyApp(ui = ui, server = server)



