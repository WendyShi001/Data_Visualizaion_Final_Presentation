library(shiny)
library(shinythemes)
library(tidycensus)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(sf)
library(here)
library(plotly)
library(ggplot2)
library(ggiraph)
library(dplyr)
library(forcats)
library(patchwork)
library(openxlsx)
library(stringr)
library(tigris)
library(leaflet)


# Load data
# Percentage of population minus percentage of people died
# If value less than 0, affect it more than other race, smaller the percentage
#more than proportion of people in that ethnicity died

plotting_data <- read.xlsx("Wendy_Plotting_data.xlsx")
selection_list <- c("Black", "Native_American", "Asian", "Pacific_Islander", "Hispanic")
#selection_list <- c("White", "Black", "Native_American", "Asian", "Pacific_Islander", "Hispanic")

election_data = read.csv("us_election_2020.csv")

election_data <- election_data %>% 
  select(c(state_abr, trump_win))

plotting_data <- plotting_data %>% 
  inner_join(election_data, by = c("State" = "state_abr"))

US_state_hexgrid <- sf::st_read("us_states_hexgrid.geojson")
US_state_hexgrid <- sf::st_transform(US_state_hexgrid, crs = 3857)

st_crs(US_state_hexgrid)
US_state_hexgrid <- st_make_valid(US_state_hexgrid)


US_state_hexgrid <- US_state_hexgrid %>% 
  rename(State = 'iso3166_2')

#Vika's Data Transformation-------------------------------------------------------

covid_case_orginial <- read.csv('../data/covid-cases.csv')
death <- read.csv('../data/Provisional_COVID-19_Deaths_by_County__and_Race_and_Hispanic_Origin_20241120.csv')

merged_data <- read.csv('../data/vika_mask_data.csv')

# Aggregate COVID metrics by mandate status and date
aggregated_data <- merged_data %>%
  group_by(mandate_status, date,fips_code,county,state,state_fips) %>%
  summarize(
    avg_cumulative_cases_per_100k = mean(cumulative_cases_per_100k, na.rm = TRUE),
    avg_cumulative_deaths_per_100k = mean(cumulative_deaths_per_100k, na.rm = TRUE),
    .groups = "drop"
  )

library(leaflet)
census_api_key('c3877d776d03f23f4f589f28c7344fe1a41a6297')

#-------------------------------------------------------------------------------------------------------

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$div(
    style = "text-align: center;",
    tags$div(
      style = "text-align: center; font-weight: bold; font-size: 30px;",
      "Analysis of Covid Death, Political Ideology and Race & Ethnicity"
    ),
    h3("Vika, Holt, Jiali, Jasmine, Wendy"),
    h4("Data Visualization Group Presentation")
  ),
  
  tabsetPanel(
    
        # Other Tabs without Selection Bar
        tabPanel("Death per County", 
               titlePanel("COVID-19 Death Rates by County"),
               sidebarLayout(
                 sidebarPanel(
                   p(HTML("This graph shows the Covid Death count by county")),
                 ),
             mainPanel(
               leafletOutput("covidMap", height = "800px")
             )
        )
        ),
    
    tabPanel("Political Ideology and Covid Death", 
             sidebarLayout(
               sidebarPanel(
                 p(HTML("This graph shows the relationship between Political Ideology and Covid Death")),
               ),
               mainPanel(
                 plotlyOutput("plotly")
               )
             )
    ),
    
    tabPanel("Ideology, Death and Population",
             sidebarPanel(
             selectInput("Selection", "Choose a Political Party:",
                         choices = c("Democrat", "Republican")),
             p(HTML("This graph shows the relationship between Political Ideology and Covid Death Count")),
             ),
             mainPanel(
               uiOutput("dynamicContent")
             )
    ),
    
    tabPanel("Covid Time Series",
             sidebarPanel(
               p(HTML("This graph provide a Covid time series analysis")),
             ),
             mainPanel(
               tags$div(
                 style = "display: flex; justify-content: center; align-items: center; overflow: hidden;",
                 tags$iframe(
                   src = "jasmine_chart.html",
                   width = "100%",
                   height = "600px",
                   frameborder = "0"
                 )
               )
             )
    ),
    
    # ggiraph Plot with Selection Bar
    tabPanel("Race & Ethnicity", 
             sidebarLayout(
               sidebarPanel(
                 p(HTML("<b>Example</b><br><br>
                 Index = Percentage of death for chosen race - percentage of chosen race in state population <br><br>
                 If New York State has 20% of Covid Death that is African American
                 and New York State has African Americans at 10% in the whole state
                 the index will be 10%. <br><br>
                 Positive index means the chosen race is more likely to die compared to other races. <br><br>")),
                 
                 selectInput(inputId = "ethni_select",
                             label = "Select Race:",
                             choices = selection_list,
                             selected = "Black"),
                 p(HTML("conclusions:")),
               ),
               mainPanel(
                 girafeOutput("interactive_plot")
               )
             )
    ),
    
    tabPanel("Mask Mandate", 
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("date_filter", 
                                "Select Date Range:",
                                start = as.Date("2020-06-01"),  # Default start date
                                end = as.Date("2020-07-31"),    # Default end date
                                min = min(merged_data$date),
                                max = max(merged_data$date)),
                 checkboxInput("show_trend", "Show Trend Line", value = TRUE),
                 p(HTML("conclusions:"))
               ),
               mainPanel(
                 plotlyOutput("scatter_plot")
               )
             )
    )#Current TabPanel
    
  )# End Bracket to Tabset Panel
)# UI/UX Panel

# Define Server
server <- function(input, output) {
  output$output1 <- renderText({
    input$select1
  })
  output$output2 <- renderText({
    input$select2
  })
  output$output3 <- renderText({
    input$select3
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



# Define server logic
server <- function(input, output) {
  
  #Holt's Data Visualization --------------------------------------------------------
  election_2020 <- read_csv("us_election_2020.csv", show_col_types = FALSE)
  covid_deaths_per_100k <- read_csv("covid_deaths_per_100K.csv", show_col_types = FALSE)
  state_deaths_per_100k <- left_join(covid_deaths_per_100k, election_2020, by = c("STATE" = "state_abr"))
  
  state_deaths_per_100k <- state_deaths_per_100k %>%
    filter(YEAR == 2020) %>%
    mutate(
      winner = case_when(
        trump_pct > biden_pct ~ "Trump",
        biden_pct > trump_pct ~ "Biden",
        TRUE ~ "Tie"  # Optional for ties
      ),
      winning_pct = pmax(trump_pct, biden_pct) # Max percentage of the winner
    )
  
  output$plotly <- renderPlotly({
    # Create ggplot object
    gg <- ggplot(state_deaths_per_100k, aes(x = winning_pct, y = RATE)) +
      # Points colored by winner with interactive text
      geom_point(aes(color = winner, 
                     text = paste0("State: ", state, 
                                   "<br>Winner: ", winner,
                                   "<br>Vote %: ", winning_pct, 
                                   "<br>Deaths per 100k: ", RATE)), 
                 size = 3, alpha = 0.7) +
      
      # Regression line for Trump
      geom_smooth(data = filter(state_deaths_per_100k, winner == "Trump"), 
                  aes(x = winning_pct, y = RATE, color = "Trump"), 
                  method = "lm", se = FALSE, linetype = "dashed") +
      
      # Regression line for Biden
      geom_smooth(data = filter(state_deaths_per_100k, winner == "Biden"), 
                  aes(x = winning_pct, y = RATE, color = "Biden"), 
                  method = "lm", se = FALSE, linetype = "dashed") +
      
      # Custom color scale
      scale_color_manual(
        name = "Winner",
        values = c("Trump" = "red", "Biden" = "blue", "Tie" = "gray"),
        labels = c("Trump (Red)", "Biden (Blue)", "Tie (Gray)")
      ) +
      
      # Labels and theme
      labs(
        title = "COVID-19 Death Rate vs. State Voting Percentages (2020 Election)",
        x = "Winning Candidate Voting Percentage by State",
        y = "COVID-19 Deaths per 100k",
        caption = "Source: Your Data Source"
      ) +
      theme_minimal()
    
    # Convert to interactive plotly
    ggplotly(gg, tooltip = "text")
  })
  
  
  #Jiali's Dynamic output----------------------------------------------------------
  
  output$dynamicContent <- renderUI({
    if (input$Selection == "Democrat") {
      tags$iframe(
        src = "Democratic_Votes_vs_Total_Deaths(2020).html",
        width = "100%",
        height = "600px",
        frameborder = "0"
      )
    } else if (input$Selection == "Republican") {
      tags$iframe(
        src = "Republican_Votes_vs_Total_Deaths(2020).html",
        width = "100%",
        height = "600px",
        frameborder = "0"
      )
    }
  })
  
  #Wendy's Visualization ----------------------------------------------------------
  output$interactive_plot <- renderGirafe({
    
    # Generate geo plot data
    geo_data <- US_state_hexgrid %>% 
      inner_join(plotting_data, by = c("State" = "State")) %>% 
      mutate(tooltip = paste0(
        "<b>State:</b> ", State_name, "<br>",
        "<b>Ethnicity:</b> ", input$ethni_select, "<br>",
        "<b>Difference:</b> ", !!sym(input$ethni_select), "<br>",
        "<b>Trump Win:", trump_win
      ))
    
    geo_interactive_plot <- ggplot(geo_data) +
      geom_sf_interactive(
        aes(fill = !!sym(input$ethni_select), tooltip = tooltip, data_id = State),
        color = "white"
      ) +
      geom_sf_text(aes(label = State), color = "white", size = 3) +
      scale_fill_distiller(palette = "YlGnBu", name = "Index value") +
      labs(title = "Percentage of Death and Percentage of Populatoin Difference",
           subtitle = "A visualization of how covid affect different ethnicity",
           caption = "Source: Center of Disease Control") + 
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
            plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 9),
            plot.caption  = element_text(size = 6),
            legend.position = "bottom",
            legend.key.width = unit(2, "cm"), 
            legend.key.height = unit(0.5, "cm"))
    
    # Generate bar plot data
    subset <- plotting_data %>% 
      select(State, State_name, all_of(input$ethni_select), trump_win) %>% 
      arrange(!!sym(input$ethni_select))
    
    top_5 <- subset %>%
      arrange(desc(!!sym(input$ethni_select))) %>%
      slice(1:5)
    
    bottom_5 <- subset %>%
      arrange(!!sym(input$ethni_select)) %>%
      slice(1:5)
    
    result <- bind_rows(top_5, bottom_5) %>%
      mutate(tooltip = paste0(
        "<b>State:</b> ", State_name, "<br>",
        "<b>Ethnicity:</b> ", input$ethni_select, "<br>",
        "<b>Difference:</b> ", !!sym(input$ethni_select), "<br>",
        "<b>Trump Win:", trump_win
      ))
    
    bar_interactive <- ggplot(result, aes(x = fct_reorder(as.factor(State), !!sym(input$ethni_select)),
                                          y = !!sym(input$ethni_select),
                                          fill = !!sym(input$ethni_select) > 0)) +
      geom_bar_interactive(stat = "identity",
                           aes(tooltip = tooltip, data_id = State),
                           show.legend = FALSE) +
      scale_fill_manual(values = c("pink", "lightblue")) +
      labs(title = "Top5 and Bottom 5 Difference",
           x = "States",
           y = "Percentage") +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 8))
    
    # Combine plots
    combined_plot <- (geo_interactive_plot / bar_interactive) + plot_layout(heights = c(2.5, 1))
    interactive_combined_plot <- girafe(ggobj = combined_plot)
    
    # Apply hover options
    girafe_options(
      interactive_combined_plot,
      opts_hover(css = "fill:red;stroke:black;")
    )
  })
  
  #Vika's visualization--------------------------------------------------------------
  #First Plot:
  # Load population data
  population_data <- reactive({
    get_acs(
      geography = "county",
      variables = "B01003_001",
      year = 2020,
      survey = "acs5"
    ) %>%
      rename(
        fips_code = GEOID,
        population = estimate
      )
  })
  
  # Load COVID-19 case data
  covid_case <- reactive({
    original_covid_case <- read.csv('../data/covid-cases.csv')
    
    original_covid_case %>%
      mutate(fips_code = str_pad(as.character(fips_code), width = 5, pad = "0")) %>%
      group_by(county, state, fips_code) %>%
      summarize(total_deaths = max(cumulative_deaths, na.rm = TRUE)) %>%
      left_join(population_data(), by = "fips_code") %>%
      mutate(death_rate_per_100k = (total_deaths / population) * 100000)
  })
  
  # Prepare map data
  map_data <- reactive({
    counties_sf <- counties(cb = TRUE, year = 2020, class = "sf")
    counties_sf %>%
      left_join(covid_case(), by = c("GEOID" = "fips_code")) %>%
      st_transform(crs = 4326)
  })
  
  output$covidMap <- renderLeaflet({
    leaflet(data = map_data()) %>%
      setView(lng = -98.35, lat = 39.50, zoom = 4) %>%  
      addPolygons(
        fillColor = ~colorNumeric("YlOrRd", death_rate_per_100k)(death_rate_per_100k),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        popup = ~paste(
          "<b>County:</b> ", county, "<br>",
          "<b>Total Deaths:</b> ", total_deaths, "<br>",
          "<b>Population:</b> ", population, "<br>",
          "<b>Death Rate (per 100k):</b> ", round(death_rate_per_100k, 2)
        )
      ) %>%
      addLegend(
        pal = colorNumeric("YlOrRd", NULL),
        values = ~death_rate_per_100k,
        title = "Deaths per 100,000",
        position = "bottomright"
      )
  })
  
  #Vika's Second plot---------------------------------------
  
  
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
  
  output$covidMap <- renderLeaflet({
    leaflet(data = map_data()) %>%
      setView(lng = -98.35, lat = 39.50, zoom = 4) %>%  
      addPolygons(
        fillColor = ~colorNumeric("YlOrRd", death_rate_per_100k)(death_rate_per_100k),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        popup = ~paste(
          "<b>County:</b> ", county, "<br>",
          "<b>Total Deaths:</b> ", total_deaths, "<br>",
          "<b>Population:</b> ", population, "<br>",
          "<b>Death Rate (per 100k):</b> ", round(death_rate_per_100k, 2)
        )
      ) %>%
      addLegend(
        pal = colorNumeric("YlOrRd", NULL),
        values = ~death_rate_per_100k,
        title = "Deaths per 100,000",
        position = "bottomright"
      )
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