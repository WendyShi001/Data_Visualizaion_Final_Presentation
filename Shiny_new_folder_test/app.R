library(shiny)
library(shinythemes)
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
             tags$div(
             style = "display: flex; justify-content: center; align-items: center;",
             tags$img(src = "deathratepercounty.png", alt = "Death rate", 
                      style = "width:75%; height:auto;")
             )
    ),
    
    tabPanel("Political Ideology and Covid Death", 
             plotlyOutput("plotly")
    ),
    
    tabPanel("Ideology, Death and Population",
             sidebarPanel(
             selectInput("Selection", "Choose a Political Party:",
                         choices = c("Democrat", "Republican"))
             ),
             mainPanel(
               uiOutput("dynamicContent")
             )
    ),
    
    tabPanel("Covid Time Series",
             tags$div(
               style = "display: flex; justify-content: center; align-items: center; overflow: hidden;",
               tags$iframe(
                 src = "jasmine_chart.html",
                 width = "100%",
                 height = "600px",
                 frameborder = "0"
               )
             )
    ),
    
    # GGiraph Plot with Selection Bar
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
                             selected = "Black")
               ),
               mainPanel(
                 girafeOutput("interactive_plot")
               )
             )
    ),
    
    tabPanel("Mask Mandate", 
             tags$div(
               style = "display: flex; justify-content: center; align-items: center;",
               tags$img(src = "the effect of mask mandate.png", alt = "Death rate", 
                      style = "max-width: 60%; height: auto;"),
             )
    )#current tabPanel
    
  )# bracket to Tabset Panel
)#bracket to fluid page

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
}

# Run the application 
shinyApp(ui = ui, server = server)