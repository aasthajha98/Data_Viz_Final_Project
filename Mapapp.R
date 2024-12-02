library(shiny)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(tigris)
library(plotly)


# Load the survey data
survey_data <- read.csv("survey_cleaned.csv")
survey_data <- survey_data %>%
  mutate(
    age_bin = case_when(
      Age >= 18 & Age <= 24 ~ "18-24",
      Age >= 25 & Age <= 34 ~ "25-34",
      Age >= 35 & Age <= 44 ~ "35-44",
      Age >= 45 & Age <= 54 ~ "45-54",
      Age >= 55 & Age <= 64 ~ "55-64",
      Age >= 65 ~ "65+",
      TRUE ~ "Unknown"
    )
  )


survey_data = survey_data %>%
  mutate(work_interfere_num = case_when(work_interfere == "Often" ~ 4,
                                        work_interfere == "Sometimes" ~ 3,
                                        work_interfere == "Rarely" ~ 2,
                                        work_interfere == "Never" ~ 1,
                                        TRUE ~ 0),
         obs_consequence_num = case_when(obs_consequence == "Yes" ~ 1,
                                         obs_consequence == "No" ~ 0,
                                         TRUE ~ 0))
# Load state shapefile for the choropleth
states <- tigris::states(cb = TRUE, year = 2015) # Ensure this returns a valid sf object


# UI
ui <- fluidPage(
  titlePanel("Choropleth of Work Interference by State"),
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        inputId = "age_bin",
        label = "Filter by Age Bin",
        choices = unique(survey_data$age_bin),
        selected = unique(survey_data$age_bin),
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        inputId = "gender",
        label = "Filter by Gender",
        choices = unique(survey_data$Gender),
        selected = unique(survey_data$Gender),
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        inputId = "no_employees",
        label = "Filter by Number of Employees",
        choices = unique(survey_data$no_employees),
        selected = unique(survey_data$no_employees),
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        inputId = "remote_work",
        label = "Filter by Remote Work",
        choices = unique(survey_data$remote_work),
        selected = unique(survey_data$remote_work),
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        inputId = "tech_company",
        label = "Filter by Tech Company",
        choices = unique(survey_data$tech_company),
        selected = unique(survey_data$tech_company),
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      )
    ),
    mainPanel(
      plotlyOutput("choropleth", height = "800px", width = "1200px")  # Matches server dimensions
    )
  )
)

# Server
server <- function(input, output, session) {
  filtered_data <- reactive({
    survey_data %>%
      filter(
        age_bin %in% input$age_bin,
        Gender %in% input$gender,
        no_employees %in% input$no_employees,
        remote_work %in% input$remote_work,
        tech_company %in% input$tech_company
      ) %>%
      group_by(state) %>%
      summarize(work_interfere_avg = mean(work_interfere_num, na.rm = TRUE))
  })
  
  output$choropleth <- renderPlotly({
    # Ensure `states` is correctly initialized
    if (!inherits(states, "sf")) {
      states <- tigris::states(cb = TRUE, year = 2020) %>%
        filter(!(STUSPS %in% c("HI", "AS", "GU", "MP", "PR", "VI")))  # Exclude Hawaii and territories
      states <- tigris::shift_geometry(states)  # Shift Alaska closer to mainland
    }
    
    map_data <- states %>%
      left_join(filtered_data(), by = c("STUSPS" = "state"))
    
    # Create ggplot
    p <- ggplot(map_data) +
      geom_sf(aes(fill = work_interfere_avg, text = paste(
        "State: ", NAME, "<br>",
        "Avg Work Interference: ", round(work_interfere_avg, 2)
      )), color = "white") +
      scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
      coord_sf() +
      theme_minimal(base_size = 14) +
      theme(
        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "white", color = NA)
      ) +
      labs(
        title = "Work Interference Distribution by State",
        fill = "Avg Work Interference"
      )
    
    # Convert ggplot to plotly for interactivity
    ggplotly(p, tooltip = "text") %>%
      layout(
        height = 800,  # Increase height of the map
        width = 1200,  # Increase width of the map
        dragmode = "zoom",  # Enable zooming and panning
        margin = list(l = 0, r = 0, t = 50, b = 0)  # Reduce margins
      )
  })
  
  
  
  
}

shinyApp(ui, server)
