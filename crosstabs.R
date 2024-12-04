# Libraries
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr) 

# load the dataset
dataset <- read.csv("survey_cleaned.csv", stringsAsFactors = FALSE)

# UI
ui <- fluidPage(
  titlePanel("Mental Health Survey Visualizations"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_choice", "Choose a Plot:",
                  choices = c("Age vs Work Interference",
                              "Company Size vs Mental Health Benefits",
                              "Remote Work vs Treatment",
                              "Mental vs Physical Health Consequences",
                              "Supervisor Support vs Coworker Support",
                              "Treatment vs Family History")),
      selectInput("gender", "Filter by Gender:",
                  choices = c("All", unique(dataset$Gender)))
    ),
    mainPanel(
      textOutput("plot_subtitle"),  # Add subtitle above the plot
      plotlyOutput("selected_plot", width = "100%", height = "700px")
    )
  )
)

# server
server <- function(input, output) {
  
  # Reactive dataset filtered by gender
  filtered_data <- reactive({
    if (input$gender == "All") {
      dataset
    } else {
      dataset %>% filter(Gender == input$gender)
    }
  })
  
  # Reactive subtitle text
  output$plot_subtitle <- renderText({
    if (input$plot_choice == "Age vs Work Interference") {
      return("Q: If you have a mental health condition, do you feel that it interferes with your work?")
    } else if (input$plot_choice == "Company Size vs Mental Health Benefits") {
      return("Q: Does your employer provide mental health benefits?")
    } else if (input$plot_choice == "Remote Work vs Treatment") {
      return("Q: Do you work remotely (outside of an office) at least 50% of the time?\nQ: Have you sought treatment for a mental health condition?")
    } else if (input$plot_choice == "Mental vs Physical Health Consequences") {
      return("Q: Do you think that discussing a mental health issue with your employer would have negative consequences?\nQ: Do you think that discussing a physical health issue with your employer would have negative consequences?")
    } else if (input$plot_choice == "Supervisor Support vs Coworker Support") {
      return("Q: Would you be willing to discuss a mental health issue with your coworkers?\nQ: Would you be willing to discuss a mental health issue with your direct supervisor(s)?")
    } else if (input$plot_choice == "Treatment vs Family History") {
      return("Q: Do you have a family history of mental illness?\nQ: Have you sought treatment for a mental health condition?")
    }
  })
  
  # Render  selected plot
  output$selected_plot <- renderPlotly({
    data <- filtered_data()
    
    if (input$plot_choice == "Age vs Work Interference") {
      plot <- ggplot(data, aes(x = work_interfere, y = Age)) +
        geom_point(alpha = 0.7, color = "#377eb8") +
        labs(x = "Work Interference", y = "Age") +
        theme_minimal(base_size = 15)
      
    } else if (input$plot_choice == "Company Size vs Mental Health Benefits") {
      benefits_data <- data %>%
        group_by(no_employees, benefits) %>%
        summarise(count = n(), .groups = "drop") %>%
        group_by(no_employees) %>%
        mutate(percent = count / sum(count) * 100) %>%
        mutate(no_employees = factor(no_employees, levels = c("1-5", "6-25", "26-100", "100-500", "500-1000", "More than 1000")))
      
      plot <- ggplot(benefits_data, aes(y = no_employees, x = percent, fill = benefits)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(aes(label = sprintf("%.2f%%", percent)), position = position_stack(vjust = 0.5), size = 3) +
        labs(x = "Percentage (%)", y = "", fill = "Benefits") +
        scale_fill_brewer(palette = "Set2") +
        theme_minimal(base_size = 15)
      
    } else if (input$plot_choice == "Remote Work vs Treatment") {
      treatment_data <- data %>%
        group_by(remote_work, treatment) %>%
        summarise(count = n(), .groups = "drop") %>%
        group_by(remote_work) %>%
        mutate(percent = count / sum(count) * 100)
      
      plot <- ggplot(treatment_data, aes(x = remote_work, y = percent, fill = treatment)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_text(aes(label = sprintf("%.2f%%", percent)), position = position_dodge(width = 0.9), size = 3) +
        labs(x = "Remote Work", y = "%", fill = "Treatment") +
        scale_fill_brewer(palette = "Dark2") +
        ylim(0, 100) +
        theme_minimal(base_size = 15)
      
    } else if (input$plot_choice == "Mental vs Physical Health Consequences") {
      # Prepare data for heatmap
      heatmap_data <- data %>%
        group_by(mental_health_consequence, phys_health_consequence) %>%
        summarise(count = n(), .groups = "drop") %>%
        complete(mental_health_consequence, phys_health_consequence, fill = list(count = 0)) %>% # Fill missing with zero
        group_by(mental_health_consequence) %>%
        mutate(percent = count / sum(count) * 100)  
      
      # Plot the heatmap
      plot <- ggplot(heatmap_data, aes(x = phys_health_consequence, y = mental_health_consequence, fill = percent)) +
        geom_tile() +
        geom_text(aes(label = sprintf("%.2f%%", percent)), color = "black", size = 3) +
        scale_fill_distiller(palette = "YlGnBu", name = "Percentage", direction = 1) + 
        labs(
          title = "Mental vs Physical Health Consequences",
          subtitle = "Q: Do you think that discussing a mental health issue with your employer would have negative consequences?\nQ: Do you think that discussing a physical health issue with your employer would have negative consequences?",
          x = "Physical Health Consequences",
          y = "Mental Health Consequences"
        ) +
        theme_minimal(base_size = 15)
      
    } else if (input$plot_choice == "Supervisor Support vs Coworker Support") {
      support_data <- data %>%
        group_by(coworkers, supervisor) %>%
        summarise(count = n(), .groups = "drop") %>%
        group_by(coworkers) %>%
        mutate(percent = count / sum(count) * 100)
      
      plot <- ggplot(support_data, aes(x = coworkers, y = percent, fill = supervisor)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(aes(label = sprintf("%.2f%%", percent)), position = position_stack(vjust = 0.5), size = 3) +
        labs(x = "Coworker Support", y = "%", fill = "Supervisor") +
        scale_fill_brewer(palette = "Pastel1") +
        theme_minimal(base_size = 15)
      
    } else if (input$plot_choice == "Treatment vs Family History") {
      family_data <- data %>%
        group_by(family_history, treatment) %>%
        summarise(count = n(), .groups = "drop") %>%
        group_by(family_history) %>%
        mutate(percent = count / sum(count) * 100)
      
      plot <- ggplot(family_data, aes(x = family_history, y = percent, fill = treatment)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_text(aes(label = sprintf("%.2f%%", percent)), position = position_dodge(width = 0.9), size = 3) +
        labs(x = "Family History", y = "%", fill = "Treatment") +
        scale_fill_brewer(palette = "Paired") +
        ylim(0, 100) +
        theme_minimal(base_size = 15)
    }
    
    # Render t
    ggplotly(plot)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
                        