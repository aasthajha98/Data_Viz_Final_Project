# Load required libraries
library(shiny)
library(tm)
library(dplyr)
library(syuzhet)
library(RColorBrewer) 
library(ggplot2)
library(tidytext)
library(dplyr)
library(stringr)
library(ggwordcloud)
library(plotly)
library(tidyr) 
library(shinyWidgets)
library(sf)
library(tigris)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(shinythemes)
library(scales)

# Set the colors for the plots
# Create custom color palette
custom_colors <- c(brewer.pal(8, "Set2"), brewer.pal(12, "Set3"), brewer.pal(12, 'Paired'))
custom_colors <- custom_colors[!custom_colors %in% c("#FFFFB3", "#FFED6F")]

# Load the dataset
dataset <- read.csv("survey_cleaned.csv", stringsAsFactors = FALSE)

# Preprocess the dataset
# Filters out empty or NA comments and adds sentiment analysis
wc_dataset <- dataset %>% 
  mutate(
    # Indicator for whether a comment was left
    has_comment = !is.na(comments) & comments != "" & comments != "NA" & comments != "-",
    
    # Calculate sentiment score using AFINN lexicon
    sentiment_score = get_sentiment(comments, method = "afinn"),
    
    # Categorize sentiment based on score
    sentiment_category = case_when(
      sentiment_score > 0 ~ "Positive",
      sentiment_score < 0 ~ "Negative",
      TRUE ~ "Neutral"
    ),
    
    # Perform NRC emotion lexicon analysis
    nrc_emotions = lapply(comments, function(comment) {
      # Get emotion scores for each comment
      emotion_scores <- get_nrc_sentiment(comment)
      
      # Convert to a named list of emotions
      emotion_names <- names(emotion_scores)[1:10]
      emotion_values <- as.numeric(emotion_scores[1:10])
      names(emotion_values) <- emotion_names
      emotion_values
    }),
    
    # Determine dominant emotion for each comment
    dominant_emotion = sapply(nrc_emotions, function(emotions) {
      if(max(emotions) > 0) {
        names(emotions)[which.max(emotions)]
      } else {
        "neutral"
      }
    })
  )

# Age bins
survey_data <- dataset %>%
  mutate(
    age_bin = case_when(
      Age >= 18 & Age <= 30 ~ "18-30",
      Age >= 30 & Age <= 45 ~ "30-45",
      Age >= 45 & Age <= 60 ~ "45-60",
      Age >= 60 ~ "60+",
      TRUE ~ "Unknown"
    )
  )

survey_data <- survey_data %>%
  mutate(
    age_bin = factor(age_bin, levels = c("18-30", "30-45", "45-60", "60+"), ordered = TRUE),
    no_employees = factor(no_employees, levels = c("1-5", "6-25", "26-100", "100-500", "500-1000", "More than 1000"), ordered = TRUE)
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

us_data = survey_data %>%
  filter(Country == 'United States')


state_data = states()


us_data_summarized = us_data %>%
  mutate(work_interfere_num = case_when(work_interfere == "Often" ~ 4,
                                        work_interfere == "Sometimes" ~ 3,
                                        work_interfere == "Rarely" ~ 2,
                                        work_interfere == "Never" ~ 1,
                                        TRUE ~ 0),
         obs_consequence_num = case_when(obs_consequence == "Yes" ~ 1,
                                         obs_consequence == "No" ~ 0,
                                         TRUE ~ 0)) %>%
  group_by(state) %>%
  summarise(avg_work_interfere = mean(work_interfere_num),
            avg_obs_consequence = mean(obs_consequence_num))


merged_data = merge(state_data, us_data_summarized, by.x = 'STUSPS',
                    by.y = 'state')

merged_data = merged_data %>%
  st_transform(crs = 4326)

bins = c(0, 1, 2, 3, 4)

pal <- colorBin("YlOrRd", domain = merged_data$avg_work_interfere, bins = bins)



merged_us_data_geom = merge(us_data, 
                            state_data, 
                            by.x = 'state', 
                            by.y = "NAME")


# Define UI
ui <- navbarPage(
  "Mental Health Survey Dashboard",theme = shinytheme("flatly"),
  
tabPanel("Insights", 
           fluidPage(
             titlePanel("Mental Health Survey Visualizations"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("plot_choice", "Choose a Plot:",
                             choices = c(
                               "Company Size vs Mental Health Benefits",
                               "Remote Work vs Treatment",
                               "Mental vs Physical Health Consequences",
                               "Supervisor Support vs Coworker Support",
                               "Treatment vs Family History"
                               
                             )
                 ),
                 selectInput("gender2", "Filter by Gender:",
                             choices = c("All", unique(dataset$Gender))
                 )
               ),
               mainPanel(
                 wellPanel(
                   textOutput("plot_subtitle")
                 ),
                 wellPanel(
                   plotlyOutput("selected_plot", width = "100%", height = "700px")
                 ),
                 wellPanel(
                   textOutput("plot_description")
                 )
               )
               
             )
           )
  ),
  
  tabPanel("Work Interference in the US", 
  fluidPage(
    titlePanel("Choropleth of Work Interference by State"),
    sidebarLayout(
      sidebarPanel(
        pickerInput(
          inputId = "age_bin",
          label = "Filter by Age Bin",
          choices = unique(levels(survey_data$age_bin)),
          selected = unique(survey_data$age_bin),
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        pickerInput(
          inputId = "gender3",
          label = "Filter by Gender",
          choices = unique(survey_data$Gender),
          selected = unique(survey_data$Gender),
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        pickerInput(
          inputId = "no_employees",
          label = "Filter by Number of Employees",
          choices = unique(levels(survey_data$no_employees)),
          selected = unique(levels(survey_data$no_employees)),
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
        plotlyOutput("choropleth", height = "600px", width = "900px"),
        # Add an explanatory panel
        wellPanel(
          tags$h4("How to Read This Visualization"),
          tags$p(
            "This choropleth map shows the average rate of work interference reported by survey respondents across different U.S. states. ", 
            "The color intensity represents the severity of work interference: lighter colors indicate higher rates of interference."
          ),
          tags$h4("Key insights"),
          tags$p(
            "We can see that the colors across states are much darker when we filter on just the 45+ age groups compared the below 45 age groups which indicates tha elder people do not report mental health interfering with their work as much as younger employees do.", 
            "This tracks with existing knowledge about older demographics not being as open about speaking of their mental health as those from younger demographics and thus being less likely to report those effects to a survey.",
            "We also see that when filtering on remote work vs no remote work that the distribution is much lighter in color across states when looking at respondants at companies that do not offer remote work."
          ),
          tags$p(
            HTML("<strong>Note:</strong> Grayed-out states indicate that no survey data was available for those locations.")
          )
        )
      )
    )
  )
),
tabPanel("Sentiment Analysis", 
         fluidPage(
           titlePanel("Sentiment Word Cloud of Comments in Survey"),
           sidebarLayout(
             sidebarPanel(
               # Dropdown for selecting country
               selectInput("country", "Select Country:",
                           choices = c("All", unique(dataset$Country))),
               
               # Dropdown for selecting gender
               selectInput("gender", "Select Gender:",
                           choices = c("All", unique(dataset$Gender))),
               
               # Dropdown for selecting sentiment
               selectInput("sentiment", "Select Sentiment of the Comment:",
                           choices = c("All", "Positive", "Neutral", "Negative")),
               
               # Slider for number of words in word cloud
               sliderInput("numWords", "Number of Words in the Word Cloud:", 
                           min = 1, max = 100, value = 50),
               
               # Checkbox group for selecting emotion categories
               checkboxGroupInput("emotion_categories", "Select Main Emotion of Comment:",
                                  choices = c("anger", "anticipation", "disgust", 
                                              "fear", "joy", "sadness", 
                                              "surprise", "trust", "neutral"),
                                  selected = c("anger", "anticipation", "disgust", 
                                               "fear", "joy", "sadness", 
                                               "surprise", "trust", "neutral"))
             ),
             
             # Main panel with data table and visualizations
             mainPanel(
               # Explanation of sentiment analysis
               tags$p("The sentiment was analyzed using the 'AFINN' lexicon, 
         which assigns scores from -5 (negative) to 5 (positive) 
         based on the words in each comment. The emotions come from 
         sentiment analysis from NRC and represent the most common 
         emotion from the words in the comment."),
               
               tags$p("The most prominent fact about this analysis is the difference 
             between gender. Females have a much larger negative sentiment 
             average on their comments."),
               
               # Table to show comment statistics
               tableOutput("comment_stats"),
               
               # Word cloud plot
               plotOutput("word_cloud"),
               
               # Comparative Demographic Plots
               tags$h3("Comparative Demographics: All Respondents vs Comment Leavers"),
               tags$p("To understand the possible bias in the comments, here is a comparison
             between comment leavers and those who did not."),
               
               tags$p("In general, they are older, work in smaller companies and are not as likely
             to feel that discussing mental health problems at work will have negative consequences."),
               
               # No. of Employees Comparison
               tags$p("People that work in smaller companies are more likely to have left a comment."),
               plotOutput("wc_no_employees_comparison"),
               
               # Mental Health Consequence Comparison
               tags$p("The people that left a comment are more likely to state that discussing
             mental health consequences at work will not have a consequence
             with their employers."),
               plotOutput("wc_mental_health_consequence_comparison"),
               
               # Age Comparison
               tags$p("The people that left comments are older than the ones that did not."),
               plotOutput("wc_age_comparison")
             )
         )
      )
),
tabPanel(
  "Insights by State",
  fluidRow(
    column(
      width = 4,
      selectInput(
        inputId = "input_state",
        label = "State:",
        choices = c("All States", sort(unique(us_data$state))),
        selected = "All States"
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      h3("Treatment vs. Gender"),
      plotlyOutput("bar_chart_treatment_gender", height = "auto"),
      tags$p("This chart shows the distribution of counts of respondents receiving treatment based on their gender. Use the filters above to refine the data."),
      tags$div(style = "margin-bottom: 40px;")
    )
  ),
  fluidRow(
    column(
      width = 12,
      h3("Leave Policies vs. Company Size"),
      plotlyOutput("bar_chart_leave_company", height = "auto"),
      tags$p("This chart displays how leave policies are perceived by employees across different company sizes. Adjust the filters to explore specific subsets."),
      tags$div(style = "margin-bottom: 40px;")
    )
  ),
  fluidRow(
    column(
      width = 12,
      h3("Work Interference vs. Tech Company"),
      plotlyOutput("bar_chart_work_interfere_tech", height = "auto"),
      tags$p("This chart compares the levels of work interference reported by employees in tech vs. non-tech companies. Apply filters to focus on specific groups."),
      tags$div(style = "margin-bottom: 40px;")
    )
  )
)

)

# Define server logic
server <- function(input, output) {
  # Reactive function to filter dataset based on user inputs
  filtered_data <- reactive({
    data <- wc_dataset
    
    # Apply country filter
    if (input$country != "All") {
      data <- data %>% filter(Country == input$country)
    }
    
    # Apply gender filter
    if (input$gender != "All") {
      data <- data %>% filter(Gender == input$gender)
    }
    
    # Apply sentiment filter
    if (input$sentiment != "All") {
      data <- data %>% filter(sentiment_category == input$sentiment)
    }
    
    # Apply emotion category filter
    if (length(input$emotion_categories) > 0) {
      data <- data %>% 
        filter(sapply(nrc_emotions, function(emotions) {
          any(names(emotions)[emotions > 0] %in% input$emotion_categories)
        }))
    }
    
    data
  })
  # Render comment statistics table
  output$comment_stats <- renderTable({
    # Ensure we have filtered data
    req(filtered_data())
    
    # Get filtered and total data
    data <- filtered_data()
    total_data <- wc_dataset %>% filter(comments != "NA" & comments != "")
    
    # Calculate comments by emotion
    emotion_comments <- data %>%
      group_by(dominant_emotion) %>%
      summarise(Emotion_Comments = n()) %>%
      arrange(desc(Emotion_Comments))
    
    # Create data frame with statistics
    stats_df <- data.frame(
      Category = c("Total Comments", "Filtered Comments"),
      Count = c(nrow(total_data), nrow(data)),
      `Average Sentiment Score` = c(
        round(mean(total_data$sentiment_score), 2), 
        round(mean(data$sentiment_score), 2)
      )
    )
    
    # Combine with emotion comments
    emotion_stats_df <- data.frame(
      Category = emotion_comments$dominant_emotion,
      Count = emotion_comments$Emotion_Comments,
      `Average Sentiment Score` = round(sapply(emotion_comments$dominant_emotion, function(emotion) {
        subset_data <- data %>% filter(dominant_emotion == emotion)
        mean(subset_data$sentiment_score)
      }), 2)
    )
    
    # Combine and return
    rbind(stats_df, emotion_stats_df)
  })
  
  # Render word cloud (previous implementation remains the same)
  output$word_cloud <- renderPlot({
    # Get comments from filtered data
    comments <- filtered_data()$comments
    
    # Preprocess comments
    comments <- na.omit(comments) 
    comments <- tolower(paste(comments, collapse = " ")) 
    
    # Create corpus and clean text
    corpus <- Corpus(VectorSource(comments))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    
    # Remove stopwords and common non-informative words
    corpus <- tm_map(corpus, removeWords, stopwords("en"))
    corpus <- tm_map(corpus, removeWords, c("the", "a", "an", "and", "or", "but", "in", "on", "at", 
                                            "to", "for", "of", "with", "by", "from", "up", "about", 
                                            "into", "over", "after", "is", "are", "was", "were", "mental", "health", "ive", 
                                            "also", "just", "etc", "like", "physical", "dont", 
                                            "didnt", "will", "work", "one", "many", "doesnt")) 
    
    # Create term-document matrix and get term frequencies
    term_matrix <- TermDocumentMatrix(corpus)
    term_freq <- sort(rowSums(as.matrix(term_matrix)), decreasing = TRUE)
    
    # Limit number of words based on user input
    term_freq <- term_freq[1:input$numWords]
    
    # Convert to data frame
    word_freq <- data.frame(term_freq)
    word_freq$word <- rownames(word_freq)
    rownames(word_freq) <- NULL
    colnames(word_freq) <- c("Freq", "word")
    
    # Assign colors to words
    word_freq$custom_colors <- rep(custom_colors, length.out = nrow(word_freq))
    
    # Create word cloud using ggplot
    ggplot(word_freq, aes(
      label = word, 
      size = Freq,
      alpha = 1-1/Freq,
      color = custom_colors
    )) +
      geom_text_wordcloud_area(rm_outside = FALSE) +
      scale_size_area(max_size = 40) +
      theme_minimal()
  })
  
  # Number of Employees Comparison
  output$wc_no_employees_comparison <- renderPlot({
    # Prepare data for comparison
    employees_comp <- wc_dataset %>%
      group_by(no_employees, has_comment) %>%
      summarize(
        number = n()
      ) %>%
      group_by(has_comment) %>%
      mutate(
        percentage = number / sum(number)
      ) %>%
      ungroup()
    
    # Create bar plot
    ggplot(employees_comp, aes(x = no_employees, y = percentage, fill = has_comment)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
      labs(title = "Percentage of Respondents by Company Size",
           x = "Number of Employees", 
           y = "Percentage") +
      scale_fill_manual(values = c("FALSE" = custom_colors[1], "TRUE" = custom_colors[2]),
                        labels = c("No Comment", "Commented"),
                        name = "Comment Status") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = percent_format())
  })
  
  # Mental Health Consequence Comparison
  output$wc_mental_health_consequence_comparison <- renderPlot({
    mental_health_comp <- wc_dataset %>%
      group_by(mental_health_consequence, has_comment) %>%
      summarize(
        number = n()
      ) %>%
      group_by(has_comment) %>%
      mutate(
        percentage = number / sum(number)
      ) %>%
      ungroup()
    
    ggplot(mental_health_comp, aes(x = mental_health_consequence, y = percentage, fill = has_comment)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
      labs(title = "Mental Health Consequence by Comment Status",
           x = "Mental Health Consequence", 
           y = "Percentage") +
      scale_fill_manual(values = c("FALSE" = custom_colors[1], "TRUE" = custom_colors[2]),
                        labels = c("No Comment", "Commented"),
                        name = "Comment Status") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = percent_format())
  })
  
  # Age Comparison
  output$wc_age_comparison <- renderPlot({
    ggplot(wc_dataset, aes(x = Age, fill = has_comment)) +
      geom_density(color = NA, alpha = 0.3) +
      labs(title = "Age Distribution by Comment Status",
           x = "Age",
           y = "Density") +
      scale_fill_manual(values = c("FALSE" = custom_colors[1], "TRUE" = custom_colors[2]),
                        labels = c("No Comment", "Commented"),
                        name = "Comment Status") +
      theme_minimal()
  })
  
  # Reactive dataset filtered by gender
  filtered_data_2 <- reactive({
    if (input$gender2 == "All") {
      dataset
    } else {
      dataset %>% filter(Gender == input$gender2)
    }
  })
  
  # Reactive description text
  output$plot_description <- renderText({
    if (input$plot_choice == "Age vs Work Interference") {
      return("Age does not ..")
    } else if (input$plot_choice == "Company Size vs Mental Health Benefits") {
      return("We can see that larger companies tend to have employees who are more aware of the benefits available to them. We can also see that female employees tend to be more aware of mental health benefits available to them.")
    } else if (input$plot_choice == "Remote Work vs Treatment") {
      return("From this plot we can see that when all genders are aggregated, there is a slightly larger uptake in mental health treatment by employees who work remotely. 
             When breaking down by gender, we see that female remote workers are more likely to get mental health treatment but non remote workers are less likely to get treatment.
             Among male workers, we see that across both working groups, male workers are less likely to get treatment for mental health.")
    } else if (input$plot_choice == "Mental vs Physical Health Consequences") {
      return("A majority of employees do not think there will be consequences to speaking out about both mental and physical health issues with their employers.")
    } else if (input$plot_choice == "Supervisor Support vs Coworker Support") {
      return("Employees who do no feel comfortable talking to a coworker about mental health issues also do not feel comfortable discussing mental health issues with a supervisor. 
             Of employees willing to talk to a coworker about mental health issues, 86% are willing to talk to a superviser as well.")
    } else if (input$plot_choice == "Treatment vs Family History") {
      return("Employees with a family history of treatment are far more likely to seek treatment for mental health issues than employees without a family history of mental health issues. 
             Female employees are more likely to receive treatment whether or not there is a family history of mental health issues.")
    }
  })
  
  # Reactive subtitle text
  output$plot_subtitle <- renderText({
    if (input$plot_choice == "Age vs Work Interference") {
      return("Q: If you have a mental health condition, do you feel that it interferes with your work?")
    } else if (input$plot_choice == "Company Size vs Mental Health Benefits") {
      return("Q: Does your employer provide mental health benefits?.")
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
    data <- filtered_data_2()
    
    if (input$plot_choice == "Age vs Work Interference") {
      plot <- ggplot(data %>% filter(!is.na(work_interfere), !is.na(Age)), aes(x = work_interfere, y = Age)) +
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
  
  filtered_data_3 <- reactive({
    survey_data %>%
      filter(
        age_bin %in% input$age_bin,
        Gender %in% input$gender3,
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
      left_join(filtered_data_3(), by = c("STUSPS" = "state"))
    
    # Create ggplot
    p <- ggplot(map_data) +
      geom_sf(aes(fill = work_interfere_avg, text = paste(
        "State: ", NAME, "<br>",
        "Avg Work Interference: ", round(work_interfere_avg, 2)
      )), color = "white") +
      scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
      scale_x_continuous(limits = c(-125, -67))+
      scale_y_continuous(limits = c(25, 50))+
      coord_sf() +
      theme_minimal(base_size = 14) +
      theme(
        axis.title = element_blank(),  # Remove axis titles
        axis.text = element_blank(),   # Remove axis text
        axis.ticks = element_blank(),  # Remove axis ticks
        panel.grid = element_blank(),  # Remove gridlines
        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "white", color = NA)
      ) +
      labs(
        title = "Work Interference Distribution by State",
        fill = "Avg Work Interference"
      )
    
    # Convert ggplot to plotly for interactivity
    ggplotly(p, tooltip = "text", width = 900, height = 600) %>%
      layout(
        dragmode = "zoom",  # Enable zooming and panning
        margin = list(l = 0, r = 0, t = 50, b = 0)  # Reduce margins
      )
  })
  # Filtered data reactive expression
  d <- reactive({
    us_data %>%
      filter(
        (input$input_state == "All States" | state == input$input_state)
      )
  })
  # bar_chart_1: Treatment vs. Gender
  output$bar_chart_treatment_gender <- renderPlotly({
    agg_data <- d() %>%
      group_by(Gender, treatment) %>%
      summarise(Count = n(), .groups = "drop") %>%
      mutate(Percentage = Count / sum(Count) * 100,
             Count = Count)
    
    plot_ly(
      data = agg_data,
      x = ~Gender,
      y = ~Count,
      color = ~treatment,
      type = "bar",
      text = ~paste("Count:", Count, "<br>Percentage:", round(Percentage, 1), "%"),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Treatment vs. Gender",
        xaxis = list(title = "Gender"),
        yaxis = list(title = "Count"),
        barmode = "stack"
      )
  })
  
  # bar_chart_2: Leave Policies vs. Company Size
  output$bar_chart_leave_company <- renderPlotly({
    agg_data <- d() %>%
      group_by(no_employees, leave) %>%
      summarise(Count = n(), .groups = "drop") %>%
      mutate(Percentage = Count / sum(Count) * 100,
             Count = Count)
    
    plot_ly(
      data = agg_data,
      x = ~no_employees,
      y = ~Count,
      color = ~leave,
      type = "bar",
      text = ~paste("Count:", Count, "<br>Percentage:", round(Percentage, 1), "%"),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Leave Policies vs. Company Size",
        xaxis = list(title = "Company Size"),
        yaxis = list(title = "Count"),
        barmode = "group"
      )
  })
  
  # bar_chart_3: Work Interference vs. Tech Company
  output$bar_chart_work_interfere_tech <- renderPlotly({
    agg_data <- d() %>%
      group_by(tech_company, work_interfere) %>%
      summarise(Count = n(), .groups = "drop") %>%
      mutate(Percentage = Count / sum(Count) * 100,
             Count = Count)
    
    plot_ly(
      data = agg_data,
      x = ~tech_company,
      y = ~Count,
      color = ~work_interfere,
      type = "bar",
      text = ~paste("Count:", Count, "<br>Percentage:", round(Percentage, 1), "%"),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Work Interference vs. Tech Company",
        xaxis = list(title = "Tech Company"),
        yaxis = list(title = "Count"),
        barmode = "group"
      )
  })
  
  }

# Run the Shiny app
shinyApp(ui = ui, server = server)