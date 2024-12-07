# Load required libraries
library(shiny)
library(tm)
library(dplyr)
library(syuzhet)
library(RColorBrewer) 
library(ggplot2)
library(tidytext)
library(stringr)
library(ggwordcloud)
library(scales)

# Set the colors for the plots
# Create custom color palette
custom_colors <- c(brewer.pal(8, "Set2"), brewer.pal(12, "Set3"), brewer.pal(12, 'Paired'))
custom_colors <- custom_colors[!custom_colors %in% c("#FFFFB3", "#FFED6F")]

# Load the dataset
# Reads the survey data and performs initial preprocessing
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

# Define UI for the Shiny application
ui <- fluidPage(
  # Title of the application
  titlePanel("Sentiment Analysis of Comments"),
  
  # Sidebar layout for input controls
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

}

# Run the Shiny app
shinyApp(ui = ui, server = server)