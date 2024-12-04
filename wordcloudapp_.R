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

# Load the dataset
dataset <- read.csv("survey_cleaned.csv", stringsAsFactors = FALSE)

# Preprocess and add sentiment analysis
dataset <- dataset %>% 
  filter(!is.na(comments) & comments != "" & comments!="NA" & comments !="-") %>%
  mutate(
    sentiment_score = get_sentiment(comments, method = "afinn"),
    sentiment_category = case_when(
      sentiment_score > 0 ~ "Positive",
      sentiment_score < 0 ~ "Negative",
      TRUE ~ "Neutral"
    ),
    # NRC emotion lexicon analysis
    nrc_emotions = lapply(comments, function(comment) {
      # Get emotion scores for each comment
      emotion_scores <- get_nrc_sentiment(comment)
      # Convert to a named list of emotions
      emotion_names <- names(emotion_scores)[1:10]
      emotion_values <- as.numeric(emotion_scores[1:10])
      names(emotion_values) <- emotion_names
      emotion_values
    }),
    
    # Determine dominant emotion
    dominant_emotion = sapply(nrc_emotions, function(emotions) {
      if(max(emotions) > 0) {
        names(emotions)[which.max(emotions)]
      } else {
        "neutral"
      }
    }),
  )

# Define UI
ui <- fluidPage(
  titlePanel("Sentiment Word Cloud for Comments"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:",
                  choices = c("All", unique(dataset$Country))),
      selectInput("gender", "Select Gender:",
                  choices = c("All", unique(dataset$Gender))),
      selectInput("sentiment", "Select Sentiment of the Comment:",
                  choices = c("All", "Positive", "Neutral", "Negative")),
      sliderInput("numWords", "Number of Words:", 
                  min = 1, max = 200, value = 100),
      # NRC emotion category selection
      checkboxGroupInput("emotion_categories", "Select Main Emotion of Comment:",
                         choices = c("anger", "anticipation", "disgust", 
                                     "fear", "joy", "sadness", 
                                     "surprise", "trust", "neutral"),
                         selected = c("anger", "anticipation", "disgust", 
                                      "fear", "joy", "sadness", 
                                      "surprise", "trust", "neutral"))
      # actionButton("update", "Update Word Cloud")
    ),
    mainPanel(
      tags$p("\nThe sentiment were used with the lexicon 'affin' that gives
             scores from -5 (negative) to 5 (positive) based on the value of 
             each of the words in the comment."),
      plotOutput("percentage_bar", height = "100px"),
      plotOutput("word_cloud")
      # wordcloud2Output("wordcloud", width = "90%", height = "550px")
      
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Filtered dataset based on input
  filtered_data_1 <- reactive({
    data <- dataset
    if (input$country != "All") {
      data <- data %>% filter(Country == input$country)
    }
    if (input$gender != "All") {
      data <- data %>% filter(Gender == input$gender)
    }
    if (input$sentiment != "All") {
      data <- data %>% filter(sentiment_category == input$sentiment)
    }
    if (length(input$emotion_categories) > 0) {
      data <- data %>% 
        filter(sapply(nrc_emotions, function(emotions) {
          any(names(emotions)[emotions > 0] %in% input$emotion_categories)
        }))
    }
    data
  })
  # Percentage bar of the number of comments
  output$percentage_bar <- renderPlot({
    req(filtered_data_1())
    data <- filtered_data_1()
    total_comments <- nrow(dataset)
    filtered_comments <- nrow(data)
    
    # Prepare data for percentage bar
    plot_data <- data.frame(
      Category = c("Selected Comments", "Total Comments"),
      Count = c(filtered_comments, total_comments),
      Percentage = c(
        filtered_comments / total_comments * 100,
        100
      )
    )
  
    # Calculate average sentiment score for selected comments
    avg_sentiment <- c(round(mean(data$sentiment_score), 2), round(mean(dataset$sentiment_score), 2))

    # Create ggplot
    ggplot(plot_data, aes(x = Category, y = Percentage, fill = Category)) +
      geom_bar(stat = "identity", position = "identity") +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      scale_fill_manual(values = c("Selected Comments" = "#4682B4", "Total Comments" = "#808080")) +
      geom_text(aes(label = paste0(round(Percentage, 1), "% (", Count, " comments)\nAvg Sentiment: ", avg_sentiment)), 
                position = position_stack(vjust=0.5),
                color = "white",
                size=4,  family="Arial",
                fontface = "bold") +
      theme_minimal() +
      theme(
        text=element_text(size=10,  family="Arial"),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA)
      ) +
      coord_flip()
  })
  
  output$word_cloud <- renderPlot({
    comment <- filtered_data_1()$comments
    
    # Tokenize and remove stop words
    comments <- filtered_data_1()$comments
    comments <- na.omit(comments) 
    comments <- tolower(paste(comments, collapse = " ")) 
    
    # Text preprocessing
    corpus <- Corpus(VectorSource(comments))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("en"))
    corpus <- tm_map(corpus, removeWords, c("the", "a", "an", "and", "or", "but", "in", "on", "at", 
                                            "to", "for", "of", "with", "by", "from", "up", "about", 
                                            "into", "over", "after", "is", "are", "was", "were", "mental", "health", "ive", 
                                            "also", "just", "etc", "like", "physical", "dont", 
                                            "didnt", "will", "work", "one", "many")) 
    
    # Term frequencies
    term_matrix <- TermDocumentMatrix(corpus)
    term_freq <- sort(rowSums(as.matrix(term_matrix)), decreasing = TRUE)
    
    # Limit the number of words
    term_freq <- term_freq[1:input$numWords]
    
    #to Dataframe
    word_freq <- data.frame(term_freq)
    word_freq$word <- rownames(word_freq)
    rownames(word_freq) <- NULL
    colnames(word_freq) <- c("Freq", "word")
    print(word_freq)
    
    # Custom color palette
    custom_colors <- c(brewer.pal(8, "Set2"), brewer.pal(12, "Set3"), brewer.pal(12, 'Paired'))
    custom_colors <- custom_colors[!custom_colors %in% c("#FFFFB3", "#FFED6F")]
    word_freq$custom_colors <- rep(custom_colors, length.out = nrow(word_freq))
    
    # Create the word cloud
    ggplot(word_freq, aes(
      label = word, 
      size = Freq,
      color = custom_colors
    )) +
      geom_text_wordcloud_area(rm_outside = TRUE) +
      scale_size_area(max_size = 80) +
      theme_minimal() 
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)