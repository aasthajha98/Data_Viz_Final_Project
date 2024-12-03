library(shiny)
library(tm)
library(wordcloud)
library(wordcloud2)
library(dplyr)
library(RColorBrewer) 

#load the dataset
dataset <- read.csv("survey_cleaned.csv", stringsAsFactors = FALSE)
dataset <- dataset %>% filter(!is.na(comments) & comments != "")

# Define UI
ui <- fluidPage(
  titlePanel("Word Cloud for Comments"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:",
                  choices = c("All", unique(dataset$Country))),
      selectInput("gender", "Select Gender:",
                  choices = c("All", unique(dataset$Gender))),
      sliderInput("numWords", "Number of Words:", 
                  min = 10, max = 200, value = 100),
      actionButton("update", "Update Word Cloud")
    ),
    mainPanel(
      wordcloud2Output("wordcloud", width = "80%", height = "350px")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # filtered dataset based on input
  filtered_data <- reactive({
    data <- dataset
    if (input$country != "All") {
      data <- data %>% filter(Country == input$country)
    }
    if (input$gender != "All") {
      data <- data %>% filter(Gender == input$gender)
    }
    data
  })
  
  wordcloud_data <- reactive({
    req(filtered_data())
    comments <- filtered_data()$comments
    comments <- na.omit(comments) 
    comments <- tolower(paste(comments, collapse = " ")) 
    
    # Text preprocessing
    corpus <- Corpus(VectorSource(comments))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("en"))
    corpus <- tm_map(corpus, removeWords, c("mental", "health", "ive", 
                                            "also", "just", "etc", 
                                            "like", "physical")) 
    
    # term frequencies
    term_matrix <- TermDocumentMatrix(corpus)
    term_freq <- sort(rowSums(as.matrix(term_matrix)), decreasing = TRUE)
    # Limit the number of words
    term_freq <- term_freq[1:input$numWords]
    data.frame(word = names(term_freq), freq = term_freq)
  })
  
  # genrate the word cloud
  output$wordcloud <- renderWordcloud2({
    req(wordcloud_data())
    
    custom_colors <- c(brewer.pal(8, "Set2"), brewer.pal(12, "Set3"), brewer.pal(12, 'Paired'))
    custom_colors <- custom_colors[!custom_colors %in% c("#FFFFB3", "#FFED6F")]
    custom_colors <- rep(custom_colors, length.out = input$numWords)
    
    wordcloud2(wordcloud_data(),
               size = 1.1,
               shape = "square",
               fontFamily = "Roboto, Arial, sans-serif",
               color = custom_colors,
               backgroundColor = "white")
  })
}


# Run
shinyApp(ui = ui, server = server)


