#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tigris)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shinythemes)
library(tm)
library(wordcloud)



survey_data = read.csv('survey_cleaned.csv')

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


us_data %>% 
  mutate(age_bin = cut(Age, breaks=c(18, 30, 40, 50, 60, 70)))


corpus = Corpus(VectorSource(text))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removeWords, stopwords("SMART"))

dtm = TermDocumentMatrix(corpus,
                           control = list(minWordLength = 1))

m = as.matrix(dtm)

sort(rowSums(m), decreasing = TRUE)



# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),

    # Application title
    titlePanel("Mental Health in Tech Survey"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId= "input_tech",
                      label = "Tech Company?:",
                      choices = sort(unique(us_data$tech_company)),
                      selected = "Yes"),
          selectInput(inputId= "Gender",
                      label = "Gender:",
                      choices = sort(unique(us_data$Gender)),
                      selected =  levels(us_data$Gender),
                      multiple = TRUE),
          selectInput(inputId= "Age",
                      label = "Age:",
                      choices = unique(us_data$age_bin),
                      selected =  levels(us_data$age_bin),
                      multiple = TRUE),
          dateRangeInput(inputId = "input_date",
                         label = "Date Range",
                         start = "2014-08-27",
                         end = "2016-02-01",
                         min = min(us_data$Timestamp),
                         max = max(us_data$Timestamp),
                         startview = "year")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Map", leafletOutput("avg_map", height = 600)),
            tabPanel("Bar Chart", plotOutput('bar_chart'), plotOutput('bar_chart_2')),
            tagPanel("Word Cloud", 
                     sliderInput("freq_words",
                                 "Minimum Frequency:",
                                 min = 1,  max = 50, value = 15),
                     sliderInput("max_words",
                                 "Maximum Number of Words:",
                                 min = 1,  max = 300,  value = 100),
                     plotOutput('word_cloud'))
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    d <- reactive({
      us_data %>%
        filter(tech_company==input$input_tech)
    })
    
    terms <- reactive({
      survey_data %>% filter()
      isolate({
        withProgress({
          setProgress(message = "Processing corpus...")
          getTermMatrix(input$selection)
        })
      })
    })
    
    wordcloud_rep <- repeatable(wordcloud)
    
    output$plot <- renderPlot({
      v <- terms()
      wordcloud_rep(names(v), v, scale=c(4,0.5),
                    min.freq = input$freq_words, max.words=input$max_words,
                    colors=brewer.pal(8, "Dark2"))
    })
    
    d_agg = reactive({us_data %>%
        filter(tech_company == input$input_tech) %>%
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
    })
    

    output$avg_map <- renderLeaflet({
      leaflet(merged_data) %>%
        addProviderTiles("OpenStreetMap",
                         group = "OpenStreetMap")  %>%
        addProviderTiles("Esri.WorldStreetMap",
                         group = "ESRI") %>%
        addLayersControl(baseGroups = c("OpenStreetMap",
                                        "ESRI"),
                         position = "topright")%>%
        setView(lng = as.numeric(-94),
                lat = as.numeric(38), zoom = 3.2) %>%
        addPolygons(fillColor = ~pal(avg_work_interfere),
                    weight = .5,
                    opacity = .3,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7) %>%
        addLegend(position = "topright",  # Legend position: topleft, topright, bottomleft, bottomright
                      pal = pal,
                      values = ~avg_work_interfere,
                      title = "Average Work Interference")
      
    })
    
    output$bar_chart = renderPlot({
      ggplot(d(), aes(x = family_history)) +
        geom_bar(fill = 'steelblue', color = 'black')+
        labs(
          title = "Family History Responses",
          x = "Response",
          y = "Count"
        ) +
        theme_minimal()
      
        
    })
    
    output$bar_chart_2 = renderPlot({
      ggplot(d(), aes(x = leave)) +
        geom_bar(fill = 'steelblue', color = 'black')+
        labs(
          title = "Leave",
          x = "Response",
          y = "Count"
        ) +
        theme_minimal()
      
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
