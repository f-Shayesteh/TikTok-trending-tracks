#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
dataset <- "https://github.com/romeoben/DSC7-Sprint2-TeamDan/raw/main/data/_TIKTOK_playlist_tracks_data.csv"
TikTok1<- read.csv(dataset)
TikTok <- TikTok1 %>% dplyr::sample_n(size = 500, replace = F)
library(shiny)
library(dplyr)
library(ggplot2)
library(ggwordcloud)

# Define UI for application that draws a histogram
ui <- fluidPage(

  radioButtons("variableChoice","choice of variable",
               choices = c("artist_name", "track_name"),
               selected = "artist_name"),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
      if (input$variableChoice == "artist_name") 
        {   
      word.cloud = TikTok %>% 
        ggplot(aes(label = TikTok$artist_name, 
                   size = TikTok$popularity, 
                   color = TikTok$artist_name)) +
        geom_text_wordcloud() +
        scale_size_area(max_size = 30) +
        scale_color_viridis_d(option = "magma") +
        theme_minimal()
      word.cloud
      }
      if (input$variableChoice == "track_name") {      
        word.cloud2 = TikTok %>% 
        ggplot(aes(label = TikTok$track_name, 
                   size = TikTok$popularity, 
                   color = TikTok$track_name)) +
        geom_text_wordcloud() +
        scale_size_area(max_size = 30) +
        scale_color_viridis_d(option = "magma") +
        theme_minimal()
      word.cloud2
      }})

  output$value <- renderPrint({ input$checkGroup })
}
    
       
# Run the application 
shinyApp(ui = ui, server = server)
