#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

df.date <- TikTok.data %>% 
  tidyr::separate(col = release_date, into = c("Year", "Month", "DayofMonth"), sep = "/") %>% 
  mutate(Month = as.numeric(Month), # remove leading zeros
         year = as.numeric(DayofMonth))%>% 
  arrange(Year, Month, year)

df.date$year <- as.Date(as.character(df.date$year), format = "%Y")


# Define UI for application that draws a histogram
ui <- fluidPage(
       titlePanel("Dates and date ranges"),
   wellPanel(
      dateRangeInput('dateRange',
                     label = 'Date range input: yyyy-mm-dd',
                     start = Sys.Date() - 2, end = Sys.Date() + 2
      ),
      column(6,
             
             verbatimTextOutput("dateRangeText"),
             mainPanel(
               plotOutput("distPlot")
             )
      )
    ))
    
    # Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    df.date %>% 
      ggplot(aes(x = year, y = popularity)) +
      geom_line(size = 1, color = "green") +
      # scale_x_date(date_breaks = "year")
      # scale_x_date(date_breaks = "5 years")
      # scale_x_date(date_breaks = "10 years")
      # scale_x_date(date_breaks = "month")
      # scale_x_date(date_breaks = "10 years", date_labels = "%Y") # show only year
      scale_x_date(date_breaks = "2 years", date_labels = "%Y")+ # show only year
      xlab("Year") +
      ylab("Popularity") +
      ggtitle("TikTok tracks' populairty over time - line plot") +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 12),
            plot.title = element_text(size = 20, face = "bold"),
            legend.title = element_text(size = 17),
            legend.text = element_text(size = 12),
            panel.border = element_rect(color = "black", fill = NA, size = 1.5))
  })
  output$dateRangeText  <- renderText({
    paste("input$dateRange is", 
          paste(as.character(input$dateRange), collapse = " to ")
    )
  })
} 

       

# Run the application 
shinyApp(ui = ui, server = server)
