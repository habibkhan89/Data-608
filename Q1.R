#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)
library(ggthemes)
library(ggplot2)
library(dplyr)

# Connecting with shinyapps.io

rsconnect::setAccountInfo(name='hkhan10',
                          token='26F6F284258F2E148BD865AFBDF15253',
                          secret='sjvrVP8nrfMJXdK6hJCVpY/88IaxHSNm3AFormXF')


# Read and filter data for 2010

mortality_rate <- read.csv("mortality rate.csv", stringsAsFactors = FALSE)
mortality_rate2010 <- mortality_rate %>% filter(Year== 2010 & ICD.Chapter== "Neoplasms")

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("Crude rate of Neoplasms in different states in 2010"),
   
   # Sidebar layout 
   sidebarLayout(
      selectInput("select", label = NULL, 
                     choices = mortality_rate2010$ICD.Chapter, 
                     selected = 1,
                     width = '90%'),
         
  
      mainPanel(
         plotOutput("Chart")
      )
   )
 )



# Define server 
server <- function(input, output) {
    output$Chart <- renderPlot({
    ggplot(mortality_rate2010[mortality_rate2010$ICD.Chapter == input$select,] , aes(x = reorder(State, Crude.Rate), y = Crude.Rate)) +
      labs(x = "States", y = "Crude Rate") + geom_bar(stat = "identity") +coord_flip() 
    
  }, width = 700, height = 500)
  
}
# Run the application 
shinyApp(ui = ui, server = server)


deployApp()
