#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(httr)
library(jsonlite)
library(tidyr)
library(KoladaPackage)
library(ggplot2)
library(scales)
# Define UI for application that draws a histogram
data_frame <- get_kolda_data('kpi/n60026/year/2020,2019,2018,2017,2015,2016') 
data_frame <- cbind(data_frame[,1:5], value = as.integer(data_frame[,7]))
ui <- fluidPage(

    # Application title
    titlePanel("Municipality Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("chooseKPI",
                        label = 'Select KPI',
                        choices = c(data_frame$kpi)),
            selectInput('chooseMunici',
                        label = 'Select Municipality',

                        choices = c(data_frame$municipality),
                        )),

        # Show a plot of the generated distribution
        mainPanel(
          textOutput("municipality"),
          plotOutput('plot1', width = "700px", height = '500px')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot1 <- renderPlot({
      if(input$chooseMunici != ""){
        df <- data_frame[data_frame['kpi'] == input$chooseKPI &
                           data_frame['municipality'] == input$chooseMunici,]
        ggplot(data = df, 
               aes(x=period, y=value, color = gender)) + 
          geom_line() +
          geom_point() +
          scale_y_continuous(breaks = pretty_breaks())
      }
      
    })
    output$municipality <- {(
      renderText(input$chooseMunici)
    )}

}

# Run the application 
shinyApp(ui = ui, server = server)
