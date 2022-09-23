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
data_frame <- get_kolda_data('kpi/n60026/year/2020,2019,2018')
data_frame
# Define UI for application that draws a histogram
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
          textOutput(outputId = 'chooseKPI'),
          textOutput(outputId = 'chooseMunici'),
          plotOutput('plot1', width = "700px", height = '500px')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$chooseKPI <- renderText({
      chooseKPI <- print(input$chooseKPI)
    })
    output$chooseMunici <- renderText({
      chooseMunici <- print(input$chooseMunici)
    })
    output$plot1 <- renderPlot({
      plot(x = data_frame$period, y = data_frame$value)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
