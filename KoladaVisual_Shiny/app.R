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
devtools::install_github('ash-rulz/KoladaPackage')
library("KoladaPackage")
library(ggplot2)
library(scales)

# Define UI for application that draws a histogram
kolda_lst <- get_kolda_data('kpi/n60026/year/2020,2019,2018,2017,2015,2016') 
data_frame <- kolda_lst$FinalData
mun_mast_df <- kolda_lst$MunMaster
mun_mast_df <- mun_mast_df[mun_mast_df$id %in% 
                             unique(data_frame$municipality),]
ui <- fluidPage(

    # Application title
    titlePanel("Municipality Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectizeInput("chooseKPI",
                        label = 'Select KPI',
                        choices = c(data_frame$kpi)),
            selectizeInput('chooseMunici',
                        label = 'Select Municipality',
                        choices = setNames(mun_mast_df$id, mun_mast_df$title),
                        multiple = TRUE
                        )),

        # Show a plot of the generated distribution
        mainPanel(
          # textOutput("municipality"),
          plotOutput('plot1', width = '1000px', height = '1000px')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot1 <- renderPlot({
      
      if(length(input$chooseMunici) == 1){
        df <- data_frame[data_frame[,'kpi'] == input$chooseKPI &
                           data_frame[,'municipality'] %in% input$chooseMunici,]
        ggplot(data = df, 
               aes(x=period, y=value, color = gender)) + 
          geom_line() +
          geom_point() +
          scale_y_continuous(breaks = pretty_breaks())
      }
      else if (length(input$chooseMunici) > 1) {
        munici_vec <- c(input$chooseMunici)
        df <- NULL
        df <- data_frame[data_frame[,'kpi'] == input$chooseKPI &
                           data_frame[,'municipality'] %in% munici_vec &
                           data_frame[,'gender'] == 'T',]
        ggplot(data = df, 
               aes(x=period, y=value, color = municipality)) + 
          geom_line() +
          geom_point() +
          scale_y_continuous(breaks = pretty_breaks())
      }
      
    })
    # output$municipality <- {(
    #   renderText(input$chooseMunici)
    # )}

}

# Run the application 
shinyApp(ui = ui, server = server)
