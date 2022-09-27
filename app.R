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

library(ggplot2)
library(scales)
devtools::install_github('ash-rulz/KoladaPackage')
library("KoladaPackage")

# Pulling data from KoladaPackage and 
kolda_lst <- get_kolda_data('kpi/n60026/year/2020,2019,2018,2017,2015,2016,2014,2013') 
data_frame <- kolda_lst$FinalData
mun_mast_df <- kolda_lst$MunMaster
mun_mast_df <- mun_mast_df[mun_mast_df$id %in% 
                             unique(data_frame$municipality),]
data_frame <- merge(x=data_frame, 
                    y=mun_mast_df[,c('id', 'title')], 
                    by.x = 'municipality', by.y = 'id', 
                    all = TRUE)

# Define UI for application that draws a Line Graph using ggplot2
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
          plotOutput('plot1', height = '900px')
        )
    )
)

# Define server logic required to draw a line graph
server <- function(input, output) {
  
  # reactive expression for data frame manipulation required as data for ggplot2
  df_react <- reactive({
    munici_vec <- c(input$chooseMunici)
    df <- data_frame[data_frame[,'kpi'] == input$chooseKPI &
                       data_frame[,'municipality'] %in% munici_vec &
                       data_frame[,'gender'] == 'T',]
  })
  
  # reactive expression for plotting line graph
  plot_react <- reactive({
    ggplot(data = df_react(), 
           aes(x=period, y=value, color = title)) + 
      geom_line() +
      ggtitle('Total Monthly Salary per Region')+
      theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 25), 
            axis.title.x = element_text(vjust = -2, face = 'bold', size = 12),
            axis.title.y = element_text(vjust = 2, face = 'bold', size = 12), 
            legend.title = element_text(face = 'bold'), 
            legend.background = element_rect(colour = 'Grey'))+
      xlab('Period/Years')+
      ylab('Value/Salary')+
      geom_point() +
      scale_y_continuous(breaks = pretty_breaks()) +
      scale_x_continuous(breaks = unique(data_frame$period))
  })
    
    output$plot1 <- renderPlot({
      
      if(length(input$chooseMunici) >= 1){
        
        plot_react()
      }
      
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
