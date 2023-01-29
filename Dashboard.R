require(shiny)
require(ggplot2)
require(data.table)

sample_Data <- fread('C:/Users/Akhil/Downloads/Economist_Assignment_Data.csv',drop=1)

## head(sample_Data) -- To view all the different data points of the data set

## unique(sample_Data$Region) -- To check the unique regions of the data set

# User interface ----

ui <- fluidPage(titlePanel("HDI vs Corruption perception Dashboard"),
                sidebarLayout(
                  sidebarPanel(
                    radioButtons("selected_Region",
                                 label= "Select Region:",
                                 choices = c("All", unique(sample_Data$Region)),
                                 selected = "All"
                                 ), width = 2
                ),
    
    mainPanel(  textOutput("selected_var"), br(), plotOutput("data_Plot", height = 550),width = 10, align = "center")
              )
)

# Server logic ----

server <- function(input, output){

    output$selected_var <- renderText({ paste("You have selected", input$selected_Region) })
    
    output$data_Plot <- renderPlot({ 
     
     plot_Data <- switch(input$selected_Region,
                         "All" = sample_Data,
                         "Asia Pacific" = sample_Data[which (sample_Data$Region == "Asia Pacific"),],
                         "East EU Cemt Asia" = sample_Data[which (sample_Data$Region == "East EU Cemt Asia"),],
                         "MENA" = sample_Data[which (sample_Data$Region == "MENA"),],
                         "SSA" = sample_Data[which (sample_Data$Region == "SSA"),],
                         "Americas" = sample_Data[which (sample_Data$Region == "Americas"),],
                         "EU W. Europe" = sample_Data[which (sample_Data$Region == "EU W. Europe"),])
     
     
     p <- ggplot(plot_Data, aes(x=CPI,y=HDI,color=Region))
     p <- p + geom_point(size=4,shape=1) + geom_smooth(method=lm, se=FALSE, linetype='dashed', color='red')
     p <- p + geom_text(aes(label = Country), color = "gray20", check_overlap = TRUE)
     print(p)
     })

}

# Run app ----
shinyApp(ui, server)
