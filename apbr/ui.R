library(shiny)

shinyUI(fluidPage(
        
        titlePanel("AOT and PM by range"),
        
        sidebarLayout(
                sidebarPanel(
                        dateRangeInput("daterange",label="Date Range",
                                       start= "2012-07-01", end = "2012-07-03", format="dd-mm-yyyy", 
                                       startview="July", language="en"),
                        numericInput("ylim",label = "upper y limit", value = 100)
                        ),
                        
                        mainPanel(
                                plotOutput("apbrplot")
                        ) # mainPanel
                ) # sidebarLayout
                
        ) # fluidPage 
) # shinyUI