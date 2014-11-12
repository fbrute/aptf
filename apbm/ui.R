library(shiny)

shinyUI(fluidPage(
        
        titlePanel("AOT and PM by month for 2012"),
        
        sidebarLayout(
                sidebarPanel(
                        dateRangeInput("daterange",label="Date Range",
                                       start= "2012-01-01", end = "2012-11-30", format="dd-mm-yyyy", 
                                       startview="July", language="en")
                ),
                mainPanel(
                        plotOutput("apbrplot")
                ) # mainPanel
        ) # sidebarLayout
        
) # fluidPage 
) # shinyUI