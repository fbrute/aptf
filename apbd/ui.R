library(shiny)

shinyUI(fluidPage(
  
  titlePanel("AOT and PM by day"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput("date",label="Date",
                value= "2012-07-03", format="dd-mm-yyyy", startview="July", language="en")),
    mainPanel(
      plotOutput("apbmplot")
      ) # mainPanel
    ) # sidebarLayout
  
  ) # fluidPage 
) # shinyUI