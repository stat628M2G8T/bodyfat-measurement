# Documentation
# This Shiny app acquires input values from the user to calculate BodyFat for a 
# variety of heights and weights. It contains two inputs, one for height in inches and one 
# for weight in pounds, and calculates Body Fat rate. The function takes two arguments, height and 
# weight, and returns a numerical value based on a simple formula.
# This is the front end file.

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Body Fat Caculator"),
  
  # Sidebar with two numeric Input 
  sidebarLayout(
    sidebarPanel(
      numericInput('in1', 'Enter your height in inches', 70, min = 10, max = 80, step = 1),
      numericInput('in2', 'Enter your weight in pounds', 200, min = 100, max = 300, step = 5)
    ),
    
    mainPanel(
      h3('Results'),
      h4('Your height is:'),
      verbatimTextOutput("ou1"),
      h4('Your weight is:'),
      verbatimTextOutput("ou2"),
      h4('Your body fat is:'),
      verbatimTextOutput("bodyfat"),
      h6(em('Reactive output displayed as a result of server calculations.'))
    )
  )
)

Bodyfat = function(height,weight){
  return(0.45455*weight/(.0254*height)^2)
}


server <- function(input, output) {
  output$ou1 <- renderPrint({input$in1})
  output$ou2 <- renderPrint({input$in2})
  output$bodyfat <- renderPrint({Bodyfat(input$in1, input$in2)})
}

# Run the application 
shinyApp(ui = ui, server = server)
