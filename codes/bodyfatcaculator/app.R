# Documentation
# This Shiny app acquires input values from the user to calculate BodyFat for a 
# It contains two inputs. One is abdomen 2 circumference, the other is wrist circumference.
# By using two inputs, it calculates Body Fat rate. The function takes two arguments, 
# abdomen 2 circumference, the other is wrist circumference., and returns a numerical 
# value based on a simple formula.
# This is the front end file.

library(shiny)

# Define UI for application that caculates body fat
ui <- fluidPage(
  
  # Application title
  titlePanel("Body Fat Caculator"),
  
  # Sidebar with two numeric Input 
  sidebarLayout(
    sidebarPanel(
      numericInput('in1', 'Enter your abdomen 2 circumference in cm', 90, min = 60, max = 130, step = 0.1),
      numericInput('in2', 'Enter your wrist circumference in cm', 18, min = 10, max = 25, step = 0.1)
    ),
    
    mainPanel(
      h4(tabsetPanel(type = "tabs",
                     tabPanel("Men", tableOutput("table1")),
                     tabPanel("Women", tableOutput("table2")))),
      h6(em('https://www.builtlean.com/2010/08/03/ideal-body-fat-percentage-chart/')),
      h3('Results'),
      h4('Your abdomen 2 circumference is:'),
      verbatimTextOutput("ou1"),
      h4('Your wrist circumference is:'),
      verbatimTextOutput("ou2"),
      h4('Your body fat percentage is:'),
      verbatimTextOutput("bodyfat"),
      h6(em('Reactive output displayed as a result of server calculations.'))
    )
  )
)

# The linear model we finally choose
Bodyfat = function(in1,in2){
  return(-10.0553204695359+(0.718707203647659*in1)-(2.05356376175185*in2))
}
war = function(in1,in2){
  bf = -10.0553204695359+(0.718707203647659*in1)-(2.05356376175185*in2)
  temp1 = rep("",5)
  temp2 = rep("",5)
  if(bf<2){
    temp1[1]="Wrong Input"
  }else if(bf<6){
    temp1[1]=as.character(bf)
  }else if(bf<14){
    temp1[2]=as.character(bf)
  }else if(bf<18){
    temp1[3]=as.character(bf)
  }else if(bf<25){
    temp1[4]=as.character(bf)
  }else if(bf<40){
    temp1[5]=as.character(bf)
  }else{
    temp1[5]="Wrong Input"
  }
  if(bf<10){
    temp2[1]="Wrong Input"
  }else if(bf<14){
    temp2[1]=as.character(bf)
  }else if(bf<21){
    temp2[2]=as.character(bf)
  }else if(bf<25){
    temp2[3]=as.character(bf)
  }else if(bf<32){
    temp2[4]=as.character(bf)
  }else if(bf<40){
    temp2[5]=as.character(bf)
  }else{
    temp2[5]="Wrong Input"
  }    
  return(data.frame(temp1=temp1,temp2=temp2))
}



#Define serve function
server <- function(input, output) {
  output$ou1 <- renderPrint({input$in1})
  output$ou2 <- renderPrint({input$in2})
  output$bodyfat <- renderPrint({Bodyfat(input$in1, input$in2)})
  Description = c("Essential fat", "Athletes", "Fitness", "Average", "Obese")
  Men = c("2-5%","6-13%","14-17%","18-24%","25%+")
  Women = c("10-13%","14-20%","21-24%","25-31%","32%+")
  #table1 = data.frame(Description=Description,Men=Men,Out=temp)
  output$table1 <-renderTable({data.frame(Description=Description,Men=Men,Out=war(input$in1, input$in2)$temp1)}) 
  output$table2 <-renderTable({data.frame(Description=Description,Women=Women,Out=war(input$in1, input$in2)$temp2)})
}

# Run the application 
shinyApp(ui = ui, server = server)
