#SL2F4
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
source("jaxmat.R")
#The user interface
header <- dashboardHeader(
  title = HTML("Iso F<sub>4</sub>"),
  titleWidth = 500
)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  useShinyjs(),
  fluidRow(
    column(
      width = 6,
      title = "Create a Matrix",
      radioButtons("trace", "Choose the trace",
        choiceNames = c("Trace 0","Trace 1","Trace x","Trace x+1"),
        choiceValues = c("0","1","x","x+1")
      ),
      actionButton("generate","Create Matrix A & B"),
      uiOutput("matrixA"),
      uiOutput("matrixB"),
      actionButton("permute","Construct Permutations: PA & PB"),
      actionButton("calcProduct","Calculate P1 = PA*PB"),
      actionButton("calcMatrix","Calculate A*B"),
      hidden(actionButton("confirm","Construct P2 from A*B"))
    ),
    column(
      width = 6,
      h3("Show Calculations"),
      uiOutput("permA"),
      uiOutput("permB"),
      uiOutput("permProduct"),
      uiOutput("prodAB"),
      uiOutput("P2")
         
    ),
  )
)
ui <- dashboardPage(header, sidebar, body)

#Functions that implement the mathematics
source("F4calc.R")
source("permutecalc.R")

#Functions that read the input and modify the ouptpu and input
server <- function(session, input, output) {
  #Variables that are shared among server functions
  A <- matrix(c("0","x","x+1","1"))

    
  #Functions that respond to events in the input
  observeEvent(input$generate,{
    
     A <<- F4CreateMatrix(input$trace)
     B <<- F4CreateMatrix(input$trace)
     
     output$matrixA <- renderUI(jax.matrix(A, name = "A"))
     output$matrixB <- renderUI(jax.matrix(B, name = "B"))
     
     output$permProduct <- renderUI("")
     output$permA <- renderUI("")
     output$permB <- renderUI("")
     output$permProduct <- renderUI("")
     output$prodAB <- renderUI("")
     output$P2 <- renderUI("")
     shinyjs::hide(id = "confirm")
     
    # sampleFodder <- c("A","B","C1","D1","E","F1","G","H")
    # matrices <- sample(sampleFodder,2)
     # output$matrixA <- renderUI({jax.matrix(eval(parse(text=matrices[1])), name = "A")})
     # output$matrixB <- renderUI({jax.matrix(eval(parse(text=matrices[2])), name = "B")})

  })
  observeEvent(input$calcProduct,{
    prodAB <- multiply(permA,permB)
    output$permProduct <- renderUI(h3(paste("P1 = ",prodAB)))
  })
  
  observeEvent(input$permute,{
    # Change 6 -> 5
    fvalA <- sapply(1:5,Transform,A=A)
    permA <<- cycle.convert(fvalA)
    output$permA <-renderUI(h3(paste(HTML("PA ="),permA)))
    
    fvalB <- sapply(1:5,Transform,A=B)
    permB <<- cycle.convert(fvalB)
    output$permB <-renderUI(h3(paste(HTML("PB ="),permB)))
  })
  
  observeEvent(input$calcMatrix,{
    prodAB <<- F4MatProd(A,B)
    output$prodAB <- renderUI({jax.matrix(prodAB, name = "AB")})
    shinyjs::show(id = "confirm")
  })
  
  observeEvent(input$confirm,{
    # cat(prodAB)
    # if(prodAB == c(1,0,0,1)) {
    #   output$P2 <-renderUI(h3(paste(HTML("P2 ="),"I")))
    # }
    # 
    fvalAB <- sapply(1:5,Transform,A=prodAB)
    perm2 <<- cycle.convert(fvalAB)
    output$P2 <-renderUI(h3(paste(HTML("P2 ="),perm2)))
  })
}

#Run the app
shinyApp(ui = ui, server = server)