
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("X CHROMOSOME INACTIVATION (XCI)"),
  
  
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("type",label = "Mechanism (Select any one)",choices = list("RANDOM"="RANDOM","IMPRINTED"="IMPRINTED","SKEWED"="SKEWED")),    
      
      selectInput("row","No of rows:",choices = c(1,2,3,4,5)),
      
      selectInput("columns","No of columns:",choices = c(1,2,3,4,5)),
      
      selectInput("divisions","No of divisions:", choices = c(0,1,2,3,4,5)),
      
      conditionalPanel(condition="input.type == 'SKEWED'", selectInput("prob_Xp","Probability of Xp:",choices = c(0.8,0.2,0.1,0.9)))
     
    ),
    
    mainPanel(
      p(h3("Description")),
      p("In the below diagram blue squares indicate the activation of paternal x chromosome (Xp) and yellow squares indicate the activation of maternal X chromosome (Xm)."),
      p("If the choice of mechanism is Random then, the probability of activation of both Xp and Xm is 0.5 each."),
      p("If the choice of mechanism is Imprinted then, the probability of activation of both Xp and Xm is 0, 1 respectively."),
      p("If the choice of mechanism is Skewed then, the probability selected in the dropdown box will be the probability of activation
        Xp and 1-probability of activation of Xp is the probability of activation of Xm."),
      p("The selection of probability is based on literature."),
      p("Cells are depicted in a form of matrix. You can specify number of cells you want using number of rows and number of columns dropdown box. Number of divisions dropdown box corresponds to number of division of each cell"),
      plotOutput("cellplot")
    )
  )
))
