library(shiny)
library(ggplot2)
library(shinyWidgets)

ui = fluidPage(
  #helpText("App builder: Xinran Miao: xmiao27@wisc.edu.   ",
  #         "   To report problems: Contact Yudi Mu:  / Runze You: ryou3@wisc.edu"),
  
  titlePanel(h1("Improve your Bar's Yelp Ratings!",align="center")),
  
  sidebarPanel(
    p(h4("Who are this app built for?",align="left"),'All bar owners who desire to improve their ratings.'
         ),
    p(h4('How do you build this app?'),
      'By analyzing a large amount of Yelp data.'),
    p(h4('How can I use this app'),
      'First, click the first tab, type in your basic information and get general advice.')),
  mainPanel(tabsetPanel(
    tabPanel("General advice", br(), 
             fluidRow(
               column(2,selectInput("HasTV", 'Has TV',choices = c(YES = TRUE, NO = FALSE))),
               column(2,selectInput("BikeParking","Bike Parking", choices = c(YES = TRUE, NO = FALSE))),
               column(2,  selectInput("NoiseLevel",'Noise Level', choices = c(Loud = "loud", 
                                                                              VeryLoud = "very loud",
                                                                              Quiet = 'quiet',
                                                                              Average = 'average'))),
               column(2,numericInput("hours.time", "Hours/week", value=44)),
               column(2, numericInput("review_count", "Review Count", value=4)),
               br(),
               br(),
               submitButton("Calculate!", icon("refresh")),
               p(h4(span("Estimated Yelp Rating", verbatimTextOutput("res")))),
               p(h4('Here is our advice:'),verbatimTextOutput("suggestion_text")),
               plotOutput("Plot")
             )
             
    ),
    
    tabPanel("tab2", br(), 
             p(h4("text"),"To get the most accurate result"),
             p(h4("")
             ))
  ))
)
shinyApp(ui= ui, server=server)
