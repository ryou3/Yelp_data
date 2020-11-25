library(shiny)
library(ggplot2)
library(shinyWidgets)

ui = fluidPage(
  #helpText("App builder: Xinran Miao: xmiao27@wisc.edu.   ",
  #         "   To report problems: Contact Yudi Mu:  / Runze You: ryou3@wisc.edu"),
  
  titlePanel(h1("Improve your Bar's Yelp Ratings!",align="center")),
  
  sidebarPanel(
    p(h4("Please fill in the input boxes and click \'Submit\'",
                    align="center"))),
  mainPanel(tabsetPanel(
    tabPanel("How to use the Body fat Calculator", br(), 
             sidebarPanel(
               selectInput("HasTV", 'Has TV',choices = c(YES = TRUE, NO = FALSE)),
               selectInput("BikeParking","Bike Parking", choices = c(YES = TRUE, NO = FALSE)),
               selectInput("NoiseLevel",'Noise Level', choices = c(Loud = "loud", 
                                                                   VeryLoud = "very loud",
                                                                   Quiet = 'quiet',
                                                                   Average = 'average')),
               numericInput("hours.time", "Open hours / week", value=44),
               submitButton("Calculate!", icon("refresh"))
             ),
             plotOutput("Plot"),
             #conditionalPanel(
             #  condition = "as.numeric(res()) > 0 ",
             #  verbatimTextOutput("res")
             #),
             #br(),p(h4("Normally, people's bodyfat level is as bellow distributed")),plotOutput("Plot"),
             #p(h4( verbatimTextOutput("res"))),
             #plotOutput("Plot"),
             #p(h3(span("Yelp Rating:", verbatimTextOutput("res"), style = "font-weight: 300; float:left"), 
             #   style = "font-family: 'Source Sans Pro'; text-align: left; padding: 20px; float:left"),
             #  plotOutput("Plot"))
            ),
    
    tabPanel("tab2", br(), 
             p(h4("text"),"To get the most accurate result"),
             p(h4("")
             ))
  ))
)
shinyApp(ui= ui, server=server)




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
               column(3,selectInput("HasTV", 'Has TV',choices = c(YES = TRUE, NO = FALSE))),
               column(3,selectInput("BikeParking","Bike Parking", choices = c(YES = TRUE, NO = FALSE))),
               column(3,  selectInput("NoiseLevel",'Noise Level', choices = c(Loud = "loud", 
                                                                              VeryLoud = "very loud",
                                                                              Quiet = 'quiet',
                                                                              Average = 'average'))),
               column(3,numericInput("hours.time", "Hours/week", value=44)),
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

