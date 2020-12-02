if(!require("LDAvis")) install.packages("LDAvis")
devtools::install_github("cpsievert/LDAvis")
library(shiny)
library(LDAvis)
library(shinydashboard)
library(ggplot2)
library(shinyWidgets)
library(dplyr)

load("../Data/bar_review.RData")
load('../Data/model.RData')
df = read.csv("../Data/used_data.csv")
bus_id_name = read.csv("../Data/bus_id_name.csv")
suggest = read.csv("../Data/suggestOrNot.csv")

server = function(input, output) {
  f = function(x){
    a = predict(model,newdata = data.frame(BikeParking =as.logical(x[1]),
                                           HasTV = as.logical(x[2]),
                                           NoiseLevel = x[3],
                                           hours.time = as.numeric(x[4]),
                                           review_count = as.numeric(x[5])))
    return(a)
  }
  res <- reactive({
    as.character(f(c(input$HasTV,input$BikeParking,input$NoiseLevel,input$hours.time,input$review_count)))
  })
  output$res <- renderPrint({cat(res())})
  
  suggestion = function(x){
    y = 'It\'s always a great idea to get more reviews from your customer!\n'
    if(x[1] == 'FALSE'){
      y = paste0(y,'Try to add BikeParking.\n')
    }
    if(x[2] =='TRUE'){
      y = paste0(y,'Try to turn down the voice of your TV.\n')
    }
    if(x[3] != 'quiet'){
      y = paste0(y,'Try to be less noisy.\n')
    }
    if(y==''){y = 'Try to decrease your open hours.'}
    return(y)
  }
  
  reactive_suggestion = reactive(suggestion(
    c(input$HasTV,input$BikeParking,input$NoiseLevel,input$hours.time)
  ))
  output$suggestion_text =  renderPrint({cat(reactive_suggestion())})
  
  stars_level = function(x){
    a=paste0('Your bar rating is ',res(),'. It is higher than ',
             round(100*sum(df$stars<as.numeric(res()))/nrow(df),0),
             '% of other bars! ',sep='')
    return(a)
  }
  
  output$stars_level <-renderPrint({
    res_level =stars_level(res())
    cat(res_level)
  })
  
  output$Plot <- renderPlot({
    user =as.numeric(res()) 
    # Determine the data for plot.
    dataset = data.frame(stars = as.numeric(c(user,df$stars)),
                         group = cut(as.numeric(c(user,df$stars)),8))
    table(cut(df$stars,8)) %>% 
      as.data.frame() %>%
      dplyr::rename('group'='Var1') %>%
      mutate(density = Freq/2174,
             color = ifelse(group==dataset$group[1],"You Bar Is Here!",'Other Bars'),
             stars = dataset %>%
               ddply(.(group),summarise,stars = mean(stars),.drop=FALSE) %>% .[,2] %>% round(digits=1))  %>%
      ggplot() +
      geom_bar(aes(x = factor(stars), y = density, fill=color), 
               stat="identity")+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0))+                  
      labs(x = "Stars", y = "Percentage among Bars",
           title = stars_level(res()))         # Labels on each axis
    scale_fill_manual(values=c('#FDE469','#EDA086'))                # Set the color of bins
    
    
  })
  
  output$myChart = renderVis({
    if(!is.null(input$nTerms)){
      with(bar_review, 
           createJSON(phi, theta, doc.length, vocab, frequency, 
                      R = input$nTerms))
    } 
  })
  
  
  sug_asp_f=function(logi_asp){
    logi1=logi_asp[1:9]
    logi2=logi_asp[10:17]
    logi3=logi_asp[18:20]
    logi4=logi_asp[21:33]
    specificfood=c('cheese','chicken','burger','sushi',
                   'fries','sauce','salad','sandwich','pizza')
    spec=specificfood[logi1]
    flavour = c('food','bar', 'menu','beer','restaurant','dinner','drink','meal')
    fla=flavour[logi2]
    price = c('ordered','order','table')
    pri=price[logi3]
    service = c('place','service','time','night','staff','people',
                'experience','atmosphere','wait','server','area','minutes','lunch')
    ser=service[logi4]
    aaa1=paste(spec,collapse=",")
    aaa2=paste(fla,collapse=",")
    aaa3=paste(pri,collapse=",")
    aaa4=paste(ser,collapse=",")
    aaa0=paste(aaa1,aaa2,aaa3,aaa4)
    aaa=paste("Aspects needed improved:",aaa0,sep = " ")
    return(aaa)
  }
  sugestion111=function(){
    name = input$name
    aspects = c('cheese','chicken','burger','sushi',
                'fries','sauce','salad','sandwich','pizza','food','bar', 'menu','beer','restaurant','dinner','drink','meal',
                'ordered','order','table','place','service','time','night','staff','people',
                'experience','atmosphere','wait','server','area','minutes','lunch')
    if (name %in% bus_id_name$name) {
      bus_id=bus_id_name$business_id[bus_id_name$name==name]
      col_sug = which(colnames(suggest)==bus_id)
      logi_asp = suggest[,col_sug]
      if (sum(logi_asp)==0){
        aspect = "You have done better than the average level of all aspects."
      } else{
        name_all=aspects[logi_asp==T]
        aspect = sug_asp_f(logi_asp)
      }
    } else {
      aspect = "Please enter a correct form of your name. (Capitalize the first letter or add the space). Make sure you are Bars."
    }
    return(aspect)
  }
  aspect1=reactive({
    sugestion111()
  })
  
  output$aspect= renderPrint({
    cat(aspect1())
  })
} 


body =  dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            fluidPage(
              titlePanel(h1("Improve your Bar's Yelp Ratings!",align="center")),
              
              sidebarPanel(
                p(h4("Who are this app built for?",align="left"),'All bar owners who desire to improve their ratings.'
                ),
                p(h4('How do you build this app?'),
                  'By analyzing a large amount of Yelp data.'),
                p(h4('How can I use this app'),
                  'First, click the first tab, type in your basic information and get general advice.')),
              mainPanel(#tabsetPanel(
                #tabPanel("General advice", br(), 
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
                # )
                #   )
              )
            )
    ),
    
    # Second tab content
    tabItem(tabName = "widgets",
            fluidPage(
              sliderInput("nTerms", "Number of terms to display", min = 20, max = 40, value = 30),
              visOutput('myChart')),
            tags$style("
              body {
             -moz-transform: scale(0.8, 0.8); /* Moz-browsers */
             zoom: 0.8; /* Other non-webkit browsers */
             zoom: 80%; /* Webkit browsers */
             }
             ")
    ),
    
    # Third tab content
    tabItem(tabName = "widgets2",
            fluidPage(
              titlePanel(h1("Improve your Bar's Yelp Ratings!",align="center")),
              
              sidebarPanel(
                p(h4("How do we get the conclusion and give you advice?",align="left"),
                  'We deal with the collected, about 250,000+, reviews. Choose the 30+ most frequent aspects mentioned 
                  in the reviews and divide them into four parts, which are Specific Foods, Service, Flavour and Price. 
                  Then, tell you which parts and aspects you need to improve compared with your competitors in the whole market.'
                ),
                p(h4('The usage of this app?'),
                  'For reference, you will see the parts and aspects need improved'),
                p(h4('The selected aspects and correponding parts:'),
                  'specific food: cheese, chicken, burger, sushi, fries, sauce, salad, sandwich, pizza'),
                p('flavour: food, bar, menu, beer, restaurant, dinner, drink, meal'),
                p('price: ordered, order, table'),
                p('service: place, service, time, night, staff, people,
                            experience, atmosphere, wait, server, area, minutes, lunch')
              ),
              mainPanel(#tabsetPanel(
                #tabPanel("General advice", br(),
                fluidRow(
                  textInput("name","What's your name of your business?" ),
                  br(),
                  br(),
                  submitButton("Results:", icon("refresh")),
                  p(h4(span("Estimated Yelp Rating", verbatimTextOutput("aspect"))))
                  # )
                  #   )
                )
              )
            )
    )
  )
)


ui = dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("General advice", tabName = "dashboard"),
      menuItem("LDA", tabName = "widgets"),
      menuItem("Specific Advice: Word Count", tabName = "widgets2")
    )
  ),
  body
)

shinyApp(ui = ui, server = server)
