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


server <- function(input, output) {

  
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
      menuItem("Specific Advice: Word Count", tabName = "widgets2")
    )
  ),
  body
)


shinyApp(ui, server)
