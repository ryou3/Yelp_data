load('../Data/model.RData')
df = read.csv("../Data/used_data.csv")
server = shinyServer(function(input, output) {
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
               ddply(.(group),summarise,stars = mean(stars),.drop=FALSE) 
             %>%.[,2] %>% round(digits=1))  %>%
      ggplot() +
      geom_bar(aes(x = factor(stars), y = density, fill=color), 
               stat="identity")+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0))+                  
     labs(x = "Stars", y = "Percentage among Bars",
           title = stars_level(res()))+          # Labels on each axis
      scale_fill_manual(values=c('#FDE469','#EDA086'))                # Set the color of bins
    
    
  })
  
 

})

shinyApp(ui= ui, server=server)
