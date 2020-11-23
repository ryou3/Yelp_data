library(dplyr)
library(ggplot2)
library(gridExtra)

# Load th models and data.
source("data_cleaning.R")
business_bars_cleaned = read.csv("../Data/business_bars_cleaned.csv")

# Linearity ####
# 9 Plots
# X: logit , Y: predictors
logit = as.data.frame(m5$fitted.values)
names(logit) = sapply(seq(1,5,by=0.5),
         function(x){paste0('logit_star',x,sep ='')})
p1=cbind(df5,logit) %>% ggplot(aes(x=logit_star1,y = hours.time)) + 
  geom_point(size = 0.5, alpha = 0.5) + theme_bw() +labs(y = 'Open Hours / Week')
p2=cbind(df5,logit) %>% ggplot(aes(x=logit_star1.5,y = hours.time)) + 
  geom_point(size = 0.5, alpha = 0.5) + theme_bw() +labs(y = 'Open Hours / Week')
p3=cbind(df5,logit) %>%  ggplot(aes(x=logit_star2,y = hours.time)) + 
  geom_point(size = 0.5, alpha = 0.5) + theme_bw() +labs(y = 'Open Hours / Week')
p4=cbind(df5,logit) %>%  ggplot(aes(x=logit_star2.5,y = hours.time)) + 
  geom_point(size = 0.5, alpha = 0.5) + theme_bw() +labs(y = 'Open Hours / Week')
p5=cbind(df5,logit) %>%  ggplot(aes(x=logit_star3,y = hours.time)) + 
  geom_point(size = 0.5, alpha = 0.5) + theme_bw() +labs(y = 'Open Hours / Week')
p6=cbind(df5,logit) %>%  ggplot(aes(x=logit_star3.5,y = hours.time)) + 
  geom_point(size = 0.5, alpha = 0.5) + theme_bw() +labs(y = 'Open Hours / Week')
p7=cbind(df5,logit) %>%  ggplot(aes(x=logit_star4,y = hours.time)) + 
  geom_point(size = 0.5, alpha = 0.5) + theme_bw() +labs(y = 'Open Hours / Week')
p8=cbind(df5,logit) %>%  ggplot(aes(x=logit_star4.5,y = hours.time)) + 
  geom_point(size = 0.5, alpha = 0.5) + theme_bw() +labs(y = 'Open Hours / Week')
p9=cbind(df5,logit) %>%  ggplot(aes(x=logit_star5,y = hours.time)) + 
  geom_point(size = 0.5, alpha = 0.5) + theme_bw()+labs(y = 'Open Hours / Week')
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,layout_matrix = matrix(1:9,nrow = 3))


