library(dplyr)
library(ggplot2)
library(gridExtra)
library(car)
library(broom)
library(PResiduals)

# Load th models and data.
source("data_cleaning.R")
business_bars_cleaned = read.csv("../Data/business_bars_cleaned.csv")

# Inflation ####
car::vif(m5)

# Linearity ####

# 9 Plots
# X: logit , Y: the only continuous predictor
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

# probability-level residuals
pres <- presid(m5)
ggplot(data.frame(y = pres), aes(sample = y)) +
  stat_qq(distribution = qunif, dparams = list(min = -1, max = 1), alpha = 0.5) + xlab("Sample quantile") +
  ylab("Theoretical quantile") + theme_bw()

# Check the link function ####

l1 = polr(as.factor(stars)~.,df5,Hess = TRUE)
l2 = polr(as.factor(stars)~.,df5,Hess = TRUE,method = "logistic")
l3 = polr(as.factor(stars)~.,df5,Hess = TRUE,method = "loglog")
l4 = polr(as.factor(stars)~.,df5,Hess = TRUE,method = "cloglog")
l5 = polr(as.factor(stars)~.,df5,Hess = TRUE,method = "cauchit")
par(mfrow = c(2, 3), mar = c(2, 4, 2, 2) + 0.1)
plot(gof(m5, nsim = 100, test = "ad"), main = "Probit")
plot(gof(l1, nsim = 100, test = "ad"), main = "Proportional Odds")
plot(gof(l2, nsim = 100, test = "ad"), main = "Logistic")
plot(gof(l3, nsim = 100, test = "ad"), main = "Loglog")
plot(gof(l4, nsim = 100, test = "ad"), main = "cloglog")
plot(gof(l5, nsim = 100, test = "ad"), main = "cauchit")

