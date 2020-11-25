library(MASS)

business_bars_cleaned = read.csv("business_bars_cleaned.csv")

# 1. linear regression model ####
mod1_0.5 = lm(stars~.,business_bars_cleaned[,good_variables])
summary(mod1_0.5) # R2=0.1958
mod2_0.5 = step(lm(stars~.,na.omit(business_bars_cleaned[,good_variables])),k=2)
summary(mod2_0.5)


# 2. ordered logistic regression model ####
m1 = polr(as.factor(stars)~.,business_bars_cleaned[,-1])
summary(m1)
step(m1,k=2)

subset = c('stars','BikeParking','BusinessParking',
            'HasTV' ,'NoiseLevel','RestaurantsPriceRange2',
            'happyhour','hours.time','review_count')
df = na.omit(business_bars_cleaned[,subset])
df$RestaurantsPriceRange2 = as.factor(df$RestaurantsPriceRange2)
m = polr(as.factor(stars)~.,df,Hess = TRUE,method = 'probit')
summary(m)
model = step(m5,k=2) 

save(model,file='../Data/model.RData') # save the model to Data folder


# Functions to summarise the model
inference = function(model){
  ctable <- coef(summary(model))
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ctable <- cbind(ctable, "p value" = p)
  ci <- confint(model)
  odds = exp(coef(model))
  return(list(ctable = ctable, # coefficient table with p-values
              ci=ci, # confidence intervals
              odds = odds)) # odds ratios
}

inferece(model)
