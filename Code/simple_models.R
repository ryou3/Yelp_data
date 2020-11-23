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

subset = c('stars','BusinessAcceptsCreditCards' ,
             'BikeParking' ,'BusinessParking','Alcohol',
             'RestaurantsDelivery', 'HasTV' ,'NoiseLevel')
df = na.omit(business_bars_cleaned[,subset])
m2 = polr(as.factor(stars)~.,df)

m3 = stepAIC(m2,k=2)
summary(m3)

subset2 = c('stars','BikeParking','BusinessParking',#'RestaurantsDelivery',
            'HasTV' ,'NoiseLevel','RestaurantsPriceRange2')
df2$RestaurantsPriceRange2 = as.factor(df2$RestaurantsPriceRange2)
df2 = na.omit(business_bars_cleaned[,subset2])

m4 = polr(as.factor(stars)~.,df2,Hess = TRUE)
summary(m4)
step(m4,k=2)


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
inference(m4)
