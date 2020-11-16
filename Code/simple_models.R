business_bars_cleaned = read.csv("business_bars_cleaned.csv")

# 1. linear regression model ####
mod1_0.5 = lm(stars~.,business_bars_cleaned[,good_variables])
summary(mod1_0.5) # R2=0.1958
mod2_0.5 = step(lm(stars~.,na.omit(business_bars_cleaned[,good_variables])),k=2)
summary(mod2_0.5)


# 2. logistic regression model ####

logit1 = glm(as.factor(stars)~HasTV,business_bars_cleaned,
             family =binomial(link="logit"))
summary(logit1) # AIC:  64.872


logit2 = glm(as.factor(stars)~ HasTV+
               BusinessAcceptsCreditCards + RestaurantsDelivery+
               +BusinessParking+NoiseLevel+intimate+hipster+classy+trendy+
               RestaurantsPriceRange2,business_bars_cleaned,
             family =binomial(link="logit"))
summary(logit2) # 不收敛，很多p-value = 1


logit3 = glm(ifelse(business_bars_cleaned$star_level=='high',1,0)~HasTV,business_bars_cleaned,
             family =binomial(link="logit"))
summary(logit3) # AIC: 4439.8

logit4 = glm(ifelse(business_bars_cleaned$star_level=='high',1,0)~ HasTV+
               BusinessAcceptsCreditCards + RestaurantsDelivery+
               +BusinessParking+NoiseLevel+intimate+hipster+classy+trendy+
               RestaurantsPriceRange2,business_bars_cleaned,
             family =binomial(link="logit"))
summary(logit4) # AIC: 2359.4






# 3. regression tree model. ####
regtree2 = rpart(as.factor(stars)~.,business_bars_cleaned[,good_variables])
summary(regtree1) # expected loss=0.7351211


regtree2 = rpart(as.factor(stars)~BusinessAcceptsCreditCards + RestaurantsDelivery+
                   HasTV+BusinessParking+intimate+touristy+hipster+classy+trendy+
                   RestaurantsPriceRange2+NoiseLevel,
                 data = business_bars_cleaned[,good_variables])
summary(regtree2) # expected loss=0.7351211

regtree3 = rpart(as.factor(stars)~BusinessAcceptsCreditCards + RestaurantsDelivery+
                   HasTV+BusinessParking+NoiseLevel,
                 data = business_bars_cleaned[,good_variables],
                 na.action = na.omit)
summary(regtree3)
plot(regtree3)
text(regtree3,use.n = TRUE)


regtree4 = rpart(as.factor(stars)~HasTV+BusinessAcceptsCreditCards,
                 data = business_bars_cleaned[,good_variables],
                 na.action = na.omit)
summary(regtree4)
plot(regtree4)
text(regtree4,use.n = TRUE)

regtree5 = rpart(business_bars_cleaned$star_level~HasTV+BusinessAcceptsCreditCards,
                 data = business_bars_cleaned[,good_variables],
                 na.action = na.omit)
summary(regtree5) # expected loss=0.3893324
par(mfrow = c(1,1), xpd = NA)
plot(regtree5)
text(regtree5)


regtree6 = rpart(business_bars_cleaned$star_level~HasTV+BusinessAcceptsCreditCards+
                   RestaurantsDelivery+BusinessParking+NoiseLevel,
                 data = business_bars_cleaned[,good_variables],
                 na.action = na.omit)
summary(regtree6)
par(mfrow = c(1,1), xpd = NA)
plot(regtree6)
text(regtree6)


business_bars_cleaned$star_level = ifelse(business_bars_cleaned$stars>=4,"high",'low')
regtree7 = rpart(business_bars_cleaned$star_level~HasTV+
                   RestaurantsDelivery,
                 data = business_bars_cleaned[,good_variables],
                 na.action = na.omit)
summary(regtree7) #xpected loss=0.3713252 
par(mfrow = c(1,1), xpd = NA)
plot(regtree7)
text(regtree7)
