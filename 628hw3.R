install.packages("jsonlite")

setwd("C:/Users/27477/Desktop/628HW3/Data")

rm(list = ls())


## read the data and change to data frame
library("jsonlite")

user_city <- jsonlite::stream_in(file("user_city.json"),pagesize = 272024)
business_city <- jsonlite::stream_in(file("business_city.json"),pagesize = 36327)
review_city <- jsonlite::stream_in(file("review_city.json"),pagesize = 942027)
tip_city <- jsonlite::stream_in(file("tip_city.json"),pagesize = 129571)

user_city=as.data.frame(user_city);
business_city=as.data.frame(business_city)
review_city=as.data.frame(review_city)
tip_city=as.data.frame(tip_city)

View(tip_city)

bus1 = subset(business_city,select = -c(attributes,hours))

bus1
## 

