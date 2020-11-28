install.packages("jsonlite")
install.packages("stringr")

setwd("C:/Users/27477/Desktop/628HW3/Data")

rm(list = ls())

getwd()

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

## view with non-list/list dataset
View(bus1)

## find the needed categories, eg. Fast Food, has 1638 entries
target="Bars"
a=bus1$categories

library("stringr")

bus=str_detect(a,target)
summary(bus)
a[which(bus==T)[4]]


## eg. 29-th business's category is NA
### a close view of the business data

bar_t=business_city[which(bus==T),]
View(bar_t)
attach(bar_t)
table(stars)
View(hours)

## data cleaning

## time cleaning as morning, afternoon, night

### more precise version is provided in Github

## select useful attributes

### WiFi cleaning


## specific Recommedations, for the particular question within the industry
## based on RNN

#### select the reviews as same bussiness ID in buss_city
##### then extract the needed dataset in lstm

bus.ID=bus1$business_id[which(bus==T)]


reviews1=review_city[review_city$business_id %in% bus.ID,]
attach(reviews1)
reviews=subset(reviews1,select = c(review_id,business_id,stars,text))

View(reviews)
dir=getwd()
dir1=paste0(dir,"/reviews.csv")
library("readr")
write_csv(reviews,file=dir1)
length(unique(business_id))
### Here, we ended with generating the dataset.























