install.packages("jsonlite")
install.packages("stringr")

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
## view with non-list/list dataset

View(bus1)

## find the needed categories, eg. Fast Food, has 1638 entries
target="Ice Cream"
a=bus1$categories

library("stringr")

bus=str_detect(a,target)
summary(bus)
which(bus==T|bus==F)
## eg. 29-th business's category is NA

IceCream=business_city[which(bus==T),]
View(IceCream)
attach(IceCream)
table(stars)
View(hours)

# data cleaning

## time cleaning as morning, afternoon, night

time=hours

for (i in 1:dim(time)[2]) {
  time1=time[,i]
  time1=str_replace_na(time1)
  time1=str_replace_all(time1, "[-:]", " ")
  for (j in 1:dim(time)[1]) {
    t=time1[j]
    if (t == "NA"){
    }
    else {
      m=strsplit(t," ")
      m[[1]][1]
    }
  }
}


## select useful attributes
View(attributes)
### WiFi cleaning
wifi=attributes$WiFi
View(attributes$WiFi)
table(attributes$WiFi)
View(wifi)
wifi=str_replace_na(wifi)
wifi[str_detect(wifi,"free")]="free"
wifi[str_detect(wifi,"no")]="no"
wifi[str_detect(wifi,"NA")]="no"

### 
 

### combined business's names with its reviews
#### a eg. the first restaurants has 7 reviewer
a=FastFood$business_id[1]
review_num=str_detect(review_city$business_id,a)
sum(review_num)


## General Recommedations, based on ordered logit model




## specific Recommedations, for the particular question within the industry
## based 

