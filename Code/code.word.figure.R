library(readxl)
library(jiebaR)
library(plyr)
library(wordcloud2)
library(dplyr)
library(stringr)
library(ggplot2)
#install.packages("devtools")
#devtools::install_github("thomasp85/patchwork")
library(patchwork)

review_city <- jsonlite::stream_in(file("review_city.json"),pagesize = 942027)
review_city = as.data.frame(review_city)

bar = read.csv("bars.csv")
nrow(bar)

review = review_city %>%
  filter(business_id  %in% bar$business_id)
nrow(review)

review$index = 1:nrow(review)
engine = worker()
temp = tolower(review$text)
temp = gsub("'s"," is ", temp)
temp = gsub(";"," ", temp)
temp = gsub(","," ", temp)
temp = gsub("'"," ", temp)
temp = gsub(":"," ", temp)
temp = gsub("-"," ", temp)
temp = gsub("`"," ", temp)
temp = gsub("\n"," ", temp)
temp = gsub('\"'," ", temp, fixed = TRUE)
temp = gsub("?"," ", temp, fixed = TRUE)
temp = gsub("*"," ", temp, fixed = TRUE)
temp = gsub("."," ", temp, fixed = TRUE)
temp = gsub("!"," ", temp, fixed = TRUE)
temp = gsub("[^a-zA-Z]"," ",temp)
segwords = llply(temp, segment, engine)

wordsFreq = data.frame(table(unlist(segwords)))
wordsFreq1 = data.frame(lapply(wordsFreq, as.character), stringsAsFactors=FALSE)                      
wordsFreq2 = wordsFreq1[-which(nchar(wordsFreq1[,1])<4),]
wordsFreq3 = wordsFreq2[which(wordsFreq2[,2]>20),] 
wordsFreq4 = wordsFreq3[order(-as.numeric(wordsFreq3$Freq)),]
data = wordsFreq4[1:500,]
#write.table(wordsFreq4, "wordsfreq.sorted")
#write.table(wordsFreq, "wordsfreq")


word = cbind(review$stars, segwords)
wordsFreq4 %>%
   filter(Var1 == "old")

#great
great = c()
for (i in 1:length(segwords)) {
  if ("great" %in% segwords[[i]]){great[i] = review$stars[i]
  }else{next}}; great = data.frame(table(great))
p1 = ggplot(great, aes(x=great,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")

#good
good = c()
for (i in 1:length(segwords)) {
  if ("good" %in% segwords[[i]]){good[i] = review$stars[i]
  }else{next}}; good1 = data.frame(table(good))
p2 = ggplot(good1, aes(x=good,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")

#like
like = c()
for (i in 1:length(segwords)) {
  if ("like" %in% segwords[[i]]){like[i] = review$stars[i]
  }else{next}};nlike1 = data.frame(table(like))
p3 = ggplot(like1, aes(x=like,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")

#favorite
favorite = c()
for (i in 1:length(segwords)) {
  if ("favorite" %in% segwords[[i]]){favorite[i] = review$stars[i]
  }else{next}}; favorite1 = data.frame(table(favorite))
p4 = ggplot(favorite1, aes(x=favorite,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")



#disappointing
disappointing = c()
for (i in 1:length(segwords)) {
  if ("disappointing" %in% segwords[[i]]){disappointing[i] = review$stars[i]
  }else{next}}; disappointing1 = data.frame(table(disappointing))
p5 = ggplot(disappointing1, aes(x=disappointing,y=Freq))+
  geom_bar(stat="identity", fill = "cadetblue3")


#complaint
complaint = c()
for (i in 1:length(segwords)) {
  if ("complaint" %in% segwords[[i]]){complaint[i] = review$stars[i]
  }else{next}}; complaint1 = data.frame(table(complaint))
p6 = ggplot(complaint1, aes(x=complaint,y=Freq))+
  geom_bar(stat="identity", fill = "cadetblue3")

#average
average = c()
for (i in 1:length(segwords)) {
  if ("average" %in% segwords[[i]]){average[i] = review$stars[i]
  }else{next}}; average1 = data.frame(table(average))
p7 = ggplot(average1, aes(x=average,y=Freq))+
  geom_bar(stat="identity", fill = "cadetblue3")

#awful
awful = c()
for (i in 1:length(segwords)) {
  if ("awful" %in% segwords[[i]]){awful[i] = review$stars[i]
  }else{next}}; awful1 = data.frame(table(awful))
p8 = ggplot(awful1, aes(x=awful,y=Freq))+
  geom_bar(stat="identity", fill = "cadetblue3")

(p1+p2)/(p3+p4); (p5+p6)/(p7+p8)

#wait
wait = c()
for (i in 1:length(segwords)) {
  if ("wait" %in% segwords[[i]]){wait[i] = review$stars[i]
  }else{next}}; wait1 = data.frame(table(wait))
p9 = ggplot(wait1, aes(x=wait,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")


#reservation
reservation = c()
for (i in 1:length(segwords)) {
  if ("reservation" %in% segwords[[i]]){reservation[i] = review$stars[i]
  }else{next}}; reservation1 = data.frame(table(reservation))
p10 = ggplot(reservation1, aes(x=reservation,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")


#expensive
expensive = c()
for (i in 1:length(segwords)) {
  if ("expensive" %in% segwords[[i]]){expensive[i] = review$stars[i]
  }else{next}}; expensive1 = data.frame(table(expensive))
p11 = ggplot(expensive1, aes(x=expensive,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")

#cheap
cheap = c()
for (i in 1:length(segwords)) {
  if ("cheap" %in% segwords[[i]]){cheap[i] = review$stars[i]
  }else{next}}; cheap1 = data.frame(table(cheap))
p12 = ggplot(cheap1, aes(x=cheap,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")

#late  
late = c()
for (i in 1:length(segwords)) {
  if ("late" %in% segwords[[i]]){late[i] = review$stars[i]
  }else{next}}; late1 = data.frame(table(late))
p13 = ggplot(late1, aes(x=late,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")

#space
space = c()
for (i in 1:length(segwords)) {
  if ("space" %in% segwords[[i]]){space[i] = review$stars[i]
  }else{next}}; space1 = data.frame(table(space))
p14 = ggplot(space1, aes(x=space,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")

(p9+p10)/(p11+p12)/(p13+p14)


#pizza
pizza = c()
for (i in 1:length(segwords)) {
  if ("pizza" %in% segwords[[i]]){pizza[i] = review$stars[i]
  }else{next}}; pizza1 = data.frame(table(pizza))
p15 = ggplot(pizza1, aes(x=pizza,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")

#fries 
fries = c()
for (i in 1:length(segwords)) {
  if ("fries" %in% segwords[[i]]){fries[i] = review$stars[i]
  }else{next}}; fries1 = data.frame(table(fries))
p16 = ggplot(fries1, aes(x=fries,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")

#side 
side  = c()
for (i in 1:length(segwords)) {
  if ("side" %in% segwords[[i]]){side[i] = review$stars[i]
  }else{next}}; side1 = data.frame(table(side))
p17 = ggplot(side1, aes(x=side,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")

#sandwich
sandwich = c()
for (i in 1:length(segwords)) {
  if ("sandwich" %in% segwords[[i]]){sandwich[i] = review$stars[i]
  }else{next}}; sandwich1 = data.frame(table(sandwich))
p18 = ggplot(sandwich1, aes(x=sandwich,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")

#appetizers 
appetizers = c()
for (i in 1:length(segwords)) {
  if ("appetizers" %in% segwords[[i]]){appetizers[i] = review$stars[i]
  }else{next}}; appetizers1 = data.frame(table(appetizers))
p19 = ggplot(appetizers1, aes(x=appetizers,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")

#delicious 
delicious  = c()
for (i in 1:length(segwords)) {
  if ("delicious" %in% segwords[[i]]){delicious [i] = review$stars[i]
  }else{next}}; delicious1 = data.frame(table(delicious ))
p20 = ggplot(delicious1, aes(x=delicious,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")

(p15+p16)/(p17+p18)/(p19+p20)


#scotch 
scotch  = c()
for (i in 1:length(segwords)) {
  if ("scotch" %in% segwords[[i]]){scotch[i] = review$stars[i]
  }else{next}}; scotch = data.frame(table(scotch ))
p21 = ggplot(scotch, aes(x=scotch,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")

#whisky 
whisky = c()
for (i in 1:length(segwords)) {
  if ("whisky" %in% segwords[[i]]){whisky [i] = review$stars[i]
  }else{next}}; whisky = data.frame(table(whisky ))
p22 = ggplot(whisky, aes(x=whisky,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")

#bottle 
bottle  = c()
for (i in 1:length(segwords)) {
  if ("bottle" %in% segwords[[i]]){bottle [i] = review$stars[i]
  }else{next}}; bottle = data.frame(table(bottle ))
p23 = ggplot(bottle, aes(x=bottle,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")

#draft 
draft  = c()
for (i in 1:length(segwords)) {
  if ("draft" %in% segwords[[i]]){draft [i] = review$stars[i]
  }else{next}}; draft = data.frame(table(draft ))
p24 = ggplot(draft, aes(x=draft,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")

#mojito
mojito  = c()
for (i in 1:length(segwords)) {
  if ("mojito" %in% segwords[[i]]){mojito [i] = review$stars[i]
  }else{next}}; mojito = data.frame(table(mojito ))
p25 = ggplot(mojito, aes(x=mojito,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")

#coffee 
coffee  = c()
for (i in 1:length(segwords)) {
  if ("coffee" %in% segwords[[i]]){coffee [i] = review$stars[i]
  }else{next}}; coffee = data.frame(table(coffee ))
p26 = ggplot(coffee, aes(x=coffee,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")

#white 
white = c()
for (i in 1:length(segwords)) {
  if ("white" %in% segwords[[i]]){white[i] = review$stars[i]
  }else{next}}; white1 = data.frame(table(white))
p27 = ggplot(white1, aes(x=white,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")

#beer
beer  = c()
for (i in 1:length(segwords)) {
  if ("beer" %in% segwords[[i]]){beer [i] = review$stars[i]
  }else{next}}; beer = data.frame(table(beer ))
p28 = ggplot(beer, aes(x=beer,y=Freq))+
  geom_bar(stat="identity", fill = "deepskyblue3")


(p21+p22)/(p23+p24)/(p25+p26)/(p27+p28)






# Plots about variables in bars.csv
library(gridExtra)
business_bars_cleaned = read.csv("../Data/business_bars_cleaned.csv")

ggplot(business_bars_cleaned, aes(x=stars))+
  geom_histogram(aes(y=..count..), binwidth = 0.5, center=1,
                 color = 'white',fill = 'deepskyblue3') +
  theme_bw()+
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))+
  labs(x = 'Stars',y = 'Counts',
       title = 'Histogram of Review Stars')

par(mfrow = c(1,3))
# HasTV
good_rows = which(is.na(business_bars_cleaned[,'HasTV']) == FALSE)
p1=business_bars_cleaned %>% .[good_rows,] %>% ggplot()+
  geom_boxplot(aes(x = HasTV,y = stars),color = 'deepskyblue3') + 
  theme_bw()

# NoiseLevel
grid.arrange(p1,p2)
good_rows = which(is.na(business_bars_cleaned[,'NoiseLevel']) == FALSE)
p2=business_bars_cleaned %>% .[good_rows,] %>% ggplot()+
  geom_boxplot(aes(x = NoiseLevel,y = stars),color = 'deepskyblue3') + 
  theme_bw()
# BusinessAcceptsCreditCards
good_rows = which(is.na(business_bars_cleaned[,'BusinessAcceptsCreditCards']) == FALSE)
p3=business_bars_cleaned %>% .[good_rows,] %>% ggplot()+
  geom_boxplot(aes(x = BusinessAcceptsCreditCards,y = stars),color = 'deepskyblue3') + 
  theme_bw()

# RestaurantsDelivery
good_rows = which(is.na(business_bars_cleaned[,'RestaurantsDelivery']) == FALSE)
p4=business_bars_cleaned %>% .[good_rows,] %>% ggplot()+
  geom_boxplot(aes(x = RestaurantsDelivery,y = stars),color = 'deepskyblue3') + 
  theme_bw()

# Parking
good_rows = which(is.na(business_bars_cleaned[,'BusinessParking']) == FALSE)
p5=business_bars_cleaned %>% .[good_rows,] %>% ggplot()+
  geom_boxplot(aes(x = BusinessParking,y = stars),color = 'deepskyblue3') + 
  theme_bw()

#Price

good_rows = which(is.na(business_bars_cleaned[,'RestaurantsPriceRange2']) == FALSE)
p6=business_bars_cleaned %>% .[good_rows,] %>% ggplot()+
  geom_boxplot(aes(x = RestaurantsPriceRange2,y = stars),color = 'deepskyblue3') + 
  theme_bw()

# intimate
good_rows = which(is.na(business_bars_cleaned[,'intimate']) == FALSE)
p7=business_bars_cleaned %>% .[good_rows,] %>% ggplot()+
  geom_boxplot(aes(x = intimate,y = stars),color = 'deepskyblue3') + 
  theme_bw()

good_rows = which(is.na(business_bars_cleaned[,'hipster']) == FALSE)
p8=business_bars_cleaned %>% .[good_rows,] %>% ggplot()+
  geom_boxplot(aes(x = hipster,y = stars),color = 'deepskyblue3') + 
  theme_bw()

good_rows = which(is.na(business_bars_cleaned[,'classy']) == FALSE)
p9=business_bars_cleaned %>% .[good_rows,] %>% ggplot()+
  geom_boxplot(aes(x = classy,y = stars),color = 'deepskyblue3') + 
  theme_bw()

good_rows = which(is.na(business_bars_cleaned[,'trendy']) == FALSE)
p10=business_bars_cleaned %>% .[good_rows,] %>% ggplot()+
  geom_boxplot(aes(x = trendy,y = stars),color = 'deepskyblue3') + 
  theme_bw()
grid.arrange(p1,p2,p3,p4,layout_matrix = matrix(1:4,nrow = 2))
grid.arrange(p5,p6,layout_matrix = matrix(1:2,nrow = 1))
grid.arrange(p7,p8,p9,p10,layout_matrix = matrix(1:4,nrow = 2))

