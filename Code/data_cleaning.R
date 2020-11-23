if (!require("stringr")) {install.packages("stringr"); stopifnot(require("stringr")) }
if (!require("ggplot2")) {install.packages("ggplot2");stopifnot(require("ggplot2")) }
if (!require("dplyr")) {install.packages("dplyr");stopifnot(require("dplyr")) }

rm(list = ls())

## read the data and change to data frame

business_bars = read.csv('../Data/bars.csv')
apply(sub_business_bars,1,function(row){
  sapply(row, function(col){col==''})%>% sum >=1
}) %>% sum
#  2371

# Data Cleaning ####
business_bars_cleaned = business_bars
# Attributes with True/False already ####
goodAttributes = c("BusinessAcceptsCreditCards",'BikeParking',
                   "RestaurantsTakeOut","GoodForKids",
                   'RestaurantsDelivery','OutdoorSeating',
                   'RestaurantsReservations','RestaurantsGoodForGroups',
                   'HasTV','Caters',"WheelchairAccessible", 
                   'RestaurantsTableService','BusinessAcceptsBitcoin','HappyHour','DogsAllowed')
# Define a function to convert "True" to TRUE
# We can find that for these attributes, there are only True,False,None, and "" for them.
apply(business_bars[,goodAttributes],2,unique)
string2bool = function(x){
  if(x == 'True'){
    return(TRUE)}else if(x=='False'){
      return(FALSE)}else if((x == '') | (x == 'None') | (x == 'none')){
        return(NA)
      }else(return(x))
}

for (attr in goodAttributes) {
  business_bars_cleaned[,attr] = sapply(business_bars_cleaned[,attr],string2bool)%>% unlist
  
}
# Next, process attributes that haven't been considered one by one.

 dict2vector = function(x){
  y = str_split(x,'\'|:|\\{|\\}| |,',simplify = TRUE) 
  y = y[y!='']
  keys = y[(y!='True') & (y!='False') & (y!='None')]
  value = y[(y=='True') | (y=='False') | (y == 'None')]
  value = sapply(value,string2bool)
  df = matrix(value,nrow=1,byrow=TRUE) %>% as.data.frame
  names(df) = keys
  return(df)
}


one2more = function(column,from = 1){
  # Input column: One column in the `business_bars` dataframe, 
  #               with values in a string of a python dictionary form.
  # Input from: The location where we should start converting this column.
  #             If the first value is NA, then it's needed to set this argument manually.
  # Output df: A dataframe with columns indicating keys of python dictionary.

  column = sapply(column, string2bool)
  df =dict2vector(column[from])
  for(i in (1:length(column))){
    if(is.na(column[i]) == TRUE){
      tem = rep(NA,ncol(df)) %>% matrix(nrow = 1, byrow =TRUE) %>% as.data.frame()
      names(tem) = names(df)
      df = rbind(df,tem)
    }else{
      temp = dict2vector(column[i])
      s1 = setdiff(names(temp),names(df))
      s2 = setdiff(names(df),names(temp))
      if(length(s1)>0){
        name = s1
        df[name] = rep(NA,nrow(df))
      }
      if(length(s2)>0){
        name = s2
        temp[name] = rep(NA,length(name))
      }
      df = rbind(df,temp)
    }
  }
  df = df[-1,]
  return(df)
}

# BusinessParking ####
BusinessParking = one2more(business_bars$BusinessParking)
BusinessParking = apply(BusinessParking,1,sum)
BusinessParking = sapply(BusinessParking, function(x){
  if(is.na(x)==TRUE){
    return(NA)
  }else if(x>0){
    return('YES')
  }else{ 
    return("NO")}
})

  
# WiFi ####
WiFi = sapply(business_bars$WiFi,function(x){
  if(str_detect(x,'free')){
    return('free')
  }else if(str_detect(x,'paid')){
    return('paid')
  }else if(str_detect(x,'no')){
    return('no')
  }else{return(NA)}
})
names(WiFi) = NULL


# Ambience ####
Ambience = one2more(business_bars$Ambience)


#  RestaurantsPriceRange2 ####
RestaurantsPriceRange2 = sapply(business_bars$RestaurantsPriceRange2,string2bool)


# Alcohol ####
Alcohol = sapply(business_bars$Alcohol,string2bool)
Alcohol = sapply(Alcohol,function(x){
  if(is.na(x) == FALSE){
    if(str_detect(x,pattern = 'none') == TRUE){
      return(NA)
    }else if(str_detect(x,pattern = 'full_bar') == TRUE){
      return("full_bar")}else if(str_detect(x,pattern = 'beer_and_wine') == TRUE){
        return("beer_and_wine")
      }else{
        return(x)
      }
  }else return(x)
  
 
})
names(Alcohol) = NULL

# RestaurantsAttire ####
RestaurantsAttire =  sapply(business_bars$RestaurantsAttire,function(x){
  if(is.na(x) == FALSE){
    if(str_detect(x,pattern = 'none') == TRUE){
      return(NA)
    }else if(str_detect(x,pattern = 'casual') == TRUE){
      return("casual")}else if(str_detect(x,pattern = 'dressy') == TRUE){
        return("dressy")
      }else if(str_detect(x,'formal')==TRUE){
        return('formal')
      }else{
        return(NA)
      }
  }else return(NA)
  
  
})
names(RestaurantsAttire) = NULL
RestaurantsAttire[RestaurantsAttire=='dressy'] = 'formal'


# NoiseLevel ####
NoiseLevel =  sapply(business_bars$NoiseLevel,function(x){
  if(is.na(x) == FALSE){
    if(str_detect(x,pattern = 'very_loud') == TRUE){
      return("very_loud")}else if(str_detect(x,pattern = 'quiet') == TRUE){
        return("quiet")
      }else if(str_detect(x,'average')==TRUE){
        return('average')
      }else if(str_detect(x,'loud')==TRUE){
        return('loud')
      }else{
        return(NA)
      }
  }else return(NA)
  
  
})
names(NoiseLevel) = NULL

# ByAppointmentOnly
ByAppointmentOnly=sapply(business_bars$ByAppointmentOnly,string2bool)

#GoodForMeal ####
# We applied lm to every factor and we found that dinner and brunch are significant under 0.05 
GoodForMeal=one2more(business_bars$GoodForMeal,from = 3)

# Music ####
# We applied lm to every factor and we found dj and jukebox are significant.
business_bars$Music %>% unique
Music = one2more(business_bars$Music ,from =3)
Music


# BestNights ####
BestNights = one2more(business_bars$BestNights,from = 3)
#hour.day ####
hour.day = function(x){
  if((!is.na(x)) & (x!= '')){
    y = str_extract_all(x, "[0-9]{1,2}:[0-9]{1,2}") %>% unlist
    hour = 0
    for (i in seq(1,length(y),2)) {
      if((!is.na(y[i+1])) & (!is.na(y[i])) ){
        today = strptime(y[i+1], "%M:%S") - strptime(y[i], "%M:%S")
        today = as.numeric(today)
        if(today < 0){today = today+24}
        hour = hour+today
      }else{hour=NA}
    }
  }else(hour = NA)
  
  return(hour) 
}
 
business_bars$hours.time = sapply(business_bars$hours,hour.day) %>% unlist
#Happy hour####
business_bars$happyhour =as.logical(business_bars$HappyHour)

# Merge the dataset ####
names(business_bars_cleaned)
business_bars_cleaned = business_bars_cleaned[,c('business_id','state','stars',
                                                 goodAttributes)]
business_bars_cleaned = cbind(business_bars_cleaned,BusinessParking,
                                WiFi,Ambience,RestaurantsPriceRange2,Alcohol,
                                RestaurantsAttire,NoiseLevel,ByAppointmentOnly,
                                GoodForMeal$dinner,GoodForMeal$brunch,Music$dj,
                                Music$jukebox,BestNights$friday,
                              business_bars$hours.time,business_bars$happyhour)

# Remove columns with linear regression p-value > 0.1
x = names(business_bars_cleaned)[-c(1,3)]
p=sapply(x, function(name){
  y=lm(business_bars_cleaned$stars~business_bars_cleaned[,name]) %>% summary() %>% .$coefficients %>% .[2,4]
  return(y)
})
which(p>0.1) # state  RestaurantsReservations RestaurantsGoodForGroups 
#  Caters   BusinessAcceptsBitcoin   WiFi  Music$jukebox

business_bars_cleaned[,names(which(p>0.1))] = NULL


# Convert character / logical variables to factors.
for(attr in which(sapply(business_bars_cleaned[1,],class) == 'character') %>% names %>% .[-1]){
  business_bars_cleaned[,attr] = as.factor(business_bars_cleaned[,attr])
}
complete.cases(business_bars_cleaned)
for(attr in which(sapply(business_bars_cleaned[1,],class) == 'logical') %>% names %>% .[-1]){
  business_bars_cleaned[,attr] = as.factor(business_bars_cleaned[,attr])
}

# Keep variables with less than half NA's and remove the rest
num_na =data.frame(attr = names(business_bars_cleaned),
                   missing = sapply( names(business_bars_cleaned),function(x){
                     is.na(business_bars_cleaned[,x]) %>% sum
                   }))
good_variables = num_na$attr[num_na$missing/nrow(business_bars_cleaned)<0.5]
good_variables = good_variables[-c(1)]


business_bars_cleaned$star_level = ifelse(business_bars_cleaned$stars>=4,"high",'low')
business_bars_cleaned = business_bars_cleaned[,c('star_level',good_variables)]

business_bars_cleaned$RestaurantsPriceRange2 = as.factor(business_bars_cleaned$RestaurantsPriceRange2)
business_bars_cleaned = business_bars_cleaned %>% mutate(
  hours.time=business_bars$hours.time,
  happyhour=business_bars$happyhour
)

  
# write.csv(../Data/business_bars_cleaned.csv')


