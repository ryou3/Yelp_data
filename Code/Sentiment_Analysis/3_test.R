setwd("C:/Users/27477/Desktop/628HW3/")

rm(list=ls())

bus = read.csv('business_pos_neg.csv', header = T)
View(bus)
unique(bus$business_id)

## combine all the seperated words
specificfood=c('cheese','chicken','burger','sushi',
              'fries','sauce','salad','sandwich','pizza')
flavour = c('food','bar', 'menu','beer','restaurant','dinner','drink','meal')
price = c('ordered','order','table')
service = c('place','service','time','night','staff','people',
           'experience','atmosphere','wait','server','area','minutes','lunch')

combind_f= function(word,bus){
  pos_num=c()
  neg_num=c()
  for (i in 1:length(word)) {
      pos_num[i]=sum(bus$pos_num[bus$word==word[i]])
      neg_num[i]=sum(bus$neg_num[bus$word==word[i]])
  }
  frame=data.frame(word=word,pos_num=pos_num,neg_num=neg_num)
  return(frame)
}

specificfood1=combind_f(specificfood,bus)
flavour1=combind_f(flavour,bus)
price1=combind_f(price,bus)
service1=combind_f(service,bus)
wordcount = rbind(specificfood1,flavour1,price1,service1)

chiTestBus = function(word, busid){
  t1 = as.matrix(bus[bus$word==word&bus$business_id==busid,c(2,3)])
  t2 = as.matrix(wordcount[wordcount$word==word,c(2,3)])
  re = chisq.test(rbind(t1,t2))
  return (re$p.value)
}
chiTestBus('cheese','RJNAeNA-209sctUO0dmwuA')

chiTestState = function(word,state){
  t1 = t(as.matrix(colSums(bus[bus$state==state&bus$word==word,c(2,3)])))
  t2 = as.matrix(wordcount[wordcount$word==word,c(2,3)])
  re = chisq.test(rbind(t1,t2))
  return (re$p.value)
}
chiTestState('taste','NC')

fisherTestBus = function(word, busid){
  t1 = as.matrix(bus[bus$word==word&bus$business_id==busid,c(2,3)])
  t2 = as.matrix(wordcount[wordcount$word==word,c(2,3)])
  re = fisher.test(rbind(t1,t2))
  return (re$p.value)
}
fisherTestBus('taste','irft4YkdNsww4DNf_Aftew')

fisherTestState = function(word,state){
  t1 = t(as.matrix(colSums(bus[bus$state==state&bus$word==word,c(2,3)])))
  t2 = as.matrix(wordcount[flavour$word==word,c(2,3)])
  re = fisher.test(rbind(t1,t2))
  return (re$p.value)
}
fisherTestState('taste','NC')


sub.bus = bus[bus$review_num>50,] 
states = unique(bus$state)
wordlist = unique(wordcount$word)
busID = unique(sub.bus$business_id)

words = c()
for (word in wordlist){
  words = c(words,word)
}
busid = c()
for (id in busID){
  busid = c(busid,id)
}

wordCountPvalue = function(){
  result = c()
  col.names = c()
  for (id in busid){
    p.values = c()
    for (word in wordlist){
      p = chiTestBus(word, id)
      p.values = c(p.values, p)
    }
    col.names = c(col.names, id)
    result = cbind(result,p.values)
  }
  return (list(result, col.names))
}
re = wordCountPvalue()
pValueDf = data.frame(word = words,
                      pvalues = re[[1]])
names(pValueDf) <- c('Word',re[[2]])
write.csv(pValueDf, "wordBusPvalue.csv", row.names=FALSE)
wordp = read.csv('wordBusPvalue.csv')

suggestOrNot = function(){
  p.result = re[[1]]
  suggestion = c()
  for (id in busid){
    p.value.id = p.result[,which(busid==id)]
    sub.word = words[which(p.value.id < 0.05)]
    word = sub.word[1]
    use.word = c()
    for (word in sub.word){
      t1 = bus[bus$word==word&bus$business_id==id,c(2,3)]
      t2 = wordcount[wordcount$word==word,c(2,3)]
      if(t1$pos_num/t1$neg_num < t2$pos_num/t2$neg_num)
        use.word = c(use.word,word)
    }
    norm.suggest = rep(FALSE,33)
    norm.suggest[which(words %in% use.word)]=TRUE
    suggestion = cbind(suggestion,norm.suggest)
  }
  rownames(suggestion) = wordlist
  colnames(suggestion) = busid
  return (suggestion)
}
sug = suggestOrNot()
write.csv(sug, "suggestOrNot.csv")