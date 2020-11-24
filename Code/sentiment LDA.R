library(dplyr)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(textmineR)
library(tm) 
library(lda)
detach("package:plyr", unload=TRUE)
#read in review text (already separated into words and had sentiment score)
review.word = read.csv("~/Desktop/628/Module3/Data/review sentiment.csv")

#Find reviews with negtive scores 
neg = review.word %>% filter(sentiment.score.new < 0)
neg$id = 1:nrow(neg)
data = neg[, c(4,5)]

#standardize sentences
data$text = sub("RT.*:", "", data$sentence)
data$text = sub("@.* ", "", data$text)
text_cleaning_tokens = data %>% unnest_tokens(word, text)
text_cleaning_tokens$word = gsub('[[:digit:]]+', '', text_cleaning_tokens$word)
text_cleaning_tokens$word = gsub('[[:punct:]]+', '', text_cleaning_tokens$word)
text_cleaning_tokens = text_cleaning_tokens %>% filter(!(nchar(word) == 1))%>% 
  anti_join(stop_words)
tokens = text_cleaning_tokens %>% filter(!(word==""))
tokens = tokens %>% mutate(ind = row_number())
tokens = tokens %>% group_by(id) %>% mutate(ind = row_number()) %>%
  tidyr::spread(key = ind, value = word)
tokens [is.na(tokens)] = ""
tokens = tidyr::unite(tokens, text,-id,sep =" " )
tokens$text = trimws(tokens$text)

#create dtm 
dtm1 = CreateDtm(doc_vec = tokens$text,
                 doc_names = tokens$id, 
                 ngram_window = c(1, 1))

#fit LDA model with 3 topics
lda1 <- LDA(dtm1, k = 2, control = list(seed = 1234))

topics.neg <- tidy(lda1, matrix = "beta")
top_terms <- topics.neg %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#plot first 8 words
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()





#Find reviews with negtive scores 
pos = review.word %>% filter(sentiment.score.new > 0)
pos$id = 1:nrow(pos)
data = pos[, c(4,5)]
data$text = sub("RT.*:", "", data$sentence)
data$text = sub("@.* ", "", data$text)

#standardize sentences
text_cleaning_tokens <- data %>% 
  tidytext::unnest_tokens(word, text)
text_cleaning_tokens$word = gsub('[[:digit:]]+', '', text_cleaning_tokens$word)
text_cleaning_tokens$word = gsub('[[:punct:]]+', '', text_cleaning_tokens$word)
text_cleaning_tokens = text_cleaning_tokens %>% filter(!(nchar(word) == 1))%>% 
  anti_join(stop_words)
tokens = text_cleaning_tokens %>% filter(!(word==""))
tokens = tokens %>% mutate(ind = row_number())
tokens = tokens %>% group_by(id) %>% mutate(ind = row_number()) %>%
  tidyr::spread(key = ind, value = word)
tokens [is.na(tokens)] = ""
tokens = tidyr::unite(tokens, text,-id,sep =" " )
tokens$text = trimws(tokens$text)
tokens$text = sub("ive", "", tokens$text)
tokens$text = sub("te", "", tokens$text)
tokens %>% filter(text == "te")

#create dtm 
dtm2 = CreateDtm(doc_vec = tokens$text,
                doc_names = tokens$id, 
                ngram_window = c(1, 1))

#fit LDA model with 3 topics
lda2 <- LDA(dtm2, k = 2, control = list(seed = 1234))
lda2

topics.pos <- tidy(lda2, matrix = "beta")
top_terms <- topics.pos %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#plot first 8 words
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
