#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov 12 14:20:08 2020

@author: yudimu
"""

import pandas as pd
import os 
import string
import nltk
import matplotlib.pyplot as plt
import seaborn as sns
import spacy
import numpy as np
import csv
import nltk.classify.util
from nltk.probability import FreqDist
from nltk.tokenize import sent_tokenize, word_tokenize, RegexpTokenizer
from nltk.classify import NaiveBayesClassifier
from nltk.corpus import stopwords, names


os.chdir('/Users/yudimu/Desktop/628/Module3/Data')
os.getcwd()

review = pd.read_csv('review.bar.csv')
review.head()


#start
review['Text length'] = review['text'].apply(len)
review['Text length']

review_cor = review[['stars', 'cool', 'useful', 'funny', 'Text length']].corr()

fig = plt.figure(figsize=(12,8))
axes = sns.heatmap(review_cor, cmap='coolwarm', linewidth=1, linecolor='white', annot=True)
axes.set_title('Heatmap of Variables', fontsize=30)



en_nlp = spacy.load('en')

def preprocess(reviews):
    stemword = []
    temp = []
    tokenizer = RegexpTokenizer(r'\w+')
    stop_words = set(stopwords.words('english'))

    for review in reviews:
        words = tokenizer.tokenize(review)
        for w in words:
            if w.lower() not in stop_words:
                temp.append(w.lower())
        stemword.append(temp)
        temp = []

    return stemword

preprocess = preprocess(review['text'])
preprocess[0]
review['words'] = preprocess

words_count = []
for i in range(0,9999): 
    for word in preprocess[i]:
        words_count.append(word)
FreqDist(words_count).plot(30, cumulative=False)


posword = open("positive-words.txt", "r", encoding='ISO-8859-1')
posdict = posword.read().split('\n')
posword.close() 
negword = open("negative-words.txt", "r", encoding='ISO-8859-1')
negdict = negword.read().split('\n')
negword.close()
poscount = 0
negcount = 0
sentiment_ratio = 0
sentiment_score = []
    
for entry in preprocess:
        for word in entry:
            if word in posdict:
                poscount = poscount + 1
            if word in negdict:
                negcount = negcount + 1
            if len(entry) != 0:
                sentiment_ratio = float(poscount - negcount) / float(len(entry))  
            else:
                sentiment_ratio = 0
        sentiment_score.append(sentiment_ratio)
        poscount = 0
        negcount = 0
review['sentiment score'] = sentiment_score
round(sentiment_score, 3)
sentiment = [round(num, 4) for num in sentiment_score]
sentiment
review.describe()

review
with open('review_word.csv', 'w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(review)

review_sentimentscore = review[['business_id', 'stars', 'sentiment score']]   
review_sentimentscore.to_csv (r'~/Desktop/628/Module3/Data/review_word.csv', index = False, header=True)
