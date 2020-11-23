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
import sklearn
import re
import nltk.classify.util
import nltk.sentiment
from nltk.probability import FreqDist
from nltk.tokenize import sent_tokenize, word_tokenize, RegexpTokenizer
from nltk.classify import NaiveBayesClassifier
from nltk.corpus import stopwords, names
from nltk.stem import WordNetLemmatizer
from nltk.stem.porter import *

#change directory
os.chdir('/Users/yudimu/Desktop/628/Module3/Data')
os.getcwd()
#read in csv file
review = pd.read_csv('review.bar.csv')
review.head()

#start
review['Text length'] = review['text'].apply(len)
review['Text length']

#calculate correlation 
review_cor = review[['stars', 'cool', 'useful', 'funny', 'Text length']].corr()
#heat map
fig = plt.figure(figsize=(12,8))
axes = sns.heatmap(review_cor, cmap='coolwarm', linewidth=1, linecolor='white', annot=True)
axes.set_title('Heatmap of Variables', fontsize=30)

#preprocess review text
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

#count the word and plot freq
words_count = []
for i in range(0,9999): 
    for word in preprocess[i]:
        words_count.append(word)
FreqDist(words_count).plot(30, cumulative=False)

#import positive and negtive words
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

#calculate sentiment score    
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
        
dataframe = pd.DataFrame(preprocess, sentiment_score)
review.index = (range(0, len(review)))
dataframe.index = (range(0, len(dataframe)))
new = pd.concat([review, dataframe], axis=1) 
 
#add sentiment score to review data    
review['sentiment score'] = sentiment_score
round(sentiment_score, 3)
sentiment = [round(num, 4) for num in sentiment_score]

print(np.shape(review))
sum(i < 0 for i in sentiment_score) + sum(i == 0 for i in sentiment_score)
review.describe()

#write out in a new file
review_sentimentscore = review[['business_id', 'stars', 'sentiment score', 'text', 'words']]   
review_sentimentscore.to_csv (r'~/Desktop/628/Module3/Data/review_word.csv', index = False, header=True)


#preprare for information gain
porter_stemmer = PorterStemmer()
wordnet_lemmatizer = WordNetLemmatizer()

def tokenization_and_stemming_all(text):
    attitude = 0  # Initialize reviewer's attitude
    attitude += len(re.findall(r'[A-Z]{4,}', text))
    attitude += len(re.findall(r'[?!]+', text)) # !? exists
    attitude += len(re.findall(r'[?!]{2,}', text)) # !? occurance >= 2
    
    text = re.compile("(?<=[a-zA-Z])'s").sub(" is", text)
    text = re.compile("wo(n't|n)").sub("shall not",text)
    text = re.compile("(?<=[a-zA-Z])(n't|n')").sub(" not", text)
    text = re.compile("(?<=[I|i])'m").sub(" am", text)
    text = re.compile("(?<=[a-zA-Z])'re").sub("are", text)
    text = re.compile("(?<=[a-zA-Z])'ll").sub(" will", text)
    text = re.sub("[nN]'[tT]", " not", text)
    text = re.sub("[nN]'", " not", text)
    text = re.sub(r'[^A-Za-z0-9_\ ]{2,}', " . ", text) 
    text = re.sub(',', " . ", text)
    text = re.sub(r'[^A-Za-z_\ .!?;:]', " ", text)
    text = re.sub(r'[!?;:]', " ", text)
    
    from nltk.sentiment.util import mark_negation
    stopword = set(stopwords.words('english'))              
    tokens = [word.lower() for sent in nltk.sent_tokenize(text) for word in nltk.word_tokenize(sent) if word.lower() not in stopword]

    ### FEATURE: good / bad words ###
    good_num = sum(word in posdict for word in tokens)
    bad_num = sum(word in negdict for word in tokens)

   
    stems = nltk.pos_tag(tokens)
    stems = [token for token in stems if token[1] == 'NN']
    stems = [token[0] for token in stems]

    return (attitude, good_num, bad_num, stems)


def token_wrapper(text):  # Wrap up the tokenizer
    attitude, good_num, bad_num, stems =  tokenization_and_stemming_all(text)
    return stems

#Countvectorize
df = pd.DataFrame(review)
vect = sklearn.feature_extraction.text.CountVectorizer(tokenizer=token_wrapper, max_features = 4000)
x, y = vect.fit_transform(df['text']), df['stars']
name = vect.get_feature_names() 

#Information gain
def information_gain(X, y):
    def _calIg():
        probs = 0
        entropy_x_set = 0
        entropy_x_not_set = 0
        for c in classCnt:
            probs = classCnt[c] / float(featureTot)
            if probs <= 0 :
                pass
            else:
             entropy_x_set = entropy_x_set - probs * np.log(probs)
             probs = (classTotCnt[c] - classCnt[c]) / float(tot - featureTot)
             if probs <= 0 :
              pass
             else:
              entropy_x_not_set = entropy_x_not_set - probs * np.log(probs)
           
        for c in classTotCnt:
            if c not in classCnt:
                probs = classTotCnt[c] / float(tot - featureTot)
                if probs <= 0 :
                    pass
                else :
                  entropy_x_not_set = entropy_x_not_set - probs * np.log(probs)
               
        return entropy_before - ((featureTot / float(tot)) * entropy_x_set
                                 + ((tot - featureTot) / float(tot)) * entropy_x_not_set)

    tot = X.shape[0]
    classTotCnt = {}
    entropy_before = 0
    for i in y:
        if i not in classTotCnt:
            classTotCnt[i] = 1
        else:
            classTotCnt[i] = classTotCnt[i] + 1
    for c in classTotCnt:
        probs = classTotCnt[c] / tot
        entropy_before = entropy_before - probs * np.log(probs)

    nz = X.T.nonzero()
    pre = 0
    classCnt = {}
    featureTot = 0
    information_gain = []
    for i in range(0, len(nz[0])):
        if (i != 0 and nz[0][i] != pre):
            for notappear in range(pre + 1, nz[0][i]):
                information_gain.append(0)
            ig = _calIg()       
            information_gain.append(ig)
            pre = nz[0][i]
            classCnt = {}
            featureTot = 0
        featureTot = featureTot + 1
        yclass = y[nz[1][i]]
        if yclass not in classCnt:
            classCnt[yclass] = 1
        else:
            classCnt[yclass] = classCnt[yclass] + 1
    ig = _calIg()
    information_gain.append(ig)

    return np.asarray(information_gain)

#revise some mistakes
name1 = str(name)
name1 = name1.replace('manag', 'manage')
name1 = name1.replace('excelentl', 'excelent')
name1 = name1.replace('friendli', 'friendly')
name =  name1.split("', '")
informgain = information_gain(x, y)
combine_informgain = [(name[i], informgain[i]) for i in range(len(informgain))]
combine_informgain.sort(key=lambda x: x[1], reverse=True)

#splite into train and test data sets

from sklearn.model_selection import train_test_split
X_train,X_test,Y_train,Y_test=train_test_split(review['text'].values, review['stars'].values,test_size=0.3,random_state=1)
print(X_train.shape,Y_train.shape)
print(X_test.shape,Y_test.shape)


#model decision tree
from sklearn.metrics import classification_report
from sklearn.metrics import confusion_matrix
from sklearn.tree import DecisionTreeClassifier
depth= 100
split= 50
model=DecisionTreeClassifier(max_depth=depth, min_samples_split =split, class_weight='balanced')
model.fit(Bow_train, Y_train)
weight = model.feature_importances_
all_features=vect.get_feature_names()

#plot decision tree
from sklearn import tree
import pydotplus
from IPython.display import Image
from IPython.display import SVG
import graphviz
from graphviz import Source
from IPython.display import display

target = ['negative','positive']
# Create DOT data
data = tree.export_graphviz(model, out_file=None, feature_names = all_features, max_depth=3, filled=True,rounded=True,special_characters=True)
# Draw graph
graph = pydotplus.graph_from_dot_data(data)
# Show graph
Image(graph.create_png())
graph.write_png("decision_tree.png")
