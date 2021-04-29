#Restaurant help staff#
#Yuyu Fan#
library(tidyverse)
library(dplyr)
library(quanteda)

##subsetting data--ihop##
load("C:\\Users\\18800\\Desktop\\21 Spring\\genTweets.RData")
a<-tweets.df%>%group_by(firm)%>%
  summarise(n=n())
ihop<-subset(tweets.df,firm=="ihop")

##tokenlize words##
doc.corpus <- corpus(ihop$text)
summary(doc.corpus)
doc.tokens <- tokens(doc.corpus)
#doc.tokens.sentence <- tokens(doc.corpus, what = "sentence")
#doc.tokens.character <- tokens(doc.corpus, what = "character")
#remove punctuation & words
doc.tokens <- tokens(doc.tokens, remove_punct = TRUE, 
                     remove_numbers = TRUE)
#remove stop words
doc.tokens <- tokens_select(doc.tokens, stopwords('english'),selection='remove')
doc.tokens <- tokens_wordstem(doc.tokens)
doc.tokens <- tokens_tolower(doc.tokens)
summary(doc.tokens)


##converting to DFM##
doc.dfm.final <- dfm(doc.tokens)

##analysis##
topfeatures(doc.dfm.final, 30)#their pancak are famous
waiter<-kwic(doc.tokens, "waiter", window = 100)
waitress<-kwic(doc.tokens, "waitress", window = 100)

##analysis##
waiter_tok<-c(waiter$pre,waiter$post)
toks_waiter <- tokens_compound(tokens(waiter_tok), data_dictionary_LSD2015)
waiter_review<-dfm_lookup(dfm(toks_waiter), data_dictionary_LSD2015)
topfeatures(waiter_review) #pos-690,neg-523#
##analysis##
waitress_tok<-c(waitress$pre,waitress$post)
toks_waiteress <- tokens_compound(tokens(waitress_tok), data_dictionary_LSD2015)
waitress_review<-dfm_lookup(dfm(toks_waiteress), data_dictionary_LSD2015)
topfeatures(waitress_review) #pos-1352,neg-942#

