setwd("F:/rfiles/delta_air_group4")
options(stringsAsFactors = F)

library(readxl)
library(tm)
library(textstem)
library(hunspell)
library(stringr)

library(qdap)
library(wordcloud)
library(ggplot2)
library(ggthemes)
library(utils)

delta=read_excel("delta_air.xlsx")
delta=c(unlist(delta))
corp <- Corpus(VectorSource(delta))

tdm <- TermDocumentMatrix(corp)
inspect(tdm)
custom_stopwords <- c(stopwords('english'), 'hello','thats','can','let','youre','amp','get','will','look','followdm','delta','thx','ill','also','theyll')

clean.corpus<- function (corp) {
  corp <- tm_map(corp,content_transformer(tolower))
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, removeNumbers)
  corp <- tm_map(corp, stripWhitespace)
  corp <- tm_map(corp, removeWords,custom_stopwords)
  corp <- tm_map(corp, lemmatize_strings)
  return(corp)
}

corp=clean.corpus(corp)
tdm <- TermDocumentMatrix(corp)
inspect(tdm)

#wordcloud
library(wordcloud)
library("RColorBrewer")

m = as.matrix(tdm) # converts tdm which is list into matrix, typeof function to view datastructure
v = sort(rowSums(m),decreasing = TRUE) # in order to sort based on frequency, we retrive the frequencies first using- rowsums function 
d = data.frame(word = names(v),freq =v) # converting v into a dataframe with headings word and freq

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, scale=c(2, 0.35), 
          colors=brewer.pal(8, "Dark2"))

#visualization
library(ggplot2)
library(ggthemes)

tdm.m<-as.matrix(tdm)
term.freq<-rowSums(tdm.m)
freq.df<-data.frame(word=names(term.freq),frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]

#Plot Bar chart for frequencies
freq.df$word<-factor(freq.df$word,levels=unique(as.character(freq.df$word)))
ggplot(freq.df[1:20,], aes(x=word,y=frequency))+
  geom_bar(stat="identity", fill='darkred')+
  coord_flip()+
  theme_gdocs()+
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)

find.association<- function (word) {
  associations<-findAssocs(tdm, word, 0)
  associations<-as.data.frame(associations)
  associations$terms<-row.names(associations)
  associations$terms<-factor(associations$terms,levels=associations$terms)
  View(associations)
}

#Find words association
find.association('please')
find.association('sorry')
find.association('thank')
find.association('team')
find.association('assistance')



delta<-read.csv('delta_air.csv')
delta.df <- data.frame(reviews=delta$comment)
View(delta.df)

#obtain list containing polarity
pol<-polarity(delta.df$reviews)
#add this to air.df dataframe as a column
delta.df$polarity<- pol$all$polarity
View(delta.df)
# Get a histogram to show distribution of polarity scores
ggplot(pol$all, aes(x=polarity, 
                    y=..density..)) + theme_gdocs() + 
  geom_histogram(binwidth=.25,
                 fill="darkred",colour="grey60", size=.2) +
  geom_density(size=.75)

#Create word cloud to compare postive and negative comments
# create subsets for postive and negative polarity documents
pos.comments<-subset(delta.df$reviews,delta.df$polarity>0)
neg.comments<-subset(delta.df$reviews,delta.df$polarity<0)

# Include only terms
pos.terms<-paste(pos.comments,collapse = " ")
neg.terms<-paste(neg.comments,collapse = " ")
all.terms<-c(pos.terms,neg.terms)
all.corpus<-VCorpus(VectorSource(all.terms))

#Build an tfidf document 
all.tdm<-TermDocumentMatrix(all.corpus, control=list(weighting=weightTfIdf, removePunctuation =TRUE,stopwords=stopwords(kind='en')))

all.tdm.m<-as.matrix(all.tdm)
colnames(all.tdm.m)<-c('positive','negative')

set.seed(1234)
#Build a comparison cloud using TFIDF
comparison.cloud(all.tdm.m, max.words=100, colors=c('darkgreen','darkred'))

neut.comments<-subset(delta.df$reviews,delta.df$polarity==0)

no.pcom = length(pos.comments)
no.ncom = length(neg.comments)
no.ntcom = length(neut.comments)

all.comments <-data.frame(ncoms=c(no.pcom,no.ncom,no.ntcom))
all.comments$sentimentgrp<- c('positive','negative','neutral')
View(all.comments)
