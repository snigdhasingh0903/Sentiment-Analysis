install.packages("tm")
install.packages("wordcloud")
install.packages("syuzhet")
install.packages("dplyr")
library(tm)
library(wordcloud)
library(syuzhet)
library(ggplot2)
library(reshape2)
library(dplyr)
reviews<-read.csv(file.choose(),header = T)
str(reviews)
corpus<-iconv(reviews$title)
corpus<-Corpus(VectorSource(corpus))
inspect(corpus[1:5])
corpus<-tm_map(corpus,tolower)
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,removeNumbers)
corpus<-tm_map(corpus,removeWords,stopwords("english"))
inspect(corpus[2])
corpus<-tm_map(corpus,stripWhitespace)
review_final<-corpus
tdm<-TermDocumentMatrix(review_final)
tdm<-as.matrix(tdm)
tdm[1:10,1:5]
tdm
w<-rowSums(tdm)
w<-subset(w, w>=2)
barplot(w,las=2,col="blue")
w
corpus<-tm_map(corpus,removeWords,c("good"))
w<-sort(rowSums(tdm),decreasing = T)
set.seed(2000)
wordcloud(words=names(w),
          freq=w,
          max.words = 10,
          random.order = T,
          min.freq = 1,
          colors=brewer.pal(25,"Dark2"),
          scale=c(3,0.3))
sentiment_data<-iconv(reviews$title)
s<-get_nrc_sentiment(sentiment_data)
s[1:10]
s$score<-s$positive-s$negative
s[1:10]

write.csv(x=s,file="C:/R/Final_scores")
review_score<-colSums(s[,])
print(review_score)
barplot(colSums(s),
        las=2,
        col=rainbow(10),
        ylab='Count',
        main='Sentiment')



