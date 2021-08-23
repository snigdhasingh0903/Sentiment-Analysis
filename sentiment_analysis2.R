library(tm)
library(wordcloud)
library(syuzhet)
library(ggplot2)
library(reshape2)
library(dplyr)
review_m<-read.csv(file.choose(),header = T)
str(review_m)
corpus_m<-iconv(review_m$title)
corpus_m<-Corpus(VectorSource(corpus_m))
inspect(corpus_m[1:5])
corpus_m<-tm_map(corpus_m,tolower)
corpus_m<-tm_map(corpus_m,removePunctuation)
corpus_m<-tm_map(corpus_m,removeNumbers)
corpus_m<-tm_map(corpus_m,removeWords,stopwords("english"))
inspect(corpus_m[2])
corpus_m<-tm_map(corpus_m,stripWhitespace)
review_final_m<-corpus_m
tdm_m<-TermDocumentMatrix(review_final_m)
tdm_m<-as.matrix(tdm_m)
tdm_m
w_m<-rowSums(tdm_m)
w_m<-subset(w_m, w_m>=2)
barplot(w_m,las=2,col="red")
corpus_m<-tm_map(corpus_m,removeWords,c("good","product"))
w_m<-sort(rowSums(tdm_m),decreasing = T)
set.seed(2000)
w_m
wordcloud(words=names(w),
          freq=w,
          max.words = 10,
          random.order = T,
          min.freq = 1,
          colors=brewer.pal(25,"Dark2"),
          scale=c(3,0.3))
sentiment_data_m<-iconv(review_m$title)
s_m<-get_nrc_sentiment(sentiment_data_m)
s_m[1:10]
s_m$score<-s_m$positive-s_m$negative
s_m[1:10]
s_m$score
write.csv(x=s,file="C:/R/Final_scores_m")
review_score_m<-colSums(s_m[,])
print(review_score_m)
barplot(colSums(s_m),
        las=2,
        col=rainbow(10),
        ylab='Count',
        main='Sentiment2')

