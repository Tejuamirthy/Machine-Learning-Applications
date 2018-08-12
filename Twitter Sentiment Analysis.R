#name:   Amirthy Tejeshwar
#email:  tejuamirthi@gmail.com
#paymentid:  MOJO8310005A9756217 

#Importing the tweets file
tweets<-read.csv("C:/Users/Teju/Desktop/ML/Major/Qs/tweets.csv",stringsAsFactors = FALSE)

tweets$label<-as.factor(tweets$label)
View(tweets)
T(tweets_corpus_cleandf)
dim(tweets_corpus_cleandf)
#importing the tm package for performing text mining 
library(tm)
#creating a corpus
tweets_corpus<-VCorpus(VectorSource(tweets$tweet))


print(tweets_corpus)
inspect(tweets_corpus[1:2])
as.character(tweets_corpus[[1]])
lapply(tweets_corpus[1:2],as.character)


#cleaning the text stored in the corpus
tweets_corpus_clean<-tm_map(tweets_corpus,removeNumbers)
as.character(tweets_corpus_clean[[1]])

tweets_corpus_clean <- tm_map(tweets_corpus_clean, content_transformer(tolower))
tweets_corpus_clean<-tm_map(tweets_corpus_clean,removeWords,stopwords())
tweets_corpus_clean<-tm_map(tweets_corpus_clean,removePunctuation)
removeDesichar<-function(x) gsub('[^\x20-\x7E]', '', x)
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
tweets_corpus_clean <- tm_map(tweets_corpus_clean, content_transformer(removeNumPunct), lazy = TRUE)
tweets_corpus_clean <- tm_map(tweets_corpus_clean, content_transformer(removeDesichar), lazy = TRUE)
#this is the library used for stemming the document
library(SnowballC)
tweets_corpus_clean<-tm_map(tweets_corpus_clean,stemDocument)
tweets_corpus_clean<-tm_map(tweets_corpus_clean,stripWhitespace)

tweets_corpus_clean

lapply(tweets_corpus[1:3],as.character)
lapply(tweets_corpus_clean[1:3],as.character)
tweets_corpus_clean[]

tweets_dtm<-DocumentTermMatrix(VCorpus(VectorSource(tweets_corpus_clean)))
as.character(tweets_dtm[[1]])


head(tweets_dtm[[1]])
#alternate solution:crate a document-term sparse matrix directly from the SMS corpus
tweets_dtm2<-DocumentTermMatrix(tweets_corpus,control = list(
  tolower=TRUE,
  removeNumbers=TRUE,
  stopwords=TRUE,
  removePunctuation=TRUE,
  stemming=TRUE
))
as.character(tweets_dtm2[[1]])
tweets_dtm2
#alternativesolution:using custom stop words function ensures identical result
tweets_dtm3<-DocumentTermMatrix(tweets_corpus,control = list(
  tolower=TRUE,
  removeNumbers=TRUE,
  stopwords=TRUE,
  removePunctuation=TRUE,
  stemming=TRUE
))
tweets_corpus_cleandf<-as.data.frame(matrix(tweets_dtm))
(tweets_corpus_cleandf)
set.seed(1)
library(caTools)
splitting1<-sample.split(tweets_dtm,SplitRatio = 0.7)


as.character(tweets_corpus_clean[[2]])

tweets_m<-as.matrix(tweets_dtm)

length(tweets_dtm)
tweets_dtm_train<-tweets_dtm[1:20998,]
tweets_dtm_test<-tweets_dtm[20999:31692,]
tweets_train_labels<-tweets[1:20998,]$label
tweets_test_labels<-tweets[20999:31692,]$label



tweets_dtm_freq_train<-removeSparseTerms(tweets_dtm_train,0.999)
tweets_dtm_freq_train


tweets_freq_words<-findFreqTerms(tweets_dtm_train,5)
str(tweets_freq_words)

tweets_dtm_freq_train<-tweets_dtm_train[,tweets_freq_words]
tweets_dtm_freq_test<-tweets_dtm_test[,tweets_freq_words]

#convert counts to a facotor
convert_counts<-function(x){
  x<-ifelse(x>0,"Yes","No")
}

#apply convert_counts() to columns of train/test data
tweets_train<-apply(tweets_dtm_freq_train,MARGIN=2,convert_counts)
tweets_test<-apply(tweets_dtm_freq_test,MARGIN=2,convert_counts)

##step 3:Training a model on the data
library(e1071)
tweets_classifier<-naiveBayes(tweets_train,tweets_train_labels)
length(tweets_train)
length(tweets_train_labels)
tweets_classifier
#naives bayes theorem works on conditions and probability


tweets_test_pred<-predict(tweets_classifier,tweets_test)

#showing and comparing the output predicted values with the test labels
CrossTable(tweets_test_pred,tweets_test_labels,prop.chisq = FALSE,prop.t=FALSE,prop.r=FALSE,dnn=c('predicted','actual'))


#improving modle performance

tweets_classifier2<-naiveBayes(tweets_train,tweets_train_labels,laplace=1)
tweets_test_pred2<-predict(tweets_classifier2,tweets_test)
CrossTable(tweets_test_pred2,tweets_test_labels,prop.chisq = FALSE,prop.t=FALSE,prop.r=FALSE,dnn=c('predicted','actual'))

           

