# load libraries
library(tm)
library(tidyverse)
library(dplyr)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
require(maps)
library(gridExtra)

# load data
mydata <- read.csv("/Users/varun/trunk/kumud/COURSES/Intermediate Analytics/final project/twitter-airline-sentiment/Tweets.csv")
dim(mydata)
names(mydata)
head(mydata)
tail(mydata)
summary(mydata)

#####################
#### EDA ############
#####################

# Distribution of tweets in dataset
TotalTweets = mydata %>% group_by(airline_sentiment) %>% dplyr::summarise(count = n())
write.csv(TotalTweets, "Table1.1.csv")

## Research Question 1 - What is the proportion of sentiments in all airlines?

# proportion of tweets by airlines
prop.table(table(mydata$airline_sentiment,mydata$airline))

#saving the proprtion as data frame
airline_tweet <- data.frame(prop.table(table(mydata$airline_sentiment,mydata$airline)))
colnames(airline_tweet) = c('Sentiment', 'Airline', 'Percentage_Tweets')


##plotting of the sentiment of the airlines
P1 <-ggplot(airline_tweet, aes(x=Airline,y=Percentage_Tweets, fill=Sentiment)) + geom_col() +
  ggtitle("Airline wise proportion of tweets ") 

#plotting of the sentiment of the airlines on scaling
P2 <-ggplot(airline_tweet, aes(x=Airline,y=Percentage_Tweets, fill=Sentiment)) +
  geom_col(position = "fill") + ggtitle("Airline wise scaled proportion of tweets") 

grid.arrange(P1,P2)


### Question 2 - Is there any particular date on which the tweets are maximum?

#separating the data from the tweet_created column
mydata$date <-  as.Date(mydata$tweet_created)

#total tweets for every airline
TotalTweetsByDate_Airline <- mydata %>% group_by(airline,date) %>%dplyr::summarise(count = n())
TotalTweetsByDate_AirlinePlot = ggplot() + geom_line(data=TotalTweetsByDate_Airline, aes(x=date, y=count , color=airline)) + ylim(0,1200)+
  ggtitle("Datewise total number of tweets") 
TotalTweetsByDate_AirlinePlot


#Negative tweets for every airline 
NegativeTweetsByDate_Airline <- mydata %>% group_by(airline,date) %>% filter(airline_sentiment =="negative") %>%dplyr::summarise(count = n())
NegativeTweetsByDate_AirlinePlot = ggplot() + 
  geom_line(data=NegativeTweetsByDate_Airline, aes(x=date, y=count , color=airline)) + ylim(0,1200)+
  ggtitle("Datewise number of Negative tweets") 
NegativeTweetsByDate_AirlinePlot

#Positive tweets for every airline
PositiveTweetsByDate_Airline <- mydata %>% group_by(airline,date) %>% filter(airline_sentiment =="positive") %>%dplyr::summarise(count = n())
PositiveTweetsByDate_AirlinePlot = ggplot() + 
  geom_line(data=PositiveTweetsByDate_Airline, aes(x=date, y=count, color=airline)) +ylim(0,1200)+
  ggtitle("Datewise number of Positive tweets") 
PositiveTweetsByDate_AirlinePlot

#Neutral tweets for every airline
NeutralTweetsByDate_Airline <- mydata %>% group_by(airline,date) %>% filter(airline_sentiment =="neutral") %>%dplyr::summarise(count = n())
NeutralTweetsByDate_AirlinePlot = ggplot() + 
  geom_line(data=NeutralTweetsByDate_Airline, aes(x=date, y=count, color=airline)) +ylim(0,1200)+
  ggtitle("Datewise number of Neutral tweets") 
NeutralTweetsByDate_AirlinePlot

grid.arrange(TotalTweetsByDate_AirlinePlot,NegativeTweetsByDate_AirlinePlot,PositiveTweetsByDate_AirlinePlot,NeutralTweetsByDate_AirlinePlot)

########### Qusetion 3 - What are the reasons for negative tweets? Which airline is worst for one particular 
# negative reasons
Negative_reasons_df = data.frame(prop.table(table(mydata$airline,mydata$negativereason)))
colnames(Negative_reasons_df) = c('Airline','Reason', 'Frequency')
# dropping tweets of all airlines with no reasons 
Negative_reasons_df=Negative_reasons_df[-1:-6,] 

#plotting of negative reasons
ggplot(Negative_reasons_df, aes(x=Reason,y=Frequency, fill=Airline)) + 
  geom_col(position = "dodge")+ coord_flip() + ggtitle("Reasons of Negative Tweets") 


######## Question 4 - Analyzing tweet length to know whether people use more words to express negative sentiments?
mydata$tweet_length <- sapply(as.character(mydata$text),function(x) nchar(x))

ggplot(mydata, aes(x=tweet_length, fill=airline_sentiment)) + geom_density(alpha=0.2)

# Do the negative tweet density is more for one specific airline?
#plotting the tweet length according to the different airlines
ggplot(mydata, aes(x=tweet_length, fill=airline_sentiment)) + 
  geom_density(alpha=0.5) + facet_wrap(~airline, scale = 'free') +xlim(0,180)+ylim(0,0.030)+
  ggtitle("Airline wise length of tweets") 


# Question 5- What are the tweet locations? 
location <- mydata$tweet_coord
location <- location[complete.cases(location)]

location <- data.frame(location)
location$count=1
location$location=as.character(location$location)
location=aggregate(count~location,data=location,FUN = sum)
#removing [0,0] location
location=location[-6,]
location=location[-1,]
#separating the coordinates
coordinate = strsplit(location$location, ',') 

lat= NULL
long=NULL
for (i in 1:length(coordinate)) {
  lat = c(lat, substring(coordinate[[i]][1], 2)) # removes first character which is [
  long = c(long, coordinate[[i]][2]) 
}

location$lat = lat
location$long = long
location$long = substr(location$long, 1, nchar(location$long)-1)
#changing the mode of lat and long from character to numeric
location$lat = as.numeric(location$lat)
location$long = as.numeric(location$long)

#Mapping of tweet location in the US
USstates_map <- map_data("state")
ggplot() + geom_polygon(data=USstates_map, aes(x=long, y=lat, group = group), colour="black", fill = 'lightblue') + 
  ggtitle("Location of tweets across the States") + geom_point(data=location, aes(x=long, y=lat, size = count), color="coral1") + 
  scale_size(name="Total Tweets") + xlim(-125, -65) + ylim(25, 50)

##########################
## NLP/Data Wrangling ####
##########################

clean_text <- function(x){ 
  gsub('[[:punct:]]', " ", x) 
}
clean_text2 <- function(x){
  gsub("[^\x01-\x7F]|", "", x)
}
clean_text3 <- function(x){
  gsub('http\\S+\\s*', '', x)
}

# we are interested in text of tweets to find the high frequency words and 
# words associated to one particular category.

# creating corpus of tweets
corpus1 <- Corpus(VectorSource(mydata$text))

# function for pre-processing of text in the corpus
pre_process_corpus <- function(corpus){
  # Convert the text to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Remove english common stopwords
  corpus <- tm_map(corpus, removeWords, c(stopwords("english"),"usairways","united","americanair","southwestair","flight","jetblue","virginamerica"))
  # Remove punctuations
  corpus <- tm_map(corpus, removePunctuation)
  # Remove special characters
  corpus <- tm_map(corpus, clean_text)
  #remove emtoicons
  corpus <- tm_map(corpus, clean_text2)
  #remove urls
  corpus <-tm_map(corpus,clean_text3)
  # Remove numbers
  corpus <- tm_map(corpus, removeNumbers)
  # Eliminate extra white spaces
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

### Bag of words - Document term matrix

dtm <- DocumentTermMatrix(pre_process_corpus(corpus1))

wf_df <- data.frame(as.matrix(dtm))
wf <- sort(colSums(wf_df), decreasing = TRUE)
df <- data.frame(word = names(wf),freq=wf)


# plot top 30 words in the tweets
total_term <- ggplot(head(df,20), aes(reorder(word,freq), freq)) +
  geom_bar(stat = "identity",fill = "blue") + coord_flip() +
  xlab("Terms") + ylab("Term Frequency") +  
  ggtitle("Top 20 words in tweets") 
total_term 
set.seed(1234)

#making the wordcloud of the word
wc_total <- wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8,"Dark2"))

# Negative words 

NegativeTweets <- mydata %>% filter(airline_sentiment == "negative") %>% select(text)

corpusNeg <- Corpus(VectorSource(NegativeTweets$text))
dtm_neg <- DocumentTermMatrix(pre_process_corpus(corpusNeg))

wf_df_neg <- data.frame(as.matrix(dtm_neg))
wf_neg <- sort(colSums(wf_df_neg), decreasing = TRUE)
df_neg <- data.frame(word = names(wf_neg),freq=wf_neg)


# bar plot of top 20 negative words 
neg_terms <- ggplot(head(df_neg,20), aes(reorder(word,freq), freq)) +
  geom_bar(stat = "identity",fill = "red") + coord_flip() + 
  xlab("Terms") + ylab("Term Frequency") +  
  ggtitle("Top 20 words in negative tweets") 

# word cloud of 200 negative words
wc_neg <- wordcloud(words = df_neg$word, freq = df_neg$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8,"Dark2"))


### positive tweets analysis

PositiveTweets <- mydata %>% filter(airline_sentiment == "positive") %>% select(text)
corpusPos <- Corpus(VectorSource(PositiveTweets$text))
dtm_pos <- DocumentTermMatrix(pre_process_corpus(corpusPos))

wf_df_pos <- data.frame(as.matrix(dtm_pos))
wf_pos <- sort(colSums(wf_df_pos), decreasing = TRUE)
df_pos <- data.frame(word = names(wf_pos),freq=wf_pos)

pos_terms <- ggplot(head(df_pos,20), aes(reorder(word,freq), freq)) +
  geom_bar(stat = "identity",fill = "green") + coord_flip() + 
  xlab("Terms") + ylab("Term Frequency") + 
  ggtitle("Top 20 words in positive tweets") 

# top 20 words associated with positive and negative tweets
grid.arrange(pos_terms,neg_terms)

# word cloud of negative words
wc_pos <- wordcloud(words = df_pos$word, freq = df_pos$freq, min.freq = 1,
                    max.words = 200, random.order = FALSE, rot.per = 0.35,
                    colors = brewer.pal(8,"Dark2"))


#################################################
## Predicting Sentiments From Text features #####
#################################################

## dropping terms which appear least in the documents 
dim(dtm)
inspect(dtm[100:110,1:10])
sparse <- removeSparseTerms(dtm, 0.997) # 494 terms
sparse

feature_matrix <- as.matrix(sparse)
n_obs <- nrow(mydata)
set.seed(123)
prop_split <- .80
training_index <- sample(1:n_obs, round(n_obs * prop_split))
y_true <- as.matrix(mydata$airline_sentiment)
x_train <- feature_matrix[training_index, ]
x_test <- feature_matrix[-training_index, ]

########## Naive Bayes ########

library(e1071)
nb <- naiveBayes(x=x_train , y=as.factor(y_true[training_index]))
y_hat_naive <- predict(nb, x_test)
accuracy_nb <- sum(y_true[-training_index] == y_hat_naive)/ length(y_hat_naive)
accuracy_nb # 46%

nb_conf_mat <- table(y_hat_naive, true = y_true[-training_index])
nb_conf_mat
write.csv(nb_conf_mat,"table 2.1.csv")


## Multinomial Logistic Regression classifier ###
library(glmnet)
glm_fit <- glmnet(x_train , y_true[training_index], lambda = 0, family = "multinomial")
y_hat <- predict(glm_fit, x_test, type = "class")
accuracy_glm <- sum(y_true[-training_index] == y_hat)/ length(y_hat)
accuracy_glm # 75% 

conf_mat_lr <- table(y_hat, true = y_true[-training_index])
write.csv(conf_mat_lr,"table2.2.csv")

### Random forest #######
library("randomForest")

rf <- randomForest(x=x_train, y=as.factor(y_true[training_index]), ntree= 50)
y_hat_rf <- predict(rf, newdata=x_test)
accuracy_rf <- sum(y_true[-training_index] == y_hat_rf)/ length(y_hat_rf)
accuracy_rf
conf_mat_rf <- table(y_hat_rf, true = y_true[-training_index])
conf_mat_rf
write.csv(conf_mat_rf,"table2.3.csv")


