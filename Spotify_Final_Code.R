# Set the working directory
setwd('C:\\Users\\saitu\\Downloads')
getwd()
install.packages("tidytext")
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("textstem")
install.packages("tm")
install.packages("stringr")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("lubridate")
install.packages("ggplot2")

library(readr)  # for read_csv
library(dplyr)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(textstem)  # For lemmatization
library(syuzhet)  # For sentiment analysis
library(ggplot2)
library(lubridate)  # For handling dates

# Read tweets data from the CSV file
sentdata <- read_csv("Spotify_tweets_2023.csv")

# Display column names to understand the structure
print(names(sentdata))

tweets[] <- lapply(tweets, function(x) replace(x, is.na(x), 0))

# Convert 'created_at' from string to POSIXct format using lubridate
sentdata$created_at <- mdy_hms(sentdata$created_at)  # Adjust the function based on your date format
attach(sentdata)
# Calculate sentiment using syuzhet and bing methods
sentdata$sent1 <- get_sentiment(sentdata$text, method = "syuzhet")
sentdata$sent2 <- get_sentiment(sentdata$text, method = "bing")

# Save the dataset with sentiment scores for consistency
write_csv(sentdata, "Final_Spotify_tweets_with_sentiments.csv")


# Read back the data to ensure use of updated data
sentdata <- read_csv("Final_Spotify_tweets_with_sentiments.csv")
print(names(sentdata))
# Plot sentiment over time
ggplot(sentdata, aes(x = created_at, y = sent1)) + 
  geom_line() + 
  labs(title = "Syuzhet Sentiment Over Time", x = "Time", y = "Sentiment Score", type = "histogram")

ggplot(sentdata, aes(x = created_at, y = sent2)) + 
  geom_line() + 
  labs(title = "Bing Sentiment Over Time", x = "Time", y = "Sentiment Score", type = "histogram")
# keyword "Taylor Swift"
music <- grepl("music", sentdata$text, ignore.case = FALSE)
music[1:10] # grepl() function returns to "TRUE"/"FALSE" values

sentdata <- cbind(sentdata,sent1,sent2, music)

# 3.1. create a wordcloud
library(tidytext) 
library(magrittr)
library(dplyr)
library(wordcloud)
library(tm)

# create a file at the word level
worddata <- sentdata %>%
  unnest_tokens(word, text)# output=word, input=text

# wordcloud
worddata %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, colors = brewer.pal(6,"Dark2")))


## save your wordcould
png("Spotify_wordcloud.png", width=12,height=8, units='in', res=300)
worddata %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100,colors=brewer.pal(6,"Dark2"))) # first 100 words with highest counts 
dev.off();




# output the data as an excel file
#install.packages("writexl")
library("writexl")
write_xlsx(worddata, "Spotify_wordsdata.xlsx") 

#3.2.   create a file at the word level with a cleaned dataset
# data preprocess
# 3.2.1 remove duplicated cases
attach(sentdata)
print(names(sentdata))
dup <- duplicated(status_id)
table(dup)

dupcase <- status_id[duplicated(status_id)]
newdata <- sentdata[!duplicated(sentdata$status_id),]# remove duplicated cases and create a new dataset


# 3.2.2. create a dataframe with only interested variables
data_tweet <- newdata[,c("status_id","text")] 
data_tweet <- data.frame(doc_id = status_id, text = text, stringsAsFactors = FALSE)


# Construct the corpus
# let R (tm package) know that we use this data as our corpus for text analysis
tweets_1 = Corpus(DataframeSource(data_tweet))
tweets_1[[1]]$content   #check the text of first document


# 3.2.3. noise removal -- for tweets
# (1) Retweet removal
removeRT <- function(x){gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", x)}#The gsub() function in R is used for replacement operations.
tweets_2 = tm_map(tweets_1,content_transformer(removeRT))
tweets_2[[1]]$content

# (2) Hashtag removal
removeHashtag <- function(x){gsub("#\\S+", "", x)}
tweets_3 = tm_map(tweets_2,content_transformer(removeHashtag))
tweets_3[[1]]$content

# (3) URL removal
removeURL <- function(x){gsub("http[^[:space:]]*", "", x)}
tweets_4 = tm_map(tweets_3,content_transformer(removeURL))
tweets_4[[1]]$content

# (4) HTML removal
unescapeHTML <- function(str) {return(gsub("<.*?>", "", str))}
tweets_5 = tm_map(tweets_4,content_transformer(unescapeHTML))
tweets_5[[1]]$content

# (5) Mention removal
removeMention <- function(x){gsub("@\\w+", "", x)}
tweets_6 = tm_map(tweets_5,content_transformer(removeMention))
tweets_6[[1]]$content

# (6) Carriage removal
removeCarriage <- function(x){gsub("[\r\n]", "", x)}
tweets_7 = tm_map(tweets_6,content_transformer(removeCarriage))
tweets_7[[1]]$content

#  (7) Emoticon removal
removeEmoticon <- function(x){gsub("[^\x01-\x7F]", "", x)}
tweets_8 = tm_map(tweets_7,content_transformer(removeEmoticon))
tweets_8[[1]]$content



# 3.2.4. general preprocessing procedures
# 4.1. lowercase
tweets_9 = tm_map(tweets_8,content_transformer(tolower))
tweets_9[[1]]$content

# 4.2. removePunctuation
tweets_10 <- tm_map(tweets_9, removePunctuation)
tweets_10[[1]]$content

# 4.3. removeNumbers
tweets_11 <- tm_map(tweets_10,removeNumbers)
tweets_11[[1]]$content

# 4.4. remove stopwords
tweets_12 <- tm_map(tweets_11,removeWords,stopwords("english"))
tweets_12[[1]]$content


# 4.6. stripWhitespace
tweets_14 <- tm_map(tweets_12,stripWhitespace)
tweets_14[[1]]$content

# 4.7. word stemming
tweets_15<-tm_map(tweets_14,stemDocument)
tweets_15[[1]]$content

# 4.8. word lemmatization
#install.packages('textstem')
library(textstem)
tweets_16 <- tm_map(tweets_15, lemmatize_strings)
tweets_16[[1]]$content

tweets17 <- data.frame(text = sapply(tweets_16, as.character), stringsAsFactors = FALSE)
tweets18 <- cbind(Index = rownames(tweets17),status_id, tweets17)
rownames(tweets18) <- 1:nrow(tweets18)

tweets18 <- cbind(tweets18, sent1, sent2, music)


# 3.2.5 create wordcloud with new dataset
worddata <- tweets18 %>%
  unnest_tokens(word, text,to_lower = T)# output=word, input=text, text: case sensitive

# wordcloud
worddata %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, colors = brewer.pal(6,"Dark2")))


#3.3. save your wordcloud
png("Spotify_all_wordcloud.png", width = 12, height = 8, units = 'in', res = 500)
worddata %>%
  anti_join(stop_words) %>%
  
  count(word) %>%
  with(wordcloud(word, n, max.words = 400, colors = brewer.pal(6,"Dark2")))
dev.off();




?wordcloud
?colors
