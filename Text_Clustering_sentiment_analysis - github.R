#Twitter is a great source for sentiment data and social media mining 

#furthermore it is quite easy to get significant amounts of data to be able to scrape
#data from Twitter you need a standard Twitter account and you need to update it 
#to a developer account
#-note that Twitter limits the amount of searches you can perform (15min: 15 scrapes)

#-package twitteR

setwd("C:/Users/Abheek/study materials/Abheek_Assignment/R Final Modules/Text Mining and sentiment analysis")

install.packages("twitteR")
install.packages("tm")
library("twitteR")
library("ROAuth")
require(RCurl)
library("httr")
library("base64enc")
library(devtools)
library(curl)

#-all this info is obtained for the Twitter developer account:
key = "xxxxxxxxxxxxxxxxxxxxx"
secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

#Access Token Secret from Twitter
secrettk <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
#Access Token from Twitter
mytoken <- "xxxxxxxxxxxxxxxxxxxxx"

#set a working directory for the whole process - 
#you need to download a few files and R needs to know where to look for that stuff

setwd("C:/Users/Abheek/study materials/Abheek_Assignment/Text Mining and sentiment analysis")


# we are using the setup_twitter_oauth function
?setup_twitter_oauth

# keep this order of arguments

devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0",force = TRUE)

setup_twitter_oauth(key, secret, mytoken, secrettk)


?searchTwitter

tweets_download = searchTwitter("#election2016", n=1000)

class(tweets_download)
length(tweets_download)
head(tweets_download)

library("tm")

tweets_downloadlist <- sapply(tweets_download, function(x) x$getText()) # initiating a function
tweets_downloadcorpus <- Corpus(VectorSource(tweets_downloadlist)) # use the corpus function

#a corpus is the text body consisting of all the text including the meta info

inspect(tweets_downloadcorpus)

tweets_downloadcorpus <- tm_map(tweets_downloadcorpus, PlainTextDocument)
tweets_downloadcorpus <- tm_map(tweets_downloadcorpus, tolower) # putting text to lower case
tweets_downloadcorpus <- tm_map(tweets_downloadcorpus, removePunctuation) # remove punct.
tweets_downloadcorpus <- tm_map(tweets_downloadcorpus,
                      function(x)removeWords(x,stopwords())) # remove stopwords (meaningless words)
tweets_downloadcorpus <- tm_map(tweets_downloadcorpus, removeNumbers) 
tweets_downloadcorpus <- tm_map(tweets_downloadcorpus, stripWhitespace) 
tweets_downloadcorpus <- tm_map(tweets_downloadcorpus, PlainTextDocument)



library("wordcloud")

?wordcloud

wordcloud(tweets_downloadcorpus, min.freq=10, scale=c(4,2),random.color=F, max.word=100, random.order=F)

#A DocumentTermMatrix is a very useful tool when it comes to text mining. 
#It structures the text in a matrix where each term is organized in a column. 
#Each row is a document and the number represents the counts of that term.
tweets_downloadcorpus_dm<-DocumentTermMatrix(tweets_downloadcorpus)
findFreqTerms(tweets_downloadcorpus_dm, lowfreq=11,20)

?findAssocs

# association with words with a minimum correlation limit
findAssocs(tweets_downloadcorpus_dm, 'trump', 0.45)


## Dendograms

#-remove sparse (infrequently used) terms from the term-document matrix

tweets_downloadcorpus_dm2 <-removeSparseTerms(tweets_downloadcorpus_dm, sparse=0.9)

#et's scale the data
tweets_downloadcorpus_dm2scale <- scale(tweets_downloadcorpus_dm2)


#distance matrix
#let's scale the data
tweets_downloadist <- dist(tweets_downloadcorpus_dm2scale, method = "euclidean")

#-hierarchical clustering
tweets_downloadistfit <- hclust(tweets_downloadist)

#-Visualize the result
plot(tweets_downloadistfit)


#-to calculate a certain number of groups
cutree(tweets_downloadistfit, k=6)

rect.hclust(tweets_downloadistfit, k=6, border="red")



#### SENTIMENT ANALYSIS

# Sentiment analysis is used to see if a text is neutral, positive or negative
# emotion analysis is used to see which emotion a text has (happy, fear,
# anger) both are using similar codes but the comparison lexicon is
# different.
# Example: What is the sentiment towards my company? or towards a campaign

# Sentiment Lexicon: a list of words which you are using to compare your scraped txt with
# Hu Liu Lexicon got the standard of sentiment analysis lately list of pos and negative words - manually created - approx. 6800
# -download the txt files to your wd
# -import positive and negative words

pos = readLines("Positive-Words.txt")
neg = readLines("Negative-Words.txt")

#Let's run a test to see how this works!
  mytest= c("great you re here", "awesome experience",
            "You had a bad night", "She loves ugly candy")
  

  library("stringr")
  library("plyr")

  score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
  {
    # Parameters
    # -sentences: vector of text to score
    # -pos.words: vector of words of postive sentiment
    # -neg.words: vector of words of negative sentiment
    # -.progress: passed to laply() to control of progress bar
    # -create simple array of scores with laply
    scores = laply(sentences,
                   function(sentence, pos.words, neg.words)
                   {
                     # -remove punctuation - using global substitute
                     sentence = gsub("[[:punct:]]", "", sentence)
                     # -remove control characters
                     sentence = gsub("[[:cntrl:]]", "", sentence)
                     # -remove digits
                     sentence = gsub('//d+', '', sentence)
                     # -define error handling function when trying tolower
                     tryTolower = function(x)
                     {
                       # -create missing value
                       y = NA
                       # -tryCatch error
                       try_error = tryCatch(tolower(x), error=function(e) e)
                       # -if not an error
                       if (!inherits(try_error, "error"))
                         y = tolower(x)
                       # -result
                       return(y)
                     }
                     # -use tryTolower with sapply
                     sentence = sapply(sentence, tryTolower)
                     # -split sentence into words with str_split (stringr package)
                     word.list = str_split(sentence, "//s+")
                     words = unlist(word.list)
                     # -compare words to the dictionaries of positive & negative terms
                     pos.matches = match(words, pos.words)
                     neg.matches = match(words, neg.words)
                     # -get the position of the matched term or NA
                     # -we just want a TRUE/FALSE
                     pos.matches = !is.na(pos.matches)
                     neg.matches = !is.na(neg.matches)
                     # -final score
                     score = sum(pos.matches) - sum(neg.matches)
                     return(score)
                   }, pos.words, neg.words, .progress=.progress )
    # -data frame with scores for each sentence
    scores.df = data.frame(text=sentences, score=scores)
    return(scores.df)
  }
  
  
  testsentiment = score.sentiment(mytest, pos, neg)

  testsentiment


  # -tweets for country
  usatweets = searchTwitter("#usa", n=900, lang="en")
  indiatweets = searchTwitter("#india", n=900, lang="en")
  russiatweets = searchTwitter("#russia", n=900, lang="en")
  chinatweets = searchTwitter("#china", n=900, lang="en")

  #get text
  usa_txt = sapply(usatweets, function(x) x$getText())
  india_txt = sapply(indiatweets, function(x) x$getText())
  russia_txt = sapply(russiatweets, function(x) x$getText())
  china_txt = sapply(chinatweets, function(x) x$getText())
  
  # how many tweets of each country
 
  nd = c(length(usa_txt), length(india_txt), length(russia_txt), length(china_txt))
  
  # join texts
  country = c(usa_txt, india_txt, russia_txt, china_txt)
  
  # apply function score.sentiment
  scores = score.sentiment(country, pos, neg, .progress='text')
  
  
  # add variables to data frame
  scores$country = factor(rep(c("usa", "india", "russia", "china"), nd))
  scores$very.pos = as.numeric(scores$score >= 2)
  scores$very.neg = as.numeric(scores$score <= -2)
  
  # how many very positives and very negatives
  numpos = sum(scores$very.pos)
  numneg = sum(scores$very.neg)
  # -global score
  global_score = round( 100 * numpos / (numpos + numneg) )
  head(scores)
  boxplot(score~country, data=scores)
  
  library("lattice")
  histogram(data=scores, ~score|country, main="Sentiment Analysis of 4 Countries", xlab="", sub="Sentiment Score")
  
  
  
  
#-this is a crucial step - at least for windows users
#-if you are on Linux or Mac you might skip this step, Win needs the certificate collection
#-Cacert.pem is a collection of certificates


download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="C:/Users/Abheek/study materials/Abheek_Assignment/R Final Modules/Text Mining and sentiment analysis/cacert.pem",
              method="auto")


#-hint: download.file is really handy when it comes to downloading material from the web
#-the dest file is the location on your computer, here it is my working directory
#-and url points to the place where you want to get the file from
#-we are entering the whole Twitter API info and call the whole object authenticate

authenticate <- OAuthFactory$new(consumerKey=key,
                                 consumerSecret=secret,
                                 requestURL='https://api.twitter.com/oauth/request_token',
                                 accessURL='https://api.twitter.com/oauth/access_token',
                                 authURL='https://api.twitter.com/oauth/authorize')

#-this will get you to a Twitter Site - obtain the PIN
#-the whole process is meant to provide the signature for your Twitter usage

authenticate$handshake(cainfo="F:/Abheek_Assignment/Text Mining and sentiment analysis/cacert.pem")

6863043
save(authenticate, file="twitter authentication.Rdata")
?setup_twitter_oauth
registerTwitterOAuth(authenticate)

setup_twitter_oauth(key, secret, mytoken, secrettk)

