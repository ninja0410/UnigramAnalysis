#connect all libraries0io
library("openssl")
library("httpuv")
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(httr)
library(base64enc)
#connect to API
download.file(url='http://curl.haxx.se/ca/cacert.pem', destfile='cacert.pem')
reqURL <- 'https://api.twitter.com/oauth/request_token'
accessURL <- 'https://api.twitter.com/oauth/access_token'
authURL <- 'https://api.twitter.com/oauth/authorize'

consumerKey <- 'F8153JGdHnDDoaIl2wOQJYcyq' #put the Consumer Key from Twitter Application
consumerSecret <- 'HnlcO3h5PUwqW69HvOXBxM7zpypGftPvNk8p7EPxwmI1BK1rAz'  #put the Consumer Secret from Twitter Application
accesstoken <- '802470848409964545-SyiOJGAwltoP5AxK8GeF4EyoXzOMOy2'
accesssecret <- 'RRYXStliIGRR1d2Xee7wjH6gC6JxEB9Ic8vH79iYga764'  
Cred2 <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL=reqURL,
                         accessURL=accessURL,
                         authURL=authURL)
Cred2$handshake(cainfo="cacert.pem")
save(Cred2, file='twitter authentication.Rdata')
load('twitter authentication.Rdata') #Once you launch the code first time, you can start from this line in the future (libraries should be connected)
setup_twitter_oauth(consumerKey, consumerSecret, accesstoken, accesssecret)
registerTwitterOAuth(Cred2)
search.string <- "#SalmanVerdict"
no.of.tweets <- 100000

SalmanV.list <- searchTwitter(search.string, n=no.of.tweets, lang="en")
SalmanV.df <- twListToDF(SalmanV.list)



  #evaluation tweets function
  score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
  {
    require(plyr)
    require(stringr)
    scores <- laply(sentences, function(sentence, pos.words, neg.words){
      sentence <- gsub('[[:punct:]]', "", sentence)
      sentence <- gsub('[[:cntrl:]]', "", sentence)
      sentence <- gsub('\\d+', "", sentence)
      sentence <- tolower(sentence)
      word.list <- str_split(sentence, '\\s+')
      words <- unlist(word.list)
      pos.matches <- match(words, pos.words)
      neg.matches <- match(words, neg.words)
      pos.matches <- !is.na(pos.matches)
      neg.matches <- !is.na(neg.matches)
      score <- sum(pos.matches) - sum(neg.matches)
      return(score)
    }, pos.words, neg.words, .progress=.progress)
    scores.df <- data.frame(score=scores, text=sentences)
    return(scores.df)
  }
  
  pos <- scan('positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
  neg <- scan('negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
  pos.words <- c(pos, 'upgrade')
  neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')
  
  Dataset <- SalmanV.df
  Dataset$text <- as.factor(Dataset$text)
  Scores <- score.sentiment(Dataset$text, pos.words, neg.words, .progress='text')
  
Scores$Review <- "null"
#Scores$Review <- ifelse(Scores$score < 0, Scores$Review <- "negative", ifelse(Scores$score > 0, Scores$Review <- "positive", Scores$Review <- "neutral"))
Scores$Review <- ifelse(Scores$score < -2, Scores$Review <- "Anger", ifelse(Scores$score < 0 , Scores$Review <- "Sadness", ifelse(Scores$score < 1 , Scores$Review <- "Surprise", ifelse(Scores$score < 3 , Scores$Review <- "Happy", Scores$Review <- "Pleasant"))))

write.csv(Scores, file= "SalmanV.csv", row.names=TRUE) #save evaluation results into the file
table(Scores$score)
table(Scores$Review)
stat <- Scores
stat$created <- Dataset$created
stat$created <- as.Date(Dataset$created)
by.tweet <- group_by(stat, Review, created)
by.tweet <- summarise(by.tweet, number=n())
write.csv(by.tweet, file= 'SV_opin.csv', row.names=TRUE)
qplot(factor(Review), data=stat, geom="bar", fill=factor(Review))+xlab("Reaction") + ylab("Frequency") + ggtitle("People Reactions on Salman's Verdict")
#chart
#ggplot(by.tweet, aes(created, number)) + geom_line(aes(group=tweet, color=tweet), size=2) +
 # geom_point(aes(group=tweet, color=tweet), size=4) +
  #theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) 
  #stat_summary(fun.y = 'sum', fun.ymin='sum', fun.ymax='sum', colour = 'yellow', size=2, geom = 'line') +
  #ggtitle("SalmanVerdict")

#ggsave(file='SV_plot.jpeg')
library(RTextTools)
library(e1071)
library(naivebayes)
matrix <- create_matrix(Scores[,2], language="english", 
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE) 
mat <- as.matrix(matrix)
classifier <- naiveBayes(mat[1:20000,], as.factor(Scores[1:20000,3]) )
predicted <- predict(classifier, mat[20001:25000,]) # predicted
table(Scores[20001:25000, 3], predicted)
recall_accuracy(Scores[20001:25000, 3], predicted)
