# word blobs

if (!require(wordcloud)) {
  install.packages("wordcloud", repos="http://cran.us.r-project.org")
}
if (!require(tm)) {
  install.packages("tm", repos="http://cran.us.r-project.org")
}
if (!require(slam)) {
  install.packages("slam", repos="http://cran.us.r-project.org")
}
if (!require(SnowballC)) {
  install.packages("SnowballC", repos="http://cran.us.r-project.org")
}
if (!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cran.us.r-project.org")
}
if (!require(lubridate)) {
  install.packages("lubridate", repos="http://cran.us.r-project.org")
}
if (!require(plyr)) {
  install.packages("plyr", repos="http://cran.us.r-project.org")
}
library(plyr)
library(slam)
library(tm)
library(wordcloud)
library(SnowballC)
library(ggplot2)
library(lubridate)

workingDir = '/Users/michaeltauberg/projects/trump'
csvName = "trump2.csv"
data_name = "trump"
setwd(workingDir)
dt = read.csv(file=csvName)

# extract date info

dt$year = as.integer(format(as.Date(dt$date, format=" %b %d %Y %H:%M:%S %p" ),"%Y"))
dt$hour = as.integer(format(strptime(dt$date," %b %d %Y %H:%M:%S %p"),'%H'))
dt$month = format(strptime(dt$date," %b %d %Y %H:%M:%S %p"),'%m')
#dt$ampm = format(strptime(dt$date," %b %d %Y %H:%M:%S %p"),'%p') # this doesn't work for some reason

# convert hours to 24hr format
dt$date = as.character(dt$date)
date_elems = strsplit(as.character(dt$date), " ")
for (i in 1:length(date_elems)) {
  ampm = date_elems[[i]][length(date_elems[[i]])]
  if(ampm == "PM") {
    if (dt[i,]$hour != 12) {  
      dt[i,]$hour = dt[i,]$hour + 12 # convert to 24hr format
    }
  }
  else { # AM
    if (dt[i,]$hour == 12) {
      dt[i,]$hour = 0 #12AM = hour 0 in 24 hour scale
    }
  }
}

# remove retweets
RT = dt[grep("RT @", dt$tweet), ]
trump = dt[!grepl("RT @", dt$tweet), ]
# remove links
linked_tweets = dt[grepl("http", dt$tweet), ]
trump$tweet = gsub("http.*$", "", trump$tweet, perl=TRUE)
# removce references to other apps "via"
via = trump[grep("via", trump$tweet), ]
trump = trump[!grepl("via", trump$tweet), ]


obama = trump[grepl("Obama", trump$tweet), ]
t201107 = trump[trump$month == "201107", ]

t = trump[grepl("trump", trump$tweet), ]
golf = trump[grepl("golf", trump$tweet), ] 

# remove links
#trump$tweet = gsub("https", "", dt$tweet)
#trump$tweet = gsub("http", "", dt$tweet)

# removie client info
#trump$tweet = gsub("[Twitter.*]", "", trump$tweet)


# word clouds
GenerateWordClouds <- function(tweets, data_name, color) {
  words = Corpus(VectorSource(tweets$tweet)) 
  corpus <- tm_map(words, content_transformer(tolower))
  
  words = tm_map(words, stripWhitespace)
  words = tm_map(words, tolower)
  
  #badwords = c("the", "and", "a")
  #words = tm_map(words, removeWords, badwords)
  #png(sprintf("%s_simple_wordcloud.png", data_name))
  #wordcloud(words, max.words = 120, random.order=FALSE, colors=brewer.pal(nrow(dt_uniq),"Dark2"))
  #dev.off()
  
  complete_stopwords = c(stopwords('english'), "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
  complete_stopwords = c(complete_stopwords, "com", "http", "https", "-", "—", '\"', "will", "”", "like")
  complete_stopwords = c(complete_stopwords, "am", "pm", "wallflower", "said", "tara", '“', "via", "–", "’s")
  # Generate wordcloud removing all stop words
  png(sprintf("%s_stopwords_wordcloud.png", data_name))
  words = tm_map(words, removeWords, complete_stopwords)
  wordcloud(words, max.words = 75, random.order=FALSE, colors=brewer.pal(8,"Dark2"))
  
  dev.off()
  
  dtm = TermDocumentMatrix(words)
  m = as.matrix(dtm)
  v = sort(rowSums(m),decreasing=TRUE)
  d = data.frame(word = rownames(m), 
                        freq = rowSums(m), 
                        row.names = NULL)
  #d = data.frame(word = names(v),freq=v)
  d = d[order(d$freq, decreasing=TRUE),]
  d$word = factor(d$word, levels = d$word[order(d$freq, decreasing=TRUE)])
  
  top_words = d[1:40,]
  p = ggplot(top_words, aes(x=word, y=freq, fill=data_name)) + geom_bar(stat="identity") 
  p = p + ggtitle(sprintf("%s - Top Words", toupper(data_name)))
  p = p + theme(plot.title = element_text(hjust = 0.5))
  p = p + theme(axis.text.x=element_text(angle=90, hjust=1,face="bold"))
  p = p + theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=13), axis.title.x=element_blank())
  p = p + theme(plot.title = element_text(size=18,face="bold"))
  #p = p + xlab("Word")
  p = p + scale_fill_manual(values = c(color)) + guides(fill=FALSE)
  p = p + ylab("Number of Uses") 
  
  #p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
  ggsave(filename = sprintf("./%s_top40.png", data_name) , plot=p, width=8, height=9)
}

trump$tweet = gsub("http.*$", "", trump$tweet)
GenerateWordClouds(trump, "trump_tweets", 'red')

data_name = "trump_words"
color = 'red'
trump_words = read.csv("trump_words.csv")
trump_words = trump_words[1:40,]
trump_words$word = factor(trump_words$word, levels = trump_words$word[order(trump_words$freq, decreasing=TRUE)])
p = ggplot(trump_words, aes(x=word, y=freq, fill=data_name)) + geom_bar(stat="identity") 
p = p + ggtitle("Top Words in Trump's Tweets")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1,face="bold"))
p = p + theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=13), axis.title.x=element_blank())
p = p + theme(plot.title = element_text(size=18,face="bold"))
#p = p + xlab("Word")
p = p + scale_fill_manual(values = c(color)) + guides(fill=FALSE)
p = p + ylab("Number of Uses") 
ggsave(filename = sprintf("./%s_top40.png", data_name) , plot=p, width=10, height=7)

# sentiment scores
# why the news is so boring (and depressing)
if (!require(syuzhet)) {
  install.packages("syuzhet", repos="http://cran.us.r-project.org")
}
library(syuzhet)

# By individual word
#trump_a_v <- get_tokens(trump$tweet, pattern = "\\W")
#trump_word_vector <- get_sentiment(trump_a_v, method="afinn")
#trump_word_avg = sum(trump_word_vector)/length(trump_word_vector)

# By sentence
trump_s_v <- get_sentences(trump$tweet)
trump_afinn_vector <- get_sentiment(trump$tweet, method="afinn")
#trump_syuzhet_vector <- get_sentiment(trump$tweet, method="syuzhet")
#trump_bing_vector <- get_sentiment(trump$tweet, method="bing")
#trump_nrc_vector <- get_sentiment(trump$tweet, method="nrc", lang = "english")

hist(trump_afinn_vector)

p = qplot(trump_afinn_vector,
          geom="histogram",
          binwidth = 1,
          main = "Histogram for Age")

# use sentimentr library
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sentimentr, dplyr, magrittr)
library('sentimentr')



# function  - get avg sentiment - print it, plot histogram

trump_sent = sentiment(trump$tweet)
trump_sent_avg = sum(trump_sent$sentiment)/nrow(trump_sent)

p = ggplot(data=trump_sent, aes(trump_sent$sentiment)) 
p = p + geom_histogram(col="red", fill="green", alpha = .2)
p = p + ggtitle("Sentiment of Trump Tweets")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1,face="bold"))
p = p + theme(axis.text=element_text(size=16), axis.title=element_text(size=13), axis.title.x=element_blank())
p = p + theme(plot.title = element_text(size=18))
p = p + scale_fill_manual(values = c(color)) + guides(fill=FALSE)
p = p + ylab("Count")  +  xlab("Sentiment Score (0 is neutral)")
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = "trump_tweet_histogram.png" , plot=p, width=7, height=5)
#plot histogram of his sentences

trump$sent = trump_afinn_vector
trump$id = seq(1:nrow(trump))
trump_tweet_stats = ddply(trump, "year", summarise, mean=mean(strtoi(sent),na.rm=TRUE), ntweets=length(unique(tweet)))
p = ggplot(trump_tweet_stats, aes(x=year, y=mean)) + geom_point() + geom_line()
#p = p + geom_smooth(method='lm', se = FALSE)
p = p + xlab("Date") + ylab("Tweet Sentiment") 
p = p + ggtitle("Trump Tweet Sentiment") + theme(plot.title = element_text(size=18))
p = p + theme(text = element_text(size=14), axis.text.x=element_text(angle=90, hjust=1))
ggsave(filename = "./mean_songs_per_album_per_month.png", plot=p, width=7, height=5.5) 

p = ggplot(trump_tweet_stats, aes(x=year, y=ntweets)) + geom_point() + geom_line()
#p = p + geom_smooth(method='lm', se = FALSE)
p = p + xlab("Year") + ylab("Number of Tweets") 
p = p + ggtitle("Trump Tweets Per Year") + theme(plot.title = element_text(size=18))
p = p + theme(text = element_text(size=14), axis.text.x=element_text(angle=90, hjust=1))
p = p + scale_x_discrete(limits = c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))
ggsave(filename = "./trump_tweets_per_year.png", plot=p, width=7, height=5.5) 

trump2 = trump[trump$year > 2011,]
trump$month = paste(trump$year, trump$month, sep="")
trump_tweet_stats = ddply(trump, "month", summarise, mean=mean(strtoi(sent),na.rm=TRUE), ntweets=length(unique(tweet)))
#trump_tweet_stats$month = seq(1:nrow(trump_tweet_stats))
p = ggplot(trump_tweet_stats, aes(x=month, y=mean)) + geom_point() 
p = p + geom_smooth(method='lm', se = FALSE)
p = p + xlab("Year") + ylab("Tweet Sentiment") 
p = p + ggtitle("Trump Avg Tweet Sentiment per Month") + theme(plot.title = element_text(size=18))
p = p + theme(text = element_text(size=8), axis.text.x=element_text(angle=90, hjust=1))
ggsave(filename = "./trump_tweets_sentiment_month.png", plot=p, width=11, height=7) 
###################


# trump adjectives
adjectives = read.csv("trump_adjectives.csv")
adjectives = adjectives[!duplicated(adjectives[,c('word')], fromLast=FALSE),]
adjectives$word = factor(adjectives$word, levels = adjectives$word[order(adjectives$freq, decreasing=TRUE)])
data_name = "adjectives"
p = ggplot(adjectives, aes(x=word, y=freq, fill=positivity)) + geom_bar(stat="identity") 
p = p + ggtitle("Trump Tweets Top Adjectives")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=0.5,vjust=0.5,face="bold"))
p = p + theme(axis.text=element_text(size=13), axis.title=element_text(size=11), axis.title.x=element_blank())
p = p + theme(plot.title = element_text(size=18))
p = p + xlab("Adjectives")
p = p + ylab("Number of Uses") 

#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s.png", data_name) , plot=p, width=10.5, height=7)


# trump enemies
enemies = read.csv("trump_enemies.csv")
enemies = enemies[!duplicated(enemies[,c('word')], fromLast=FALSE),]
enemies$word = factor(enemies$word, levels = enemies$word[order(enemies$freq, decreasing=TRUE)])

data_name = "enemies"
p = ggplot(enemies, aes(x=word, y=freq, fill=alliance)) + geom_bar(stat="identity") 
p = p + ggtitle("Trump Top Tweet Subjects")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=0.5,vjust=0.5,face="bold"))
p = p + theme(axis.text=element_text(size=13), axis.title=element_text(size=11), axis.title.x=element_blank())
p = p + theme(plot.title = element_text(size=18))
p = p + xlab("Entity")
p = p + ylab("Number of Uses") 

#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s.png", data_name) , plot=p, width=12, height=7)


# trump insults
#cbPalette <- c("black",   "red",     "green3",  "blue",    "cyan",    "magenta", "yellow")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
insults = read.csv("trump_insults.csv")
insults = insults[!duplicated(insults[,c('word')], fromLast=FALSE),]
insults$word = factor(insults$word, levels = insults$word[order(insults$freq, decreasing=TRUE)])
data_name = "insults"
p = ggplot(insults, aes(x=word, y=freq, fill=style)) + geom_bar(stat="identity") 
p = p + ggtitle("Trump Tweets Top Insults")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=0.5,vjust=0.5,face="bold"))
p = p + theme(axis.text=element_text(size=13), axis.title=element_text(size=11), axis.title.x=element_blank())
p = p + theme(plot.title = element_text(size=18))
p = p + xlab("Insult")
p = p + ylab("Number of Uses") 
p = p + scale_fill_manual(values=cbPalette)
p = p + scale_fill_hue(c=85, l=80)
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s.png", data_name) , plot=p, width=12, height=7)


# trump calls to action
actions = read.csv("trump_actions.csv")
actions = actions[!duplicated(actions[,c('word')], fromLast=FALSE),]
actions$word = factor(actions$word, levels = actions$word[order(actions$freq, decreasing=TRUE)])
data_name = "actions"
p = ggplot(actions, aes(x=word, y=freq)) + geom_bar(stat="identity") 
p = p + ggtitle("Trump Tweets Top Action Words")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=0.5,vjust=0.5,face="bold"))
p = p + theme(axis.text=element_text(size=13), axis.title=element_text(size=11), axis.title.x=element_blank())
p = p + theme(plot.title = element_text(size=18))
p = p + xlab("Verb")
p = p + ylab("Number of Uses") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s.png", data_name) , plot=p, width=8, height=6)


data_name = "fourgrams"
fourgrams = read.csv("fourgrams.csv")
fourgrams = fourgrams[!duplicated(fourgrams[,c('word')], fromLast=FALSE),]
fourgrams$word = factor(fourgrams$word, levels = fourgrams$word[order(fourgrams$freq, decreasing=TRUE)])
p = ggplot(fourgrams, aes(x=word, y=freq, fill=motto)) + geom_bar(stat="identity") 
p = p + ggtitle("Trump Tweets Interesting 4grams")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=0.5,vjust=0.5,face="bold"))
p = p + theme(axis.text=element_text(size=13), axis.title=element_text(size=11), axis.title.x=element_blank())
p = p + theme(plot.title = element_text(size=18))
p = p + xlab("Fourgrams")
p = p + ylab("Number of Uses") 
p = p + scale_fill_manual(values=c("grey","red")) + guides(fill=FALSE)
ggsave(filename = sprintf("./%s.png", data_name) , plot=p, width=8, height=6)

data_name = "threegrams"
threegrams = read.csv("threegrams.csv")
threegrams = threegrams[!duplicated(threegrams[,c('word')], fromLast=FALSE),]
threegrams$word = factor(threegrams$word, levels = threegrams$word[order(threegrams$freq, decreasing=TRUE)])
p = ggplot(threegrams, aes(x=word, y=freq, fill=motto)) + geom_bar(stat="identity") 
p = p + ggtitle("Trump Tweets Interesting Trigrams")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=0.5,vjust=0.5,face="bold"))
p = p + theme(axis.text=element_text(size=13), axis.title=element_text(size=11), axis.title.x=element_blank())
p = p + theme(plot.title = element_text(size=18))
p = p + xlab("Rourgrams")
p = p + ylab("Number of Uses") 
p = p + scale_fill_manual(values=c("grey","red")) + guides(fill=FALSE)
ggsave(filename = sprintf("./%s.png", data_name) , plot=p, width=8, height=6)


# Retweets
retweets = read.csv("trump_retweets.txt")
retweets$Account = gsub(":", "", retweets$Account)
rt = as.data.frame(table(retweets$Account))
data_name = "retweets"
rt = rt[order(rt$Freq, decreasing=TRUE),]
rt$Var1 = factor(rt$Var1, levels = rt$Var1[order(rt$Freq, decreasing=TRUE)])
top_rt = rt[1:20,]
p = ggplot(top_rt, aes(x=Var1, y=Freq)) + geom_bar(stat="identity") 
p = p + ggtitle("Trump Top Retweet Accounts")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=0.5,vjust=0.5,face="bold"))
p = p + theme(axis.text=element_text(size=13), axis.title=element_text(size=11), axis.title.x=element_blank())
p = p + theme(plot.title = element_text(size=18))
p = p + xlab("Retweet Accounts")
p = p + ylab("Number of Retweets") 
ggsave(filename = sprintf("./%s.png", data_name) , plot=p, width=8, height=6)

# Time of Tweets
p = ggplot(trump, aes(x=hour)) + geom_histogram(color="grey",binwidth=1, fill="#FF9999") # days of email sent # days of email sent
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Hour Sent") + ylab("Num Tweets") + ggtitle("Number of Tweets Sent by Hour")
p = p + theme(plot.title = element_text(lineheight=.8, face="bold"))
p = p + scale_x_discrete(limits = seq(0,23))
ggsave(filename = "./TrumpHoursSent.png", plot=p, width=6) 


# tweets about democrats, journalists
#https://www.cjr.org/united_states_project/trump-twitter-spreadsheet-press-attacks.php