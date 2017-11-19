library(SocialMediaLab)
library(Rfacebook)

library("RCurl")
library("tm")
library("wordcloud")
library("SnowballC")

# Need a FB developer account and published app - follow instructions here:
# http://vosonlab.net/SocialMediaLab
# https://developers.facebook.com/

fb.appid = "XYX"
fb.appsecret = "ABC"

# AUTHENTICATE WITH FACEBOOK
fb_oauth <- AuthenticateWithFacebookAPI(appID = fb.appid,
 appSecret = fb.appsecret,
 extended_permissions = FALSE, # Public Info
 useCachedToken = TRUE)        # Use existing


# GET POSTS FROM THERESA MAY
theresa.page.df <- getPage(page="TheresaMayOfficial",  # Takes page ID or page name
 token=fb_oauth,   # OAuth token
 n=200,             # Number of posts to return
 since=NULL,       # Date of earliest posts returned
 until=NULL,       # Date of latest posts returned
 feed=TRUE)        # T/F: Return posts by page non-owners

# put into encoding to get rid of emojis and other characters that will cause problems 
theresa.page.df$message  <- sapply(theresa.page.df$message,function(row) iconv(row,"latin1","ASCII",""))
docs <- Corpus(VectorSource(theresa.page.df$message))

# Based on code from JINSUH LEE http://web.ics.purdue.edu/~jinsuh/twitterwithR.R

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove other words
docs <- tm_map(docs, removeWords, c("https", "photo", "type", "video", "http", "wwwfaceookcom", "wwwfacebookcom")) 

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Text stemming
docs <- tm_map(docs, stemDocument)

# Word counts
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 2,max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))
