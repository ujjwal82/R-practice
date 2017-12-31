## ----------------------------------------------
# Reference URL: https://davetang.org/muse/2013/04/06/using-the-r_twitter-package/


# install.packages("twitteR")
# install.packages("wordcloud")
# install.packages("tm")

library("twitteR")
library("wordcloud")
library("tm")

###
# The reference site says it's necessary, but 
# i didn't find it used any where.
###
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'cr3eQVg70a8BiwrYvtdLpmDKO'
consumer_secret <- '9LBgn6FQF2VUGH0pRrf5BvG0Obqc564MmTrNn7g8FbwrXzh3T6'
access_token <- '67892304-4KcGY8R0FbBxe7zuyyFfNgUY2udUY5cWNW2ez49n0'
access_secret <- 'ZciIo4pgZQvnI0jYl7fPkJofBEbn1FSFxHFT3Io1ERNdj'
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

###
# Get the twits with hash tag 'Rstats' max twits 1500
###
r_stats <- searchTwitter("#Rstats", n=1500)

# Check the number of twits we received.
length(r_stats)
#[1] 1500

#save text
r_stats_text <- sapply(r_stats, function(x) x$getText())

#create corpus
r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))

# Convert into UTF-8 format
r_stats_text_corpus <- tm_map(r_stats_text_corpus,
                              content_transformer(function(x) iconv(x, to='UTF-8', sub='byte'))
)
# clean up
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower)) 
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()))

wordcloud(r_stats_text_corpus, min.freq = 15, random.order = FALSE, random.color =TRUE)

## ------------------------------

library(RColorBrewer)
###
# Get the twits with hash tag 'bioinformatics' max twits 1500
###
bioinformatics <- searchTwitter("#bioinformatics", n=1500)
bioinformatics_text <- sapply(bioinformatics, function(x) x$getText())
bioinformatics_text_corpus <- Corpus(VectorSource(bioinformatics_text))
bioinformatics_text_corpus <- tm_map(bioinformatics_text_corpus,
                                     content_transformer(function(x) iconv(x, to='UTF-8', 
                                                                           sub='byte')))
bioinformatics_text_corpus <- tm_map(bioinformatics_text_corpus, content_transformer(tolower))
bioinformatics_text_corpus <- tm_map(bioinformatics_text_corpus, removePunctuation)
bioinformatics_text_corpus <- tm_map(bioinformatics_text_corpus, function(x)removeWords(x,stopwords()))

pal2 <- brewer.pal(8,"Dark2")
wordcloud(bioinformatics_text_corpus,min.freq=2,max.words=100, random.order=T, colors=pal2)
