# Sentiment analysis of Obama's different speeches

# install.packages("SnowballC")
# install.packages('tm')
# install.packages("stringr")
# install.packages("wordcloud")
# install.packages('RColorBrewer')
# install.packages('syuzhet')
# install.packages('plyr')
# install.packages('sentimentr')
# install.packages('gridExtra')

library(NLP)
library(SnowballC)
library(tm)
library(stringr)
library(RColorBrewer)
library(wordcloud)
library(syuzhet)
library(plyr)
library(sentimentr)
library(gridExtra)

SentimentAnalysis <- function(textFileName, title) {
  text <- paste(readLines(textFileName), collapse = " ")
  ###
  # Remove the stop words from the speach text
  ###
  text <- removeWords(text, stopwords())
  
  ###
  # Create a list of words
  ###
  bag_of_word <- str_split(string =  text, pattern = " ")
  
  ###
  # to ceate wordcloud, we need to convert the list into vector
  ###
  bag_of_word <- unlist(bag_of_word)
  
  ###
  # Now we have the word cloud
  ###
  # wordcloud(words = bag_of_word, min.freq = 5, random.order = FALSE, random.color =FALSE)
  
  
  mySent <- get_nrc_sentiment(text)
  mySent
  colnames(mySent)
  
  grid.table(mySent,  cols = colnames(mySent))
  barplot(
    as.matrix(mySent),
    main = title,
    xlab = 'Sentiment Breakup',
    ylab = 'Score',
    col = c('red')
  )
}

somePDFPath = "exportReport.pdf"
pdf(
  file = somePDFPath,
  onefile = T,
  paper = 'A4r',
  width = 5 * 2
)

par(mfrow = c(2, 2))
textFileName <- 'Obama-ElectionNightVictorySpeech_4_Nov_2008.txt'
barplotTitle <-
  "Election Night Victory Speech (04 Nov 2008) Sentiment"
SentimentAnalysis(textFileName, barplotTitle)

textFileName <- 'Obama-InauguralAddress_20_Jan_2009.txt'
barplotTitle <- "Inaugural Address Speech (20 Jn 2009) Sentiment"
SentimentAnalysis(textFileName, barplotTitle)


library(gridExtra)
d <- head(iris)
table <- tableGrob(d)

library(grid)
library(gtable)

title <- textGrob("Title",gp=gpar(fontsize=50))
footnote <- textGrob("footnote", x=0, hjust=0,
                     gp=gpar( fontface="italic"))

padding <- unit(0.5,"line")
table <- gtable_add_rows(table, 
                         heights = grobHeight(title) + padding,
                         pos = 0)
table <- gtable_add_rows(table, 
                         heights = grobHeight(footnote)+ padding)
table <- gtable_add_grob(table, list(title, footnote),
                         t=c(1, nrow(table)), l=c(1,2), 
                         r=ncol(table))
grid.newpage()
grid.draw(table)
dev.off() 

## ---------------------
