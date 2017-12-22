## -----------------------------------------------------##
# Sentiment analysis of Obama's different speeches       #
# Source of text speeches: http://obamaspeeches.com/     #
## -----------------------------------------------------##

# install.packages("SnowballC")
# install.packages('tm')
# install.packages("stringr")
# install.packages("wordcloud")
# install.packages('RColorBrewer')
# install.packages('syuzhet')
# install.packages('plyr')
# install.packages('sentimentr')
# install.packages('grid')
# install.packages('gridBase')
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

library(grid)
library(gridBase)
library(gridExtra)




SentimentAnalysis <- function(textFileName, title) {
  # textFileName <- 'Obama-SuperTuesday_5_Feb_2008.txt'
  # title <- 'Super Tuesday (5 Feb 2008) Sentiment'
  
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

  ###
  # get the sentiment breakups
  ###
  mySent <- get_nrc_sentiment(text)

  rownames(mySent) <- sub("\\).*", "", sub(".*\\(", "", title)) 

  Comp_sent <<- rbind(Comp_sent, mySent, stringsAsFactors=FALSE)
  
  ###
  # Now lets draw the table of sentiment break-ups
  ###
  plot.new()
  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)
  grob <-  tableGrob(mySent)  
  grid.draw(grob)
  popViewport(3)

  ###
  # Draw bar chart of sentiment break-ups
  ###
  barplot(
    as.matrix(mySent),
    main = title,
    xlab = 'Sentiment Breakup',
    ylab = 'Score',
    col = c('red')
  )
}

speeachList <- list()
speeachList$textFileName <- c(
  'Obama-SuperTuesday_5_Feb_2008.txt',
  'Obama-PotomacPrimaryNight_12_Feb_2008.txt',
  'Obama-TexasAndOhioPrimaryNight_4_Mar_2008.txt',
  'Obama-AP Annual Luncheon_14_Apr_2008.txt',
  'Obama-PennsylvaniaPrimaryNight_22_Apr_2008.txt',
  'Obama-NorthCarolinaPrimaryNight_6_May_2008.txt',
  'Obama-PresumptiveDemocraticNomineeSpeech_2_Jun_2008.txt',
  'Obama-NomineeAcceptance_28_Aug_2008.txt',
  'Obama-NightBeforeElection_3_Nov_2008.txt',
  'Obama-ElectionNightVictorySpeech_4_Nov_2008.txt', 
  'Obama-InauguralAddress_20_Jan_2009.txt')

speeachList$barplotTitle <- c(
  'Super Tuesday (5 Feb\'08) Sentiment',
  'Potomac Primary Night (12 Feb\'08) Sentiment',
  'Texas And Ohio Primary Night (4 Mar\'08) Sentiment',
  'AP Annual Luncheon (14 Apr\'08) Sentiment',
  'Pennsylvania Primary Night (22 Apr 2008) Sentiment',
  'North Carolina Primary Night (6 May 2008) Sentiment',
  'Presumptive Democratic Nominee (2 Jun 2008) Sentiment',
  'Nomination Acceptance (28 Aug 2008) Sentiment',
  'Night Before Election (3 Nov 2008) Sentiment',
  'Election Night Victory Speech (04 Nov 2008) Sentiment',
  'Inaugural Address Speech (20 Jan 2009) Sentiment')


somePDFPath = "exportReport.pdf"
pdf(file = somePDFPath, onefile = T, paper = 'A4r', width = 5 * 2)

# par(mfrow = c(2, 1))
layout(matrix(c(1,2), nrow = 2, ncol = 1, byrow = TRUE))
# par(mai=c(0,1,0,1))

###
# Lets create a dataframe to compare the sentiments across the speeches
###
Comp_sent <- data.frame(anger = integer(), 
                        anticipation = integer(),
                        disgust = integer(),
                        fear = integer(),
                        joy = integer(),
                        sadness = integer(),
                        surprise = integer(),
                        trust = integer(),
                        negative = integer(), 
                        positive = integer()
                        )


for (i in 1:length(speeachList$textFileName)){
  SentimentAnalysis(speeachList$textFileName[i], speeachList$barplotTitle[i])
  
}


###
# Now let's draw the table of sentiment break-ups
###
layout(mat = matrix(c(1), nrow = 1, ncol = 1, byrow = TRUE))
plot.new()
par(mai=c(1,1,0,0))
vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)
grob <-  tableGrob(Comp_sent)  
grid.draw(grob)
popViewport(3)

###
# Plot each emotion separatly, 4 plots each page
###
layout(mat = matrix(c(1,2, 3,4), nrow = 2, ncol = 2, byrow = TRUE))

par(mai=c(1,1,0,0))
par(las = 1)


barplot(Comp_sent$anger, names.arg =  rownames(Comp_sent), horiz = TRUE, axes = TRUE, xlab = 'Anger')
barplot(Comp_sent$anticipation, names.arg =  rownames(Comp_sent), horiz = TRUE, axes = TRUE, xlab = 'Anticipation')
barplot(Comp_sent$disgust, names.arg =  rownames(Comp_sent), horiz = TRUE, axes = TRUE, xlab = 'Disgust')
barplot(Comp_sent$fear, names.arg =  rownames(Comp_sent), horiz = TRUE, axes = TRUE, xlab = 'Fear')
barplot(Comp_sent$joy, names.arg =  rownames(Comp_sent), horiz = TRUE, axes = TRUE, xlab = 'Joy')
barplot(Comp_sent$sadness, names.arg =  rownames(Comp_sent), horiz = TRUE, axes = TRUE, xlab = 'Sadness')
barplot(Comp_sent$surprise, names.arg =  rownames(Comp_sent), horiz = TRUE, axes = TRUE, xlab = 'Surprise')
barplot(Comp_sent$trust, names.arg =  rownames(Comp_sent), horiz = TRUE, axes = TRUE, xlab = 'Trust')
barplot(Comp_sent$negative, names.arg =  rownames(Comp_sent), horiz = TRUE, axes = TRUE, xlab = 'Negative')
barplot(Comp_sent$positive, names.arg =  rownames(Comp_sent), horiz = TRUE, axes = TRUE, xlab = 'Positive')
dev.off() 

## ---------------------

# Reference URL: https://stackoverflow.com/questions/25192838/arrange-base-plots-and-grid-tables-on-the-same-page
