# Code from: http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

# # Install
# install.packages("tm")  # for text mining
# install.packages("SnowballC") # for text stemming
# install.packages("wordcloud") # word-cloud generator 
# install.packages("RColorBrewer") # color palettes
# install.packages("qdap")
## install.packages("rJava")
## install.packages("RWeka") # to make n-grams
# install.packages("ngram") # to make n-grams
 
# Load
library(tidyverse)
library(modelr)
library(readxl)
library(sandwich)
library(lmtest)
library(utils)
library(openxlsx)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(qdap)
#library(rJava)
#library(RWeka)
library(ngram)

#text <- readLines(file.choose())

# Read the text file from internet
filePath_mlk <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
text_mlk <- readLines(filePath_mlk)

filePath <- "G:/NHS CB/Analytical Services (Patients and Information)/Surveys/Cancer Patient Experience/Free Text comments/CPES 2017/CPES 2017 COMMENTS MASTER FOR NHSE_Final - Copy.xlsx"
text <- read_excel(path=filePath,range="J1:J52295")
text_c <- text$`Comment1`



# Load the data as a corpus
docs <- Corpus(VectorSource(text_c))

docs_f <- Corpus(VectorSource(text))

# Text transformation
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")


docs_f <- tm_map(docs_f, toSpace, "/")
docs_f <- tm_map(docs_f, toSpace, "@")
docs_f <- tm_map(docs_f, toSpace, "\\|")


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
#docs <- tm_map(docs, stemDocument)

# Convert the text to lower case
docs_f <- tm_map(docs_f, content_transformer(tolower))
# Remove numbers
docs_f <- tm_map(docs_f, removeNumbers)
# Remove english common stopwords
docs_f <- tm_map(docs_f, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs_f <- tm_map(docs_f, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs_f <- tm_map(docs_f, removePunctuation)
# Eliminate extra white spaces
docs_f <- tm_map(docs_f, stripWhitespace)
# Text stemming
#docs_f <- tm_map(docs_f, stemDocument)




# Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
dtm_f <- TermDocumentMatrix(docs_f)
m <- as.matrix(dtm_f)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)

# Generate the Word Cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



# Explore frequent terms and their associations
findFreqTerms(dtm, lowfreq = 1000)
ass <- findAssocs(dtm, terms = "cancer", corlimit = 0.15)



# Plot word frequencies
barplot(d[1:20,]$freq, las = 2, names.arg = d[1:20,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

#list_vect2df(ass$cancer)


