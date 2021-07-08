# Code from: http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
# Also from: https://www.tidytextmining.com/ngrams.html

## Install
# install.packages("tm")  # for text mining
# install.packages("SnowballC") # for text stemming
# install.packages("wordcloud") # word-cloud generator 
# install.packages("RColorBrewer") # color palettes
# install.packages("qdap")
## install.packages("rJava")
## install.packages("RWeka") # to make n-grams
# install.packages("ngram") # to make n-grams
#install.packages("tidytext") 
# install.packages("janeaustenr") 
# install.packages("igraph") # plots with edge/vertex attributes (a gRaPh)
# install.packages("ggraph") # analogous to ggplot but for igraph objects
 

#### Load libraries ####
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
library(rJava)
#library(RWeka)
library(janeaustenr)
library(tidytext) # for sentiments
library(textdata) # for AFINN
library(igraph) # masks decompose, spectrum (from stats), union (from base)
library(ggraph) #  
library(reshape2) # to be able to use 'acast' function
#library(webshot) # to save wordclouds as png
#library(htmlwidgets) # to help save wordclouds as png
library(scales)

#### Auxiliary functions ####
visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = freq), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}


filePath <- "input/analysis_dataset_20210201.xlsx"
#text <- read_excel(path=filePath,range="L1:L52295")
text <- read.xlsx(filePath)
text <- text %>% rename(Q02 = "Q2.5..Other.(please.specify):" )
text_c <- text %>% select(Q02) %>% filter(Q02 !="No")


#### Load the data as a corpus ####
docs <- VCorpus(VectorSource(text_c))
docs_f <- VCorpus(VectorSource(text_c))


#### Text transformation #####
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

#### Tokenizer / N-Grammer (pairs) ####
token_size <- 2
NLP_tokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), token_size), paste, collapse = " "), use.names = FALSE)
}
control_list_ngram = list(tokenize = NLP_tokenizer)


#### Build a term-document matrix ####
dtm <- TermDocumentMatrix(docs,control_list_ngram)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v) %>% filter(!grepl("na",word))
head(d, 40)


#### Generate the Word Cloud ####
set.seed(1234)
my_cloud<-wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=30, random.order=FALSE, rot.per=0, 
          colors=brewer.pal(8, "Dark2"))
#saveWidget(my_cloud,"tmp.html",selfcontained = F)

#### Create a graph for bigrams ####
att_sep <- ifelse(token_size==2,list(c("word1","word2")),list(c("word1", "word2","word3"))) %>% .[[1]]
dd <- d %>% filter(!grepl("na",word)) %>% separate(word, att_sep, sep = " ")
dd %>% filter(freq>10) %>% visualize_bigrams()


##### Explore frequent terms and their associations / correlations ####
findFreqTerms(dtm, lowfreq = 1000)
ass <- findAssocs(dtm, terms = "ipad", corlimit = 0.01)
list_vect2df(ass$ipad)


#### Plot word frequencies ####

d$word <- factor(d$word,rev(unique(d$word))) # to make sure "Words" are plotted in correct order rather than alphabetically
# mutate(word = reorder(word, n)) %>%  ## try this as well to reorder!

ggplot(data=d[1:20,],mapping=aes(x=word,y=freq)) + geom_col(fill="deepskyblue") +
  labs(y="Word frequencies",x="Words",title="Q02 - Most frequent pairs of words") + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  coord_flip()+ labs(caption="Frequency that two words appear next to each other in a reply\n (excluding connectors, common stopwords, punctuation etc)")+
  geom_text(aes(label=freq),position = position_dodge(width = 1), hjust = 0, size = 5)+
  theme(text=element_text(size=18))
ggsave("./output/Q02_wordpairfreq.png",width = 24, height = 20, dpi=300,units ="cm")


barplot(d[1:20,]$freq, las = 2, names.arg = d[1:20,]$word,
        col ="lightblue", main ="Most frequent pairs of words",
        ylab = "Word frequencies") + coord_flip()

#### Single word ####
dtm_w <- TermDocumentMatrix(docs)
m_w <- as.matrix(dtm_w)
v_w <- sort(rowSums(m_w),decreasing=TRUE)
d_w <- data.frame(word = names(v_w),freq=v_w)
head(d_w, 40)
d_w$word <- factor(d_w$word,rev(unique(d_w$word)))

ggplot(data=d_w[1:20,],mapping=aes(x=word,y=freq)) + geom_col(fill="deepskyblue") +
  labs(y="Word frequencies",x="Words",title="Q02 - Most frequent words") + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  coord_flip()+ labs(caption="Frequency that a word appears\n (top is excluding connectors, common stopwords, punctuation etc)")+
  geom_text(aes(label=freq),position = position_dodge(width = 1), hjust = 0, size = 5)+
  theme(text=element_text(size=18))
ggsave("./output/Q02_wordsinglefreq.png",width = 24, height = 20, dpi=300,units ="cm")

set.seed(1234)
my_cloud<-wordcloud(words = d_w$word, freq = d_w$freq, min.freq = 1,
                    max.words=40, random.order=FALSE, rot.per=0, 
                    colors=brewer.pal(8, "Dark2"))




#### Sentiment analysis - Afinn score ####
# for words, not grams!
sentiments_afinn <- get_sentiments("afinn")

d_w_sen <- inner_join(d_w,
                      sentiments_afinn,
                      by=c("word")) #%>%
          #filter(!word %in% c("cancer","care")) # cancer and care are the two most common words for L. They have sentiment attributed but given that the survey is on "cancer care", does not feel right to tally "cancer" as negative or "care" as positive.

(d_w_sen %>% count(value)) # amount of distinct words per score
(d_w_sen %>% group_by(value) %>% summarise(freq_all = sum(freq))) # overall frequency of use of words per score

#### Sentiment analysis - Bing positive / negative lexicon ####

d_w_bing_nocast <- inner_join(d_w,get_sentiments("bing"),by="word") %>% arrange(desc(freq))
d_w_bing_nocast$word <- factor(d_w_bing_nocast$word,rev(unique(d_w_bing_nocast$word)))

myceil <- d_w_bing_nocast$freq %>% max()
myrange=c(8,0.5)
myfloor <- 4
aux <- d_w_bing_nocast %>% filter(freq>=myfloor)
aux$Scale <- rescale(aux$freq,to=rev(myrange))


#wordcloud(d_w_bing_nocast$word[d_w_bing_nocast$sentiment == "positive"], d_w_bing_nocast$freq[d_w_bing_nocast$sentiment == "positive"], scale = c(8*max(d_w_bing_nocast$freq[d_w_bing_nocast$sentiment == "positive"])/myceil, 0.5),colors="blue",rot.per=0)
#wordcloud(d_w_bing_nocast$word[d_w_bing_nocast$sentiment == "negative"], d_w_bing_nocast$freq[d_w_bing_nocast$sentiment == "negative"], scale = c(8*max(d_w_bing_nocast$freq[d_w_bing_nocast$sentiment == "negative"])/myceil, 0.5),colors="red",rot.per=0)

wordcloud(d_w_bing_nocast$word[d_w_bing_nocast$sentiment == "positive"],
          d_w_bing_nocast$freq[d_w_bing_nocast$sentiment == "positive"],
          min.freq=myfloor,
          scale = myrange,
          colors="blue",
          rot.per=0)

wordcloud(d_w_bing_nocast$word[d_w_bing_nocast$sentiment == "negative"],
          d_w_bing_nocast$freq[d_w_bing_nocast$sentiment == "negative"],
          min.freq=myfloor,
          scale = c(max(aux$Scale[aux$sentiment == "negative"]),min(aux$Scale[aux$sentiment == "negative"])),
          colors="red",
          rot.per=0)


d_w_bing <- inner_join(d_w,get_sentiments("bing"),by="word") %>%
  acast(word ~ sentiment, value.var="freq",fill=0)
d_w_bing %>% comparison.cloud(colors=c("red","deepskyblue"),max.words=10) #nb: scale is relative to each cat, so can't compare size of 'positive' and negative'

ggplot(data=d_w_bing_nocast[1:40,],mapping=aes(x=word,y=freq)) + geom_col(aes(fill=sentiment)) +
  #facet_wrap(~sentiment,scales="free_x")+
  labs(y="Q17 word frequencies",x="Words",title="Q17. 40 most frequent words with a positive or negative association") +
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  theme(text=element_text(size=18))+
  coord_flip()+
  labs(caption="Sentiment analysis informed by the Bing lexicon of positive and negative connotation words.\n Note: words not present in the lexicon ommitted.")
ggsave("./output/Q17_sentimentfreq.png",width = 30, height = 20, dpi=300,units ="cm")

