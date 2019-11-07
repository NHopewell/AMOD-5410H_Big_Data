#most of the libraries needed
library(dplyr) #data manipulation
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(wordcloud)
library(wordcloud2) #creative visualizations
library(qdap) # term frequencies and more parsing tools
library(tibble) # dataframe manip
library(tm) # corpus stuff'
library(plotrix) 
library(dendextend) # better dendograms
library(ggthemes) # for ggplot2 themes
library(RWeka) # for bigrams and tokenzations

setwd("C:/Users/nicho/Desktop/big data course proj")
set.seed(999)


tweets <- read.csv("tweets.csv", header = T, stringsAsFactors = F)
russian_tweets <- read.csv("russian_tweets.csv", header = T, stringsAsFactors = F)

# the ordering of these columns as well as the doc_id column and it's position are crucial 
tweets <- tweets %>%
        select(text = X..text,
                user_id = X..user..id, 
                screen_name = X..user..screen_name,
                created_at = X..created_at,
                retweet_count = X..retweet_count,
                favorite_count = X..favorite_count,
                tweet_id = X..id,
                hashtags = X..entities..hashtags,
                expanded_urls = X..entities..urls,
                user_mentions = X..entities..user_mentions,
                retweeted_status_id = X..retweeted_status..id) %>%
        add_column(doc_id = seq.int(nrow(tweets)), .before = "text")

russian_tweets <- russian_tweets %>%
                select(text,
                        user_id, 
                        screen_name = user_key,
                        created_at,
                        retweet_count,
                        favorite_count,
                        tweet_id,
                        hashtags,
                        expanded_urls,
                        user_mentions = mentions,
                        retweeted_status_id) %>%
        add_column(doc_id = seq.int(nrow(russian_tweets)), .before = "text")

#View(tweets[1:100, ])
#View(russian_tweets[1:100, ])


glimpse(tweets[1,])
dim(tweets)

glimpse(russian_tweets[1,])
dim(russian_tweets)


str(tweets[1, ]$text, nchar.max = 140)
str(tweets[100, ]$text, nchar.max = 140)


# term frequencies of raw data with bag of words (not semantic parsing - thus, it would care about proper nouns):
term_count <- freq_terms(tweets$text, 20)
plot(term_count)
# what one would expect before removing the most common stop words

term_count_rus <- freq_terms(russian_tweets$text, 20)
plot(term_count_rus)
# again, not so interesting.


sum(grepl("^.+(â???¦)$",tweets$text))
sum(grepl("^.+(â???¦)$",russian_tweets$text))

incomp <- sum(grepl("^.+(â???¦)$",tweets$text))
(incomp / nrow(tweets)) * 100 

rus_incomp <-sum(grepl("^.+(â???¦)$",russian_tweets$text))
(rus_incomp / nrow(russian_tweets)) * 100 

tweets <- tweets[!grepl("^.+(â???¦)$", tweets$text), ]
nrow(tweets)

russian_tweets <- russian_tweets[!grepl("^.+(â???¦)$", russian_tweets$text), ]
nrow(russian_tweets)

# confirm:
identical((incomp + nrow(tweets) - incomp), nrow(tweets))
sum(grepl("^.+(â???¦)$",tweets$text))

identical((rus_incomp + nrow(russian_tweets) - rus_incomp), nrow(russian_tweets))
sum(grepl("^.+(â???¦)$",russian_tweets$text))


## remove mentions, links, and that same special character string before links

scrap.trash <- function(doc) {
        doc <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", doc)
        doc <- gsub("((?:\\b\\W*@\\w+)+)", "", doc)
        doc <- gsub("((?:\\b\\W*#\\w+)+)", "", doc)
        doc <- gsub("((?:\\b\\W*http\\w+)+)", "", doc)
        doc <- gsub("((?:\\b\\W*://\\w+)+)", "", doc)
        doc <- gsub("((?:\\b\\W*.co/\\w+)+)", "", doc)
}

tweets$text <- scrap.trash(tweets$text)

# not as a function:
#tweets$text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets$text)
#tweets$text <- gsub("((?:\\b\\W*@\\w+)+)", "", tweets$text)
#tweets$text <- gsub("((?:\\b\\W*#\\w+)+)", "", tweets$text)
#tweets$text <- gsub("((?:\\b\\W*http\\w+)+)", "", tweets$text)
#tweets$text <- gsub("((?:\\b\\W*://\\w+)+)", "", tweets$text)
#tweets$text <- gsub("((?:\\b\\W*.co/\\w+)+)", "", tweets$text)


# fix quotes
tweets$text <- gsub("â???T","'",tweets$text)
# do we need to do this? 
# it is important to tackle the ' problem so we can identify contractions. 


#tweets$text <- gsub("http\\w+", "", tweets$text)

#  we could keep doing this, but its not neccessary:
#tweets$text <- gsub("â???~","'",tweets$text)
#tweets$text <- gsub("ðY???ºðY???","",tweets$text)



# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", "", x)
tweets$text <- removeSpecialChars(tweets$text)


## get rid of data with nothing left in text field
tweets <- tweets[!(tweets[,2] == ""), ]

# fix doc_id column
tweets$doc_id <- seq.int(nrow(tweets))
#View(tweets[1:100, ])


## Language cleaning ##


# contractions 

############# EXAMPLE OF SOME MANUAL WAYS:  DON"T USE  #######################

# function to expand contractions into their base words
# this could be used instead of making a corpus out of the matrix - one option. 
expand.contractions <- function(doc) {
                doc <- gsub("won't", "will not", doc)
                doc <- gsub("wont", "will not", doc)
                doc <- gsub("can't", "can not", doc)
                doc <- gsub("cant", "can not", doc)
                doc <- gsub("n't", " not", doc)
                doc <- gsub("'ll", " will", doc)
                doc <- gsub("'re", " are", doc)
                doc <- gsub("'ve", " have", doc)
                doc <- gsub("'m", " am", doc)
                doc <- gsub("'d", " would", doc)
                # 's could be 'is' or could be possessive: it has no expansion
                doc <- gsub("'s", "", doc)
                return(doc)
}


# remove special characters
#tweets$text <- sapply(tweets$text, removeSpecialChars)

##############################################################################


# won't use this option

# corpus = collection of docs
# two types in R: the permanent corpus, PCorpus, and the volatile corpus, VCorpus
# PCorpus = stored on disk, VCorpus = RAM

# R needs to be able to interpret each element in each record of the column as a document - done via functions within tm package
df_source <- DataframeSource(tweets)


# Convert df_source to a corpus
df_corpus <- VCorpus(df_source)        

# look at the corpus
# Convert df_source to a corpus: df_corpus
#df_corpus
# contains 2050 documents and 10 columns retained as metadata

# the corpus becomes a list of objects
# see first tweet by double indexing:
df_corpus[[1]][1]

# Now that the data is a corpus of docs, can use preprocessing tools from tm

# common functions:
# tolower() - not good if you need proper nouns like New York - from base R
# removePunctuation() - not good if you want to analyze use of emoticons
# removeNumbers() - bad if you want to mine quantities or financial data
# stripWhiteSpace() - remove extra white space
# removeWords() - remove most common words

# these functions are done via tm_map which takes the corpus and one of the preprocessing fucntions and transforms it
# functions not built into tm can also be passed to tm_map by wrapping them in content_transformer()

# the functions which do not require transformer are from the tm package not qdap
# tm_map allows preprocessing functions to be applied to a corpus

clean_corpus <- function(corpus){
        corpus <- tm_map(corpus, content_transformer(stripWhitespace))
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
        #corpus <- tm_map(corpus, content_transformer(removeSpecialChars))
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "trump", "Trump","rump", "nunes","Nunes", "devin", "Devin", "memo", "maga"))
        return(corpus)
}

# clean the corpus
clean_corp <- clean_corpus(df_corpus)
# look at a cleaned corpus doc
clean_corp[[11]][1]
# "south carolina  americans deserve better representation"

### stemming and stem completion

#corpus.copy <- clean_corp # to use as a stemming dictionary
#clean_corp <- tm_map(clean_corp, stemDocument, language = "english")

#complete_corp <- stemCompletion(clean_corp, corpus.copy)
#complete_corp <- tm_map(clean_corp, content_transformer(stemCompletion), dictionary = corpus.copy, type = "prevalent")


#stemCompletion2 <- function(x, dictionary) {
#        x <- unlist(strsplit(as.character(x), " "))
#        # Unexpectedly, stemCompletion completes an empty string to
        # a word in dictionary. Remove empty string to avoid above issue.
#        x <- x[x != ""]
#        x <- stemCompletion(x, dictionary=dictionary)
#        x <- paste(x, sep="", collapse=" ")
#        PlainTextDocument(stripWhitespace(x))
#}

#complete_corp <- lapply(clean_corp, stemCompletion, dictionary=corpus.copy)

#complete_corp <- Corpus(VectorSource(complete_corp))

#corpus <- tm_map(corpus, content_transformer(stemCompletion), dictionary = corpus.copy, lazy = TRUE)

inspect(clean_corp)
#inspect(complete_corp)



###### getting the data in a matrix format to analyze  ####

# document term matrix = transpose of term document matrix (obviously) #
# each document is a row, each word is a column
tweets_dtm <- DocumentTermMatrix(clean_corp)


# view it
#tweets_dtm

# put into matrix
tweets_m <- as.matrix(tweets_dtm)


#dimensions
dim(tweets_m)


#print a portion
tweets_m[50:55, 1000:1005]


# term-matrix document - terms in first column, docs as individual collumns #

tweets_tdm <- TermDocumentMatrix(clean_corp)

tweets_m_2 <- as.matrix(tweets_tdm)

dim(tweets_m_2)

tweets_m_2[1000:1010, 800:810]





#### VISUALIZATIONS AND WORD CLOUDS ####


# starting with frequent terms
term_frequency <- rowSums(tweets_m_2)
# sort in descending order
term_frequency <- sort(term_frequency, decreasing = TRUE)

# what are the most frequent terms?
term_frequency[1:10]
# plot freq terms
barplot(term_frequency[1:10], col = "orange", las = 2, main = "Ten most frequent terms")


## term frequency with qdap
# this is an option if you do not want to do all the preporcessing I did and just want to use the original data frame
# qdap has a different set of stop words compared to tm

## NOTE: this is without adding my additional stopewords I took out based on the very first freq plot

# Create frequency
frequency <- freq_terms(tweets$text, top = 10, at.least = 3, stopwords = "Top200Words")

# Make a frequency barchart
plot(frequency)

# Create frequency2
frequency2 <- freq_terms(tweets$text, top = 10, at.least = 3, stopwords = tm::stopwords("english"))

# Make a frequency2 barchart
plot(frequency2)

## Compare the two: second is better in my oppinion


### Word Clouds 
# more visually engaging, size = freq

# MUST choose stop words correctly. Terms which ought to be frequent in the docs need to be removed
#  to allow unexpected words to pop out. 

# also have to adjust the cleaning function. If we want proper nouns then tolower() does not work.

# Create word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency, ncol=2)

# Making use of the color brewer in R 

purple_orange <- brewer.pal(10, "PuOr")
#drop 2 most faint colours
purple_orange <- purple_orange[-(1:2)]

green_pal <- brewer.pal(8, "Greens")
green_pal <- green_pal[-(1:2)]

pal <- brewer.pal(8,"YlGnBu")

colors <- c("grey80", "darkgoldenrod1", "tomato")
colors2 <- colorRampPalette(brewer.pal(9,"Blues"))(32)[seq(8,32,6)]

# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term, 
          word_freqs$num, 
          scale = c(3,.5), 
          max.words = 200, 
          random.order = FALSE, 
          random.color = FALSE, 
          colors = colors)

wordcloud(word_freqs$term, 
          word_freqs$num, 
          scale = c(3,.5), 
          max.words = 100, 
          random.order = FALSE, 
          random.color = FALSE, 
          colors = colors2)

wordcloud(word_freqs$term, 
          word_freqs$num, 
          scale = c(3,.5), 
          max.words = 100, 
          random.order = FALSE, 
          random.color = FALSE, 
          colors = purple_orange)

wordcloud(word_freqs$term, 
          word_freqs$num, 
          scale = c(3,.5), 
          max.words = 100, 
          random.order = FALSE, 
          random.color = FALSE, 
          colors = green_pal)




# wrodcloud2
wordcloud2(word_freqs[1:300, ], size = .5)

wordcloud2(word_freqs[1:300, ], size = .6, color = rev(pal))
# not sure why this doesnt work as well



# modify the cleaning function:
clean_corpus2 <- function(corpus){
        corpus <- tm_map(corpus, content_transformer(stripWhitespace))
        corpus <- tm_map(corpus, removePunctuation)
        #corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
        corpus <- tm_map(corpus, content_transformer(removeSpecialChars))
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "trump", "trumps", "Trump","nunes","Nunes", "devin", "Devin", "memo", "maga"))
        return(corpus)
}


# repeat process with alteration
clean_corp2 <- clean_corpus2(df_corpus)
tweets_v2_tdm <- TermDocumentMatrix(clean_corp2)
tweets_m_v2 <- as.matrix(tweets_v2_tdm)
term_freq_v2 <- rowSums(tweets_m_v2)

# sort vals
term_freq_v2 <- sort(term_freq_v2, decreasing = T)

# not much change in common words
term_freq_v2[1:6]

# put in data frame of freq words
word_freqs_v2 <- data.frame(term = names(term_freq_v2),num = term_freq_v2, ncol =2 )

# Create a wordcloud
wordcloud(word_freqs_v2$term, word_freqs_v2$num, max.words = 50, colors = green_pal)
# Not very informative




