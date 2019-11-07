###  NOW TO CLEAN UP THE RUSSIAN TWEET DATA

# lets take a subset of the data

russian_tweets <- russian_tweets[1:2050, ]


russian_tweets$text <- scrap.trash(russian_tweets$text)

# fix quotes
russian_tweets$text <- gsub("â???T","'",russian_tweets$text)

russian_tweets$text <- removeSpecialChars(russian_tweets$text)


## get rid of data with nothing left in text field
russian_tweets<- russian_tweets[!(russian_tweets[,2] == ""), ]

# fix doc_id column
russian_tweets$doc_id <- seq.int(nrow(russian_twee  ts))
#View(tweets[1:100, ])

View(russian_tweets[1:100, ])

# does not appear to have the exact same issues with quotes and stuff

# remove rows with no info now in the text column after cleaning
russian_tweets <- russian_tweets[!(russian_tweets[,2] == ""), ]

# fix doc_id column
tweets$doc_id <- seq.int(nrow(tweets))


## Language cleaning ##

# rerun because I overwrote the object
clean_corpus <- function(corpus){
        corpus <- tm_map(corpus, content_transformer(stripWhitespace))
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
        corpus <- tm_map(corpus, content_transformer(removeSpecialChars))
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "trump", "Trump","rump", "nunes","Nunes", "devin", "Devin", "memo", "maga"))
        return(corpus)
}


# R needs to be able to interpret each element in each record of the column as a document - done via functions within tm package
rus_df_source <- DataframeSource(russian_tweets)
# Convert df_source to a corpus
rus_df_corpus <- VCorpus(rus_df_source) 

rus_df_corpus[[1]][1]
# lmao, nice

# using the cleaning function mad earlier 
# clean the corpus
rus_clean_corp <- clean_corpus(rus_df_corpus)
# look at a cleaned corpus doc
rus_clean_corp[[11]][1]


### document term matrix
rus_tweets_dtm <- DocumentTermMatrix(rus_clean_corp)
# put into matrix
rus_tweets_m <- as.matrix(rus_tweets_dtm)

#dimensions
dim(rus_tweets_m)


#print a portion
rus_tweets_m[50:55, 1000:1005]


### term-matrix document - terms in first column, docs as individual collumns #

rus_tweets_tdm <- TermDocumentMatrix(rus_clean_corp)
rus_tweets_m_2 <- as.matrix(rus_tweets_tdm)
dim(rus_tweets_m_2)
rus_tweets_m_2[1000:1010, 800:810]


#### VISUALIZATIONS AND WORD CLOUDS ####


# starting with frequent terms
rus_term_frequency <- rowSums(rus_tweets_m_2)
# sort in descending order
rus_term_frequency <- sort(rus_term_frequency, decreasing = TRUE)

# what are the most frequent terms?
rus_term_frequency[1:10]
# plot freq terms
barplot(rus_term_frequency[1:10], col = "orange", las = 2, main = "Ten most frequent terms")

# so - the word 'http' is the third most common word in the tweets even though removing url's is baked into my scrap.trash function


### wordclouds

# Create word_freqs
rus_word_freqs <- data.frame(term = names(rus_term_frequency), num = rus_term_frequency, ncol=2)


# Create a wordcloud for the values in word_freqs
wordcloud(rus_word_freqs$term, 
          rus_word_freqs$num, 
          scale = c(3,.5), 
          max.words = 200, 
          random.order = FALSE, 
          random.color = FALSE, 
          colors = colors)

wordcloud(rus_word_freqs$term, 
          rus_word_freqs$num, 
          scale = c(3,.5), 
          max.words = 100, 
          random.order = FALSE, 
          random.color = FALSE, 
          colors = colors2)

wordcloud(rus_word_freqs$term, 
          rus_word_freqs$num, 
          scale = c(3,.5), 
          max.words = 100, 
          random.order = FALSE, 
          random.color = FALSE, 
          colors = purple_orange)


# wrodcloud2
wordcloud2(rus_word_freqs[1:300, ], size = .5)
wordcloud2(rus_word_freqs[1:300, ], size = .6, color = rev(pal))

# modify the cleaning function: This time I will take nothin additional out and not transform to lower
rus_clean_corpus <- function(corpus){
        corpus <- tm_map(corpus, content_transformer(stripWhitespace))
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
        corpus <- tm_map(corpus, content_transformer(removeSpecialChars))
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
        return(corpus)
}


# repeat process with alteration
rus_clean_corp2 <- rus_clean_corpus(rus_df_corpus)
rus_tweets_v2_tdm <- TermDocumentMatrix(rus_clean_corp2)
rus_tweets_m_v2 <- as.matrix(rus_tweets_v2_tdm)
rus_term_freq_v2 <- rowSums(rus_tweets_m_v2)

# sort vals
rus_term_freq_v2 <- sort(rus_term_freq_v2, decreasing = T)

# not much change in common words
rus_term_freq_v2[1:6]

# put in data frame of freq words
rus_word_freqs_v2 <- data.frame(term = names(rus_term_freq_v2),num = rus_term_freq_v2, ncol =2 )

# Create a wordcloud
wordcloud(rus_word_freqs_v2$term, rus_word_freqs_v2$num, max.words = 50, colors = colors)
wordcloud(rus_word_freqs_v2$term, rus_word_freqs_v2$num, max.words = 50, colors = colors2)



# remove most common words and try again
# rerun because I overwrote the object
rus_clean_corpus_v2 <- function(corpus){
        corpus <- tm_map(corpus, content_transformer(stripWhitespace))
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
        corpus <- tm_map(corpus, content_transformer(removeSpecialChars))
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "trump", "clinton", "hillary", "obama"))
        return(corpus)
}

# removing political figures which we would expect in these tweets


# repeat process with alteration
rus_clean_corp3 <- rus_clean_corpus_v2(rus_df_corpus)
rus_tweets_v3_tdm <- TermDocumentMatrix(rus_clean_corp3)
rus_tweets_m_v3 <- as.matrix(rus_tweets_v3_tdm)
rus_term_freq_v3 <- rowSums(rus_tweets_m_v3)

# sort vals
rus_term_freq_v3 <- sort(rus_term_freq_v3, decreasing = T)

# not much change in common words
rus_term_freq_v3[1:6]

# put in data frame of freq words
rus_word_freqs_v3 <- data.frame(term = names(rus_term_freq_v3),num = rus_term_freq_v3, ncol =2 )

# Create a wordcloud
wordcloud(rus_word_freqs_v3$term, rus_word_freqs_v3$num, max.words = 50, colors = colors)
wordcloud(rus_word_freqs_v3$term, rus_word_freqs_v3$num, max.words = 50, colors = colors2)


# Create a wordcloud for the values in word_freqs
wordcloud(rus_word_freqs_v3$term, 
          rus_word_freqs_v3$num, 
          scale = c(3,.5), 
          max.words = 200, 
          random.order = FALSE, 
          random.color = FALSE, 
          colors = colors)

wordcloud(rus_word_freqs_v3$term, 
          rus_word_freqs_v3$num, 
          scale = c(3,.5), 
          max.words = 100, 
          random.order = FALSE, 
          random.color = FALSE, 
          colors = colors2)

wordcloud(rus_word_freqs_v3$term, 
          rus_word_freqs_v3$num, 
          scale = c(3,.5), 
          max.words = 100, 
          random.order = FALSE, 
          random.color = FALSE, 
          colors = purple_orange)

