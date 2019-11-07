
## Multiple words

# the default tdm and dtm are made with single words - can give new insights or improve machine learning alg performance
# so far, only looked at unigrams
# can change tokens to more words - will INCREASE SIZE of the matrixes!

# make a tokenizer function with RWeka package and then pass that function into the TermDocumentMatrix()


# Bigram tokenizer function:
tokenizer <- function (x) {
        NGramTokenizer(x, Weka_control(min = 2, max =2))
}



# Create bigram_dtm
bigram_dtm <- DocumentTermMatrix(
        clean_corp,
        control = list(tokenizer = tokenizer)
)

# look at it
bigram_dtm

# Create bigram_dtm_m
bigram_dtm_m <- as.matrix(bigram_dtm)

# Create freq
freq <- colSums(bigram_dtm_m)

# Create bi_words
bi_words <- names(freq)

# Examine part of bi_words
bi_words[2577:2597]

# Plot a wordcloud
wordcloud(bi_words, freq, max.words = 20)



## with the second version of the corpus
bigram_dtm_2 <- DocumentTermMatrix(
        clean_corp2,
        control = list(tokenizer = tokenizer)
)

# look at it
bigram_dtm_2

# Create bigram_dtm_m
bigram_dtm_m_2 <- as.matrix(bigram_dtm_2)

# Create freq
freq_2 <- colSums(bigram_dtm_m_2)

# Create bi_words
bi_words_2 <- names(freq_2)

# Examine part of bi_words
bi_words_2[2577:2597]

# Plot a wordcloud
wordcloud(bi_words_2, freq_2, max.words = 20)





## with russia data:

# Create rus_bigram_dtm
rus_bigram_dtm <- DocumentTermMatrix(
        rus_clean_corp,
        control = list(tokenizer = tokenizer)
)

# look at it
rus_bigram_dtm

# Create bigram_dtm_m
rus_bigram_dtm_m <- as.matrix(rus_bigram_dtm)

# Create freq
rus_freq <- colSums(rus_bigram_dtm_m)

# Create bi_words
rus_bi_words <- names(rus_freq)

# Examine part of bi_words
rus_bi_words[2577:2597]

# Plot a wordcloud
wordcloud(rus_bi_words, rus_freq, max.words = 30)


### with the more cleaned russian corpus

rus_bigram_dtm_2 <- DocumentTermMatrix(
        rus_clean_corp3,
        control = list(tokenizer = tokenizer)
)

# look at it
rus_bigram_dtm_2

# Create bigram_dtm_m
rus_bigram_dtm_m_2 <- as.matrix(rus_bigram_dtm_2)

# Create freq
rus_freq_2 <- colSums(rus_bigram_dtm_m_2)

# Create bi_words
rus_bi_words_2 <- names(rus_freq_2)

# Examine part of bi_words
rus_bi_words_2[2577:2597]

# Plot a wordcloud
wordcloud(rus_bi_words_2, rus_freq_2, max.words = 30)



## clustering?

#bigram_tdm <- TermDocumentMatrix(
#        clean_corp,
#        control = list(tokenizer = tokenizer)
#)

#bigram_tdm_m<- as.matrix(bigram_tdm)
# this does work - needs to be a term doc mat
#bigram_tdm_df <- as.data.frame(bigram_tdm_m)

#bigram_tdm_dist <- dist(bigram_tdm_df)

#bigram_td_HC <- hclust(bigram_tdm_dist)

#plot(bigram_td_HC)




## Term weightings 

# until now only looked at freqency of words
# can instead give words weightings through a penalty algorithm 
# TflIdf (term frequency-inverse document frequency) - essentially counts terms and gives penalities to terms appearing in many documents
# why? Idea is that words that are both common and across all docs have little informational value

# TfIdf will diminish the importance of these terms
# Doing this should mean that we don't need to add the extra terms to our stop word dict to remove 

TfIdf_clean_corpus <- function(corpus){
        corpus <- tm_map(corpus, content_transformer(stripWhitespace))
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
        #corpus <- tm_map(corpus, content_transformer(removeSpecialChars))
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "maga"))  # notice I removed the extras
        return(corpus)
}


TfIdf_clean_corp <- TfIdf_clean_corpus(df_corpus)
TfIdf_tweets_tdm <- TermDocumentMatrix(TfIdf_clean_corp, control = list(weighting = weightTfIdf))

# view it
#tweets_dtm

# put into matrix
TfIdf_tweets_m <- as.matrix(TfIdf_tweets_tdm)

#dimensions
dim(TfIdf_tweets_m)
#print a portion
TfIdf_tweets_m[50:55, 100:105]


# starting with frequent terms
TfIdf_term_frequency <- rowSums(TfIdf_tweets_m)
# sort in descending order
TfIdf_term_frequency <- sort(TfIdf_term_frequency, decreasing = TRUE)

# what are the most frequent terms?
TfIdf_term_frequency[1:10]
# plot freq terms
barplot(TfIdf_term_frequency[1:10], col = "light blue", las = 2, main = "Term frequency-inverse document frequency \n Ten most frequent words(our tweets)")

# compared to the original:
barplot(rus_term_frequency[1:10], col = "orange", las = 2, main = "Ten most frequent terms (our tweets)")
# with stop words removed:
plot(frequency2)


# Create word_freqs
TfIdf_word_freqs <- data.frame(term = names(TfIdf_term_frequency), num = TfIdf_term_frequency, ncol=2)

wordcloud(TfIdf_word_freqs$term, 
          TfIdf_word_freqs$num, 
          scale = c(3,.5), 
          max.words = 100, 
          random.order = FALSE, 
          random.color = FALSE, 
          colors = colors2)



# clustering with this data
# more terms kept
TfIdf_tdm1 <- removeSparseTerms(TfIdf_tweets_tdm, sparse = 0.975)

# less terms kept
TfIdf_tdm2 <- removeSparseTerms(TfIdf_tweets_tdm, sparse = 0.985)

# check sparsities
TfIdf_tdm1
TfIdf_tdm2


# With the first tdm
# to get distances, need ot convert to matrix as we did before, and then to a dataframe
TfIdf_tdm1_m<- as.matrix(TfIdf_tdm1)
TfIdf_tdm1_df <- as.data.frame(TfIdf_tdm1_m)
# distances
TfIdf_tweets_dist <- dist(TfIdf_tdm1_df)

# heirachical clust
TfIdf_hc <- hclust(TfIdf_tweets_dist)

# Plot the dendrogram
plot(TfIdf_hc)


# second tdm
TfIdf_tdm2_m<- as.matrix(TfIdf_tdm2)
TfIdf_tdm2_df <- as.data.frame(TfIdf_tdm2_m)
# distances
TfIdf_tweets_dist2 <- dist(TfIdf_tdm2_df)

# heirachical clust
TfIdf_hc2 <- hclust(TfIdf_tweets_dist2)

# Plot the dendrogram
plot(TfIdf_hc2)


# very similar



### Retaining metadata in the doc

# do we want to capture the meta data??? Probably?

# automtically done with this latest version of tm.




