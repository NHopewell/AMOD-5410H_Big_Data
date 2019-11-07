## More words clouds and comparision

## IMPORTANT !!###
# should use first versions before altering the cleaning function for the first corpus
# the second should be the third version


## Common words across both corpus ##

# start by collapsing all tweets from each data source into one vector, seperating by " "

# Create all_coffee
all_tweets <- paste(tweets$text, collapse = " ")
# Create all_chardonnay
all_rus_tweets <- paste(russian_tweets$text, collapse = " ")


# combine them together
complete_tweets <- c(all_tweets, all_rus_tweets)

# convert to a vector source
complete_tweets <- VectorSource(complete_tweets)

# Create all_corpus
all_corpus <- VCorpus(complete_tweets)



## clean the combined corpus

# one basic to compare with nothign removed

all_clean_corpus <- function(corpus){
        corpus <- tm_map(corpus, content_transformer(stripWhitespace))
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
        corpus <- tm_map(corpus, content_transformer(removeSpecialChars))
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
        return(corpus)
}

# one to clean from both

all_clean_corpus_complete <- function(corpus){
        corpus <- tm_map(corpus, content_transformer(stripWhitespace))
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
        corpus <- tm_map(corpus, content_transformer(removeSpecialChars))
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "trump", "trumps", "rump", "Trump","nunes","Nunes", "devin", "Devin", 
                                                                "memo", "clinton", "hillary", "obama", "memo", "maga"))
        return(corpus)
}


# Clean the corpus
all_clean <- all_clean_corpus(all_corpus)
# Create all_tdm
all_tdm <- TermDocumentMatrix(all_clean)
# Create all_m
all_m <- as.matrix(all_tdm)
# Print a commonality cloud
commonality.cloud(all_m, max.words = 100, colors = "steelblue")


## with the second

# Clean the corpus
all_clean2 <- all_clean_corpus_complete(all_corpus)
# Create all_tdm
all_tdm2 <- TermDocumentMatrix(all_clean2)
# Create all_m
all_m2 <- as.matrix(all_tdm2)
# Print a commonality cloud
commonality.cloud(all_m2, max.words = 100, colors = "steelblue")

# NOTE: Can be misleading as words can be represented disproportionately in one corpus or the other. Even if they are shared.




## Dissimilar words  ##

# Give the columns distinct names
colnames(all_tdm) <- c("our tweets", "Russian tweets")
colnames(all_tdm2) <- c("our tweets", "Russian tweets")

# Create all_m
all_m <- as.matrix(all_tdm)
all_m2 <- as.matrix(all_tdm2)

# Create comparison cloud
comparison.cloud(all_m, max.words = 50, colors = c("orange", "blue"))
comparison.cloud(all_m2, max.words = 50, colors = c("orange", "blue"))

# Key observartion:  Our tweets have more unqiue words not found in the russian tweets
#                    Russian tweets don't have many unique works. And those which are are interesting.




## Polarized tag cloud ##

#   plotrix package
# this solves the problem of the communiality cloud which does not tell you the disproportion of representation
# of words in each corpus

# make a replica of all matrix

# using version 2 with the complete cleaning function:
all_tdm_m <- as.matrix(all_tdm2)
# recall it has 2 columns - one for term freq in our tweets, one for freq in russian tweets
dim(all_tdm_m)
colnames(all_tdm_m)

# thus, can be subset to get terms appearing one or more times in both corpus'
# becuse the matrix stores frequencies, it is easy to get the difference in word representation in each corpus 
# add this as a new column and order the values to get out terms which are differently represented in each corpus'
# The top words can be pulled out and plotted as a pyramid plot


# Create common_words
common_words <- subset(all_tdm_m, all_tdm_m[, 1] > 0 & all_tdm_m[, 2] > 0)

# Create difference
difference <- abs(common_words[, 1] - common_words[, 2])

# Combine common_words and difference
common_words <- cbind(common_words, difference)

# Order the data frame from most differences to least
common_words <- common_words[order(common_words[, 3], decreasing = TRUE), ]

# Create top25_df
top25_df <- data.frame(x = common_words[1:25, 1], 
                       y = common_words[1:25, 2], 
                       labels = rownames(common_words[1:25, ]))

# Create the pyramid plot
pyramid.plot(top25_df$x, top25_df$y,
             labels = top25_df$labels, gap = 8,
             top.labels = c("Our Tweets", "", "Russian Tweets"),
             main = "Words in Common", laxlab = NULL, 
             raxlab = NULL, unit = NULL)

# Why is this? Beucase russian bots have less characters?
# the data frames are the same size 
nrow(tweets)
nrow(russian_tweets)



## Word network ##

# visualizing connections between words

# Word association for our tweets
word_associate(tweets$text, match.string = c("vindicate"), 
               stopwords = c(tm::stopwords("english")), 
               network.plot = TRUE, cloud.colors = c("gray85", "darkred"))
title(main = "Associations to 'vindicate' in our tweets")


word_associate(tweets$text, match.string = c("vindicate"), 
               stopwords = c(tm::stopwords("english"), "trump", "Trump","rump", "nunes","Nunes", "devin", "Devin", "memo", "maga"), 
               network.plot = TRUE, cloud.colors = c("gray85", "darkred"))
title(main = "Associations to 'vindicate' in our tweets")



word_associate(tweets$text, match.string = c("help"), 
               stopwords = c(tm::stopwords("english"), "trump", "Trump","rump", "nunes","Nunes", "devin", "Devin", "memo", "maga"), 
               network.plot = TRUE, cloud.colors = c("gray85", "darkred"))
title(main = "Associations to 'help' in our tweets")


## Looking at trump
word_associate(tweets$text, match.string = c("trump"), 
               stopwords = c(tm::stopwords("english"),"trump", "clinton", "hillary", "obama"), 
               network.plot = TRUE, cloud.colors = c("gray85", "darkred"))
title(main = "Associations to 'trump' in our tweets")



## russian data
word_associate(russian_tweets$text, match.string = c("trump"), 
               stopwords = c(tm::stopwords("english")), 
               network.plot = TRUE, cloud.colors = c("gray85", "darkred"))
title(main = "Associations to 'trump' in Russian tweets")

## what does this mean? All their tweets are about trump


word_associate(russian_tweets$text, match.string = c("Obama"), 
               stopwords = c(tm::stopwords("english")), 
               network.plot = TRUE, cloud.colors = c("gray85", "darkred"))
title(main = "Associations to 'Obama' in Russian tweets")



word_associate(russian_tweets$text, match.string = c("Clinton"), 
               stopwords = c(tm::stopwords("english")), 
               network.plot = TRUE, cloud.colors = c("gray85", "darkred"))
title(main = "Associations to 'Clinton' in Russian tweets")



word_associate(russian_tweets$text, match.string = c("election"), 
               stopwords = c(tm::stopwords("english")), 
               network.plot = TRUE, cloud.colors = c("gray85", "darkred"))
title(main = "Associations to 'election' in Russian tweets")

# same with more stop words:
word_associate(russian_tweets$text, match.string = c("election"), 
               stopwords = c(tm::stopwords("english"), "trump", "clinton", "hillary", "obama"), 
               network.plot = TRUE, cloud.colors = c("gray85", "darkred"))
title(main = "Associations to 'election' in Russian tweets")


