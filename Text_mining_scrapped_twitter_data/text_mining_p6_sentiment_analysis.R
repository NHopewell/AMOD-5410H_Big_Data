
# sentiment analysis


tweets <- read.csv("tweets.csv", header = T, stringsAsFactors = F)

geo_tweets <- tweets %>%
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

tweets_tdm 

our_tweets_df <- as.data.frame(tweets_m_2)

View(our_tweets_df[1:20, 1:20])
