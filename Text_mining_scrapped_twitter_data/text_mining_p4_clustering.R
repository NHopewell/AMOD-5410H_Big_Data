
## Clustering and more

# idea is to find interesting word clusters

# dendograms can be generated from term document matrixes by calling dist() to compute distances between each row
# cluster based on the dissimilarities of the distance matrix
# IMPORTANT: remember that denograms REDUCE information so that clustering can be interpretable
# IMPORTANT: many text clusters do not make sense, but sometimes, key insights can be drawn from the clusters

# Issue number one: term doc mats and doc term mats are very spare - mostly 0's
# this means a dendogram would be impossibe to interpret - overflowing with branches 

# solution - must trim sparse terms with removeSparseTerms() - the lower the 'sparse' value, the more terms are kept
# ideal number of terms is around 25 - 70


# before
dim(tweets_tdm)

# more terms kept
tdm1 <- removeSparseTerms(tweets_tdm, sparse = 0.975)

# less terms kept
tdm2 <- removeSparseTerms(tweets_tdm, sparse = 0.985)

# check sparsities
tdm1
tdm2


# With the first tdm
# to get distances, need ot convert to matrix as we did before, and then to a dataframe
tdm1_m<- as.matrix(tdm1)
tdm1_df <- as.data.frame(tdm1_m)
# distances
tweets_dist <- dist(tdm1_df)

# heirachical clust
hc <- hclust(tweets_dist)

# Plot the dendrogram
plot(hc)


# second tdm
tdm2_m<- as.matrix(tdm2)
tdm2_df <- as.data.frame(tdm2_m)
# distances
tweets_dist2 <- dist(tdm2_df)

# heirachical clust
hc2 <- hclust(tweets_dist2)

# Plot the dendrogram
plot(hc2)


## improving the dendogram with denextend
hcd <- as.dendrogram(hc2)
labels(hcd)

# the labels to annotate
hcd <- branches_attr_by_labels(hcd, c("gop", "rep", "clear", "provides", "gosar", "evidence", "treason", "russia", "intelligence",
                                      "fbi", "investigation", "release", "house", "intel"), "red")
plot(hcd)




## clustering russian tweets


# before
dim(rus_tweets_tdm)

# more terms kept
rus_tdm1 <- removeSparseTerms(rus_tweets_tdm, sparse = 0.986)

# less terms kept
rus_tdm2 <- removeSparseTerms(rus_tweets_tdm, sparse = 0.991)

#rus_tdm3 <- removeSparseTerms(rus_tweets_tdm, sparse = 0.993)

# check sparsities
#rus_tdm1
#rus_tdm2
#rus_tdm3


# With the first tdm
# to get distances, need ot convert to matrix as we did before, and then to a dataframe
rus_tdm1_m<- as.matrix(rus_tdm1)
rus_tdm1_df <- as.data.frame(rus_tdm1_m)
# distances
rus_tweets_dist <- dist(rus_tdm1_df)

# heirachical clust
rus_hc <- hclust(rus_tweets_dist)

# Plot the dendrogram
plot(rus_hc)


# second tdm
rus_tdm2_m<- as.matrix(rus_tdm2)
rus_tdm2_df <- as.data.frame(rus_tdm2_m)
# distances
rus_tweets_dist2 <- dist(rus_tdm2_df)

# heirachical clust
rus_hc2 <- hclust(rus_tweets_dist2)

# Plot the dendrogram
plot(rus_hc2)


# third tdm
#rus_tdm3_m<- as.matrix(rus_tdm3)
#rus_tdm3_df <- as.data.frame(rus_tdm3_m)
# distances
#rus_tweets_dist3 <- dist(rus_tdm3_df)

# heirachical clust
#rus_hc3 <- hclust(rus_tweets_dist3)

# Plot the dendrogram
#plot(rus_hc3)



## improving the dendogram with denextend
rus_hcd <- as.dendrogram(rus_hc2)
labels(rus_hcd)

# the labels to annotate
rus_hcd <- branches_attr_by_labels(rus_hcd, c("america", "make", "clinton", "hillary", "back", "right", "now", 
                                               "media", "today", "world", "better", "white", "house", "nothing"), "red")
plot(rus_hcd)




## K-means clustering to cluster documents

# first get distance matrix

# figure out why this isnt working when you have time.

#dtm_m<- as.matrix(tweets_dtm)
#dtm_df <- as.data.frame(dtm_m)

# distances
#tweets_dist <- dist(dtm_df)

#km <- kmeans(dtm_m , 6)
#round(km$centers, digits = 3) 


### word associations

# findAssocs() of the tm package takes a word and finds its correlation to all other words in the tdm or dtm 
# range from 0-1, 1 = always appear together, 0 = never appear together
# specifyminimum correlation threshold 
# word diversity means correlations often quite low



# Create associations
associations <- findAssocs(tweets_tdm, "russia", 0.2)

# View the venti associations
associations

# Create associations_df
associations_df <- list_vect2df(associations)[, 2:3]

# Plot the associations_df values (don't change this)
ggplot(associations_df, aes(y = associations_df[, 1])) + 
        geom_point(aes(x = associations_df[, 2]), 
                   data = associations_df, size = 3) +
        ggtitle("words associated with 'republican' in our tweets")+
        xlab("correlation") +
        ylab("associated word") +
        theme_gdocs()



## from russian tweets
rus_associations <- findAssocs(rus_tweets_tdm, "russia", 0.2)

# View the venti associations
rus_associations

# Create associations_df
rus_associations_df <- list_vect2df(rus_associations)[, 2:3]

# Plot the associations_df values (don't change this)
ggplot(rus_associations_df, aes(y = rus_associations_df[, 1])) + 
        geom_point(aes(x = rus_associations_df[, 2]), 
                   data = rus_associations_df, size = 3) +
        ggtitle("words associated with 'russia' in russian tweets")+
        xlab("correlation") +
        ylab("associated word") +
        theme_gdocs()


## more associations
associations2 <- findAssocs(tweets_tdm, "donald", 0.2)

# View the venti associations
associations2

# Create associations_df
associations_df2 <- list_vect2df(associations2)[, 2:3]

# Plot the associations_df values (don't change this)
ggplot(associations_df2, aes(y = associations_df2[, 1])) + 
        geom_point(aes(x = associations_df2[, 2]), 
                   data = associations_df2, size = 3) + 
        ggtitle("words associated with 'donald' in our tweets")+
        xlab("correlation") +
        ylab("associated word") +
        theme_gdocs()




## super interersting one: democrats

rus_associations2 <- findAssocs(rus_tweets_tdm, "democrats", 0.2)

# View the venti associations
rus_associations2

# Create associations_df
rus_associations_df2 <- list_vect2df(rus_associations2)[, 2:3]

# Plot the associations_df values (don't change this)
ggplot(rus_associations_df2, aes(y = rus_associations_df2[, 1])) + 
        geom_point(aes(x = rus_associations_df2[, 2]), 
                   data = rus_associations_df2, size = 3) + 
        ggtitle("words associated with 'democrats' in russian tweets")+
        xlab("correlation") +
        ylab("associated word") +
        theme_gdocs()


# republicans
rus_associations3 <- findAssocs(rus_tweets_tdm, "republican", 0.2)

# View the venti associations
rus_associations3

# Create associations_df
rus_associations_df3 <- list_vect2df(rus_associations3)[, 2:3]

# Plot the associations_df values (don't change this)
ggplot(rus_associations_df3, aes(y = rus_associations_df3[, 1])) + 
        geom_point(aes(x = rus_associations_df3[, 2]), 
                   data = rus_associations_df3, size = 3) +
        ggtitle("words associated with 'republican' in russian tweets")+
        xlab("correlation") +
        ylab("associated word") +
        theme_gdocs()





