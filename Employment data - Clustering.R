
# European Employment Data  -----------------------------------------------


# Packages ----------------------------------------------------------------

library(ggplot2)      
library(dplyr)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(dendextend) # for comparing two dendrograms
library(corrplot)
library(arules)



# Loading data ------------------------------------------------------------

employment <- read.csv("C:/Users/Vishnu/Desktop/Spring 2020/Data Mining/europeanJobs.txt", sep="\t")
head(employment)


#get country name and scale variables
row.names(employment)<-employment$Country
head(employment)

head(employment)
glimpse(employment)


jobs_gathered <- gather(employment ,"industry","percentage",-c("Country"))
industry_boxplot <- jobs_gathered %>%
  ggplot(aes(x = industry , y = percentage, fill=industry)) +
  geom_boxplot(alpha=0.5) + theme(legend.position="none") +
  xlab("Industry") + ylab("Percentage employed")+
  geom_jitter(alpha = 0.4)
industry_boxplot

employment <- employment[,-1]

# Scale -------------------------------------------------------------------

set.seed(13255870)
index <- sample(nrow(employment), size = 0.8*nrow(employment))
employment <- employment[index,]

employment <- scale(employment)

corrplot(cor(employment), order = "hclust")



# Calculating the distance matrix -----------------------------------------

#distance matrix
distance <- get_dist(employment)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


# K-Means Clustering ------------------------------------------------------

#kmeans with diff k vals
k2 <- kmeans(employment, centers = 2, nstart = 25)
k3 <- kmeans(employment, centers = 3, nstart = 25)
k4 <- kmeans(employment, centers = 4, nstart = 25)
k5 <- kmeans(employment, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point",  data = employment) + ggtitle("clusters = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = employment) + ggtitle("clusters = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = employment) + ggtitle("clusters = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = employment) + ggtitle("clusters = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 3)



# Setting number of clusters ----------------------------------------------

#selecting number of clusters

fviz_nbclust(employment, kmeans, method = "wss")

k.max <- 10

wss <- sapply(1:k.max, 
              function(k){ kmeans(employment, k)$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


fviz_nbclust(employment, kmeans, method = "silhouette") 



# K means with 3 clusters -------------------------------------------------

#creating final kmeans model
set.seed(13255870)
final <- kmeans(employment, 3, nstart = 25)

fviz_cluster(final, geom = c("point", "text"),  data = employment) + ggtitle("Clustering with 3 groups")


#comparing cluster wise
k_means_cluster <- cbind(employment, kmeans_cluster = final$cluster)
cluster <- aggregate(k_means_cluster,by=list(final$cluster),mean)
cluster



centers_df <- round(as.data.frame(kmeans_clustering$centers))
centers_df$cluster <- rownames(centers_df)
centers_df <- select(centers_df, cluster, everything())
kable(centers_df)



# Hierarchical CLustering -------------------------------------------------


# Dissimilarity matrix
diss_matrix <- dist(employment, method = "euclidean")
# Hierarchical clustering using Complete Linkage
hcluster <- hclust(diss_matrix, method = "complete" )
# Plot the obtained dendrogram
plot(hcluster, cex = 0.6, hang = -1)


fviz_dend(hcluster,k = 3,
          cex = 0.5, # label size
          k_colors = c( "#00AFBB", "#E7B800","#CC00FFFF"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE, # Add rectangle around groups
          rect_border = c("#00AFBB", "#E7B800","#CC00FFFF"), 
          rect_fill = TRUE)


fviz_nbclust(employment, FUN = hcut, method = "wss")

fviz_nbclust(employment, FUN = hcut, method = "silhouette")



# Final model with 2 clusters ---------------------------------------------

#creating final model
seed.2clust = cutree(hcluster,k=2)
table(seed.2clust)


#plots
plot(hcluster, cex = 0.6, hang=-1)
rect.hclust(hc1, k = 2, border = 2:5)



# plot --------------------------------------------------------------------

fviz_cluster(list(data = employment, cluster = seed.2clust))



distance <- dist(subset[,-1])

#Obtain clusters using the Wards method
hierarchical_clustering <- hclust(distance, method="ward.D")

plot(hierarchical_clustering)
hierarchical_clustering_3_clusters = cutree(hierarchical_clustering,k=2)

employment$cluster <- hierarchical_clustering_3_clusters

employment %>%group_by(cluster) %>% summarise_all(funs(mean)) %>% round() %>% kable()
