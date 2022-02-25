library ("factoextra")
library ("NbClust")

setwd("C:/Users/vivia/Desktop")


df <-read.csv("noc_quadrant_data.csv", header=TRUE, row.names="noc")

df<-scale(df)

elbow <- fviz_nbclust(df, kmeans, method = "wss") + 
      geom_vline(xintercept = 4, linetype = 2)+
      labs(subtitle = "Elbow method")
sil <- fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

gap <- fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

nbclust <- NbClust(data = df, distance = "euclidean",
        min.nc = 2, max.nc = 12, method = "kmeans")

km.res <- kmeans(df, 5, nstart = 50)
aggregate(df, by=list(cluster=km.res$cluster), mean)

c.assign <- cbind(df, cluster = km.res$cluster)
head(c.assign)


km.res$size


write.csv(c.assign, "clusters_scaled.csv")
