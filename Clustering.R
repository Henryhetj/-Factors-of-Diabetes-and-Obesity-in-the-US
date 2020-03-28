library (readxl)
Data <- read_excel ("Downloads/Data.xlsx", 
                      +     sheet = "Sheet4")
View(Data)                                                                     
install.packages('flexclust')
library(flexclust)
ds<-scale(Data[-1])
library(factoextra)

wssplot <- function(data, nc=15, seed=1234){
  +     wss <- (nrow(data)-1)*sum(apply(data,2,var))
  +     for(i in 2:nc){
    +         set.seed(seed)
    +         wss[i] <- sum(kmeans(data, centers=i)$withinss)
    +     }
  +     plot(1:nc, wss, type="b", xlab="Number of Clusters",
             +          ylab="Within groups sum of squares")
  + }
wssplot(ds)
library(NbClust)
nc<-NbClust(t(ds),distance="euclidean",min.nc=2,max.nc=15,method="average",index="duda")
nc$Best.nc

fviz_nbclust(ds, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)

#That means we should choose 3 as k for hierarchical clustering and 4 as k for kmeans.

#K means Cluster

set.seed(123)
km_result <- kmeans(ds, 4, nstart = 24)
print(km_result)
#K-means clustering with 4 clusters of sizes 3, 23, 18, 7

dd <- cbind(Data, cluster = km_result$cluster)
head(dd)
table(dd$cluster)

fviz_cluster(km_result, data = ds,palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),ellipse.type = "euclid",star.plot = TRUE, repel = TRUE,ggtheme = theme_minimal())

#Hierarchy Cluster
result <- dist(ds, method = "euclidean")
result_hc <- hclust(d = result, method = "ward.D2")
install.packages("robustbase")
library(robustbase)
fviz_dend(result_hc, cex = 0.6)

fviz_dend(result_hc, k = 3, cex = 0.5,  k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),color_labels_by_k = TRUE, rect = TRUE)

