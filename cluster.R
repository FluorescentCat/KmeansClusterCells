
library(NbClust)
library(ggplot2)
library(ggthemes)
library(ggplot2) 
library(ggthemes)
library(cluster)
library(factoextra)
library(cluster)
library(purrr)
library(geometry)
library(car)
library(SIBER)





# # set the random seed generator so we get consistent results each time
# # we run this code.
# set.seed(2)
# 
# # n random numbers
# n <- 30
# 
# # some random multivariate data
# Y <- generateSiberGroup(n.obs = 30)
# # plot this example data with column 2 by column 1
# plot(Y[,2] ~ Y[,1], type = "p", asp = 1,
#      xlim = c(-4, 4),
#      ylim = c(-4, 4))
# # add an ellipse, in this case a 95% ellipse
# mu <- colMeans(Y) # centre of the ellipse
# Sigma <- cov(Y) # covariance matrix of the ellipse
# Sigma[1,2] = 0
# Sigma[2,1] = 0
# 
# # draw the ellipse
# p <- 0.95
# tmp <- addEllipse(mu, Sigma, p.interval = p, col = "red", lty = 2)
# 
# 


#/////////////////////////////////////

df = read.csv("C:/Users/AliceAbend/Desktop/auResults.csv")
df = df[,-1]

###################### Elbow Method

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k
k <- 3
data <- df[2:3]
wss <- sapply(1:k, function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})

# normalize and plot wss f
normal_wss = wss/sum(wss)

# to find the optimal clustering count
elbow_optimal_num <- 0
m_sum_wss = 0
for (val in normal_wss) {
  if(m_sum_wss <0.65)  
  {
    m_sum_wss=m_sum_wss+val
    elbow_optimal_num = elbow_optimal_num+1
  }
}

# display the result
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = elbow_optimal_num, linetype = 2)+
  labs(subtitle = "Elbow method")


#using different method for elbow to make sure results are correct 

#use map_dbl to run many models with varying value of k 
tot_withinss <- map_dbl(1:15, function(k) { 
  model <- kmeans(x = data, centers = k)
  
  model$tot.withinss
})

elbow_df <- data.frame ( 
  k = 1:15, 
  tot_withinss = tot_withinss 
)


###################### Clustering Script
clusters <- kmeans(df[2:3], k)


# Save the cluster number in the dataset as column 'Borough'
df$clusterId <- as.factor(clusters$cluster)


#//////////////////////////////////////////////////////////////

for (n in 1:k) {
  cluster_data = df[df[,'clusterId'] == n, ]
  cluster_data = cluster_data[2:3]
  mu <- colMeans(cluster_data)
  Sigma <- cov(cluster_data)
  area = pi*sqrt(Sigma[1,1])*sqrt(Sigma[2,2])
  cat("Cluster", n," Area: ",area,"\n")
}


#//////////////////////////////////////////////////////////////

m_color=c("#999999","#E69F00","#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#A09999","#B99F00","#E6E4E9", "#777E73", "#D1A142", "#33AAB2", "#99CC00")

fviz_cluster(clusters, data = df[2:3], 
             ellipse.type = "norm",
             ellipse.level = 0.99,
             
             palette = m_color,
             geom = "point",
             axes = c(0,0), 
             show.clust.cent = TRUE,
             ggtheme = theme_minimal() 
)



ellipse.type 
ellipse.level

clusters$totss
clusters$size
clusters$centers
clusters$withinss
clusters$betweenss

#finding area aka convex hull of clusters 



# now using silhouette anaylsis 
sil_width <- map_dbl(2:15, function(k) {
  model <- pam(x = data, k = k)
  model$silinfo$avg.width
})
sil_df <- data.frame(
  k = 2:15, 
  sil_width = sil_width
)

print(sil_df)

sil <- silhouette(clusters$cluster, dist(df))
fviz_silhouette(sil)
