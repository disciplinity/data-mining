load(file="C:/Users/Daiy/Desktop/iti8730-data_mining/kdata.RData")


minkowski_distance <- function(v1,v2,p) {
  return(sum(abs(v1-v2)^p)^(1/p))
}

k_means <- function(dt,k,p) {
  centroids<-dt[sample.int(nrow(dt),k),]
  old_centroids<-rep(Inf, nrow(centroids))
  cluster<-rep(0,nrow(dt))
  
  while(!identical(centroids,old_centroids))
  {
    old_centroids<-centroids
    for(i in seq(along=dt[,1]))
    {
      d_min<-Inf
      for(j in 1:nrow(centroids))
      {
        d <- minkowski_distance(centroids[j,],dt[i,],p)
        if (d < d_min)
        {
          cluster[[i]]<-j
          d_min<-d
        }
      }
    }
    
    for(i in 1:nrow(centroids))
    {
      centroids[i,]=apply(dt[cluster==i,],2,mean)
    }
  }
  return(list(data=data.frame(dt,cluster),centroids=centroids))
}



my_data<-res$data


# Evaluate performance
# ----------------------------------------------

sum_of_distances_from_point_to_other_points<-function(d,p,calcMean=FALSE) {
  distances <- matrix(,nrow=nrow(d),ncol=1)
  for (i in 1:nrow(d))
  {
    from_i_to_j<-matrix(,nrow=nrow(d),ncol=1)
    for (j in 1:nrow(d))
    {
      from_i_to_j[j,1]<-minkowski_distance(
        c(d[i,1],d[i,2]),
        c(d[j,1],d[j,2]),p)
    }
    if (calcMean) distances[i,1]<-mean(from_i_to_j)
    else distances[i,1]<-colSums(from_i_to_j)
  }
  return(distances)
}

points_from_first_cluster <-my_data[which(my_data$cluster=='1'),]
points_from_other_clusters<-my_data[which(my_data$cluster!='1'),]

intra_inter_ratio<-function(p){
  intra<-colSums(sum_of_distances_from_point_to_other_points(points_from_first_cluster,p) / nrow(points_from_first_cluster))
  inter<-colSums(sum_of_distances_from_point_to_other_points(points_from_other_clusters,p) / nrow(points_from_other_clusters))
  return(intra/inter)
}
#[1] 1619.946
#[1] 6700.806
#[1] 0.2417539
#print(intra_inter_ratio(2))

silhouette<-function(p){
  d_in_avg<-sum_of_distances_from_point_to_other_points(points_from_first_cluster,p,calcMean = TRUE)

  distances<-matrix(,nrow=nrow(points_from_first_cluster),ncol=1)
  for(i in 1:nrow(points_from_first_cluster))
  {
    from_i_to_j<-matrix(,nrow=nrow(points_from_other_clusters),ncol=1)
    for(j in 1:nrow(points_from_other_clusters))
    {
      from_i_to_j[j,1]<-minkowski_distance(
        c(points_from_first_cluster[i,1], points_from_first_cluster[i,2]),
        c(points_from_other_clusters[j,1], points_from_other_clusters[j,2]),p)

    }
    distances[i,1]<-mean(from_i_to_j)
  }
  d_out_min<-min(distances)
   
  s<-(d_out_min - d_in_avg)/(max(d_in_avg,d_out_min))
  return(colSums(s))
}

# [1] 236.3081
#print(silhouette(2))


k_medoids <- function(d,k,metric) {
  a<-pam(d,k,metric=metric)
}

# res=k_means(x,3,2)
# print(res$centroids)
# res2=k_means(x,3,1)
# print(res2$centroids)
# 
# km1<-k_medoids(x,3,"euclidean")$medoids
# km2<-k_medoids(x,3,"manhattan")$medoids
# print(km1)
# print(km2)

