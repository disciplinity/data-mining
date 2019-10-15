minkowski_distance <- function(v1,v2,p) {
  return(sum(abs(v1-v2)^p)^(1/p))
}

canberra_distance <- function(v1,v2) {
  return(sum(abs(v1-v2)/(abs(v1)+abs(v2))))
}


mahalanobis_distance <- function(v1,v2,dt) #dt contains v1,v2 and possibly other vectors representing points 
{
  inverse_covariance_m<-solve(cov(dt))
  return(sqrt(t(v1-v2) %*% inverse_covariance_m %*% (v1-v2)))
  
}
load(file="C:/Users/Daiy/Desktop/iti8730-data_mining/kdata.RData")

vec1<-x[1,]
vec2<-x[2,]

print(minkowski_distance(vec1,vec2,1)) #Manhattan
print(minkowski_distance(vec1,vec2,2)) #Euclidean
print(minkowski_distance(vec1,vec2,Inf)) #Chebyshev
print(canberra_distance(vec1,vec2))
print(mahalanobis_distance(vec1,vec2,x))