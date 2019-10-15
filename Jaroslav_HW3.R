minkowski_distance <- function(v1,v2,p) {
  return(sum(abs(v1-v2)^p)^(1/p))
}

load(file="C:/Users/Daiy/Desktop/iti8730-data_mining/kdata_with_labels.RData")

set.seed(123)
data<-x[sample(nrow(x)),]

train_data<-data[1:as.integer(0.7*nrow(data)),]
test_data<-data[as.integer(0.7*nrow(data) + 1):nrow(data),]

my_knn<-function(tst_data, trn_data, k, p) {
  res<-c()
  for(i in 1:nrow(tst_data))
  {
    distances<-c()
    trn_data_labels<-trn_data[,3]
    
    for(j in 1:nrow(trn_data))
    {
      distances<-c(distances, minkowski_distance(tst_data[i,1:2], trn_data[j,1:2], p))
    }
    
    neighbours<-data.frame(distances,trn_data_labels)
    neighbours<-neighbours[order(neighbours$distances),]
    neighbours<-neighbours[1:k,]
    
    # We have labels [1,2,3]. Let's count what kind of labels k-nearest neighbours have
    belongs_to_label<-c(0,0,0)
    for(n in 1:nrow(neighbours))
    {
      if(neighbours[n,2] == 1) belongs_to_label[1] = belongs_to_label[1] + 1
      else if(neighbours[n,2] == 2) belongs_to_label[2] = belongs_to_label[2] + 1
      else if(neighbours[n,2] == 3) belongs_to_label[3] = belongs_to_label[3] + 1
      
    }
    
    if(belongs_to_label[1] > belongs_to_label[2] & belongs_to_label[1] > belongs_to_label[3]) res<-c(res,1)
    else if(belongs_to_label[2] > belongs_to_label[1] & belongs_to_label[2] > belongs_to_label[3]) res<-c(res,2)
    else if(belongs_to_label[3] > belongs_to_label[1] & belongs_to_label[3] > belongs_to_label[2]) res<-c(res,3)
    else res<-c(res,2) # can be 1 or 3 too; doesn't matter
    
    
  }
  return(res)
}

evaluate_performance<-function(tst_data, predicted) {
  correct<-0
  tst_data<-cbind(tst_data,predicted)
  for(i in 1:nrow(tst_data))
  {
    if(tst_data[i,3] == tst_data[i,4])
    {
      correct<-correct+1
    }
  }
  return(correct/nrow(tst_data) * 100)
}

#predicted_manhattan<-my_knn(test_data, train_data, 3, 1)
#predicted_euclidean<-my_knn(test_data, train_data, 3, 2)

#accuracy_manhattan<-evaluate_performance(test_data,predicted_manhattan)
#accuracy_euclidean<-evaluate_performance(test_data,predicted_euclidean)

#print(accuracy_manhattan)
#print(accuracy_euclidean)

