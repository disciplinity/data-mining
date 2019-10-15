source('C:/Users/Daiy/Desktop/iti8730-data_mining/Jaroslav_HW3.R')

find_hyperparameter_k<-function(tst_data, trn_data, k_min, k_max, p) {
  accuracies<-matrix(,nrow=(k_max-k_min+1), ncol=2)
  counter<-1
  for (k in k_min:k_max)
  {
    r<-my_knn(tst_data,trn_data,k,p)
    accuracies[counter,1]<-evaluate_performance(tst_data,r)
    accuracies[counter,2]<-k
    counter<-counter+1
  }
  return(which.max(accuracies))
}

find_hyperparameter_k(test_data, train_data, 1, 5, 2)