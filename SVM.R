SVM <- function(folds){
  #Read data and classes into R
  
  data<- read.table("uspsdata.txt",header=FALSE)
  class<- read.table("uspscl.txt",header=FALSE)
  
  #Generating random numbers to pick test data and training data
  
  rand <- runif(nrow(data))
  index <- order(rand)
  
  test_data <- data[index[1:trunc(length(index)/5)],]
  test_class <- class[index[1:trunc(length(index)/5)],]
  
  train_data <- data[-index[1:trunc(length(index)/5)],]
  train_class <- class[-index[1:trunc(length(index)/5)],]
  
  #Generate random variable to create folds
  
  rand_vald <- runif(nrow(train_data))
  index_vald <- order(rand_vald)
  kfolds <- as.list(rep(0,folds))
  
  #Define cost parameter and gamma parameter sequence
  
  cost_parameter <- seq(-10,2,1)
  gamma_parameter <- seq(-10,2,1)
  
  #Define cross validation array
  
  validate_linear <- rep(0,length(cost_parameter))
  validate_rbf <- matrix(0,length(gamma_parameter),length(cost_parameter))
  
  #Linear SVM
  for (j in 1:length(cost_parameter)){
    for (i in 1:folds){
      #Assign data to folds
      kfolds[[i]] <- index_vald[trunc(length(index_vald)/folds)*(i-1)+1:(trunc(length(index_vald)/folds))]
      #Train on all data except a particular fold
      model <- svm(train_data[-kfolds[[i]],],train_class[-kfolds[[i]]], cost = (2^cost_parameter[j]), gamma = 0, kernel = 'linear', type = 'C')
      #Cross Validate on a particular fold
      pred <- predict(model, train_data[kfolds[[i]],], decision.values=TRUE)
      #Add misclassification to error estimate
      validate_linear[j] <- validate_linear[j] + sum(pred != train_class[kfolds[[i]]])
    }
  }
  
  #Plot the misclassification rate as a function of cost parameter
  plot(2^cost_parameter, validate_linear, xlab = 'Cost Parameter', ylab = 'Cross Validation Misclassification', log = 'x', type = 'l')
  #Choose the best cost parameter
  linear_opt <- which(validate_linear == min(validate_linear),arr.ind = TRUE)
  
  #Radial SVM
  for (k in 1:length(gamma_parameter)){
    for (j in 1:length(cost_parameter)){
      for (i in 1:folds){
        #Assign data to folds
        kfolds[[i]] <- index_vald[trunc(length(index_vald)/folds)*(i-1)+1:(trunc(length(index_vald)/folds))]
        #Train on all data except a particular fold
        model <- svm(train_data[-kfolds[[i]],],train_class[-kfolds[[i]]], cost = (2^cost_parameter[j]), gamma = (2^gamma_parameter[k]), kernel = 'radial', type = 'C')
        #Cross Validate on a particular fold
        pred <- predict(model, train_data[kfolds[[i]],], decision.values=TRUE)
        #Add misclassification to error estimate
        validate_rbf[k,j] <- validate_rbf[k,j] + sum(pred != train_class[kfolds[[i]]])
      }  
    }
  }
  
  #Plot the misclassification rate as a function of cost parameter
  wireframe(validate_rbf, xlab = 'Gamma', ylab = 'Cost', zlab = 'Count of misclassified points')
  #Choose the best cost and gamma parameter
  radial_opt <- which(validate_rbf == min(validate_rbf),arr.ind = TRUE)
  
  #Training and testing Linear SVM with best parameter values for all training data 
  
  model <- svm(train_data,train_class, cost = (2^cost_parameter[linear_opt[1]]), gamma = 0, kernel = 'linear', type = 'C')
  pred <- predict(model, test_data, decision.values=TRUE)
  print('Error Rate of Linear SVM')
  print(sum(pred != test_class)/length(test_class))
  
  #Training and testing Radial SVM with best parameter values for all training data 
  model <- svm(train_data,train_class, cost = (2^cost_parameter[radial_opt[1,2]]), gamma = (2^gamma_parameter[radial_opt[1,1]]), kernel = 'radial', type = 'C')
  pred <- predict(model, test_data, decision.values=TRUE)
  print('Error rate of Radial SVM')
  print(sum(pred != test_class)/length(test_class))
}