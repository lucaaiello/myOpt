#' K-fold cross validation for linear model
#'
#' @param par Initial values.
#' @param X Data matrix predictiors.
#' @param Y Response vector.
#' @param K Number of folds.
#'
#' @return Returns the k-fold cross validation error.
#' @export
#'
seq_k_fold_cv_gd <- function(par, X, Y, K){

  dat <- data.frame(cbind(Y,X))

  #random shuffle data
  dat.shuffled <- dat[sample(nrow(dat)),]

  #create K equally sized folds
  folds <- cut(seq(1,nrow(dat.shuffled)),breaks=K,labels=FALSE)

  #Creating empty object to hold fit information
  err <- vector(mode = 'numeric',length=K)

  #Perform K-fold cross validation
  for(i in 1:K){

    #Segement data by fold using the which() function
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- dat.shuffled[testIndexes, ]
    trainData <- dat.shuffled[-testIndexes, ]

    #Use the test and train data partitions
    #Model fitting and evaluation
    #training regression model on training folds
    par_train = linear_gd_optim(par, X = as.matrix(trainData[,-1]), Y = as.numeric(trainData[,1]))

    #evaluating fit on the test fold
    Y_pred = my_linear_predict(par_train, X = as.matrix(testData[,-1]))

    err[i] <- my_pred_error(Y = as.numeric(testData[,1]), Y_pred = Y_pred)

  }

  err_kfold <- mean(err)

  return(err_kfold)

}
