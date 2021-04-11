#' K-fold cross validation for linear model
#'
#' @param par Initial values.
#' @param X Data matrix predictors.
#' @param Y Response vector.
#' @param K Number of folds.
#' @param method Method: "gd" for gradient descent, "sd" for steepest descent.
#' @param parallel Apply the parallel version on not: TRUE for parallel version, FALSE for sequential version.
#'
#' @return Returns the k-fold cross validation error.
#' #'
#' @examples
#' library(myOpt)
#' ## basic example code
#'
#' set.seed(8675309)
#'
#' # data simulation for example purposes
#'
#' n = 1000
#'
#' x1 = rnorm(n)
#' x2 = rnorm(n)
#' X <- cbind(rep(1,n),x1,x2)
#'
#' Y = 1 + 0.5*x1 + 0.2*x2 + rnorm(n)
#'
#' # random initial values for the parameters of the linear model
#'
#' par <- rnorm(dim(X)[2])
#'
#' # number of folds
#'
#' K <- 5
#'
#' # the function returns the k-fold cross validation error
#'
#' library(doSNOW)
#'
#' err_kfold <- my_k_fold_cv(par, X, Y, K, method="sd", parallel=TRUE)
#'
#' @export
#'
my_k_fold_cv <- function(par, X, Y, K, method="gd", parallel=FALSE){

  dat <- data.frame(cbind(Y,X))

  #random shuffle data
  dat.shuffled <<- dat[sample(nrow(dat)),]

  #create K equally sized folds
  folds <<- cut(seq(1,nrow(dat.shuffled)),breaks=K,labels=FALSE)

  #Creating empty object to hold fit information
  err <- vector(mode = 'numeric',length=K)

  #Perform K-fold cross validation
  fold <- function(i){

    #Segment data by fold using the which() function
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- dat.shuffled[testIndexes, ]
    trainData <- dat.shuffled[-testIndexes, ]

    #Use the test and train data partitions
    #Model fitting and evaluation
    #training regression model on training folds
    if(method=="gd"){
      par_train = linear_gd_optim(par, X = as.matrix(trainData[,-1]), Y = as.numeric(trainData[,1]))
    }else{
      par_train = linear_sd_optim(par, X = as.matrix(trainData[,-1]), Y = as.numeric(trainData[,1]))
    }

    #evaluating fit on the test fold
    Y_pred = my_linear_predict(par_train, X = as.matrix(testData[,-1]))

    err <- my_pred_error(Y = as.numeric(testData[,1]), Y_pred = Y_pred)

  }

  if(parallel){
    n_cpus <- parallel::detectCores()
    cluster <-  snow::makeCluster(n_cpus, type = "SOCK")
    clusterExport(cluster, c("folds", "dat.shuffled", "linear_gd_optim",
                             "linear_sd_optim", "par", "my_linear_predict",
                             "my_pred_error"))
    registerDoSNOW(cluster)
    err_fold <- snow::parLapply(cl = cluster,
              x = 1:K,
              fun = fold)
    stopCluster(cluster)
  }else{
    err_fold <- lapply(1:K, fold)
  }

  err_kfold <- mean(unlist(err_fold))

  return(err_kfold)

}
