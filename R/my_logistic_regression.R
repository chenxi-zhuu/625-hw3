#' Fitting a logistic regression model
#' 
#' This function automatically calls the default method.
#' 
#' Calling a function with a default method.A typical predictor has the form
#' \code{response ~ terms} where response is the (numeric) response vector and
#' \code{terms} is a series of terms which specifies a linear predictor for
#' \code{response}.
#' 
#' @param formula an object of class "\code{\link{formula}}"
#' @param data an optional data frame, list or
#' environment containing the variables in the model.
#' @param \dots For
#' \code{my_logistic_regression}: arguments to be used to form the default
#' control argument if it is not supplied directly.
#' @return 
#' \item{formula}{models formula} 
#' \item{coefficients}{a named vector of coefficients} 
#' \item{residuals}{models residuals}
#' \item{fitted.values}{the fitted predict probability} 
#' \item{aic}{AIC value (Akaike's An Information Criterion)} 
#' \item{bic}{BIC value} 
#' \item{call}{the matched call} 
#' \item{data}{training data}
#' @note The response variable must be a binary variable of 0 and 1.
#' @author  Chenxi Zhu
#' @seealso 
#' \code{\link{print.my_logistic_regression}},\code{\link{plot_roc}},\code{\link{predict.my_logistic_regression}}
#' @references Nick T G, Campbell K M. Logistic regression[J]. Topics in biostatistics, 2007:273-301.
#' @export
#' @examples
#' 
#' # build a logistic regression model
#' data("test_data")
#' model<-my_logistic_regression(y~x1+x2,data=test_data)
#' print(model)
#' 
my_logistic_regression <- function(formula,data, ...) UseMethod("my_logistic_regression")



#' Fitting a logistic regression model
#' 
#' 
#' \code{\link{my_logistic_regression.formula}} is used to fit generalized
#' linear models.The response variable is required to be a binary variable for
#' 0 and 1.
#' 
#'  A typical
#' predictor has the form \code{response ~ terms} where response is the
#' (numeric) response vector and \code{terms} is a series of terms which
#' specifies a linear predictor for \code{response}.
#' 
#' @param formula an object of class "\code{\link{formula}}"
#' @param data an optional data frame, list or environment containing the variables in the model.
#' @param \dots
#' For \code{my_logistic_regression}: arguments to be used to form the default control argument if it is not supplied directly.
#' @return 
#' \item{formula}{models formula} 
#' \item{coefficients}{a named vector of coefficients} 
#' \item{residuals}{models residuals} 
#' \item{fitted.values}{the fitted predict probability} 
#' \item{aic}{AIC value (Akaike's An Information Criterion)} 
#' \item{bic}{BIC value} 
#' \item{call}{the matched call}
#' \item{data}{training data}
#' @note  The response variable must be a binary variable of 0 and 1.
#' @author  Chenxi Zhu
#' @seealso
#' \code{\link{print.my_logistic_regression}},\code{\link{plot_roc}},\code{\link{predict.my_logistic_regression}}
#' @references Nick T G,Campbell K M. Logistic regression[J]. Topics in biostatistics, 2007:273-301.
#' @export
#' @examples
#' 
#' # build a logistic regression model
#' data("test_data")
#' model<-my_logistic_regression(y~x1+x2,data=test_data)
#' print(model)
#' 
my_logistic_regression.formula <- function(formula,data,...) {
  model_frame <- model.frame(formula,data)
  X <- model.matrix(formula,data)
  Y <- model.response(model_frame)
  if(!all(unique(Y) %in% c(0,1))){
    stop("Response variable must be binary: 0,1")
  }
  # estimate coefficients of logistic regression model by Newton-Raphson method
  beta <- rep(0,ncol(X))
  max_iter <- 1000
  tol <- 1e-6
  for(iter in 1:max_iter){
    p_hat <- 1/(1+exp(-(X%*%beta)))
    gradient <- t(X)%*%(Y-p_hat)
    hessian <- -t(X)%*%(diag(as.vector(p_hat*(1-p_hat))))%*%X
    beta_new <- beta-solve(hessian)%*%gradient
    if(sum(abs(beta_new-beta))<tol){
      break
    }
    beta <- beta_new
  }
  # return
  result <- list()
  result$formula <- formula
  result$coefficients <- beta
  result$residuals <- Y-p_hat
  result$fitted.values <- p_hat
  result$aic <- -2*sum(Y*log(p_hat)+(1-Y)*log(1-p_hat))+2*(length(beta)+1)
  result$bic <- -2*sum(Y*log(p_hat)+(1-Y)*log(1-p_hat))+log(nrow(data))*(length(beta)+1)
  result$call <- match.call()
  result$data <- data
  class(result) <- "my_logistic_regression"
  return(result)
}


#' Printing the logistic regression model
#' 
#'  This function prints the logistic regression model of class
#' \code{my_logistic_regression}.
#' 
#'  This function
#' prints the logistic regression model of class \code{my_logistic_regression}.
#' 
#' @param x  the logistic regression model object of class \code{my_logistic_regression}.
#' @param \dots  other option paramter.
#' @return No object returned.
#' @author  Chenxi Zhu
#' @seealso 
#' \code{\link{my_logistic_regression}},\code{\link{plot_roc}},\code{\link{predict.my_logistic_regression}}
#' @references Nick T G,Campbell K M. Logistic regression[J]. Topics in biostatistics, 2007:273-301.
#' @export
#' @examples
#' 
#' # build a logistic regression model
#' data("test_data")
#' model<-my_logistic_regression(y~x1+x2,data=test_data)
#' print(model,test_data)
#' 
print.my_logistic_regression <- function(x,...){
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients)
  cat("AIC: ",x$aic,"\n")
  cat("BIC: ",x$bic,"\n")
}


#' Predict a object model of class my_logistic_regression
#' 
#' 
#' Predict Logistic Regression Model with new data of class
#' \code{my_logistic_regression} and return the probability and prediction.
#' 
#'  Predict
#' Logistic Regression Model with new data of class
#' \code{my_logistic_regression} and return the probability and prediction.
#' 
#' @param model  the model of class
#' \code{my_logistic_regression}
#' @param new_data  the new data to predict
#' @return
#' \item{prob}{the probability of the prediction}
#' \item{predict}{the prediction}
#' @note  Predictions must specify a dataset and contain all parameters of a \code{\link{formula}} object.
#' @author  Chenxi Zhu
#' @seealso 
#' \code{\link{my_logistic_regression}},\code{\link{print.my_logistic_regression}},\code{\link{plot_roc}}
#' @references Nick T G,Campbell K M. Logistic regression[J]. Topics in biostatistics, 2007:273-301.
#' @export
#' @examples
#' 
#' # build a logistic regression model
#' data("test_data")
#' model<-my_logistic_regression(y~x1+x2,data=test_data)
#' predict(model,test_data)
#' 
predict.my_logistic_regression <- function(model,new_data) {
  beta <- model$coefficients
  new_X <- model.matrix(model$formula,new_data)
  p_hat <- 1/(1+exp(-(new_X%*%beta)))
  pre_hat <- ifelse(p_hat>0.5,1,0)
  result <- list()
  result$prob <- p_hat
  result$predict <- pre_hat
  class(result) <- "my_logistic_regression_prediction"
  return(result)
}


#' Plot roc curve
#' 
#'  This function plots the ROC curve for the \code{my_logistic_regression} object
#' and labels the AUC value.
#' 
#'  The \code{plot_roc} function draws the ROC curve of the
#' \code{my_logistic_regression} object based on the specificity and
#' sensitivity, and returns the sensitivity, specificity, threshold, and AUC
#' value of the training data set, where the threshold is an arithmetic
#' progression from 0 to 1 with a step size of 0.01.
#' 
#' @param model  An object of class \code{my_logistic_regression}.
#' @return 
#' \item{specificity}{The proportion of predicted correct examples among all samples predicted as negative examples}
#' \item{sensitivity}{The proportion of predicted correct examples among all samples predicted as positive examples} \item{threshold}{The threshold of the ROC curve} \item{auc}{The area under the ROC curve}
#' @note  This function is only applicable to the \code{my_logistic_regression} object.
#' @author  Chenxi Zhu
#' @seealso 
#' \code{\link{my_logistic_regression}},\code{\link{print.my_logistic_regression}},\code{\link{predict.my_logistic_regression}}
#' @references Tom Fawcett (2006) “An introduction to ROC analysis”. Pattern Recognition Letters 27,861–874. DOI: doi:\link{10.1016/j.patrec.2005.10.010}.
#' @export
#' @examples
#' 
#' # build a logistic regression model
#' data("test_data")
#' model<-my_logistic_regression(y~x1+x2,data=test_data)
#' plot_roc(model)
#' 
plot_roc <- function(model) {
  Y <- model$data[[all.vars(model$formula)[1]]]
  pre_p <- model$fitted.values
  pre_label <- rep(0,length(pre_p))
  specificity <- c()
  sensitivity <- c()
  threshold <- seq(1,0,-0.01)
  auc <- 0
  for(i in threshold){
    pre_label[pre_p>=i] <- 1
    pre_label[pre_p<i] <- 0
    tab <- table(c(pre_label,0,1),c(Y,0,1))
    tp <- tab[2,2]-1
    tn <- tab[1,1]-1
    fp <- tab[2,1]
    fn <- tab[1,2]
    specificity <- c(specificity,tn/(tn+fp))
    sensitivity <- c(sensitivity,tp/(tp+fn))
    if(i<1){
      auc <- auc+0.5*(1-tn/(tn+fp)-before_spec)*(tp/(tp+fn)+before_sens)
    }
    before_spec <- 1-tn/(tn+fp)
    before_sens <- tp/(tp+fn)
  }
  result <- list()
  result$specificity <- specificity
  result$sensitivity <- sensitivity
  result$threshold <- threshold
  result$auc <- auc
  class(result) <- "my_logistic_regression_prediction"
  # plot roc curve
  plot(1-result$specificity,result$sensitivity,type="l",xlab="1-Specificity",ylab="Sensitivity")
  abline(0,1,lty=2,col="red")
  text(0.8,0.2,paste("AUC=",round(result$auc,3)))
  return(result)
}
