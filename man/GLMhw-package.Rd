\name{GLMhw-package}
\alias{GLMhw-package}
\alias{GLMhw}
\docType{package}
\title{
\packageTitle{GLMhw}
}
\description{
\packageDescription{GLMhw}
}
\details{
This R package trains a logistic regression model using the \code{my_logistic_regression} function, then prints the model output using the \code{print} function, predicts new data using the \code{predict} function, and plots the ROC curve using the \code{plot_roc} function.
}
\author{
\packageAuthor{GLMhw}

Maintainer: \packageMaintainer{GLMhw}
}
\references{
Nick T G, Campbell K M. Logistic regression[J]. Topics in biostatistics, 2007: 273-301.
}
\keyword{package}

%% Uncomment below to imitate parts of library(help = GLMhw)
%\section{The \file{DESCRIPTION} File}{\packageDESCRIPTION{GLMhw}}
%\section{Documentation Index}{\packageIndices{GLMhw}}

\seealso{
%%  ~~ Optional links to other man pages, e.g. ~~
%%  ~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
\code{\link{my_logistic_regression}},\code{\link{print.my_logistic_regression}},\code{\link{plot_roc}},\code{\link{predict.my_logistic_regression}}
}
\examples{
%%  ~~ Optional simple examples of the most important functions ~~
data("test_data")
model<-my_logistic_regression(y~x1+x2,data=test_data)
print(model)
plot_roc(model)
predict(model,test_data)
}
