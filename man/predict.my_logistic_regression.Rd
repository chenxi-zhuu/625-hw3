\name{predict.my_logistic_regression}
\alias{predict.my_logistic_regression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict a object model of class my_logistic_regression
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Predict Logistic Regression Model with new data of class \code{my_logistic_regression} and return the probability and prediction.
}
\usage{
predict.my_logistic_regression(model, new_data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
the model of class \code{my_logistic_regression}
}
  \item{new_data}{
%%     ~~Describe \code{new_data} here~~
the new data to predict
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Predict Logistic Regression Model with new data of class \code{my_logistic_regression} and return the probability and prediction.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{prob}{the probability of the prediction}
\item{predict}{the prediction}
}
\references{
%% ~put references to the literature/web site here ~
Nick T G, Campbell K M. Logistic regression[J]. Topics in biostatistics, 2007: 273-301.
}
\author{
%%  ~~who you are~~
Chenxi Zhu
}
\note{
%%  ~~further notes~~
Predictions must specify a dataset and contain all parameters of a \code{\link{formula}} object.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{my_logistic_regression}},\code{\link{print.my_logistic_regression}},\code{\link{plot_roc}}
}
\examples{
# build a logistic regression model
data("test_data")
model<-my_logistic_regression(y~x1+x2,data=test_data)
predict(model,test_data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
