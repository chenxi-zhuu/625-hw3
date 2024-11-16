\name{my_logistic_regression}
\alias{my_logistic_regression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fitting a logistic regression model
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function automatically calls the default method.
}
\usage{
my_logistic_regression(formula,data, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{x} here~~
default method parameters of \code{\link{my_logistic_regression}}
}
\item{data}{
%%     ~~Describe \code{x} here~~
default method parameters of \code{\link{my_logistic_regression}}
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
For \code{my_logistic_regression}: arguments to be used to form the default control argument if it is not supplied directly.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Calling a function with a default method.A typical predictor has the form \code{response ~ terms} where response is the (numeric) response vector and \code{terms} is a series of terms which specifies a linear predictor for \code{response}. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{formula}{models formula}
\item{coefficients}{a named vector of coefficients}
\item{residuals}{models residuals}
\item{fitted.values}{the fitted predict probability}
\item{aic}{AIC value (Akaike's An Information Criterion)}
\item{bic}{BIC value}
\item{call}{the matched call}
\item{data}{training data}
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
The response variable must be a binary variable of 0 and 1.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{print.my_logistic_regression}},\code{\link{plot_roc}},\code{\link{predict.my_logistic_regression}}
}
\examples{
# build a logistic regression model
data("test_data")
model<-my_logistic_regression(y~x1+x2,data=test_data)
print(model)
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
