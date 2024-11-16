\name{print.my_logistic_regression}
\alias{print.my_logistic_regression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Printing the logistic regression model
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function prints the logistic regression model of class \code{my_logistic_regression}.
}
\usage{
\method{print}{my_logistic_regression}(x,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
the logistic regression model object of class \code{my_logistic_regression}.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
other option paramter.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
This function prints the logistic regression model of class \code{my_logistic_regression}.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
No object returned.
}
\references{
%% ~put references to the literature/web site here ~
Nick T G, Campbell K M. Logistic regression[J]. Topics in biostatistics, 2007: 273-301.
}
\author{
%%  ~~who you are~~
Chenxi Zhu
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{my_logistic_regression}},\code{\link{plot_roc}},\code{\link{predict.my_logistic_regression}}
}
\examples{
# build a logistic regression model
data("test_data")
model<-my_logistic_regression(y~x1+x2,data=test_data)
print(model,test_data)
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
