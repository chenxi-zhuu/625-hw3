\name{plot_roc}
\alias{plot_roc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot roc curve
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function plots the ROC curve for the \code{my_logistic_regression} object and labels the AUC value.
}
\usage{
plot_roc(model)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
An object of class \code{my_logistic_regression}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The \code{plot_roc} function draws the ROC curve of the \code{my_logistic_regression} object based on the specificity and sensitivity, and returns the sensitivity, specificity, threshold, and AUC value of the training data set, where the threshold is an arithmetic progression from 0 to 1 with a step size of 0.01.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{specificity}{The proportion of predicted correct examples among all samples predicted as negative examples}
\item{sensitivity}{The proportion of predicted correct examples among all samples predicted as positive examples}
\item{threshold}{The threshold of the ROC curve}
\item{auc}{The area under the ROC curve}
}
\references{
%% ~put references to the literature/web site here ~
Tom Fawcett (2006) “An introduction to ROC analysis”. Pattern Recognition Letters 27, 861–874. DOI: doi:10.1016/j.patrec.2005.10.010.
}
\author{
%%  ~~who you are~~
Chenxi Zhu
}
\note{
%%  ~~further notes~~
This function is only applicable to the \code{my_logistic_regression} object.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{my_logistic_regression}},\code{\link{print.my_logistic_regression}},\code{\link{predict.my_logistic_regression}}
}
\examples{
# build a logistic regression model
data("test_data")
model<-my_logistic_regression(y~x1+x2,data=test_data)
plot_roc(model)
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
