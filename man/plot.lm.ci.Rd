\name{plot.lm.ci}
\alias{plot.lm.ci}
\title{
plots a x,y plot with the result of a linear regression model (y ~x), with its confidence and prediction bands.
}
\description{
plots a x,y plot with the result of a linear regression model (y ~x), with its confidence and prediction bands.
}
\usage{
plot.lm.ci(x, y,main)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numeric vector of x coordinates.
}
  \item{y}{
numeric vector of y coordinates.
}
  \item{main}{
character. The title of the plot.
}

}
\value{
This function only plots and do not return anything.
}
\references{
inspired from ci.plot in library(HH)
}
\author{
Alban Ramette
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(cars)
plot.lm.ci(cars[,1],cars[,2],main="cars dataset")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
