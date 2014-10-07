\name{Count.BrokenStick}
\alias{Count.BrokenStick}

\title{
Applies the Broken Stick model to a vector of count data.
}
\description{
Applies the Broken Stick model to a vector of count data, plots summary histograms if desired.
}
\usage{
Count.BrokenStick(Counts, Plot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Counts}{
numeric vector. A vector of counts.
}
  \item{Plot}{
logical. If TRUE, two histograms are produced. The first one is a sorted version of the observed abundances and 
with the mean abundance indicated. The second one depicts observed vs. modelised relative abundances.
}
}
\details{
Broken stick model (MacArthur 1957) and function about the expected lengths of the pieces of the broken stick model from Pielou 1975

}
\value{
A list with two slots:
\item{Table}{The first line gives the observed relative abundances vs. the ones predicted by the model.}
\item{HigherThanBSM}{Provides the list of cases higher than predicted by the model.}
}
\references{
MacArthur 1957.
}
\author{
Alban Ramette
}
\note{
}

\examples{

Count.BrokenStick (Counts=seq(10,100,10))

AbundVector=c(100,50,10,5,3)
names(AbundVector)=LETTERS[1:5]
Count.BrokenStick(Counts=AbundVector, Plot = TRUE)



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
