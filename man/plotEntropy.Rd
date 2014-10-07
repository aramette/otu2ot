\name{plotEntropy}
\alias{plotEntropy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the entropy profile for a matrix of sequences
}
\description{
Plot the entropy profile for a matrix of sequences and also the base composition for the positions with highest entropy values.

}
\usage{
plotEntropy(Seqs,Legend=TRUE,Max=TRUE,EntropyThreshold=0.6)  
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Seqs}{
a matrix of bases (nt sites are the columns; rows as the individual sequences).
  }
  \item{Legend}{
logical. if the plot has a legend or not.
}
\item{Max}{
logical. if only the max entropy should be displayed.
}
\item{EntropyThreshold}{
numeric. A fixed entropy threshold above which the positions are identified. Only considered if Max=FALSE
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
This function returns nothing, but prints the positions of the maximum entropy and the statistics (number and proportions for each different base at each position).

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

data(mock.fasta)
plotEntropy(Seqs=mock.fasta[[2]],Legend = TRUE,Max=TRUE)
plotEntropy(Seqs=mock.fasta[[2]],Legend = TRUE,Max=FALSE,EntropyThreshold = 0.6)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~entropy }
\keyword{ ~sequence }
\keyword{ ~oligotyping }
