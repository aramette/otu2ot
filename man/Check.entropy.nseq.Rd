\name{Check.entropy.nseq}
\alias{Check.entropy.nseq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Check for a subset of FASTA sequences provided as a list, whether some minimum requirements are met
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The function takes one element of a list consisting of sequences (typically belonging to the split in one oligotype), and tests whether the minimum number of sequences and minimum entropy for the subalignment are met.
}
\usage{
Check.entropy.nseq(list.elt, minseq = 0, entropy.min = 0.6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{list.elt}{
%%     ~~Describe \code{list.elt} here~~
one element of a list consisting of sequences (typically belonging to the split in one oligotype.
e.g. OT.list.seq[[i]]
}
  \item{minseq}{
%%     ~~Describe \code{maxseq} here~~
the minimum number of sequences for this subalignment.
}
  \item{entropy.min}{
%%     ~~Describe \code{entropy.min} here~~
the minimum entropy threshold for this subalignment.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
The function returns a logical (TRUE/FALSE) whether both conditions are met.

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
A. Ramette
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#data(mock_env_aligned.list)
#Check.entropy.nseq(mock_env_aligned.list[[2]],minseq=21,entropy.min=0.6) # TRUE
#Check.entropy.nseq(mock_env_aligned.list[[2]],minseq=21,entropy.min=2) # FALSE
#nrow(mock_env_aligned.list[[2]]) # 2500
#Check.entropy.nseq(mock_env_aligned.list[[2]],minseq=2501,entropy.min=0.6) # FALSE
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
