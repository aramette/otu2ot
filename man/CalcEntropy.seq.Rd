\name{CalcEntropy.seq}
\alias{CalcEntropy.seq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
calculates the entropy for a nucleotide sequence provided as a vector of bases
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The function returns the entropy value for a nucleotide sequence provided as a vector of bases.
}
\usage{
CalcEntropy.seq(Seq)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Seq}{
%%     ~~Describe \code{Seq} here~~
a vector of nucleotide bases (See examples).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
In case some bases are unknown ".", the entropy is calculated with the other bases (including gaps "-") but over the total sequence length.
}
\value{
%%  ~Describe the value returned
This function returns a numeric value for the calculated entropy.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
The entropy function is defined in:
Eren et al. 2013 Methods in Ecology and Evolution. 4,1111-??1119. 
doi: 10.1111/2041-210X.12114. http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12114/pdf

}
\author{
%%  ~~who you are~~
Alban Ramette
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
CalcEntropy.word

}
\examples{

Seq1="AAAAAAAA"   # this is not the correct input
Seq2="AACCTTGG"   # this is not the correct input
Seq3=c(rep("A",8)) # this is the correct type of input
Seq4=c(rep("A",2),rep("C",2),rep("T",2),rep("G",2)) # this is the correct type of input

CalcEntropy.seq(Seq1)  # 0 
CalcEntropy.seq(Seq2)  # 0
CalcEntropy.seq(Seq3)  # 0
CalcEntropy.seq(Seq4)  # 2
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~entropy }
\keyword{ ~sequence }
\keyword{ ~oligotyping }
