\name{CalcEntropy.word}
\alias{CalcEntropy.word}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculates the entropy for a concatenated nucleotide sequence
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The function returns the entropy value for a concatenated nucleotide sequence (a "word" or a "string").
}
\usage{
CalcEntropy.word(Seq)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Seq}{
%%     ~~Describe \code{Seq} here~~
a string of nucleotide bases (See examples)

}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
This function returns the entropy as a numeric value.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
The entropy function is defined in:
Eren et al. 2013 Methods in Ecology and Evolution. 4, 1111-1119. 
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
CalcEntropy.seq
}
\examples{

Seq1="AAAAAAAA"   # this is the correct type of input
Seq2="AACCTTGG"   # this is the correct type of input
Seq3=c(rep("A",8)) #  this is not the correct input
Seq4=c(rep("A",2),rep("C",2),rep("T",2),rep("G",2)) #  this is not the correct input

CalcEntropy.word(Seq1)  # 0 
CalcEntropy.word(Seq2)  # 2
CalcEntropy.word(Seq3)  # 0
CalcEntropy.word(Seq4)  # 0


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~entropy }
\keyword{ ~sequence }
\keyword{ ~oligotyping }
