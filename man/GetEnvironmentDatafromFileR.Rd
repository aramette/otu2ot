\name{GetEnvironmentDatafromFileR}
\alias{GetEnvironmentDatafromFileR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Parse the FASTA headers to retrieve the sample information.
}
\description{
The function selects the part of the headers (starting with ">") and reports a substring that should contain the sample information for that sequence.
}
\usage{
GetEnvironmentDatafromFileR(File = "HGB_0013_GXJPMPL01A3OQX.fasta",Start=2,Stop=9,test=TRUE)
}
\arguments{
  \item{File}{
character. The stored FASTA alignment that was used for oligotyping.
	}
  \item{Start}{
numeric. Start of character string to be retrieved (character position. Start=2 means the first character after the ">" symbol.
}
  \item{Stop}{
numeric. Stop of the character string to be retrieved (character position.
}
  \item{test}{
logical. If TRUE, only the first header (i.e. the first line) of the FASTA file is analysed. This helps determine the exact length of the Start and Stop parameters above, without processing the whole dataset. Then this may be switched to FALSE for the actual computation for all headers in the file.
}
}
\details{
It may be faster to provide the sample information in a separate file and not in the header of the fasta sequences.
}
\value{
A vector of characters corresponding to the actual selected substring for each header if test is FALSE. If test is TRUE, then only one string is returned (substring from the first header).
}
\author{
Alban Ramette
}
\examples{
# GetEnvironmentDatafromFileR(File,Start=2,Stop=9,test=TRUE)
# [1] "HGB_0010"
# ENV <- GetEnvironmentDatafromFileR(File,Start=2,Stop=9,test=FALSE)
# length(ENV) ## 1175
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
