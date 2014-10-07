\name{ImportFastaAlignment}
\alias{ImportFastaAlignment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Import a FASTA alignment from a file
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Import a FASTA alignment from a file and if needed converts the space in the FASTA headers into underscore "_"

}
\usage{
ImportFastaAlignment(File,fixSpaceinHeaders=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{File}{
%%     ~~Describe \code{File} here~~
path to the FASTA file
}
\item{fixSpaceinHeaders}{
%%     ~~Describe \code{File} here~~
logical. If TRUE, replaces spaces in the headers by underscores. The function runs faster if this is turned to FALSE. Alternatively, one can "clean" the headers before importing the alignment to R!
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
require(seqinr)
}
\value{
%%  ~Describe the value returned
 The function returns a list containing a vector of names (rows) 
 and a matrix of sequence nber by site
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
List with two items:
\item{Seqs.names }{
FASTA headers}
\item{Seqs.only }{
only the FASTA sequences as vectors of bases. This is in a matrix format.}

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Alban Ramette
}
\note{
%%  ~~further notes~~
require(seqinr)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

#require(seqinr)

#write.fasta(sequences="mock-env-aligned.fasta",file.out="mock-env-aligned.fasta")
#mock_env_aligned.list <-  ImportFastaAlignment(File="mock-env-aligned.fasta",fixSpaceinHeaders=TRUE)
#data(mock_env_aligned.list) # resulting list after importing from the previous line

#FastaHeaders <-  mock_env_aligned.list[[1]]
#Sequences  <-   mock_env_aligned.list[[2]]
#SEQUENCES <- toupper(Sequences) #convert sequences to capital letters

#Nseqs <-  nrow(Sequences) #nber of sequences
#Nbases <- ncol(Sequences)#nber of bases


 }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
