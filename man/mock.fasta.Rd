\name{mock.fasta}
\alias{mock.fasta}
\docType{data}
\title{
mock data for testing the oligotyping pipeline.
}
\description{
mock fasta alignemnt as provide by the oligotyping website
}
\usage{data(mock.fasta)}
\format{
  The format is:
List of 2
 $ Seqs.names: chr [1:2500] "Environment-02_33" "Environment-03_168" "Environment-02_160" "Environment-01_202" ...
 $ Seqs.only : chr [1:2500, 1:246] "g" "g" "g" "g" ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:2500] "1" "2" "3" "4" ...
  .. ..$ : NULL
}
\details{
The data were produced by applying the following function:
ImportFastaAlignment(File="mock-env-aligned.fasta",fixSpaceinHeaders = TRUE)
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(mock.fasta)
## maybe str(mock.fasta) ; plot(mock.fasta) ...
}
\keyword{datasets}
