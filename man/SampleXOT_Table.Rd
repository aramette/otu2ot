\name{SampleXOT_Table}
\alias{SampleXOT_Table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
creates a Sample-by-OT table.
}
\description{
creates a Sample-by-OT table, filters the table by abundance too if needed, and optionally plots the results.
}
\usage{
SampleXOT_Table(OT.seq.concat = OT.seq.concat, ENV = ENV, mosaicPlot = TRUE, filterByMinAbund = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{OT.seq.concat}{
vector or a matrix of sequence ids (rows) by oligotypes, as produced by MED().
}
  \item{ENV}{
vector of characters. the sample identification for each sequence. typically produced by using GetEnvironmentDatafromFileR(File,Start=2,Stop=15,test=FALSE).
}
  \item{mosaicPlot}{
logical. Whether a mosaic plot is to be plotted.
}
  \item{filterByMinAbund}{
numeric. Whether the final abundance table is to be filtered by minimum abundance of each OT (column sum should be >= filterByMinAbund). 0 indicates no filtering.
}
}
\details{
}
\value{
Returns a list with one element if filterByMinAbund=0, and with two elements if filterByMinAbund>0. The first element is the unfiltered table, the second element the filtered table.
}

\author{
Alban Ramette
}
\note{
%%  ~~further notes~~
}


\examples{

#File="mock-env-aligned.fasta"
OT.seq.concat <- MED(File,minseq=21,entropymin=0.6,Plot=FALSE)
ENV <- GetEnvironmentDatafromFileR(File,Start=2,Stop=15,test=FALSE)

Table <- SampleXOT_Table(OT.seq.concat=OT.seq.concat, 
  ENV=ENV,
  mosaicPlot=TRUE,
  filterByMinAbund= 10
)

Table

#$SamplexOT.table
#                Oligotypes
#Samples          A   C   T
#  Environment-01 0  24 226
#  Environment-02 1  24 225
#  Environment-03 0  24 226
#  Environment-04 2  25 223
#  Environment-05 0  23 227
#  Environment-06 0 227  23
#  Environment-07 0 227  23
#  Environment-08 1 226  23
#  Environment-09 0 227  23
#  Environment-10 0 226  24
#
#$SamplexOT.table.filtered
#                Oligotypes
#Samples            C   T
#  Environment-01  24 226
#  Environment-02  24 225
#  Environment-03  24 226
#  Environment-04  25 223
#  Environment-05  23 227
#  Environment-06 227  23
#  Environment-07 227  23
#  Environment-08 226  23
#  Environment-09 227  23
#  Environment-10 226  24

}

