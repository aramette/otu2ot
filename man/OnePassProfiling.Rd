\name{OnePassProfiling}
\alias{OnePassProfiling}
\title{
Provides oligotyping based on a one-pass entropy profiling, without subsequent minimum entropy decomposition.
}
\description{
The function selects only the positions for which entropy is larger than a chosen cutoff. The positions are then aggregated to provide an oligotype identity. 
}
\usage{
OnePassProfiling(File = "HGB_0013_GXJPMPL01A3OQX.fasta", minseq = 21, entropymin = 0.6, Plot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{File}{
character. path to file.
}
  \item{minseq}{
numeric. minimum number of sequences before the procedure stops for a specific subalignment.
}
  \item{entropymin}{
numeric. minimum entropy level before the procedure stops for a specific subalignment.
}
  \item{Plot}{
logical. Plots the entropy profile, the base composition for the identified high entropy positions, and the histogram of relative abundance of concatenated oligotypes.
}
}

\value{
A list with three slots:
\item{OT.seq.concat}{A vector of concatenated positions for each sequence.}
\item{OT.count}{A summary table of overall abundance for each oligotype.}
\item{OT.freq}{A summary table of overall frequency for each oligotype.}
}
\references{
}
\author{
Alban Ramette
}
\note{
%%  ~~further notes~~
}


\seealso{
}
\examples{
OnePass <- OnePassProfiling( #to be saved as a new function!!!
  File="E:\\Oligotyping\\OT.180OTU_fasta\\arbTrimmedFASTAsForOligotyping\\HGB_0013_GXJPMPL01A3OQX.fasta",
  minseq=21,
  entropymin=0.6,#arbitrary cutoff. the whole thing can be set in a function with a parameter specifying this cutoff.
  Plot=TRUE
)
#Position: 185 
#          A      G
#Nber 193.00 982.00
#Prop   0.17   0.87
#
#Position: 241 
#         A C G      U
#Nber 568.0 3 5 599.00
#Prop   0.5 0 0   0.53
#
#Position: 242 
#          - A C      G     U
#Nber 355.00 4 4 247.00 565.0
#Prop   0.31 0 0   0.22   0.5
#
#Position: 271 
#          - A      G
#Nber 818.00 2 355.00
#Prop   0.72 0   0.31
#
#Position: 272 
#          C G      U
#Nber 981.00 1 193.00
#Prop   0.87 0   0.17

str(OnePass)
#List of 3
# $ OT.seq.concat: Named chr [1:1175] "GAU-C" "GAU-C" "AUG-U" #"AUG-U" ...
#  ..- attr(*, "names")= chr [1:1175] "1" "2" "3" "4" ...
# $ OT.count     : 'table' int [1:17(1d)] 1 1 1 1 1 188 4 562 1 2 #...
#  ..- attr(*, "dimnames")=List of 1
#  .. ..$ OT.seq.concat: chr [1:17] "AAU-C" "ACG-U" "AU--G" "AU-GC" ...
# $ OT.freq      : table [1:17(1d)] 0.000851 0.000851 0.000851 0.000851 0.000851 ...
#  ..- attr(*, "dimnames")=List of 1
#  .. ..$ OT.seq.concat: chr [1:17] "AAU-C" "ACG-U" "AU--G" "AU-GC" ...



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
