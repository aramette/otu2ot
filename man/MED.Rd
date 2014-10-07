\name{MED}
\alias{MED}
\title{
Minimum Entropy Decomposition
}
\description{
Decompose a set of aligned FASTA sequences until either the minimum entropy threshold or the minumum number of sequences in all subalignments are reached.
}
\usage{
MED(File = "E:\\Oligotyping\\OT.180OTU_fasta\\arbTrimmedFASTAsForOligotyping\\HGB_0013_GXJPMPL01A3OQX.fasta", minseq = 21, entropymin = 0.6, Plot = TRUE)
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
logical. Plots the entropy profiles and also the base composition for the identified high entropy positions.
}
}
\details{
}
\value{
A matrix of sequence ids (rows) by oligotypes.
}
\references{
Inspired by M. Eren et al. paper.
}
\author{
Alban Ramette
}
\note{
The procedure currently only takes one component, which corresponds to the highest entropy. In case of ties, it will take the first site in the list (i.e. smallest site position).
}

\seealso{

}
\examples{
## The File is stored in the current working directory.
OT.seq.concat <- MED(File="mock-env-aligned.fasta",minseq=21,entropymin=0.6,Plot=FALSE)


}

