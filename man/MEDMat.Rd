\name{MEDMat}
\alias{MEDMat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Minimum Entropy Decomposition
}
\description{
Decomposes a set of aligned FASTA sequences until either the minimum entropy threshold or the minumum number of sequences in all subalignments are reached.
}
\usage{
MEDMat(AlignedSequences=Sequences, minseq = 21, entropymin = 0.6, Plot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{AlignedSequences}{
matrix. Sequence Id-by-position matrix as produced by e.g. ImportFastaAlignmentImportFastaAlignment(). This is the main difference to MED(), the latter working on files and not on object in the current workspace.
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
## File is stored in the current working directory.
#File="HGB_0013_GXJPMPL01A3OQX.fasta"
Aln.list<- ImportFastaAlignment(File) #path to FASTA file
Names <-  Aln.list[[1]]
Sequences <- toupper(Aln.list[[2]])# do not trim trailing dots at 5' and 3' ends

OT.seq.concat <- MEDMat( 
	    AlignedSequences=Sequences,  
             minseq=21,
             entropymin=0.6,
             Plot=TRUE
)


}

