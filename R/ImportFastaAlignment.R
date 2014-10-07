ImportFastaAlignment <-
function(File,fixSpaceinHeaders=TRUE){
  #import a fasta alignment and output a list containing a vector of names (rows) 
  #and a matrix of sequence nber by site
    require(seqinr)
    aln.fasta<- read.fasta(File) #list
    Nseqs <- length(aln.fasta) #total nber of sequences
  if(fixSpaceinHeaders){#to clean the headers for space; replace by underscore
    for(i in 1:Nseqs){
      attr(aln.fasta[[1]],"Annot") <-  gsub(" ","_",x=attr(aln.fasta[[1]],"Annot"),)
    }
  }
  
  Seq <- sapply(aln.fasta,FUN=function(A) {as.vector(A)}) #"matrix"
  Seqs <- t(Seq)
  Seqs.names <- rownames(Seqs)
  Seqs.seqonly    <- Seqs; rownames(Seqs.seqonly) <- 1:nrow(Seqs)
  List <- list(Seqs.names=Seqs.names,
               Seqs.only=Seqs.seqonly)
  return(List)
}
