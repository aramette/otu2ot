OnePassProfilingMat=function( 
  AlignedSequences=Sequences,
  minseq=21,
  entropymin=0.6,#arbitrary cutoff. the whole thing can be set in a function with a parameter specifying this cutoff.
  Plot=TRUE
){
  Nseqs <-  nrow(AlignedSequences) #nber of sequences
  Nbases <- ncol(AlignedSequences)#nber of bases
  #------ first pass
  MyEntropy<- apply(AlignedSequences,2,CalcEntropy.seq)
  if(max(MyEntropy)<entropymin){return(NULL)} #stop here
  else{
 
  if(Plot) { op <- par(no.readonly = TRUE);plotEntropy(Seqs = AlignedSequences,Legend = TRUE,Max = FALSWE,EntropyThreshold = entropymin)}
  Entpos <- which(MyEntropy>=entropymin)
  OT.seq <- AlignedSequences[,Entpos]
  
  #get the concatenated sequences
  if(is.null(dim(OT.seq))){OT.seq.concat <- OT.seq}
  else{OT.seq.concat <- apply(OT.seq,1,FUN=function(r){paste(r,collapse="")})}
  
  # Frequencies of OT and display
  OT.count <- table(OT.seq.concat)
  OT.freq <- OT.count/Nseqs 
  # here one could filter out the small number of variants
  
  if(Plot) {par(op); barplot(OT.freq,main="one-pass OT profile",las=3);}
    
    LIST=list(OT.seq.concat=OT.seq.concat,OT.count=OT.count,OT.freq=OT.freq)
  return(LIST)
  }
}#-------------
