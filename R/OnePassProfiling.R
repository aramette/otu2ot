OnePassProfiling=function( 
  File="E:\\Oligotyping\\OT.180OTU_fasta\\arbTrimmedFASTAsForOligotyping\\HGB_0010_GXJPMPL01AZ2MS.fasta",
  minseq=21,
  entropymin=0.6,
  Plot=TRUE
){
  Aln.list<- ImportFastaAlignment(File) 
  Names <-  Aln.list[[1]]
  Sequences <- toupper(Aln.list[[2]])
  Nseqs <-  nrow(Sequences) #nber of sequences
  Nbases <- ncol(Sequences)#nber of bases
  #------ first pass
  MyEntropy<- apply(Sequences,2,CalcEntropy.seq)
  if(max(MyEntropy)<entropymin){return(NULL)} #stop here
  else{
  if(Plot) {op <- par(no.readonly = TRUE);plotEntropy(Seqs = Sequences,Legend = TRUE,Max = FALSE,EntropyThreshold = entropymin)}
      
  Entpos <- which(MyEntropy>=entropymin)
  OT.seq <- Sequences[,Entpos]
  
  #get the concatenated sequences
  
  
  if(is.null(dim(OT.seq))){OT.seq.concat <- OT.seq}
  else{OT.seq.concat <- apply(OT.seq,1,FUN=function(r){paste(r,collapse="")})}
  
  # Frequencies of OT and display
  OT.count <- table(OT.seq.concat)
  OT.freq <- OT.count/Nseqs 
  # here one could filter out the small number of variants
  
  if(Plot) {par(mfrow=c(1,1)); barplot(OT.freq,main="one-pass OT profile",las=3);}
    
    LIST=list(OT.seq.concat=OT.seq.concat,OT.count=OT.count,OT.freq=OT.freq)
  return(LIST)
  }
}#-------------
