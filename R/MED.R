MED <-
function( 
  File="E:\\Oligotyping\\OT.180OTU_fasta\\arbTrimmedFASTAsForOligotyping\\HGB_0013_GXJPMPL01A3OQX.fasta",
  minseq=21,
  entropymin=0.6,
  Plot=TRUE
  ){
    Aln.list<- ImportFastaAlignment(File) #path to FASTA file
    Names <-  Aln.list[[1]]
    Sequences <- toupper(Aln.list[[2]])# do not trim trailing dots at 5' and 3' ends
    Nseqs <-  nrow(Sequences) #nber of sequences
    Nbases <- ncol(Sequences)#nber of bases
    #------ to collect info for each OT
    OT.result <- matrix(NA,Nseqs,1)
    rownames(OT.result)<- rownames(Sequences)<- 1:Nseqs
    #------ first pass
    MyEntropy<- apply(Sequences,2,CalcEntropy.seq)

    #CalcEntropy.seq.old <-
    #  function(Seq){
    #    p=as.numeric(table(Seq)/length(Seq))
    #    return(-sum(log2(p)*p)) #base 2  
    #  }
    if(max(MyEntropy)<entropymin){return(NULL)} #stop here
    else{
    if(Plot) plotEntropy(Seqs = Sequences,Legend = FALSE,Max = TRUE)
    Max.ent <- which(MyEntropy==max(MyEntropy))
    if(length(Max.ent)>1){Max.ent=Max.ent[1]} #it happens that several peaks are found with the same entropy!
    #split the sequences based on that column into items of a list L1
    OT <- unique(Sequences[,Max.ent])
    OT.result[,1] <- Sequences[,Max.ent]
    OT.n <- length(OT)
    L1 <- vector("list",OT.n);
    names(L1)<-OT 
    for(i in 1:OT.n){#fill with sequences
      L1[[i]]<- Sequences[Sequences[,Max.ent]==OT[i],]
    }
    #--------
    while(length(L1)>=1){
      OTi.name <- names(L1)[1]#take systematically the first bloc
      bloc1 <- L1[[1]]

      if(is.null(nrow(bloc1))){ L1 <- L1[-1]}
      else{
        Check.ok <- Check.entropy.nseq(bloc1,minseq,entropy.min=entropymin)
        if(!Check.ok){#add the OT names and 
          L1 <- L1[-1]
          }
        if(Check.ok){   #continue splitting then
        row.OK.id <- rownames(bloc1)
        OT.result = cbind(OT.result,rep(NA,Nseqs))#adding one column
        MyEntropyi<- apply(bloc1,2,CalcEntropy.seq)
        Max.enti <- which(MyEntropyi==max(MyEntropyi))
        if(length(Max.enti)>1){Max.enti=Max.enti[1]}
        #split the sequences based on that column
        OTi <- unique(bloc1[,Max.enti])
        OT.result[row.OK.id,ncol(OT.result)] <- bloc1[,Max.enti]
        OT.ni <- length(OTi)
        Li <- vector("list",OT.ni);
        names(Li)<-OTi 
        for(i in 1:OT.ni){#fill with sequences
          Li[[i]]<- bloc1[bloc1[,Max.enti]==OTi[i],]
        }
        #remove the current bloc1    
        L1 <- L1[-1]
        #update the main list with the new blocs
        L1 = append(Li,L1)
        
      }  
        }
    }
    ConcatenatRemNA<- function(X=OT.result[1,]){
      X<- X[!is.na(X)]
      if(length(X)>1){return(
        paste(X,sep="",collapse=""))}
      else{return(X)}
    }
    AOT<- apply(OT.result,1,function(x){ConcatenatRemNA(x)})
    return(AOT) 
    }
}
