plotEntropy <-
function(Seqs,Legend=TRUE,Max=TRUE,EntropyThreshold=0.6){
  #Seqs is a matrix of bases (nt sites are the columns; rows as the individual sequences).

  # Legend logical. if the plot has a legend or not.
  # Max logical. if only the max entropy should be displayed.
  #EntropyThreshold numeric. A fixed entropy threshold above which the positions are identified
  Nbp=ncol(Seqs)#nber of nt
  if(is.matrix(Seqs)){
    MyEntropy<- apply(Seqs,2,CalcEntropy.seq)
  if(Max) Max.ent.pos <- which(MyEntropy==max(MyEntropy))
  if(!Max) {
    if(!missing(EntropyThreshold)){
      Max.ent.pos <- which(MyEntropy>=EntropyThreshold)      
    }
  }
  Max.ent.values <- round(MyEntropy[Max.ent.pos],2)
  #coloring max entropy values
  COL=rep(3,ncol(Seqs))
  COL[Max.ent.pos]<- 2
  plot(MyEntropy,type="l",ylab="entropy",xlab="position",col=COL,ylim=c(0,max(MyEntropy)+0.3))
  
  Labels=  cbind(Max.ent.pos,Max.ent.values)
  rownames(Labels) <- apply(Labels,1,function(x){paste("(",paste(x,collapse = ", "),")",collapse="",sep="")})
  colnames(Labels) <- c("Positions","entropy")
  text(Labels,rownames(Labels),col="red",pos=4,cex=0.7)
  points(Labels,col="red",pch=16,cex=0.7)
  if(Legend) legend("topright",legend=c("Position, Entropy",rownames(Labels)),bty = "n")  

  #add some base info
      max.n=length(Max.ent.pos)
      max.n.r <- (max.n/3)
        op.par <- par(no.readonly = TRUE)
    if(max.n>1)   par(mfrow=c(ceiling(max.n.r),3))
      for(P in Max.ent.pos){
        cat("\nPosition:",P,"\n")
        T <- round(table(Seqs[,P]),0)
        print(rbind(Nber=T,Prop=as.numeric(round(T/Nbp,2))))
        barplot(T,beside = FALSE,horiz = TRUE,main=paste("position:", P,sep=" "),las=2,xlim=c(0,max(T)+100))
      }
    par(op.par)
  
  
  }  else{
    print("error: expecting a matrix of bases as input.")
  }
  return()
}
