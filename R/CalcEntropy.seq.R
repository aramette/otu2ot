CalcEntropy.seq <-
function(MySeq){
    #for one seq as one vector of bases
    if(any(MySeq==".")) {
      MySeq.noDot <- MySeq[MySeq!="."]
        p=as.numeric(table(MySeq.noDot)/length(MySeq))
    }
    else {  
      p=as.numeric(table(MySeq)/length(MySeq))
    }
    return(-sum(log2(p)*p)) #base 2  
}
