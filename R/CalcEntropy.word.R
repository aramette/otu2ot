CalcEntropy.word <-
function(Seq){
    #for one seq as one word e.g. "ATCGAGAGA"
    Seq.base=strsplit(x=Seq,split="")[[1]]
    CalcEntropy.seq(Seq.base)
}
