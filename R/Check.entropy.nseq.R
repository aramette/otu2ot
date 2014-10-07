Check.entropy.nseq <-
function(list.elt,minseq=0,entropy.min=0.6){
    check.OT.nseq <- nrow(list.elt)>=minseq
    Check.OT.entropy <- (max(apply(list.elt,2,CalcEntropy.seq))>=entropy.min)
    return(all(check.OT.nseq,Check.OT.entropy))
}
