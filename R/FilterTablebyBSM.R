FilterTablebyBSM <-
function(Object=List.option1,PlotBSM=TRUE,PlotMosaic=TRUE){
    #filter a result table of OT with the BSM
    BSM<- Count.BrokenStick(Counts=Object$OT.count, Plot=PlotBSM)
    Object.BSM <- Object$ SamplebyOT.table[,BSM[[2]]]
    Object.BSM<- Object.BSM[,sort(colnames(Object.BSM))]
    Rest.OT <- setdiff(names(Object$OT.count),BSM[[2]])
    Object.rest <- Object$ SamplebyOT.table[,Rest.OT]
    if(PlotMosaic){mosaicplot(Object.BSM,col=1:ncol(Object.BSM),main="oligotype abundance by sample \n (BSM correction)",xlab="",las=2,cex=0.6)}
    List=list(
        OT.BSM.filtered.Table = Object.BSM,
        OT.rest.Table =Object.rest
    )
    return(List)
}
