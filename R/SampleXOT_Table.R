SampleXOT_Table=function( #build the Sample by OT table. filter by abundance too if needed. plot too.
  OT.seq.concat=OT.seq.concat, 
  ENV=ENV,
  mosaicPlot=TRUE,
  filterByMinAbund= 0# numeric. minimum abundance for the ot to be present
  )
  {
                              
  Table.EnV.OT <- as.data.frame(cbind(ENV, OT.seq.concat))
  colnames(Table.EnV.OT)<- c("Samples","Oligotypes")
  SamplexOT.table <-table(Table.EnV.OT)
  SamplexOT.table<- SamplexOT.table[,sort(colnames(SamplexOT.table))]
  
  LIST <- list(
    SamplexOT.table=SamplexOT.table
  )
  if(mosaicPlot) {
    mosaicplot(SamplexOT.table,col=1:ncol(SamplexOT.table),main="unfiltered",
               xlab="",las=2,cex=0.6)
  }
  
  if(filterByMinAbund != 0){  #filtering by abundance
    SamplexOT.table.abund.filt<- SamplexOT.table[,apply(SamplexOT.table,2,sum)>=filterByMinAbund]
        LIST = append(LIST,list(SamplexOT.table.filtered=SamplexOT.table.abund.filt))
    if(mosaicPlot) {
      mosaicplot(SamplexOT.table.abund.filt,col=1:ncol(SamplexOT.table),main=paste("abundance filter = ",filterByMinAbund,sep=""),
                 xlab="",las=2,cex=0.6)
    }
  }
  
  
  return(LIST)
}#-----------
