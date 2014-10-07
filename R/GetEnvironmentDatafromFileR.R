GetEnvironmentDatafromFileR <-
function(File="HGB_0013_GXJPMPL01A3OQX.fasta",
           Start=2, #start of the info to be retrieved
           Stop=9,   #end of it
           test=TRUE #to only run it on the first header to check
  ){
    # extract the sample info from the header of each fasta seq.
    #>HGB_0010_GXJPMPL01B0S2S.1.267 Archaea;Thaumarchaeota;Marine Group I;;
    # ^^^^^^^^
    Env.raw <- scan(File,quiet=TRUE,what="char")
    #Counting nber of sequences in the file
    
    if(test){
      Env.raw <- scan(File,quiet=TRUE,what="char",nlines = 1)
      Headers <- Env.raw[substr(Env.raw, start=1, stop=1)==">"]
      if(!is.na(Headers))
        ENV1 <- substr(Headers, start=Start, stop=Stop)
      return(ENV1)
    }
    
    else{
      Env.raw <- scan(File,quiet=TRUE,what="char")
      Headers <- Env.raw[substr(Env.raw, start=1, stop=1)==">"]
      Headers.n= length(Headers)
      ENV =rep(NA,Headers.n)
      for(i in 1:Headers.n){    
        ENV[i]   <- substr(Headers[i], start=Start, stop=Stop) # to be adjusted as needed!!!
      }
      return(ENV)
    }
}
