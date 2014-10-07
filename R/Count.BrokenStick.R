Count.BrokenStick=function (Counts, Plot = TRUE) 
{
  n <- length(Counts)
  if (is.null(names(Counts))) {
    names(Counts) <- 1:n
  }
  Counts.m <- as.matrix(Counts)
  Counts.m.s <- Counts.m[order(Counts.m[, 1], decreasing = TRUE),]                        
  bsm <- data.frame(j = 1:n, p = 0)
  bsm$p[1] <- 1/n
  for (i in 2:n) bsm$p[i] <- bsm$p[i - 1] + (1/(n + 1 - i))
  bsm$p <- 100 * bsm$p/n
  Table <- t(cbind(100 * Counts.m.s/sum(Counts), bsm$p[n:1]))
  rownames(Table) <- c("Observed", "FromModel")
  if (Plot) {
    b1<- barplot(Counts.m.s, main = "Read number", col = 1, las = 2)
    abline(h = mean(Counts), col = "red", lty = 2)
    text(max(b1)-2,mean(Counts)+10,"mean", col = 2)
    b2 <- barplot(Table, beside = TRUE, main = "% contribution in abundance", 
                  col = c("blue", 2), las = 2)
    legend("topright", c("observed", "broken-stick model"), 
           pch = 15, col = c("blue", 2), bty = "n")
    lines(b2[1, ], Table[1, ], col = "blue", lwd = 2, lty = 2)
    lines(b2[2, ], Table[2, ], col = "red", lwd = 2, lty = 2)
  }
  Decision <- apply(Table, 2, function(x) {
    x[1] > x[2]
  })
  if(!Decision[1]) { #stops if the largest relative abundance is lower than predicted by the model
    LIST <- list(Table = Table, 
                 HigherThanBSM = NULL)
  }
  else{
    Decision.OT <- names(Decision[Decision == TRUE])
    LIST <- list(Table = Table, 
               HigherThanBSM = Decision.OT)
  }
  return(LIST)
}

