plot.lm.ci <- function (x, y, main,xlim,ylim) 
{
  plot(x, y, type = "n", main = main,xlim,ylim)
  points(x, y, pch = 16)
  abline(a = 0, b = 1, lty = 1)
  LM <- lm(as.numeric(y) ~ as.numeric(x))
  abline(LM, col = "blue")
  newx<-seq(min(x),max(x))
  conf.plim <- predict(LM,newdata=data.frame(x=newx), interval = "confidence", 
                       level = 0.90,type="response")
  
  lines(newx, conf.plim[, 2], col = "blue", lty = 2, lwd = 0.8)
  lines(newx, conf.plim[, 3], col = "blue", lty = 2, lwd = 0.8)
  pred.plim <- predict(LM,newdata=data.frame(x=newx), interval = "prediction")
  lines(newx, pred.plim[, 2], col = "red", lty = 2, lwd = 0.8)
  lines(newx, pred.plim[, 3], col = "red", lty = 2, lwd = 0.8)
  legend("bottomright", legend = c("y=x", "confidence", "predicted"), 
         lty = c(1,2, 2), col = c(1, "blue", "red"), 
         lwd = rep(2, 3), bty = "n")
}
