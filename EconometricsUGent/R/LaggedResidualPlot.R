############################################################
## Function to easily plot residuals vis-à-vis
## lagged residuals
## Alex Verhoijsen, Ghent University
############################################################

############################################################
##################      Function Input    ##################
############################################################
## regressionModel  : regression results from lm()
## fileDirectory    : the name of the directory in which
##                    the file should be saved
## fileName         : the name of the file that should be used
##                    to save the results
## tstats(optional) : logical variable to output results with
##                    t-statistics
## pvalues(optional): logical variable to output results with
##                    p-values
## install(optional): logical variable to install stargazer
############################################################

laggedResPlot=function(residuals,lag) {
  n=length(residuals)
  resNow=residuals[(lag+1):n]
  resLag=residuals[1:(n-lag)]
  par(pty="s")
  plot(resLag,resNow,xlab = bquote(epsilon[t-.(lag)]),ylab = expression(paste(epsilon[t],""),family="serif")
       ,main = "Residual plot",col="blue",cex.lab = 1.5, cex.main = 1.5, pch= "o")
  abline(0,0,lwd=2)
  abline(v=0,lwd=2)
  par(pty="m")
}