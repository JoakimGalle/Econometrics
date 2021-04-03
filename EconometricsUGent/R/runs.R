############################################################
## Runs test for autocorrelation
## Alex Verhoijsen, Ghent University
############################################################

############################################################
##################      Function Input    ##################
############################################################
## regression : regression results from lm()
############################################################

############################################################
##################     Function Output    ##################
############################################################
## Observed runs
## N1
## N2
############################################################

runs=function(regression) {
  res=regression$residuals
  N=nobs(regression)
  
  ## Observed number of runs
  Run=c(rep(NA,(N-1)))
  for(i in 1:(N-1)) {
    Run[i]=ifelse(sign(res[i+1])!=sign(res[i]),1,0)
  }
  Run=sum(Run)+1
  
  ## Confidence Interval
  N1=sum(ifelse(res>=0,1,0))
  N2=sum(ifelse(res<0,1,0))
  
  ## Return results
  results=c(Run,N1,N2)
  names(results)=c("Observed Runs","N1","N2")
  return(results)
}
