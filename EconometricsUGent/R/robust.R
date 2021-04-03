############################################################
## Function to easily use robust standard errors
## Alex Verhoijsen, Ghent University
## By default, the function outputs the regression results
## with robust standard errors.
############################################################

############################################################
##################      Function Input    ##################
############################################################
## regression  : regression results from lm()
## type        : "White" or "HAC
############################################################

robust=function(regression,type,fileDirectory,fileName,install) {
  if(!missing(install)) {
    install.packages("stargazer")
  }
  library(stargazer,quietly = TRUE)
  directory=shQuote(fileDirectory,type = "cmd2")
  nameFile=shQuote(fileName,type = "cmd2")
  setwd(directory)
  
  results=summary(regression)
  if(type=="White") {
    results$coefficients=coeftest(regression, vcov. = vcovHC) 
  }
  if(type=="HAC") {
    results$coefficients=coeftest(regression, vcov. = vcovHAC)
  }

  robustSE=results$coefficients[,2]
  robustTstat=results$coefficients[,3]
  robustPval=results$coefficients[,4]
  
  results=stargazer(regression,regression,se=list(NULL,robustSE),t=list(NULL,robustTstat),p=list(NULL,robustPval),
                            column.labels=c("default",noquote(type)),type="html",style="all",header = FALSE,font.size = "normalsize",single.row = TRUE,
                            float = FALSE,omit.table.layout = "n",intercept.bottom = FALSE,digits = 4,digits.extra = 0,star.char = c("","",""),
                            align = FALSE,df = FALSE,column.sep.width = "10pt",no.space = FALSE,out=gsub(" ","",paste(noquote(nameFile),"SE.doc")))
  return(results)
}