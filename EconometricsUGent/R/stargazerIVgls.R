############################################################
## Function to easily use Stargazer
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
## install(optional): logical variable to install stargazer
############################################################

############################################################
##################     Function Output    ##################
############################################################
## Word document containing the results in the
## specified working directory with the specified
## file name
############################################################

stargazerIVgls=function(ivglsModel,fileDirectory,fileName,install) {
  if(!missing(install) && install==TRUE) {
    install.packages("stargazer")
  }
  library(stargazer,quietly = TRUE)
  #if(missing(fileDirectory)) fileDirectory=getwd()
  #directory=shQuote(fileDirectory,type = "cmd2")
  #nameFile=shQuote(fileName,type = "cmd2")
  nameFile=fileName
  setwd(fileDirectory)
  
  varName=ivglsModel$name
  coefs=ivglsModel$coefficients
  coefs=round(coefs,4)
  names(coefs)=varName
  se=ivglsModel$se.coef
  se=round(se,4)
  names(se)=varName
  t.stat=ivglsModel$t.stat
  t.stat=round(t.stat,4)
  names(t.stat)=varName
  p.value=ivglsModel$p.value
  names(p.value)=varName
  R2=ivglsModel$R.squared
  R2=round(R2,4)
  R2adj=ivglsModel$adj.R.squared
  R2adj=round(R2adj,4)
  regse=ivglsModel$sigma
  regse=round(regse,4)
  
  k=length(coefs)-1
  tempVar=matrix(rnorm((10*k),0,1),nrow = 10,ncol = k)
  
  tempReg=lm(rnorm(10,0,1)~tempVar)
  names(tempReg$coefficients)=varName
  tempresults=summary(tempReg)
  tempresults$coefficients=cbind(coefs,se,t.stat,p.value)
  coefs=tempresults$coefficients[,1]
  
  RSS=round(sum(ivglsModel$residuals^2),4)
  Observations=ivglsModel$observations
  
  stargazerReturn=stargazer(tempReg,type="html",style="all",header = FALSE,font.size = "normalsize",single.row = TRUE,float = FALSE,
                            covariate.labels = varName, coef=list(coefs),se=list(se),t=list(t.stat),p=list(p.value),
                            dep.var.labels = "Dependent variable",omit.table.layout = "n",intercept.bottom = FALSE,digits = 4,digits.extra = 0,
                            star.char = c("","",""),align = FALSE, omit.stat = c("n","f","rsq","adj.rsq","ser"),add.lines = list(c("RSS",RSS),
                                    c("Observations",Observations),c("R<sup>2",R2),c("Adjusted R<sup>2",R2adj),c("Residual Std. Error",regse)),
                            df = FALSE,column.sep.width = "10pt",no.space = FALSE,out=gsub(" ","",paste(noquote(nameFile),".doc")))
  return(stargazerReturn)
}