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

stargazerRegression=function(regressionModel,fileDirectory,fileName,install) {
  if(!missing(install)) {
    install.packages("stargazer")
  }
  library(stargazer,quietly = TRUE)
  #if(missing(fileDirectory)) fileDirectory=getwd()
  #directory=noquote(shQuote(fileDirectory))
  #name=noquote(shQuote(fileName))
  name=fileName
  setwd(fileDirectory)
  RSS=round(sum(regressionModel$residuals^2),4)
  
  stargazerReturn=stargazer(regressionModel,type="html",style="all",header = FALSE,font.size = "normalsize",single.row = TRUE,float = FALSE,
                            omit.table.layout = "n",intercept.bottom = FALSE,digits = 4,digits.extra = 0,star.char = c("","",""),align = FALSE,
                            add.lines = list(c("RSS",RSS)),
                            df = FALSE,column.sep.width = "10pt",no.space = FALSE,out=gsub(" ","",paste(noquote(name),".doc")))
  return(stargazerReturn)
}