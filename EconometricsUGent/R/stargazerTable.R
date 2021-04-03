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

stargazerTable=function(table,fileDirectory,fileName,install) {
  if(!missing(install)) {
    install.packages("stargazer")
  }
  if(missing(fileDirectory)) fileDirectory=getwd()
  if(missing(fileName)) {
    fileName="table"
    inputName=FALSE
  } else {
    fileName=fileName
    inputName=TRUE
  }
  library(stargazer,quietly = TRUE)
  #directory=noquote(shQuote(fileDirectory))
  #name=noquote(shQuote(fileName))
  name=fileName
  setwd(fileDirectory)
  
  if(inputName==TRUE) {
    stargazer(table,type="html", flip = FALSE, summary = FALSE, 
              notes=fileName,out=gsub(" ","",paste(name,".doc")))
  }
  if(inputName==FALSE) {
    stargazer(table,type="html", flip = FALSE, summary = FALSE, 
              out=gsub(" ","",paste(name,".doc")))
  }
}