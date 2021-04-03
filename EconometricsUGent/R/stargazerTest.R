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

stargazerTest=function(test,fileDirectory,fileName,install) {
  if(!missing(install)) {
    install.packages("stargazer")
  }
  if(missing(fileDirectory)) fileDirectory=getwd()
  #directory=noquote(shQuote(fileDirectory))
  #name=noquote(shQuote(fileName))
  name=fileName
  setwd(fileDirectory)
  
  summary=data.frame(test$statistic,test$p.value)
  names(summary)=c("Test-statistic","P-value")
  stargazer(summary,type="html", flip = TRUE, summary = FALSE, 
            notes = paste(test$method, test$data.name, sep = ': '),
            out=gsub(" ","",paste(test$method,".doc")))
}