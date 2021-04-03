\name{stargazerRegression}
\alias{stargazerRegression}
\title{Create good-looking tables in Word for your regression output}
\description{Create good-looking tables in Word for your regression output}
\usage{stargazerRegression(regressionModel, fileDirectory, fileName, install)}
\arguments{
  \item{regressionModel}{The regression model for which we want to create a table in Word
}
  \item{fileDirectory}{The directory to your folder in which the Word file should be created
}
  \item{fileName}{The name of the Word file
}
  \item{install}{Whether or not the \code{stargazer} function needs to be installed \emph{(optional)}
}
}

\references{
Hlavac, Marek (2015). stargazer: Well-Formatted Regression and Summary Statistics Tables.
 R package version 5.2. http://CRAN.R-project.org/package=stargazer 
}
\author{
Alex Verhoijsen

Teaching assistant econometrics

\emph{Department of Social Economics, Ghent University}
}

\seealso{
\link{stargazer}
}
\examples{
## Set seed
set.seed(123)

## Number of observations
n=1000

## Explanatory variables
X=rnorm(n,3,5)

## Dependent variable
y=4-2*X+rnorm(n,0,1)

## OLS regression model
regModel=lm(y~X)

fileMap=getwd()
newName="OLS_regression"

stargazerRegression(regModel, fileDirectory=fileMap, fileName=newName)
}
