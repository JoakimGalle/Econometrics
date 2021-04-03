\name{stargazerIVgls}
\alias{stargazerIVgls}
\title{Create good-looking tables in Word for your IV gls output
}
\description{
Create good-looking tables in Word for your output of two-stage least-squares regression with autocorrelated error terms
}
\usage{
stargazerIVgls(ivglsModel, fileDirectory, fileName, install)
}
\arguments{
  \item{ivglsModel}{The \code{ivgls} regression model for which we want to create a table in Word
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
\link{ivgls}, \link{stargazer}
}
\examples{
## Set seed
set.seed(123)

## Number of observations
n=1000

## Autocorrelation coefficients for an AR(2)-process
rho1=0.5
rho2=0.4

## Autocorrelated error terms
eps=rep(0,n)
eps[1]=2+rnorm(1,0,1)
eps[2]=rho1*eps[1]+rnorm(1,0,1)
for (i in 3:n) {
  eps[i]=rho1*eps[i-1]+rho2*eps[i-2]+rnorm(1,0,1)
}

## Instruments
z1=rnorm(n,0,1)
z2=5*rnorm(n,0,1)
Z=cbind(z1,z2)

## Endogenous explanatory variables
x1=50+2*z1+eps
x2=20-0.5*z2+eps
X=cbind(x1,x2)

## Exogenous explanatory variable
x3=2*rnorm(n,0,1)

## Dependent variable
y=4-2*x1+1.75*x2+8*x3+eps

## IV with AR(2) error terms
IVglsModel=ivgls(y,X,Z,x3,2)

fileMap=getwd()
newName="IV_gls_regression"

stargazerIVgls(IVglsModel, fileDirectory=fileMap, fileName=newName)
}
