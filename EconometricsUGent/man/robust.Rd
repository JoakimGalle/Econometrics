\name{robust}
\alias{robust}
\title{OLS with robust standard errors}
\description{
This function can be applied to correct for the presence of heteroscedasticity and/or autocorrelation in the model. There are
two options: (i) White-correction to create heteroskedasticity-robust error-terms and (ii) HAC to create error terms that
are robust both to heteroskedasticity and autocorrelation as proposed by White (1980). The function outputs a Word-file
containing a stargazer produced table with the robust standard errors as defined by the user.
}
\usage{robust(regression, type, fileDirectory, fileName, install)}
\arguments{
  \item{regression}{The regression model for which we want to create robust standard errors}
  \item{type}{\enumerate{
	\item \code{"White"} to implement the White-correction for heteroscedasticity
	\item \code{"HAC"} to use Heteroscedasticity and Autocorrelation Consistent error terms
	}
  }
  \item{fileDirectory}{The directory to your folder in which the Word file should be created}
  \item{fileName}{The name of the Word file}
  \item{install}{Whether or not the \code{stargazer} function needs to be installed \emph{(optional)}}
}

\references{
Hlavac, Marek (2015). stargazer: Well-Formatted Regression and Summary Statistics Tables.
 R package version 5.2. http://CRAN.R-project.org/package=stargazer

Newey, W.K. and West K.D. (1987). "A Simple, Positive Semi-Definite, Heteroskedasticity and Autocorrelation Consistent Covariance Matrix", \emph{Econometrica},pp. 703--708.

White, Halbert (1980). "A heteroskedasticity-consistent covariance matrix estimator and a direct test for heteroskedasticity",  
 \emph{Econometrica}, pp. 817--838.
}
\author{
Alex Verhoijsen

Teaching assistant econometrics

\emph{Department of Social Economics, Ghent University}
}

\seealso{\link{stargazerRegression}, \link{coeftest}, \link{vcovHC} , \link{vcovHAC}}

\examples{
## Set seed
set.seed(123)

## Number of observations
n=1000

## Explanatory variable
X=rnorm(n,0,1)
Xsort=sort(X)

## Autocorrelation coefficients for an AR(2)-process
rho1=0.5
rho2=0.4

## Autocorrelated error terms
eps=rep(0,n)
eps[1]=2+(rnorm(1,0,1)*Xsort[1])
eps[2]=rho1*eps[1]+(rnorm(1,0,1)*Xsort[2])
for (i in 3:n) {
  eps[i]=rho1*eps[i-1]+rho2*eps[i-2]+(rnorm(1,0,1)*Xsort[i])
}

## Dependent variable
y=2+7*X*eps

## File Directory
fileMap=getwd()

## Regression model
regModel=lm(y~X)

## OLS with HAC error terms compared to OLS with standard error terms
robust(regModel, type="HAC", fileDirectory=fileMap, fileName="robustExample")
}