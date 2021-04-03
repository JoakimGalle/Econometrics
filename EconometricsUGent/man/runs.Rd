\name{runs}
\alias{runs}
\title{Runs test for regression models}
\description{Implements the runs test for regression models}
\usage{runs(regression)}
\arguments{
  \item{regression}{A regression model to be inputted in the function}
}

\author{
Alex Verhoijsen

Teaching assistant econometrics

\emph{Department of Social Economics, Ghent University}
}

\examples{
## Set seed
set.seed(123)

## Number of observations
n=1000

## Explanatory variable
X=rnorm(n,0,1)

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

## Dependent variable
y=2+7*X*eps

## Regression model
regModel=lm(y~X)

runs(regModel)
}