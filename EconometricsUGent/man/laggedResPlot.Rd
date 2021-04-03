\name{laggedResPlot}
\alias{laggedResPlot}
\title{Plot residuals against lagged residuals}
\description{Function to plot contemporaneous residuals against lagged residuals}
\usage{laggedResPlot(residuals, lag)}
\arguments{
  \item{residuals}{The residuals of a regression model}
  \item{lag}{The number of lags against which you want to plot the contemporaneous residuals.
		E.g. \code{lag=4} implies we plot \eqn{\varepsilon_t} against \eqn{\varepsilon_{t-4}}
            }
}

\value{Returns a plot of the residuals}

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
X=2+5*rnorm(n,0,1)

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
y=-1+6*X+eps

regModel=lm(y~X)

resid=regModel$residuals

laggedResPlot(resid,1)
laggedResPlot(resid,2)
laggedResPlot(resid,10)
}