\name{ivgls}
\alias{ivgls}
\title{Two-stage least squares with AR(\emph{p}) error terms}
\description{Two-stage least squares with AR(\emph{p}) error terms}

\usage{ivgls(Depend, Endo, IVs, Exo, lags, iter)}

\arguments{
  \item{Depend}{The dependent variable in the model}
  \item{Endo}{The endogenous variables in the model}
  \item{IVs}{The instrumental variables used in the model.
Note that there need to be at least as many instrumental variables as endogenous variables.
Do not include lagged dependent and/or endogenous variables in the instrumental variable list. 
The function does this for you}
  \item{Exo}{The exogenous variables in the model (\emph{optional})}
  \item{lags}{The number of lags in the autocorrelation}
  \item{iter}{Number of iterations to converge, \code{IV}=100 by default (\emph{optional})}
}

\value{\code{ivgls} creates a list with the following objects:

  \item{summary()}{Prints a summary table of the coefficients, standard errors and p-values}
  \item{coefficients}{Parameter estimates, including the autocorrelation coefficients}
  \item{t.stat}{T-statistics for the parameter estimates}
  \item{se.coef}{Standard errors of the parameter estimates}
  \item{p.value}{P-values for the parameter estimates}
  \item{R.squared}{The (multivariate) determination coefficient of the model}
  \item{adj.R.squared}{The adjusted (multivariate) determination coefficient of the model}
  \item{sigma}{The standard error of the regression}
  \item{residuals}{The residuals of the regression}
  \item{fitted.values}{The fitted values of the regression}
  \item{name}{The names of the variables}
}
\references{
Fair, Ray C. (1984). "Specification, estimation, and analysis of macroeconometric models", In:
	\emph{Harvard University Press}.

Fair, Ray C. (1970). "The estimation of simultaneous equation models with lagged endogenous variables 
	and first order serially correlated errors", In: \emph{Econometrica},
	pp. 507--516.
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

## OLS for the model
OLS=lm(y~x1+x2+x3)
summary(OLS)

## IV with AR(2) error terms
IVgls=ivgls(y,X,Z,x3,2)
IVgls$summary()
}
