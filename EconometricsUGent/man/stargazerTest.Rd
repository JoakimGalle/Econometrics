\name{stargazerTest}
\alias{stargazerTest}
\title{Create good-looking tables in Word for your test results}
\description{Create good-looking tables in Word for your results}
\usage{stargazerTest(test, fileDirectory, fileName, install)}
\arguments{
  \item{test}{The test for which we want to create a table in Word
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

## First data vector
A=rnorm(n,3,5)

## Second data vector
B=rnorm(n,3.5,1)

## Test equality of means
t_test=t.test(A,B)

newFileDirec=getwd()
newFileName="t_test"

stargazerTest(t_test,newFileDirec,newFileName)
}
