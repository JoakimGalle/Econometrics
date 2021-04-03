\name{stargazerTable}
\alias{stargazerTable}
\alias{stargazerTable}
\title{Create good-looking tables in Word}
\description{Create good-looking tables in Word}
\usage{stargazerTable(table, fileDirectory, fileName, install)}
\arguments{
  \item{table}{The table for which we want to create a table in Word
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

height=c(171,183,178)
weight=c(67,75,72)

person=rbind(height,weight)

stargazerTable(person)
}
