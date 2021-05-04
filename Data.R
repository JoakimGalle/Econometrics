library(reprex)



####################################################################
####################################################################
###                   Case Econometrics                          ###
### Bachelor of Science in Economics and Business Engineering    ###
###         Academic year 2021-21 Ghent University                ###
####################################################################
####################################################################

####################################################################
### In this case we want to estimate the impact of trade         ###
###                 on GDP per worker                             ###   
####################################################################


####################################################################
## Load required packages: this needs to be done every time you
## close an R-session
####################################################################
library(pastecs)  ## Descriptive Statistics
library(psych)    ## Correlation plot
library(moments)  ## Testing for Normality check up
library(lmtest)   ## Tests for heteroscedasticity
library(sandwich) ## Newey-West HAC estimators
library(AER)      ## 2SLS
library(stargazer)## Stargazer
library(orcutt)   ## Cochrane-Orcutt EGLS
library(nlme)     ## Linear models with autocorrelated error terms
library(openxlsx) ## To read an excel xlsx data file
library(EconometricsUGent)  ## Additional functions


####################################################################
## Import data set
####################################################################
data = read.xlsx("C:/Users/joaki/OneDrive/Documenten/School/Econometrie/Econometrics/Trade.xlsx", colNames = TRUE)

####################################################################
## Determine number of observations and number of variables
####################################################################
dim(data)             ## Number of variables and sample size
n = length(data[,1])  ## Sample size

####################################################################
## Define variables 
####################################################################
gdp   = log(data$`GDP.per.worker.(in.US.dollars)`)  # ln GDP per worker
area  = log(data$`Area.(in.sq.miles)`)              # ln Area
pop   = log(data$`Workers.(in.thousands)`)          # ln Population
trade = data$Trade                                  # Trade share
landlocked = factor(data$Landlocked.dummy)          # Landlocked dummy
neighbors = data$Number.of.neighboring.countries    # Number of neighboring countries 
continent = factor(data$Continent)

                      