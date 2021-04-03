#Analysis of the validity of the assumptions

MU = residuals(OLS) #create vector of the residuals

#-------------------------------------------------------------------
##Stochastic regressors

jarque.test(trade)
jarque.test(area)
jarque.test(pop)
jarque.test(landlock)
jarque.test(neighbors)



#-------------------------------------------------------------------
##Non-normal error terms

jarque.test(MU)
#normality of estimated residuals is not rejected at 5% level

#-------------------------------------------------------------------
##Multicollinearity

#als de R² hoog is en t statistiek laag/pairwise correlation checken
cor(trade, pop)
cor(trade, area)
cor(pop, area)
coeftest(OLS)
#als oplossing voorstellen om area te droppen? (sterk gecorreleerd met populatie EN super lage t) (bias als gevolg van droppen?)

#-------------------------------------------------------------------
##Heteroscedasticity

#graphical
plot(gdp, I(MU^2))
plot(trade, I(MU^2))
plot(area, I(MU^2))
plot(pop, I(MU^2))

#Goldfeld-Quandt
gqtest(OLS)
#p value original model 0.9063 => insignificant => do not reject assumption that variance differs in first & second part

#White test
auxiliary = lm(MU ~ area + pop + trade + I(trade^2) + I(pop^2) + I(area^2) + area*pop + area*trade + pop*trade)
xiTest = 150 * summary(auxiliary)$r.squared
print(xiTest)
#homoscedasticity can't be rejected

#-------------------------------------------------------------------
##Autocorrelation
##runs test
Nruns = runs(OLS)
R = Nruns[1]
N1 = Nruns[2]
N2 = Nruns[3]
N=N1+N2
E_R = 2*N1*N2/N+1
s_R = sqrt(2*N1*N2*(2*N1*N2-N)/(N^2)/(N-1))
results_R = c(R,E_R,E_R-1.96*s_R,E_R+1.96*s_R)
names(results_R)=c("Observed Runs","Expected Runs","95% Lower bound","95% Upper bound")
stargazer(results_R,type="text")


dwtest(OLS, alternative = "two.sided")
dwtest(OLS, alternative = "greater")
dwtest(OLS, alternative = "less")

bgtest(OLS, order = 5)

#apparent autocorrelation present, now data will be reshuffled (as the current alphabetical order has no meaning)
gdpReshuffled = sample(gdp)
OLS_Reshuffled = lm(gdpReshuffled ~ trade + area + pop)

dwtest(OLS_Reshuffled, alternative = "two.sided")
dwtest(OLS_Reshuffled, alternative = "greater")
dwtest(OLS_Reshuffled, alternative = "less")

bgtest(OLS_Reshuffled, order = 5)
#after reshuffling, all autocorrelation disappears

#-------------------------------------------------------------------
##Specification error

###Ramsey RESET test for functional form

#-------------------------------------------------------------------
##Endogeneity
