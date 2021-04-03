#Analysis of the validity of the assumptions

MU = residuals(OLS) #create vector of the residuals

#-------------------------------------------------------------------
##Stochastic regressors
jarque.test(trade)
jarque.test(area)
jarque.test(pop)
jarque.test(landlock)
jarque.test(neighbors)

jarque.test(MU)
#normality of estimated residuals is not rejected at 5% level

#-------------------------------------------------------------------
##Non-normal error terms


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

dwtest(OLS, alternative = "two.sided")
dwtest(OLS, alternative = "greater")
dwtest(OLS, alternative = "less")

bgtest(OLS, order = 5)

#-------------------------------------------------------------------
##Specification error
###Ramsey RESET test for functional form

#-------------------------------------------------------------------
##Endogeneity


