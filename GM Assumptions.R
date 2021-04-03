#Analysis of the validity of the assumptions

MU = residuals(OLS) #create vector of the residuals

#-------------------------------------------------------------------
##Stochastic regressors

#?

#-------------------------------------------------------------------
##Non-normal error terms

jarque.test(MU)
hist(MU)


#normality of estimated residuals is not rejected at 5% level

#-------------------------------------------------------------------
##Multicollinearity

#als de R² hoog is en t statistiek laag/pairwise correlation checken
cor(trade, pop)
cor(trade, area)
cor(pop, area)

R_trade = summary(lm(trade ~ pop + area))$r.squared
R_area = summary(lm(area ~ pop + trade))$r.squared
R_pop = summary(lm(pop ~ trade + area))$r.squared
print(R_trade)
print(R_area)
print(R_pop)
VIF_trade = 1/(1-R_trade)
VIF_area = 1/(1-R_area)
VIF_pop = 1/(1-R_pop)
print(VIF_trade)
print(VIF_area)
print(VIF_pop)
vcov(OLS)

#als oplossing voorstellen om area te droppen of niets doen? (sterk gecorreleerd met populatie EN super lage t) (bias als gevolg van droppen?)
OLS_AreaRemoved = lm(gdp ~ trade + pop)

#-------------------------------------------------------------------
##Heteroscedasticity

#graphical
plot(gdp, I(MU^2))
plot(trade, I(MU^2))
plot(area, I(MU^2))
plot(pop, I(MU^2))

#Goldfeld-Quandt
GQ = gqtest(OLS)
GQ
#p value original model 0.9063 => insignificant => do not reject assumption that variance differs in first & second part

#White test
auxiliary = lm(MU ~ area + pop + trade + I(trade^2) + I(pop^2) + I(area^2) + area*pop + area*trade + pop*trade)
xiTest = 150 * summary(auxiliary)$r.squared
print(xiTest)
stargazer(auxiliary,type="text",style="all",dep.var.labels = "squared(res)")
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

BG = bgtest(OLS, order = 5)

BGsum = c(BG$statistic, BG$p.value)
stargazer(BGsum, type = "text")

#apparent autocorrelation present, now data will be reshuffled (as the current alphabetical order has no meaning)
gdpReshuffled = sample(gdp)
OLS_Reshuffled = lm(gdpReshuffled ~ trade + area + pop)

dwtest(OLS_Reshuffled, alternative = "two.sided")
dwtest(OLS_Reshuffled, alternative = "greater")
dwtest(OLS_Reshuffled, alternative = "less")

bgtest(OLS_Reshuffled, order = 5)
#after reshuffling, all autocorrelation disappears
#How possible?
#Remediation: GLS?

#-------------------------------------------------------------------
##Specification error

###Ramsey RESET test for functional form

#-------------------------------------------------------------------
##Endogeneity
