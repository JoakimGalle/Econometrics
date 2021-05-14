#Analysis of the validity of the assumptions

MU = residuals(OLS) #create vector of the estimated residuals

#-------------------------------------------------------------------
##Stochastic regressors

#Assume X is stochastic

#-------------------------------------------------------------------
##Non-normal error terms

skewness(MU)
kurtosis(MU)
jarque.test(MU)
hist(MU)


#normality of estimated residuals is not rejected at 5% level

#-------------------------------------------------------------------
##Multicollinearity

#als de R2 hoog is en t statistiek laag/pairwise correlation checken
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

#VIF vrij laag, niets droppen (ook omdat pop en area controlevariabelen zijn)

#-------------------------------------------------------------------
##Heteroscedasticity

orderTrade = data[order(data$Trade),]
orderPop = data[order(data$Area),]
orderArea = data[order(log(data$`Workers.(in.thousands)`)),]
OLS_OrderTrade = lm(log(orderTrade$`GDP.per.worker.(in.US.dollars)`) ~ log(orderTrade$`Area.(in.sq.miles)`)  + log(orderTrade$`Workers.(in.thousands)`) + orderTrade$Trade )
OLS_OrderArea = lm(log(orderArea$`GDP.per.worker.(in.US.dollars)`) ~ log(orderArea$`Area.(in.sq.miles)`)  + log(orderArea$`Workers.(in.thousands)`) + data$Trade )
OLS_OrderPop = lm(log(orderPop$`GDP.per.worker.(in.US.dollars)`) ~ log(orderPop$`Area.(in.sq.miles)`)  + log(orderPop$`Workers.(in.thousands)`) + orderPop$Trade )

#graphical
gdpEstimated = OLS$fitted.values
plot(gdpEstimated, I(MU^2))
plot(trade, I(MU^2))
plot(area, I(MU^2))
plot(pop, I(MU^2))

#Goldfeld-Quandt
GQ = gqtest(OLS_OrderTrade)
GQ
#p value original model 0.8084 => insignificant => do not reject assumption that variance remains constant in first & second part

#White test
auxiliary = lm(I(MU^2) ~ area + pop + trade + I(trade^2) + I(pop^2) + I(area^2) + area*pop + area*trade + pop*trade)
xiTest = 150 * summary(auxiliary)$r.squared
print(xiTest)
pchisq(q = xiTest, df = 9, lower.tail = FALSE)
qchisq(p = 0.05, df = 9, lower.tail = FALSE)

stargazer(auxiliary,type="text",style="all",dep.var.labels = "squared(res)")
#homoscedasticity can't be rejected

#-------------------------------------------------------------------
##Autocorrelation

#ORDERED BY TRADE


MU_i = summary(OLS_OrderTrade)$residuals[2:150]
MU_iMinEen = summary(OLS_OrderTrade)$residuals[1:149]
plot(MU_iMinEen,MU_i)
     
#runs test
Nruns = runs(OLS_OrderTrade)
R = Nruns[1]
N1 = Nruns[2]
N2 = Nruns[3]
N=N1+N2
E_R = 2*N1*N2/N+1
s_R = sqrt(2*N1*N2*(2*N1*N2-N)/(N^2)/(N-1))
results_R = c(R,E_R,E_R-1.96*s_R,E_R+1.96*s_R)
names(results_R)=c("Observed Runs","Expected Runs","95% Lower bound","95% Upper bound")
stargazer(results_R,type="text")


dwtest(OLS_OrderTrade, alternative = "two.sided")
dwtest(OLS_OrderTrade, alternative = "greater")
dwtest(OLS_OrderTrade, alternative = "less")

BG = bgtest(OLS_OrderTrade, order = 5)
BGsummary = c(BG$statistic, BG$p.value)
names(BGsummary) = c("Test-statistic","P-value")
stargazer(BGsummary, type = "text")

BG = bgtest(OLS_OrderTrade, order = 10)
BGsummary = c(BG$statistic, BG$p.value)
names(BGsummary) = c("Test-statistic","P-value")
stargazer(BGsummary, type = "text")

#ORDERED BY AREA

dwtest(OLS_Reshuffled, alternative = "two.sided")
dwtest(OLS_Reshuffled, alternative = "greater")
dwtest(OLS_Reshuffled, alternative = "less")
OLS_OrderArea = lm(log(orderArea$`GDP.per.worker.(in.US.dollars)`) ~ log(orderArea$`Area.(in.sq.miles)`)  + log(orderArea$`Workers.(in.thousands)`) + data$Trade )

bgtest(OLS_Reshuffled, order = 5)
#runs test
Nruns = runs(OLS_OrderArea)
R = Nruns[1]
N1 = Nruns[2]
N2 = Nruns[3]
N=N1+N2
E_R = 2*N1*N2/N+1
s_R = sqrt(2*N1*N2*(2*N1*N2-N)/(N^2)/(N-1))
results_R = c(R,E_R,E_R-1.96*s_R,E_R+1.96*s_R)
names(results_R)=c("Observed Runs","Expected Runs","95% Lower bound","95% Upper bound")
stargazer(results_R,type="text")

OLS_Continent = lm(gdp ~ trade + pop + continent)
dwtest(OLS_OrderArea, alternative = "two.sided")
dwtest(OLS_OrderArea, alternative = "greater")
dwtest(OLS_OrderArea, alternative = "less")

dwtest(OLS_Continent, alternative = "two.sided")
dwtest(OLS_Continent, alternative = "greater")
dwtest(OLS_Continent, alternative = "less")
BG = bgtest(OLS_OrderArea, order = 5)
BGsummary = c(BG$statistic, BG$p.value)
names(BGsummary) = c("Test-statistic","P-value")
stargazer(BGsummary, type = "text")

#ORDERED BY POPULATION

BG_Continent = bgtest(OLS_Continent, order = 5)
BGCsummary = c(BG_Continent$statistic, BG_Continent$p.value)
names(BGCsummary) = c("Test-statistic","P-value")
stargazer(BGCsummary, type = "text")
OLS_OrderPop = lm(log(orderPop$`GDP.per.worker.(in.US.dollars)`) ~ log(orderPop$`Area.(in.sq.miles)`)  + log(orderPop$`Workers.(in.thousands)`) + orderPop$Trade )

#runs test
Nruns = runs(OLS_OrderPop)
R = Nruns[1]
N1 = Nruns[2]
N2 = Nruns[3]
N=N1+N2
E_R = 2*N1*N2/N+1
s_R = sqrt(2*N1*N2*(2*N1*N2-N)/(N^2)/(N-1))
results_R = c(R,E_R,E_R-1.96*s_R,E_R+1.96*s_R)
names(results_R)=c("Observed Runs","Expected Runs","95% Lower bound","95% Upper bound")
stargazer(results_R,type="text")


dwtest(OLS_OrderPop, alternative = "two.sided")
dwtest(OLS_OrderPop, alternative = "greater")
dwtest(OLS_OrderPop, alternative = "less")

BG = bgtest(OLS_OrderPop, order = 5)
BGsummary = c(BG$statistic, BG$p.value)
names(BGsummary) = c("Test-statistic","P-value")
stargazer(BGsummary, type = "text")

#-------------------------------------------------------------------
##Specification error

#Visual inspection
plot(trade, MU)
plot(area, MU)
plot(pop, MU)

#No heteroskedasticity, no autocorrelation (see previous steps)

#Ramsey RESET test
resettest(OLS_OrderTrade)
#Null hypothesis can't be rejected => not enough proof towards specification error

#Langrange multiplier test

##trade
OLS_MU_Trade = lm(MU ~ poly(trade, degree = 3))
xiTest = 150 * summary(OLS_MU_Trade)$r.squared
print(xiTest)
qchisq(p = 0.05, df = 2, lower.tail = FALSE)

##area
OLS_MU_Area = lm(MU ~ poly(area, degree = 3))
xiTest = 150 * summary(OLS_MU_Area)$r.squared
print(xiTest)
qchisq(p = 0.05, df = 2, lower.tail = FALSE)

##population
OLS_MU_Pop = lm(MU ~ poly(pop, degree = 3))
xiTest = 150 * summary(OLS_MU_Pop)$r.squared
print(xiTest)
qchisq(p = 0.05, df = 2, lower.tail = FALSE)
#No proof towards specification error

#Forecast Xi² test

##trade
OLS_OrderTrade = lm(log(orderTrade$`GDP.per.worker.(in.US.dollars)`)[1:100] ~ log(orderTrade$`Area.(in.sq.miles)`)[1:100]  + log(orderTrade$`Workers.(in.thousands)`)[1:100] + orderTrade$Trade[1:100] )
res_holdout = log(orderTrade$`GDP.per.worker.(in.US.dollars)`)[101:150] - summary(OLS_OrderTrade)$coefficients[1] - summary(OLS_OrderTrade)$coefficients[2] * log(orderTrade$`Area.(in.sq.miles)`)[101:150] - summary(OLS_OrderTrade)$coefficients[3] * log(orderTrade$`Workers.(in.thousands)`)[101:150] - summary(OLS_OrderTrade)$coefficients[4] * orderTrade$Trade[101:150]
chi2 = sum(res_holdout^2)/(sigma(OLS_OrderTrade)^2)
chi2_summary=c(chi2,pchisq(chi2,df=50,lower.tail=FALSE))
names(chi2_summary)=c("Test-statistic","P-value")
stargazer(chi2_summary,type="text")

##area
OLS_OrderArea = lm(log(orderArea$`GDP.per.worker.(in.US.dollars)`)[1:100] ~ log(orderArea$`Area.(in.sq.miles)`)[1:100]  + log(orderArea$`Workers.(in.thousands)`)[1:100] + orderArea$Trade[1:100] )
res_holdout = log(orderArea$`GDP.per.worker.(in.US.dollars)`)[101:150] - summary(OLS_OrderArea)$coefficients[1] - summary(OLS_OrderArea)$coefficients[2] * log(orderArea$`Area.(in.sq.miles)`)[101:150] - summary(OLS_OrderArea)$coefficients[3] * log(orderArea$`Workers.(in.thousands)`)[101:150] - summary(OLS_OrderArea)$coefficients[4] * orderArea$Trade[101:150]
chi2 = sum(res_holdout^2)/(sigma(OLS_OrderArea)^2)
chi2_summary=c(chi2,pchisq(chi2,df=50,lower.tail=FALSE))
names(chi2_summary)=c("Test-statistic","P-value")
stargazer(chi2_summary,type="text")

##population
OLS_OrderPop = lm(log(orderPop$`GDP.per.worker.(in.US.dollars)`)[1:100] ~ log(orderPop$`Area.(in.sq.miles)`)[1:100]  + log(orderPop$`Workers.(in.thousands)`)[1:100] + orderPop$Trade[1:100] )
res_holdout = log(orderPop$`GDP.per.worker.(in.US.dollars)`)[101:150] - summary(OLS_OrderPop)$coefficients[1] - summary(OLS_OrderPop)$coefficients[2] * log(orderPop$`Area.(in.sq.miles)`)[101:150] - summary(OLS_OrderPop)$coefficients[3] * log(orderPop$`Workers.(in.thousands)`)[101:150] - summary(OLS_OrderPop)$coefficients[4] * orderPop$Trade[101:150]
chi2 = sum(res_holdout^2)/(sigma(OLS_OrderPop)^2)
chi2_summary=c(chi2,pchisq(chi2,df=50,lower.tail=FALSE))
names(chi2_summary)=c("Test-statistic","P-value")
stargazer(chi2_summary,type="text")

#-------------------------------------------------------------------
##Endogeneity

## 2SLS estimation
reg_IV=ivreg(gdp ~ trade + area + pop | neighbors + landlock + pop + area)

## First stage OLS estimation
reg_1stage=lm(trade ~ neighbors + landlock + area + pop)
stargazer(reg_1stage,style="all",type="text")

## Hausman test
reg_Haus=lm(gdp ~ trade + pop + area + reg_1stage$residuals)
stargazer(OLS,reg_IV,reg_Haus,type="text",style="all")
