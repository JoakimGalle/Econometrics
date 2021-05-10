plot(continent, gdp)
OLS_Final = lm(gdp ~ trade + area + pop + continent)
summary(OLS_Final)
stargazer(OLS, OLS_Final, type = "text", style = "all")
