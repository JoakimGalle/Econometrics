finalSpecification=ivreg(gdp ~ trade + area + pop | neighbors + landlock + pop + area)

summary(finalSpecification)
stargazer(OLS, finalSpecification, type = "text", style = "all")









