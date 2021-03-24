landlock = as.numeric(landlocked) - 1
matrix = data.frame(gdp, trade, neighbors, landlock, pop, area)

#statistics of variables
summary(matrix)
sapply(matrix, var)
cor(matrix)
cov(matrix)
pairs(matrix)

# hypothesis: trade has a positive impact on GDP per worker (one-sided test)
# null-hypothesis: coefficient of trade <= 0
# alternative hypothesis: coefficient of trade > 0

#OLS (STEP 2)
OLS = lm(gdp ~ trade + area + pop)
summary(OLS)
stargazer(OLS, type = "text", digits = 4)