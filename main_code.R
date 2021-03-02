#tabel uit Excel halen
library(readxl)
Trade <- read_excel("Trade.xlsx")
View(Trade)

#data in vectoren steken voor de regressie
gdppw = unlist(Trade[,3])
area = unlist(Trade[,4])
nrWorkers = unlist(Trade[,5])
trade = unlist(Trade[,6])
landlocked = unlist(Trade[,7])
nrNeighbours = unlist(Trade[,8])
#landnaam en nummer toevoegen? (lijkt mij redelijk zinloos)