library(dplyr)
library(ggplot2)
library(sqldf)
library(ca)
# Preprocessing the raw data ----------------------------------------------
dt <- read.csv("STIO_2016.csv", header = T, sep = ",")
#remove Thailand
dt <- dt[-which(dt$COUNTRY == "THA" | dt$COUNTRY == "EU28"),]
#remove data with flags
#with B: break; D: difference in methodology; E: Estimated value; P: provisonal value, M&L, missing values
dt <- dt[-which(dt$Flag.Codes != ""),]
#remove the flags
dt <- dt[, -c(3,5,8,9)]
#remove number and headcount
remove <- c("number", "headcount", "million")
dt <- dt[-grep(paste(remove, collapse="|"), dt$Indicator),]
dt$Indicator <- factor(dt$Indicator)

# list all of the concerned indicators
indicators <- c("R&D personnel, per thousand total employment", 
               "Applied research expenditures, government, % of GDP",
               "Applied research expenditures, higher education, % of GDP",
               "Applied research expenditures, public research, % of GDP",
               "GERD, % of GDP",
               "ICT investments, total, % of GDP",
               "Women researchers, % of total researchers")
temp = dt$Indicator == indicators[1]
for(i in 2:length(indicators)){
  temp = temp | (dt$Indicator == indicators[i])
}
row_data <- dt[which(temp), -2]


# View R&D personnel, per thousand total employment -----------------------
personnel <- dt[which(dt$Indicator == "R&D personnel, per thousand total employment" ), -2]
#average, group by country
ave_personnel <- sqldf("select avg(Value) as ave_value, COUNTRY from personnel group by COUNTRY")
ave_personnel$COUNTRY <- factor(ave_personnel$COUNTRY, levels = ave_personnel[order(ave_personnel$ave_value),]$COUNTRY)


# Applied research expenditure, % of gdp ----------------------------------
applied_government <- dt[which(dt$Indicator == "Applied research expenditures, government, % of GDP"),]

