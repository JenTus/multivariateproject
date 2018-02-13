# Preprocessing the raw data ----------------------------------------------
library(dplyr)
dt <- read.csv("STIO_2016_12022018205633163.csv", header = T, sep = ",")
#remove Thailand
dt <- dt[-which(dt$COUNTRY == "THA"),]
#remove data with flags
#with B: break; D: difference in methodology; E: Estimated value; P: provisonal value, M&L, missing values
dt <- dt[-which(dt$Flag.Codes != ""),]
#remove the flags
dt <- dt[, -c(1,3,5,8,9)]
#remove number and headcount
remove <- c("number", "headcount", "million")
dt <- dt[-grep(paste(remove, collapse="|"), dt$Indicator),]
dt$Indicator <- factor(dt$Indicator)
