library(dplyr)
library(ggplot2)
library(sqldf)
# Preprocessing the raw data ----------------------------------------------
dt <- read.csv("STIO_2016.csv", header = T, sep = ",")
#remove Thailand
dt <- dt[-which(dt$COUNTRY == "THA"),]
#remove data with flags
#with B: break; D: difference in methodology; E: Estimated value; P: provisonal value, M&L, missing values
dt <- dt[-which(dt$Flag.Codes != ""),]
#remove the flags
dt <- dt[, -c(3,5,8,9)]
#remove number and headcount
remove <- c("number", "headcount", "million")
dt <- dt[-grep(paste(remove, collapse="|"), dt$Indicator),]
dt$Indicator <- factor(dt$Indicator)
View(levels(dt$Indicator))

# Explore the data --------------------------------------------------------

#data respect to government
government <- dt[grep("government", dt$Indicator),]
government$Country <- factor(government$Country)
government$Indicator <- factor(government$Indicator)

View(dt[which(dt$Indicator == "Adult population at tertiary education level, % total" ),])
View(dt[which(dt$Indicator == "R&D personnel, per thousand total employment" ),])


# View R&D personnel, per thousand total employment -----------------------
personnel <- dt[which(dt$Indicator == "R&D personnel, per thousand total employment" ), -2]
#average, group by country
ave_personnel <- sqldf("select avg(Value) as ave_value, COUNTRY from personnel group by COUNTRY")
ave_personnel$COUNTRY <- factor(ave_personnel$COUNTRY, levels = ave_personnel[order(ave_personnel$ave_value),]$COUNTRY)


ggplot(ave_personnel, aes(x = COUNTRY, y = ave_value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "light blue")


# "ICT investments, total, % of GDP" --------------------------------------
ict <- dt[which(dt$Indicator == "ICT investments, total, % of GDP" ), -2]
#average, group by country
ave_ict <- sqldf("select avg(Value) as ave_value, COUNTRY from ict group by COUNTRY")
ave_ict$COUNTRY <- factor(ave_ict$COUNTRY, levels = ave_ict[order(ave_ict$ave_value),]$COUNTRY)


ggplot(ave_ict, aes(x = COUNTRY, y = ave_value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "light blue")



# "PMR - Ease of entrepreneurship index" ----------------------------------
pmr <- dt[which(dt$Indicator == "PMR - Ease of entrepreneurship index"), -2]
ave_pmr <- sqldf("select avg(Value) as ave_value, COUNTRY from pmr group by COUNTRY")
ave_pmr$COUNTRY <- factor(ave_pmr$COUNTRY, levels = ave_pmr[order(ave_pmr$ave_value),]$COUNTRY)
ggplot(ave_pmr, aes(x = COUNTRY, y = ave_value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "light blue")



