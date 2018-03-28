library(dplyr)
library("ggplot2")
library(sqldf)
library(ca)
library(reshape)
library("gridExtra")
library(GGally)
library("ca")
library(cluster)
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
raw_data <- dt[which(temp),]
# View(row_data)
raw_cast <- cast(raw_data, Country + COUNTRY~Indicator, mean)
# View(raw_cast)

themean <- function(x){
  m <- mean(x, na.rm = T)
}

applied_research <- apply(raw_cast[, c(3, 4, 5)], 1, themean)

dd <- cbind(raw_cast[, c(1, 2)], applied_research, raw_cast[, -c(1, 2, 3, 4, 5)])

# View(dd)

complete_data <- cbind(dd[complete.cases(dd), c(1, 2)], round(dd[complete.cases(dd), -c(1, 2)], 2))
row.names(complete_data) <- c(1:length(complete_data[,1]))
View(complete_data)


# explore the dataset univariate----------------------------------------------------
p1 <- ggplot(complete_data, aes(x = reorder(COUNTRY, applied_research), y = applied_research)) +
  geom_bar(stat = "identity", position = "dodge", fill = "light blue")+
  xlab("country")

p2 <- ggplot(complete_data, aes(x = reorder(COUNTRY, `GERD, % of GDP`), y = `GERD, % of GDP`)) +
  geom_bar(stat = "identity", position = "dodge", fill = "light blue")+
  xlab("country") +
  ylab("GERD")

p3 <- ggplot(complete_data, aes(x = reorder(COUNTRY, `ICT investments, total, % of GDP`), y = `ICT investments, total, % of GDP`)) +
  geom_bar(stat = "identity", position = "dodge", fill = "light blue")+
  xlab("country") +
  ylab("ICT")

p4 <- ggplot(complete_data, aes(x = reorder(COUNTRY, `R&D personnel, per thousand total employment`), y = `R&D personnel, per thousand total employment`)) +
  geom_bar(stat = "identity", position = "dodge", fill = "light blue") +
  xlab("country") +
  ylab("R&D")

p5 <- ggplot(complete_data, aes(x = reorder(COUNTRY, `Women researchers, % of total researchers`), y = `Women researchers, % of total researchers`)) +
  geom_bar(stat = "identity", position = "dodge", fill = "light blue") +
  xlab("country") +
  ylab("woman")

grid.arrange(p1, p2, p3, p4, p5, ncol=1)

# change into quality values ----------------------------------------------

change_quantity <- function(coldata){
  y <- quantile(coldata, c(0.8, 0.6, 0.4, 0.2))
  c1 <- coldata >= y[1]
  c2 <- (coldata >= y[2] & coldata <= y[1])
  c3 <- (coldata >= y[3] & coldata <= y[2])
  c4 <- (coldata >= y[4] & coldata <= y[3])
  c5 <- coldata <= y[4]
  return(list(c1, c2, c3, c4, c5))
}

form_quantity_df <- function(df, name, aa){
  for(i in 1:5){
    df[aa[name][[1]][[i]], name] <- as.character(i)
  }
  return(df)
}

ll <- change_quantity(complete_data[,3])
aa <-apply(complete_data[, -c(1, 2)], 2, change_quantity)
quantitydf <- data.frame(COUNTRY = complete_data$COUNTRY)

for(i in 1 : 5){
  quantitydf <- form_quantity_df(quantitydf, names(aa)[i], aa)
}

rownames(quantitydf) <- quantitydf$COUNTRY
colnames(quantitydf) <- c("country", "applied research", "GERD", "ICT", "R&D", "women")
quality <- quantitydf[,-1]
# View(quality)


# bivariate correspondence analysis,
indicators = c("applied research", "GERD", "ICT", "R&D")
par(mfrow=c(2,2))
for(i in 1 : length(indicators)){
  tbb <- quality[, which(names(quality) == "women" | names(quality) == indicators[i])]
  tmp <- table(tbb)
  tmp.ca <- ca(tmp)
  plot(tmp.ca, arrows = c(T, T), what = c("all", "all"))
  legend("topleft", names(tbb), fill = c("blue", "red"), horiz=F, cex=0.5)
}


# multivariate correspondence analysis ------------------------------------
quality.mca <- mjca(quality, lambda = "indicator")
summary(quality.mca)

tb_mca_df = data.frame(quality.mca$colcoord, Variable = rep(names(quality.mca$levels.n), quality.mca$levels.n))
rownames(tb_mca_df) = quality.mca$levelnames

country_point_df = data.frame(quality.mca$rowcoord, country = quantitydf$country)
# plot
xx = round(quality.mca$sv[1]^2/sum(quality.mca$sv^2)*100, 2)
yy = round(quality.mca$sv[2]^2/sum(quality.mca$sv^2)*100, 2)
p <- ggplot() +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(data = tb_mca_df, aes(x = X1, y = X2, colour = Variable, label = rownames(tb_mca_df))) + 
  geom_segment(data = tb_mca_df, aes(x=0, y=0, xend=X1, yend=X2, colour = Variable), arrow=arrow(length=unit(0.2,"cm")), size=0.3) +
  geom_text(data = country_point_df, aes(x = X1, y = X2, label = country)) + 
  labs(x = paste("pca1 = ", xx,"%",sep=""), y = paste("pca2 = ", yy,"%",sep="")) 
p + ggplot2:: theme(legend.position = "bottom")




# k-means clustering ------------------------------------------------------

# View(complete_data)
scale_data <- scale(complete_data[, -c(1, 2)])
rownames(scale_data) <- complete_data$Country
data.pca <- princomp(scale_data)
pca_frame <- data.frame(data.pca$scores)
set.seed(200)
k.mean <- kmeans(pca_frame[, c(1, 2, 3, 4, 5)], 5)
summary(k.mean)

# k-means visualize

p1<- ggplot(pca_frame, aes(x = Comp.1, y = Comp.2)) + 
  geom_text(aes(label=rownames(pca_frame), colour = factor(k.mean$cluster)), show.legend = T) +
  theme(legend.position = "bottom") + labs(color='Centroid Cluster Result') 

p1


# hierarchical clustering -------------------------------------------------



