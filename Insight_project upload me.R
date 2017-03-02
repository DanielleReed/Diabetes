#load libraries
library(readr)
library(reshape2)
library(tidyr)
library(ggplot2)

setwd("C:/Users/Reed/Dropbox/Dani life/2017 Data Insight/Diabetes")
#source data
#Analysis by the GDELT Project using data from the Internet Archive Television News Archive
#keyword diabetes
#input file
Diabetes_percent_sentences <- read_csv("C:/Users/Reed/Dropbox/Dani life/2017 Data Insight/Obesity/Diabetes_percent_sentences.csv")
View(Diabetes_percent_sentences)
db <- Diabetes_percent_sentences
dbm <- melt(db, id=c("Date")) 
View(dbm)
dates <- read_csv("C:/Users/Reed/Dropbox/Dani life/2017 Data Insight/Obesity/dates.csv")
dbmd <- merge(dbm, dates, by.x = "variable", by.y = "Station Description", all = TRUE)
write.csv(dbmd, file = "file.csv")
#had to fix dates in Excel and create column 'valid' b/c I could not figure it out in R
#'valid' means the word use was monitored during the period
file <- read_csv("C:/Users/Reed/Dropbox/Dani life/2017 Data Insight/Diabetes/file.csv")
#limit to valid data
valid <- subset(file, Valid == "yes") 
#to check
table(valid$Valid)
valid$value <- as.numeric(valid$value)
str(valid$value)
cor(valid$Month, valid$value, use = "pairwise.complete.obs", method = "pearson")
plot(valid$Month, valid$value)
str(valid)

fit <- aov(value ~ Network, valid) 
summary(fit)

sums <- aggregate(value ~ Network, data=valid, FUN=mean)

#Plot 1 
par(las=2) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.
barplot(sums$value, main="Diabetes during news by network", horiz=TRUE, names.arg=c("ABC", "CBS", "FOX", "MYTV", "NBC", "PBS"), cex.names=1.0, xlab = "")
dev.copy(png,"Network.png", width=480, height=480)
dev.off

#Better drop MYTV, seems too obscure 

major <- subset(valid, Network != "MYTV")  
#to check
table(major$Network)

cor(major$Month, major$value, use = "pairwise.complete.obs", method = "pearson")
plot(major$Month, major$value, log = "y")

str(valid)
fit <- aov(value ~ Network, major) 
summary(fit)


sums <- aggregate(value ~ City, data=major, FUN=mean)

#Plot 2 
par(las=2) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin
barplot(sums$value, main="Diabetes, by network", horiz=TRUE, names.arg=c(""), cex.names=1.0, xlab = "% sentences with 'diabetes")
dev.copy(png,"Network.png", width=480, height=480)
dev.off

#Need to remove NAs from values
nomissing <- na.omit(major) 
#to check
table(nomissing$value)

# redo plots

fit <- aov(value ~ Network, nomissing) 
summary(fit)

sums <- aggregate(value ~ Network, data=nomissing, FUN=mean)

#Plot 3 
par(las=2) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.
barplot(sums$value, main="Diabetes during news by network", horiz=TRUE, names.arg=c("ABC", "CBS", "FOX", "NBC", "PBS"), cex.names=1.0, xlab = "")
dev.copy(png,"Network.png", width=480, height=480)
dev.off

sums <- aggregate(value ~ City, data=nomissing, FUN=mean)

#Plot 4
par(las=2) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin
barplot(sums$value, main="Diabetes, by network", horiz=TRUE, names.arg=c(""), cex.names=1.0, xlab = "% sentences with 'diabetes")
dev.copy(png,"Network.png", width=480, height=480)
dev.off

data(nomissing)
ftable(nomissing$City, nomissing$Network)

#okay - we need to limit to San Francisco an Washington DC - large and abuntant measures
TwoCities <- subset(nomissing, City == "San Francisco"| City == "Washington DC")  
#Check
str(TwoCities$City)
table(TwoCities$City)

#Do these cities differ in diabetes reporting
fit <- aov(value ~ Network, TwoCities) 
summary(fit)

sums <- aggregate(value ~ Network, data=TwoCities, FUN=mean)

#Plot 5 
par(las=2) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.
barplot(sums$value, main="Diabetes during news by network", horiz=TRUE, names.arg=c("ABC", "CBS", "FOX", "NBC", "PBS"), cex.names=1.0, xlab = "")
dev.copy(png,"Network.png", width=480, height=480)
dev.off


sums <- aggregate(value ~ City, data=TwoCities, FUN=mean)

#Plot 6
par(las=2) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin
barplot(sums$value, main="Diabetes, by city", horiz=TRUE, names.arg=c("San Francisco", "Washington DC"), cex.names=1.0, xlab = "% sentences with 'diabetes")
dev.copy(png,"Network.png", width=480, height=480)
dev.off

#Any interaction between Network and City?

#Plot 6 
sums <- aggregate(value ~ City + Network, data=TwoCities, FUN=mean)

ggplot(sums, aes(factor(Network), value, fill = City)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

dev.copy(png,"plot6.png", width=480, height=480)
dev.off()

#Argh - looks like no PBS in Washington DC 
noPBS <- subset(TwoCities, Network != "PBS")  
#Check
str(noPBS$Network)
table(noPBS$Network)

#Plot 7
sums <- aggregate(value ~ City + Network, data=noPBS, FUN=mean)

p1 <- ggplot(sums, aes(factor(Network), value, fill = City)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

p1 + annotate("text", x = 3.5, y = 0.06, label = "Diabetes") + annotate("text", x=3.75, y = 0.055, label = "mentioned") +
theme_bw()

dev.copy(png,"plot6.png", width=480, height=480)
dev.off()

#Might be useful to work by year too.
table(TwoCities$Year, TwoCities$Network, TwoCities$City)
#need to exclude 2009 b/c no data from SanFran
no2009 <- subset(noPBS, Year != "2009" &  Year != "2017")
no2009$Year <- as.factor((no2009$Year))

#Plot 8
sums <- aggregate(value ~ Year + Network, data=no2009, FUN=mean)

p1 <- ggplot(sums, aes(factor(Network), value, fill = Year)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

p1 + annotate("text", x = 3.5, y = 0.07, label = "Diabetes") + annotate("text", x=3.75, y = 0.065, label = "mentioned") +
  theme_bw()

dev.copy(png,"plot6.png", width=480, height=480)
dev.off()


fit <- aov(value ~ Network + City + Network:City, data=no2009)
summary(fit) 

fit <- aov(value ~ Network + Year + Network:Year, data=no2009)
summary(fit) 



#Now work on diabetes prevalence by year 
#2010 to 2016
CDC <- read_csv("C:/Users/Reed/Dropbox/Dani life/2017 Data Insight/Diabetes/CDC.csv")
sum
Limit <- subset(CDC, Year == "2010" | Year == "2011" | Year == "2012" | Year == "2013" | Year == "2014")
sums <- aggregate(Percentage ~ Year, data=Limit, FUN=mean)
str(sums)
p3 <- ggplot(data=sums, aes(x=Year, y=Percentage)) + geom_bar(stat="identity") 



p1 + annotate("text", x = 3.5, y = 0.07, label = "Diabetes") + annotate("text", x=3.75, y = 0.065, label = "mentioned") +
  theme_bw()

dev.copy(png,"plot6.png", width=480, height=480)
dev.off()
