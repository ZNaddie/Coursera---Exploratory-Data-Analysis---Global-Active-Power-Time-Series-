library(plyr)
library(ggplot2)
library(data.table)
library(grid)
library(scales)
library(httr)

datalink = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(datalink, "Summary.zip", method = "curl")

unzip(zipfile = "Summary.zip")
unlink("Summary.zip")

summarydata <- readRDS("summarySCC_PM25.rds")
sourcedata <- readRDS("Source_Classification_Code.rds")

View(summarydata)
View(sourcedata)

summarydata <- transform(summarydata, SCC = factor(SCC))
summarydata <- transform(summarydata, year = factor(year))

#1
total <- aggregate(Emissions ~ year,summarydata, sum)
head(total)
png(filename = "Exploratory Data Analysis - P2 - Global Emissions Analysis - Plot 1.png", width = 300, height = 300, units = "px", bg = "white")
barplot(total$Emissions, xlab="Years", ylab="Emissions", main = "Emission Totals", names.arg = total$year, horiz=TRUE, col=c("darkgrey", "brown", "darkblue","red"))
dev.off()

#2
baltimore <- summarydata[which(summarydata$fips == 24510),]
head(baltimore)
totalbaltimore <- aggregate(Emissions ~ year, baltimore, sum)
head(totalbaltimore)
png(filename = "Exploratory Data Analysis - P2 - Global Emissions Analysis - Plot 2.png", width = 300, height = 300, units = "px", bg = "white")
barplot(totalbaltimore$Emissions, xlab="Years", ylab="Emissions", main = "Total Baltimore emissions", names.arg = totalbaltimore$year, col=c("darkblue","red"))
dev.off()

#3
type_year_totals <- aggregate(summarydata$Emissions, list(summarydata$type, summarydata$year),mean)
type_year_totals
png(filename = "Exploratory Data Analysis - P2 - Global Emissions Analysis - Plot 3.png", width = 300, height = 300, units = "px", bg = "white")
g3 <- ggplot(type_year_totals, aes(y=x, x=Group.2))+geom_point(aes(colour = factor(Group.1)), size = 6, fill="black")+geom_line(aes(group=Group.1,colour=Group.1), size = 1)
g3
dev.off()
 
#4
coal_SCC <- summarydata[grep("*coal*|*Coal*",summarydata$Short.Name),]$SCC
head(coal_SCC)
coal_data <- sourcedata[sourcedata$SCC %in% coal_SCC,]
head(coal_data)
agg_coal <- aggregate(coal_data, list(coal_data$year), mean)
png(filename = "Exploratory Data Analysis - P2 - Global Emissions Analysis - Plot 4.png", width = 300, height = 300, units = "px", bg = "white")
g4 <- ggplot(agg_coal, aes(x=Group.1, y=Emissions, group =1)) + geom_point(aes(colour = factor(Group.1)), size = 4)+geom_line()
g4
dev.off()

#5
vehicle_SCC <- sourcedata[grep("*vehicle*|*Vehicle*",sourcedata$Short.Name),]$SCC
motor_baltimore <- baltimore[baltimore$SCC %in% vehicle_SCC,]
head(vehicle_SCC)
head(motor_baltimore)
agg_motor_baltimore <- aggregate(motor_baltimore$Emissions, list(motor_baltimore$year),mean)
agg_motor_baltimore
png(filename = "Exploratory Data Analysis - P2 - Global Emissions Analysis - Plot 5.png", width = 300, height = 300, units = "px", bg = "white")
g5 <- ggplot(agg_motor_baltimore, aes(x=Group.1, y=x, group =1)) + geom_point(aes(colour = factor(Group.1)), size = 5) + geom_line(size=2, color="brown") + xlab("Years") + ylab("Emissions")+ ggtitle("Emissions Fluctuations in Baltimore over time")
g5
dev.off()

#6
LA <- summarydata[which(summarydata$fips == "06037"),]
baltimore <- summarydata[which(summarydata$fips == "24510"),]
motor_LA <- LA[LA$SCC %in% vehicle_SCC,]
motor_baltimore <- baltimore[baltimore$SCC %in% vehicle_SCC,]
agg_motor_LA <- aggregate(motor_LA$Emissions, list(motor_LA$year),mean)
agg_motor_baltimore <- aggregate(motor_baltimore$Emissions, list(motor_baltimore$year),mean)
agg_motor_LA$city <- rep("LA",4)
agg_motor_baltimore$city <- rep("Baltimore",4)
agg_motor_LA
agg_motor_baltimore
LA_Baltimore <- rbind.data.frame(agg_motor_baltimore,agg_motor_LA)
LA_Baltimore
png(filename = "Exploratory Data Analysis - P2 - Global Emissions Analysis - Plot 6.png", width = 300, height = 300, units = "px", bg = "white")
g6 <- ggplot(LA_Baltimore, aes(x=factor(Group.1), y=x, group =city)) + geom_point(aes(colour = city), size = 4)+geom_line(aes(colour=city), size = 2)+ xlab("Years") + ylab("Motor Emissions")+ ggtitle("Comparison of Motor Source Related Emissions between LA & Baltimore over time")
g6
dev.off()

