download <- download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",destfile = "temp")
unzip("temp")
unlink("temp")


data <- read.table("household_power_consumption.txt", sep = ";", header = TRUE, na.strings = "?")
data$dt <- strptime(paste(data$Date, data$Time), "%d/%m/%Y %H:%M:%S")
data$Date <- as.Date(data$Date, "%d/%m/%Y")
data_sub <- subset(data, Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))

# Polt 1

hist(data_sub$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", ylab = "Frequency", main = "Global Active Power", breaks = 13, ylim = c(0,1200), xlim = c(0, 6))

# Polt 2

plot(data_sub$dt, data_sub$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")

# Polt 3

plot(data_sub$dt, data_sub$Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
points(data_sub$dt, data_sub$Sub_metering_2, type = "l", col = "red")
points(data_sub$dt, data_sub$Sub_metering_3, type = "l", col = "blue")
legend("topright", lty = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

# Polt 4

par(mfrow=c(2,2))
par(mar=c(4,4,4,4))
plot(data_sub$dt, data_sub$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
plot(data_sub$dt, data_sub$Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
plot(data_sub$dt, data_sub$Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
points(data_sub$dt, data_sub$Sub_metering_2, type = "l", col = "red")
points(data_sub$dt, data_sub$Sub_metering_3, type = "l", col = "blue")
legend("topright", lty = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), bty = "n", cex = 0.8)
plot(data_sub$dt, data_sub$Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power")
