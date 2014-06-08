plot4 <- function (){

  # read file
  ePCFrame <- read.csv("../../data/household_power_consumption.csv",sep=";",na.string="?", colClasses = "character")
  
  # remove NA
  completeEPCFrame <- ePCFrame[complete.cases(ePCFrame[,c("Date","Time","Global_active_power")]),] 
  
  #change to date
  suppressWarnings(completeEPCFrame[, c("Date")] <- as.Date(completeEPCFrame[,c("Date")],format = "%d/%m/%Y"))
  
  # choose subset
  toDate <- as.Date("01/02/2007",format = "%d/%m/%Y")
  fromDate <- as.Date("02/02/2007",format = "%d/%m/%Y")
  subset <- completeEPCFrame[(completeEPCFrame$Date == toDate | completeEPCFrame$Date == fromDate),]                  
  
  # convert timestamp here
  subset <- within(subset, { timestamp=format(as.POSIXct(paste(Date,Time)), "%d/%m/%Y %H:%M:%S") })
  subset$timestamp <- strptime(subset$timestamp, "%d/%m/%Y %H:%M:%S")
  
  
  #draw plot
  par(mfrow = c(2, 2), mar = c(5, 4, 2, 1))
  
  # draw plot 1
  plot(subset$timestamp, subset$Global_active_power,type="n", main="",ylab="Global Active Power",xlab="") 
  lines(subset$timestamp, subset$Global_active_power, type="S") 
  
  # draw plot 2
  plot(subset$timestamp, subset$Voltage,type="n", main="",ylab="Voltage",xlab="datetime") 
  lines(subset$timestamp, subset$Voltage, type="S") 
  
  # draw plot 3
  plot(subset$timestamp, subset$Sub_metering_1,type="n", main="",ylab="Energy sub metering",xlab="",) 
  
  lines(subset$timestamp, subset$Sub_metering_1, type="S",col="black", pch="Sub_metering_1") 
  lines(subset$timestamp, subset$Sub_metering_2, type="S",col = "red", pch="Sub_metering_2")
  lines(subset$timestamp, subset$Sub_metering_3, type="S",col="blue", pch="Sub_metering_3")
  
  # add a legend 
  legend('topright', c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), cex=.50, bty='n',col=c("black","red","blue"),lty=1, title="")
  
  # draw plot 4
  plot(subset$timestamp, subset$Global_reactive_power,type="n", main="",ylab="Global_reactive_power",xlab="datetime") 
  lines(subset$timestamp, subset$Global_reactive_power, type="S") 
  
  # copy plot to a PNG file 
  dev.copy(png, file = "plot4.png",width=480,height=480) 
  dev.off()
  
}