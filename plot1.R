plot1 <- function (){

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
  
  suppressWarnings(subset[, c("Global_active_power")] <- as.numeric(subset[,c("Global_active_power")]))
  
  # draw plot here 
  par(mfrow = c(1, 1), mar = c(5, 4, 2, 1))
  hist(subset$Global_active_power, col = "red",xlab="Global Active Power (kilowatts)",main= "Global Active Power")
 
  # copy plot to a PNG file 
  dev.copy(png, file = "plot1.png",width=480,height=480) 
  dev.off() 
}