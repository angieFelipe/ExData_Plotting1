#################################################################
#                ExData_Plotting1                               #
#                ================                               #
#                                                               #
#         Plotting Assignment 1 for Exploratory Data Analysis   #
#################################################################

#creation date: 20/1/2018
#final date: 20/1/2018

# REQUIRED PACKAGES
# 
lib<-c("dplyr" )

lapply(lib, function(lib) {if (!require(lib, character.only=T)) 
{install.packages(lib);require(lib)}}) 



#Environtment required
#=====================
#

#if english dates should be required 
#

Sys.setenv(TZ="Europe/Madrid")

Sys.setlocale("LC_TIME","en_US.UTF-8")


# FILES & LINKS
#


if(!file.exists("data")) {
        dir.create("data")
}
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(fileUrl, destfile="./data/power.zip", method="curl")
dateDownloaded <- date()

zipF<- "./data/power.zip"
outDir<-"./data"
unzip(zipF,exdir=outDir,overwrite = TRUE)

ldb<-'./data/household_power_consumption.txt'

######################
#  VARIABLE GLOSSARY #
######################



# Date: (vdate) Date in format dd/mm/yyyy 
# Time: (vtime) time in format hh:mm:ss 
# Global_active_power:(GAP) household global minute-averaged active power (in
# kilowatt)
# Global_reactive_power: (GRP) household global minute-averaged reactive
# power (in kilowatt)
# Voltage: (Voltage) minute-averaged voltage (in volt) 
# Global_intensity: (GI) household global minute-averaged current intensity (in
# ampere)
# Sub_metering_1: (SM1) energy sub-metering No. 1 (in watt-hour of active
# energy). It corresponds to the kitchen, containing mainly a dishwasher, an
# oven and a microwave (hot plates are not electric but gas powered). 
# Sub_metering_2: (SM2) energy sub-metering No. 2 (in watt-hour of active energy). It
# corresponds to the laundry room, containing a washing-machine, a tumble-drier,
# a refrigerator and a light.
# Sub_metering_3: (SM3) energy sub-metering No. 3 (in
# watt-hour of active energy). It corresponds to an electric water-heater and an
# air-conditioner.



#####################
#  LOADING DATA     #
#####################

# there is 1 line per minute, the file starts on 16/12/2006 17:24 and we are
# interested in lines between 1/2/2007 00:00 and 2/2/2007 23:59. A drat
# calculation of the lines to capture has been done, after a precise database
# will be obtained

varnames<-c("vdate","vtime","GAP","GRP","Voltage","GI","SM1","SM2","SM3")

exdataset <- read.csv2(
        ldb,
        header = F,
        sep = ";",
        dec=".",
        nrows = 4000,
        skip = 66000,
        quote = "",
        na.strings = "?",
        numerals = c("warn.loss"),
        col.names = varnames,
        strip.white = T,
        comment.char = "",
        stringsAsFactors = FALSE
        
)

#adequating date & time class

exdataset<-mutate(exdataset,datetime=paste(vdate," ",vtime))
exdataset$datetime<-strptime(exdataset$datetime,"%d/%m/%Y %H:%M:%S")
exdataset$datetime<-as.POSIXct(exdataset$datetime)

#filtering desired dates for analysis

exdataset<-filter(exdataset, datetime>"2007-01-31 23:59:59" & datetime<"2007-02-03")

#####################
#      PLOT 4       #
#####################
png(file = "plot4.png",
    width = 480, height = 480)


par(mfcol=c(2,2),mar=c(4,4,2,2))

with(exdataset,{
        
        #top left graph        
        plot(datetime,GAP,
             type="l",
             ylab="Global Active Power (kilowatts)",
             xlab = ""
        )
        
        #bottom left graph
        plot(datetime,SM1,
             type="l",
             ylab="Energy sub metering",
             xlab = "")
        
        lines(exdataset$datetime,exdataset$SM2, col="red")
        lines(exdataset$datetime,exdataset$SM3, col="blue")
        
        legend(
                "topright",
                bty="n",
                lty= c(1,1,1),
                col = c("black","blue", "red"),
                legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"))
        
        #top right graph   
        plot(datetime,Voltage, type = "l")
        
        #bottom right graph   
        plot(datetime,GRP,
             type = "l",
             ylab = "Global_rective_power")
        
        
})




dev.off()
#to set graph parameter to default
par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1))
