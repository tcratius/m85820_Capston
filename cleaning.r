
library(chron)
library(dplyr)
#############################################################################
#
# 1. Clean data and arrange.
#
#############################################################################
park.space.fill <- read.csv(choose.files(), as.is = T)

park.space.fill[is.na(park.space.fill)] <- 0
  
park.space.fill$Date <- as.POSIXct(park.space.fill$Date.Time, tz='', '%d/%m/%Y %H:%M')

# Remove columns Date.Time.
park.space.fill <- subset(park.space.fill, select =  -Date.Time)


# Tidy name(s).
names(park.space.fill)[names(park.space.fill) == 'ï..Parking.space.num'] <- 'Parking.space.num'

names(park.space.fill)[names(park.space.fill) == 'Park.num.counted'] <- 'Check.4.Car'

#################################################################################
# May merge column 1 Parking space num with column 7 & 8 to create continuous
# of column 1.
park.space.new$Parking.space.num <- paste(park.space.new$Parking.space.num,".",
                                          park.space.new$Date, ".",
                                          park.space.new$Time)

park.space.new$Parking.space.num <-  apply(park.space.new
                                           ,2,function(x)gsub('\\s+', '', 
                                                              park.space.new$Parking.space.num ))
#################################################################################

# remove rows with NA's
park.space.filtered <- na.omit(park.space.new)


# write to file
write.table(park.space.fill, file = "Data/three/Park.space.ag5.data", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE,
            qmethod = c("escape", "double"), fileEncoding = "")
