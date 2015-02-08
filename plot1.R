## Exploratory Data Analysis - Course Project 1 - January 2013
## Read Date
## by Fernando Torres

## The First Project involves creating several charts from a data set
## the readHouseholdFile function reads the appropriate subset of data from the data source

{
      
      ## the path2files variable allows to customize the location files are read from
      ## currently, the script requires the files to be located in the in the base directory.
      path2files <- ""
      file2read <- paste(path2files,"household_power_consumption.txt", sep = "")
      
      ## To speed up the reading, we use the technique of reading 5 lines to determine the column classes
      tab5rows <- read.table(file2read, header=TRUE, nrows=5, sep =";")
      classes <- sapply(tab5rows, class)
      classes[1] <- "character"
      rfile <- read.table(file2read, header=TRUE, colClasses = classes, nrows=1, sep =";")
      
      ## Upon inspection, the source file is sorted by Date. By definition of the project, we need to use data
      ## from Feb 1st and 2nd, 2007.
      ## We are going to scan the file in order to read only the the required data.
      ## This first loop scans the file to find the first line dated Feb-1-2007
      read_line <- TRUE
      rows2skip <- 0
      skipfactor <- 10000
      while(read_line) {
            dfile <- read.table(file2read, header=TRUE, colClasses = classes, skip = rows2skip, nrows=1, sep =";")
            rdate <- as.Date(dfile[1,1],"%d/%m/%Y")
            if(rdate >= '2007-2-1' ) {
                  rows2skip <- rows2skip - skipfactor
                  skipfactor <- skipfactor / 10
            } else {rows2skip <- rows2skip + skipfactor}
            if (skipfactor < 1 ) {read_line <- FALSE}
      }

      ## The second loop scans the file to find the first line dated Feb-3-2007
      firstdatarow <- rows2skip + 1
      skipfactor <- 10000
      read_line <- TRUE
      while(read_line) {
            dfile <- read.table(file2read, header=TRUE, colClasses = classes, skip = rows2skip, nrows=1, sep =";")
            rdate <- as.Date(dfile[1,1],"%d/%m/%Y")
            if(rdate > '2007-2-2' ) {
                  rows2skip <- rows2skip - skipfactor
                  skipfactor <- skipfactor / 10
            } else {rows2skip <- rows2skip + skipfactor}
            if (skipfactor < 1 ) {read_line <- FALSE}
      }
            
      ## Finally, we read only the appropriate rows into the dataframe dfile and we convert the first two columns
      ## to time and date, respectively.
      rows2read <- rows2skip - firstdatarow + 1
      dfile <- read.table(file2read, header=TRUE, colClasses = classes, skip = firstdatarow, nrows=rows2read, sep =";")
      colnames(dfile) <- colnames(tab5rows)     
      dfile$Time <- strptime(paste(dfile$Date,dfile$Time), format = "%d/%m/%Y %H:%M:%S")
      dfile$Date <- as.Date(dfile$Date, "%d/%m/%Y")
      
      
      ## This final section creates the file which is the object of the project
      png(file = "plot1.png", width = 480, height = 480)
      hist(dfile$Global_active_power, main = NULL, sub = NULL, xlab = NULL, ylab = NULL, col="red")
      title(main = "Global Active Power", xlab = "Global Active Power (kilowatts)", ylab = "Frequency")
      dev.off()
      
}