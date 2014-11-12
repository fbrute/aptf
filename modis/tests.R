modis.test <- function () {
        
        library(RUnit)

testsuite.modis <- defineTestSuite("modis", 
                                   dirs = file.path(paste(getwd(),
                                "/modis",sep="")),
                                testFileRegexp = "^runit.+\\.R", 
                                testFuncRegexp = "^test.+", 
                                rngKind = "Marsaglia-Multicarry", 
                                rngNormalKind = "Kinderman-Ramage")

#source("~/Documents/Trafin/aptf/aptf2k14.R/modis/utils.R")

source('~/Documents/Trafin/aptf/aptf2k14.R/modis/getmodis.R')


testResult <- runTestSuite(testsuite.modis)

print(testResult)

}
