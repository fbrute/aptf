# Get aot550 from modis files

main <- function() {
        
        files <- list.files("./modis/data/",pattern = ".txt$")

        for (i in 1:length(files)) {
                filename = paste("./modis/data/", files[i], sep="")
                
                gdata <- getData(filename)
                date <- getDate(filename)
                con <- getCon()
                data = gdata$aot[1]
                if (is.na(data)) data = 9.99999
                injectAot2Db(con, data, date)
                dbDisconnect(con)
        }
        
        
}

mainIntegration <- function(filename) {
        
        gdata <- getData(filename)
        date <- getDate(filename)
        con = getCon()
        injectAot2Db(con, gdata$aot[1], date)
        dbDisconnect(con)

}

getDate <- function(text) {
        # get date of sounding, 2012-08-01 (yy-mm-dd by default) 
#         posDate <- regexpr("[0-9]{2} [a-yA-Y]* [0-9]{4}$",text)
        posDate <- regexpr("[0-9]{2}[a-yA-Y]*[0-9]{4}",text)
        strDate <- substr(text, posDate[1],
                          posDate[1] + attr(posDate,"match.length") -1)
        #browser()
        
        # fix NAs with default local system
        Sys.setlocale("LC_TIME", "C")
        as.Date(strDate,"%d%b%Y") 
}
# receives a filename and return df
getData <- function(filename) {
        gdata <- read.delim(filename,
                            skip = 9, na.strings = "-9.99900e+03",
                            sep="", 
                            col.names=c("lat","long","aot"))
        
        
        library(dplyr)
        
        gdata2 <- filter(gdata, aot > 0 & lat > 16 & lat < 17 & long < -61 & long > -62)
        #gdata2 <- filter(gdata, aot > 0 & lat > 15 & lat < 18 & long < -60 & long > -63)
        
        gdata2
        
}

getCon <- function () {
        library(RMySQL)
        
        con = dbConnect(dbDriver("MySQL"), user="dbmeteodb", 
                        password="dbmeteodb",
                        dbname="dbmeteodb",
                        host="localhost")
}


getInsertSqlstr <- function(aot, date) {
        
        sqlstr <- paste("insert into aot550", 
                        "(date, aot)",
                        "values(",
                        "'", date, "'" ,",",
                        aot,")")
}

getSelectSqlstr <- function(date) {
        
        sqlstr <- paste("select aot from aot550 where date =", 
                        "'", date, "'")
}

injectAot2Db <- function(con, aot, date) {
                
        library(RMySQL)
       
        #browser()
        sqlstr = getSelectSqlstr(date)
        
        queryResultsData <- dbSendQuery(con, sqlstr)
        
        #get the data
        data <- fetch(queryResultsData, n=-1)
        # freeing resources
        dbClearResult(queryResultsData) 
        
        # If date already in db, cancels.         
        
        if (nrow(data) < 1) {
                sqlstr = getInsertSqlstr(aot, date)
                #browser()
                dbSendQuery(con, sqlstr)}
        
        
        
 
        
        #browser()
        #         tryCatch(dbSendQuery(con,
        #                     sqlstr), finally = dbDisconnect(con))
        
 
        #browser()
        
}