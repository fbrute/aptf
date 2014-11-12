.setUp <- function () {
        library(RMySQL)
        
        library(testthat)
         
}

test.mainIntegration <- function() {
        
        con = getCon()
        
        sqlstr <- paste("delete from aot550 where date = '2000-12-07'")
        queryResultsData <- dbSendQuery(con, sqlstr)
        dbClearResult(queryResultsData)
        
        filename <- "./modis/data/MOD08_D3.051.Optical_Depth_Land_And_Ocean_Mean.07Dec2000.G3.input.txt"
        
        df.modis = mainIntegration(filename)
        
        
        sqlstr <- paste("select aot from aot550 where date = '2000-12-07'")
        
        
        queryResultsData <- dbSendQuery(con, sqlstr)
        
        
        #get the data
        data <- fetch(queryResultsData, n=-1)
        # freeing resources
        dbClearResult(queryResultsData) 
        
        #         
        
        expect_true(nrow(data) == 1)
        expect_true(data$aot[1] == 0.076)
        
        dbDisconnect(con)
}

test.getData <- function() {
        filename <- "./modis/data/MOD08_D3.051.Optical_Depth_Land_And_Ocean_Mean.01Apr2012.G3.input.txt"
        df.modis = getData(filename)
        checkTrue(is.data.frame(df.modis))
        checkEquals(names(df.modis),c("lat","long","aot"))
        print(df.modis$aot[1])
        expect_true(df.modis$aot[1] == 0.134)
}

test.getDate <- function() {
        
        filename <- "./modis/data/MOD08_D3.051.Optical_Depth_Land_And_Ocean_Mean.01Apr2012.G3.input.txt"
        modis.date = getDate(filename)
        # fix NAs with default local system
        Sys.setlocale("LC_TIME", "C")
        
        checkTrue(!is.na(modis.date))
        checkEquals(modis.date, as.Date("20120401","%Y%m%d"))
        
        test_that("tests", {expect_false(modis.date == as.Date("20071225","%Y%m%d"))})
}

test.getCon <-function () {
        con = getCon()
        expect_true(class(con) == "MySQLConnection")
        expect_true(dbDisconnect(con))
}


test.getInsertSqlstr <-function ()  {
        sqlstr <- getSqlstr(aot= 0.777, date= as.Date("2012-04-01"))
        #browser()
        expect_true(sqlstr == "insert into aot550 (date, aot) values( ' 2012-04-01 ' , 0.777 )") 
}

test.getSelectSqlstr <-function ()  {
        sqlstr <- getSelectSqlstr(date= as.Date("2012-04-01"))
        #browser()
        expect_true(sqlstr == "select aot from aot550 where date = ' 2012-04-01 '") 
}

# DEACTIVATED()

test.getMysqldata <- function() {
        
        
}

test.injectAot2Db <- function() {
        
        #browser()
        library(RMySQL)
        
        # First test where date doesn't already exist in db
        
        con = getCon()
        
        sqlstr <- paste("delete from aot550 where date = '1980-01-01'")
        queryResultsData <- dbSendQuery(con, sqlstr)
        dbClearResult(queryResultsData)
        
        
        
        injectAot2Db(con, 0.777, as.Date("1980-01-01"))
        
        sqlstr <- paste("select aot from aot550 where date = '1980-01-01'")
        
        
        queryResultsData <- dbSendQuery(con, sqlstr)
        
        
        #get the data
        data <- fetch(queryResultsData, n=-1)
        # freeing resources
        dbClearResult(queryResultsData) 
        
        
        expect_true(nrow(data) == 1)
        expect_true(data$aot[1] == 0.777)
        
        # Data already in db, try to inject again 
        
        injectAot2Db(con, 0.777, as.Date("1980-01-01"))
        
        sqlstr <- paste("select aot from aot550 where date = '1980-01-01'")
        
        
        queryResultsData <- dbSendQuery(con, sqlstr)
        
        
        #get the data
        data <- fetch(queryResultsData, n=-1)
        # freeing resources
        dbClearResult(queryResultsData) 
        
        #         
        
        expect_true(nrow(data) == 1)
        expect_true(data$aot[1] == 0.777)
        
        dbDisconnect(con)
        
        
}
