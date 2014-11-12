getMysqlData <- function(queryString=""){
        #   View(queryString)
        con = dbConnect(dbDriver("MySQL"), user="dbmeteodb", 
                        password="dbmeteodb",
                        dbname="dbmeteodb",
                        host="localhost")
        
        # send the query
        #                 if (dbg) browser()
        #                 tryCatch(queryResultsData <- dbSendQuery(con, queryString),
        #                          finally = dbDisconnect(con))
        queryResultsData <- dbSendQuery(con, queryString)
        
        
        #get the data
        data <- fetch(queryResultsData, n=-1)
        # freeing resources
        dbClearResult(queryResultsData) 
        dbDisconnect(con)
        View(data)  
        data
}