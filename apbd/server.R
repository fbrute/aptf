library(RMySQL)

library(shiny)

shinyServer(function (input, output) {
  
  output$apbmplot <- renderPlot({

    aotyeardata <- getData("aot")
    pm10_24data <- getData("pm10_24")
    pm10_1016data <- getData("pm10_1016")
    pm25data <- getData("pm25")
    #View(data)
#     View(aotdata)
#     View(pm10_24data)
#     View(pm10_1016data)
#     View(pm25data)
    
    pm10_24data <- subset(pm10_24data, select = c(date,hour,pm10_24))
    pm10_1016data <- subset(pm10_1016data, select = c(date,hour,pm10_1016))
    aotyeardata <- subset(aotyeardata, select = c(date,hour,aot500))
    aotdata <- filter(aotyeardata, date == input$date)
    aotdata <- aotdata[order(aotdata$date, aotdata$hour),]
    View(aotdata)
    # merge is left inner join from sql
    data <- merge(aotdata, pm10_24data, by=c("date","hour"),all.x = T, all.y = T)
    
    data <- merge(data, pm10_1016data, by=c("date","hour"),all.x = T, all.y = T)

    data <- merge(data, pm25data, by=c("date","hour"), all.x = T, all.y = T)


    data <- data[order(data$date, data$hour),]
     View(data)
    
    
    plot(data$hour,data$pm10_24, type='b', col='blue',ylim=c(1,100), 
         xlab= input$date)
    title("pm10global(blue), pm10_1016h(violet),aot500(green), pm25(red)")
    points(data$pm10_1016, type='b', col='violet')
    points(data$aot500*100, type='b', col='green')
    points(data$pm25, type='b', col='red')
#     plot(data$hour,data$aot500)
  })
prepData <- function () {
  
}
getData <- function(datatype=""){
  # Init string to know if it is valid at the end of the function
  QueryString <- ""
  
  if (datatype == "aot")
    QueryString <- paste( 
    "select year(date) as year, 
        month(date) as month, day(date) as day,
        hour(subtime(time,'04:00:00')) as hour,
        floor(julian_day) as julian_day,
        avg(aot_500) as aot500,
        date
    from aotv1500",
    "where year(date) =", substr(input$date,1,4), 
#       "and month(date) =", substr(input$date,6,7),
#       "and day(date) =", substr(input$date,9,10),
      "and aot_500 > 0 and aot_500 < 1 
     group by date,hour(time)
     order by date,hour(time);"
    )
  if (datatype == "pm10_24")
    QueryString <-paste(
    "select year(datetime) as year, 
      month(datetime) as month, day(datetime) as day,
      hour(datetime) as hour,
      cast(date_format(date(datetime),'%j') as decimal(5,0)) as julian_day , 
      avg(pmptp) as pm10_24 ,date(datetime) as date
    from pm10",
    "where year(datetime) =", substr(input$date,1,4),
      "and month(datetime) =", substr(input$date,6,7),
      "and day(datetime) =", substr(input$date,9,10),
      "and hour(datetime) > 0",
      "and pmptp > 0 
    group by hour(datetime)
    order by hour(datetime)")
  
  if (datatype == "pm10_1016")
    QueryString <- paste(
    "select year(datetime) as year, 
      month(datetime) as month, day(datetime) as day,
      hour(datetime) as hour,
      cast(date_format(date(datetime),'%j') as decimal(5,0)) as julian_day , 
      avg(pmptp) as pm10_1016 ,date(datetime) as date
    from pm10",
    "where year(datetime) =", substr(input$date,1,4),
      "and month(datetime) =", substr(input$date,6,7),
      "and day(datetime) =", substr(input$date,9,10),
      "and pmptp > 0 and hour(datetime) between 10 and 16
    group by hour(datetime)
    order by hour(datetime)")
  
  if (QueryString != "")
    return(getMysqlData(QueryString))
  
  if (QueryString == "")
    getPm25Data()
}

getPm25Data <- function(){
  library(dplyr)
  pm25data <- read.csv("~/Documents/Trafin/aptf/pm2.5/PM25_hourly_2012.csv")
  #View(pm25data)
  # rename columns
  colnames(pm25data) <- c("DateHour", "Jour","pm25", "pmdeb","x", "tc2")
  pm25data <- subset(pm25data, select = c(DateHour, pm25))
  pm25data <- mutate(pm25data, date = paste(substr(DateHour,1,10)))
  pm25data <- mutate(pm25data, hour = as.integer(substr(DateHour,12,13)))
  
  # reorder date from dd/mm/yyyy to yyyy/mm/dd (french to english)
  pm25data <- mutate(pm25data, date = paste(substr(DateHour,7,10),"-", substr(DateHour,4,5),"-", substr(DateHour,1,2), sep=''))
  
  # filter to retain selected day
  #pm25data <- filter(pm25data, as.integer(substr(DateHour,4,5))== input$month)
  #View(pm25data)
  pm25data <- filter(pm25data, date == input$date & hour > 0)
  
  # select columns of interest
  #pm25data <- subset(pm25data,select = c("date","pm25"))
  pm25data <- select(pm25data,date,hour,pm25)
}

getMysqlData <- function(queryString=""){
#   View(queryString)
  con = dbConnect(dbDriver("MySQL"), user="dbmeteodb", 
                  password="dbmeteodb",
                  dbname="dbmeteodb",
                  host="localhost")
  
  # send the query
  queryResultsData <- dbSendQuery(con, queryString)
    
    
  #get the data
  data <- fetch(queryResultsData, n=-1)
  # freeing resources
  dbClearResult(queryResultsData) 
  dbDisconnect(con)
  View(data)  
  data
}

})
# TODO
# décalage d'une heure entre pm10_16 et pm10_24, ok entre 1 et 23 pour l'instant
# comment traiter les jours où il y a peu de données aot ? moins de 3 ?
