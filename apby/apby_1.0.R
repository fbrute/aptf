mainapby <- function () {
        dbg <- 0
        date_start = "2012-01-01"
        date_stop = "2012-12-31"
        
        library(RMySQL)
        lblmonths <- c("January","February","March","April","May","June","July","August",
                       "September","October","November","December")
        # lblmonth <- lblmonths[month]
        # config <- list(year = 2012,
        #                month = 7,
        #                lblmonth = lblmonths[month])
        
        config <- list(year = 2012,
                       month = 7)
        
        con = dbConnect(dbDriver("MySQL"), user="dbmeteodb", 
                        password="dbmeteodb",
                        dbname="dbmeteodb",
                        host="localhost")
        
        # data available by hour. So aggregate by the day.
        aotquery <- dbSendQuery(con,
                                paste( 
                                        "select year(date) as year,", 
                                                "month(date) as month, day(date) as day,",
                                                "floor(julian_day) as julian_day,",
                                                "avg(aot_500) as aot500,",
                                                "date",
                                        "from aotv1500",
                                        "where date between ","'", date_start, "'"," and ","'", date_stop, "'","",
                                                "and aot_500 > 0 and aot_500 < 1", 
                                        "group by date"
                                )
        )
        if (dbg) browser()
        
        aotdata <- fetch(aotquery, n=-1)
        
        if (dbg) browser()
        
        
        #View(d);
        #plot(aotdata$aot500*100)
        
        #dbClearResult(dbListResults(con)[[1]])
        dbClearResult(aotquery)
        
        
        #where datetime between ","'", date_start, "'"," and '2012/10/31'
        pm10_24query <- dbSendQuery(con, 
                                    
                                    paste(
                                            "select year(datetime) as year,", 
                                                "month(datetime) as month, day(datetime) as day,",
                                                "cast(date_format(date(datetime),'%j') as decimal(5,0)) as julian_day," ,
                                                "avg(pmptp) as pm10_24 ,date(datetime) as date",
                                            "from pm10",
                                            "where datetime between ","'", date_start, "'"," and ","'", date_stop, "'","",
                                                "and pmptp > 0", 
                                            "group by date(datetime)",
                                            "order by date(datetime);"
                                    )
        )
        
        pm10_24data <- fetch(pm10_24query, n=-1);
        
        dbClearResult(pm10_24query)
        
        pm10_1016query <- dbSendQuery(con, 
                                      paste(
                                              "select year(datetime) as year,", 
                                                "month(datetime) as month, day(datetime) as day,",
                                                "cast(date_format(date(datetime),'%j') as decimal(5,0)) as julian_day," ,
                                                "avg(pmptp) as pm10_1016 ,date(datetime) as date",
                                              "from pm10",
                                              "where datetime between ","'", date_start, "'"," and ","'", date_stop, "'","",
                                                "and pmptp > 0",
                                                "and hour(datetime) between 10 and 16",
                                              "group by date(datetime)",
                                              "order by date(datetime);"
                                      )
        )
        
        pm10_1016data <- fetch(pm10_1016query, n=-1);
        
        dbClearResult(pm10_1016query)
        
        pm10_0817query <- dbSendQuery(con, 
                                      paste(
                                              "select year(datetime) as year,", 
                                              "month(datetime) as month, day(datetime) as day,",
                                              "cast(date_format(date(datetime),'%j') as decimal(5,0)) as julian_day," ,
                                              "avg(pmptp) as pm10_0817 ,date(datetime) as date",
                                              "from pm10",
                                              "where datetime between ","'", date_start, "'"," and ","'", date_stop, "'","",
                                              "and pmptp > 0",
                                              "and hour(datetime) between 8 and 17",
                                              "group by date(datetime)",
                                              "order by date(datetime);"
                                      )
        )
        
        pm10_0817data <- fetch(pm10_0817query, n=-1);
        
        dbClearResult(pm10_0817query)
        
        dbDisconnect(con)
        
        # get pm25 data from csv file
        library(dplyr)
        pm25data <- read.csv("~/Documents/Trafin/aptf/pm2.5/PM25_daily_2012.csv")
        # rename columns
        colnames(pm25data) <- c("DateHour", "Jour","pm25", "pmdeb","x", "tc2")
        pm25data <- subset(pm25data, select = c(DateHour, pm25))
        
        # filter to retain selected month
        pm25data <- filter(pm25data, as.integer(substr(DateHour,4,5))== config$month)
        
        # reorder date from dd/mm/yyyy to yyyy/mm/dd (french to english)
        pm25data <- mutate(pm25data, date = paste(substr(DateHour,7,10),"-", substr(DateHour,4,5),"-", substr(DateHour,1,2), sep=''))
        
        # select columns of interest
        #pm25data <- subset(pm25data,select = c("date","pm25"))
        pm25data <- select(pm25data,date,pm25)
        #View(d);
        #plot(pm10data$pm10_24)
        
        #dbClearResult(dbListResults(con)[[1]])
        #dbClearResult(dbListResults(con))
        pm10_24data <- subset(pm10_24data, select = c(date,pm10_24))
        pm10_1016data <- subset(pm10_1016data, select = c(date,pm10_1016))
        pm10_0817data <- subset(pm10_0817data, select = c(date,pm10_0817))
        aotdata <- subset(aotdata, select = c(date,aot500))
        
        if (dbg) browser()
        
        # merge is left inner join from sql
        data2k12 <- merge(aotdata, pm10_24data, by="date",all.x = T, all.y = T)
        data2k12 <- merge(data2k12, pm10_1016data, by="date",all.x = T, all.y = T)
        data2k12 <- merge(data2k12, pm10_0817data, by="date",all.x = T, all.y = T)
        data2k12 <- merge(data2k12, pm25data, by="date", all.x = T, all.y = T)
        data2k12 <- data2k12[order(data2k12$date),]
        
        save(data2k12, file="data2k12byday.RData")
        
        # plot(data2k12$pm10_24, type='b', col='blue',ylim=c(1,100), 
        #      xlab=paste(c("days of",config$lblmonth," ",config$year)))
        # title("pm10global(blue), pm10_1016h(violet),aot500(green), pm25(red)")
        # points(data2k12$pm10_1016, type='b', col='violet')
        # points(data2k12$aot500*100, type='b', col='green')
        # points(data2k12$pm25, type='b', col='red')
        
        library(lattice)
        data2k12 <- mutate(data2k12, month= as.numeric(substr(date,6,7)))
        xyplot(pm10_24 ~ aot500*100 | month, data = data2k12, layout =c(4,3))
        xyplot(pm10_1016 ~ aot500*100 | month, data = data2k12, layout =c(4,3))
        library(ggplot2)
        qplot(pm10_24, aot500 , data = data2k12, color=month, geom = c("point"), method = "lm")
        # histogram
        qplot(pm10_24, data = data2k12, fill=month)
        qplot(log(pm10_24), data= data2k12, geom = "density")
        
        cor10_24 = round(cor(data2k12$pm10_24, data2k12$aot500, use="complete.obs"),2)
        g <- ggplot(data2k12, aes(aot500, pm10_24) ) 
        g <- g + geom_point() + geom_smooth(method="lm") + ggtitle(paste("2012","(cor=",cor10_24,")" ))
        
        print(g)
        ggsave(g, file="reg2012_pm10_24.pdf")
        
        cor10_1016 = round(cor(data2k12$pm10_1016, data2k12$aot500, use="complete.obs"),2)
        g <- ggplot(data2k12, aes(aot500, pm10_1016) ) 
        g <- g + geom_point() + geom_smooth(method="lm") + ggtitle(paste("2012","(cor=",cor10_1016,")" ))
        
        print(g)
        ggsave(g, file="reg2012_pm10_1016.pdf")
        
        cor10_0817 = round(cor(data2k12$pm10_0817, data2k12$aot500, use="complete.obs"),2)
        g <- ggplot(data2k12, aes(aot500, pm10_0817) ) 
        g <- g + geom_point() + geom_smooth(method="lm") + ggtitle(paste("2012","(cor=",cor10_0817,")" ))
        
        print(g)
        ggsave(g, file="reg2012_pm10_0817.pdf")
        
        
#         qplot(aot500, pm10_24, data= data2k12, geom = c("point", "smooth"), method = "lm", main="2012")
#         qplot(aot500, pm10_1016, data= data2k12, geom = c("point", "smooth"), method = "lm", main="2012")
#         qplot(aot500, pm10_0817, data= data2k12, geom = c("point", "smooth"), method = "lm", main="2012")
#         
        #library(ggplot2)
        #qplot(pm10_24, data=data)
        data2 <- na.omit(select(data2k12,pm10_24, aot500))
        datacor24 <- cor(data2$pm10_24, data2$aot500)
        # datacor242 <- lm(data2$pm10_24 ~ data2$aot500)
        #View(datacor24)
        data3 <- na.omit(select(data2k12,pm10_1016, aot500))
        datacor1016 <- cor(data3$pm10_1016, data3$aot500)
        #View(c(datacor24,datacor1016))
        # merge and allow NA values 
        # data.all <- merge(aotdata, pm10data, all.x = T, all.y = T)
        # plot(data.all$pm10_24, type='b', col='blue')
        # points(data.all$aot500*100, type='b', col='green')
}