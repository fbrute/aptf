mainRegModis <- function () {
        dbg <- 0
        date_start = "2012-01-01"
        date_stop = "2012-11-20"
        
        library(RMySQL)
        lblmonths <- c("January","February","March","April","May","June","July","August",
                       "September","October","November","December")
        # lblmonth <- lblmonths[month]
        # config <- list(year = 2012,
        #                month = 7,
        #                lblmonth = lblmonths[month])
        
        config <- list(year = 2012,
                       month = 7)
        
        data2k12 <- getData(date_start, date_stop)
        #if dbg browser()
        periode = paste(date_start," to ",date_stop, sep='')
        output(data2k12, periode)
}

getData = function(date_start, date_stop) {
        con = dbConnect(dbDriver("MySQL"), user="dbmeteodb", 
                        password="dbmeteodb",
                        dbname="dbmeteodb",
                        host="localhost")
        
        # data available by hour. So aggregate by the day.
        aotquery <- dbSendQuery(con,
                                paste( 
                                        "select date, hour , avg(aot500) as aot500, count(date) as nbhours from",
                                                "(select date, hour(subtime(time,'03:00:00')) as hour,",
                                                        "avg(aot_500) as aot500",
                                                        "from aotv1500",
                                                        "where date between ","'", date_start, "'"," and ","'", date_stop, "'","",
                                                        "and aot_500 > 0 and aot_500 < 1",
                                                        "and date not in (select date from exdatesaptf2k14)",
                                                        "group by date, hour) as aot",
                                        "group by date",
                                        "having nbhours >= 5"
                                )
        )
        #if (dbg) browser()
        
        aotdata <- fetch(aotquery, n=-1)
        
        #if (dbg) browser()
        
        
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
        
        aot550query <- dbSendQuery(con,  "select date, aot as aotmodis from aot550 where year(date) = 2012 and aot < 1")

        
        aot550data <- fetch(aot550query, n=-1);
        
        dbClearResult(aot550query)
        
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
        aotdata.dates <- subset(aotdata, select = c(date))
        
        #browser()
        
        #aot550data <- merge(aot550data, aotdata.dates, by="date", all.x = T, all.y = T)
        # restrict to safe dates of aot500 (photometer)
        #aot550data <- merge(aot550data, aotdata.dates, by="date")
        
        
        
        # merge is left inner join from sql
        data2k12 <- merge(aotdata, pm10_24data, by="date",all.x = T, all.y = T)
        data2k12 <- merge(data2k12, pm10_1016data, by="date",all.x = T, all.y = T)
        data2k12 <- merge(data2k12, pm10_0817data, by="date",all.x = T, all.y = T)
        data2k12 <- merge(data2k12, pm25data, by="date", all.x = T, all.y = T)
        data2k12 <- merge(data2k12, aot550data, by="date", all.x = T, all.y = T)
        
#         data2k12 <- subset(data2k12, date)
        
        data2k12 <- data2k12[order(data2k12$date),]
        
        fname <- paste(as.character(date_start),
                       "_",
                       as.character(date_stop),
                       "_",
                       "aotnsup4",
                       ".RData",
                       sep="")
        
        save(data2k12, file=fname)
        
        
        data2k12
        
        
}

output = function(data, periode) {
        
        output2(periode, data, "aot500", "aotmodis", xpos=0.4, ypos=0.2,xinc=0.05, yinc=0.05)
        output2(periode, data, "aotmodis", "pm10_24",xpos=0.5, ypos=25, xinc=5,    yinc=5)
}

output2 = function(periode, data2k12, x_string, y_string, xpos=0.4, ypos=0.2,xinc=0.05, yinc=0.05) {
        #         output.cors(Data = data2k12, x= "aot500", y = "pm10_24",
        #                     data.x= data2k12$aot500, data.y= data2k12$pm10_24, periode)
        
        basename <- paste(as.character(date_start),
                       "_",
                       as.character(date_stop),
                       "_",
                       "aotnsup4_exdatesmodis",
                       y_string,
                       "~",
                       x_string,
                       ".",
                       sep="")
        
        fname = paste(basename, "RData",sep='')
        
        x = data2k12[grepl(names(data2k12), pattern = x_string)]
        y = data2k12[grepl(names(data2k12), pattern = y_string)]
        
        #data2k12 = na.omit(select(data2k12,date,y,x))
        data2k12 = na.omit(subset(data2k12, select =c("date", eval(x_string), eval(y_string))))
        #colnames(data2k12.pm10_24.aot500) = c("idx","date","pm10_24","aot500")
        save(data2k12, file=fname)
                
        
        cor2k12 = round(cor(x, y , use="complete.obs"),2)
        
        formula = paste(y_string, "~", x_string)
        lmfit = lm( formula, data2k12)
        
        a2k12 = round(lmfit$coefficients[2],2)
        b2k12 = round(lmfit$coefficients[1],2)
        
        df.cor = data.frame(a = paste("a=",a2k12), 
                            b= paste("b=",b2k12),
                            cor = paste("cor=",cor2k12),
                            xpos=xpos,
                            ypos=ypos,
                            yinc = yinc)
        
        library(ggplot2)
        g <- ggplot(data2k12, aes_string(x= x_string, y= y_string) )
        
        title = paste(periode,
                      "(cor=",cor2k12,",a=",a2k12,
                      ",b=",b2k12,
                      "n=",nrow(data2k12),")"            
        )
        
        title = paste(periode, "with n=",nrow(data2k12))            
        
        g <- g + geom_point() + geom_smooth(method="lm") + ggtitle(title)
        #g <- g + geom_point() + geom_smooth(method="lm") + ggtitle(paste(periode,"(cor=",cor10_24,")" ))
        g <- g + geom_text(data=df.cor,  aes(x = xpos, y = ypos, label = cor), parse = FALSE)
        g <- g + geom_text(data=df.cor,  aes(x = xpos, y = ypos-yinc, label = a), parse = FALSE)
        g <- g + geom_text(data=df.cor,  aes(x = xpos, y = ypos-2*yinc, label = b), parse = FALSE)
        
        #browser()
        print(g)
        
        fname = paste(basename, "pdf",sep='')
        ggsave(g, file=fname)
        
        fname = paste(basename, "jpg",sep='')
        ggsave(g, file=fname)
}




output.cors = function(Data, x,y, data.x, data.y, periode) {
        browser()
        #         corx = with( eval(Data), eval(x), sep='')
        #         cory = with( eval(Data), y, sep='')
        corxy = round(cor(data.x, data.y, use="complete.obs"),2)
        
        g <- ggplot(Data, aes(eval(x), eval(y) )) 
        g <- g + geom_point() + geom_smooth(method="lm") + ggtitle(paste(periode,"(cor=",corxy,")" ))
        print(g)
        
}

