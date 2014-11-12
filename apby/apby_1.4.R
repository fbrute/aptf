mainapby <- function () {
        dbg <- 1
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
        hourquery <- dbSendQuery(con,
                                paste( 
                                        
                                        "select aothourly.date, aothourly.hour, pm10, aot500 from",
                                        "(select date(datetime) as date, hour(datetime) as hour, pmptp as pm10 from pm10",
                                                "where date(datetime) between ","'", date_start, "'"," and ","'", date_stop, "'",
                                                "and pmptp > 0",
                                                ")",
                                                "as pm10hourly",
                                                ",",
                                        "(select date, hour(subtime(time,'03:00:00')) as hour, avg(aot_500) as aot500",
                                                "from aotv1500",
                                                "where date between ","'", date_start, "'"," and ","'", date_stop, "'",
                                                "group by date, hour)",
                                                "as aothourly",
                                        "where aothourly.date in ",
                                                "(select date from",
                                                        "(select date, hour(subtime(time,'03:00:00')) as hour,",
                                                                "avg(aot_500) as aot500",
                                                        "from aotv1500",
                                                        "where date between ","'", date_start, "'"," and ","'", date_stop, "'","",
                                                                "and aot_500 > 0 and aot_500 < 1",
                                                                "and date not in (select date from exdatesaptf2k14)",
                                                        "group by date, hour) as aot",
                                                        "group by date",
                                                        "having count(date) >= 5)",
                                        "and pm10hourly.date = aothourly.date",
                                        "and pm10hourly.hour = aothourly.hour",
                                        "order by date, hour"
                                )
        )
        #if (dbg) browser()
        
        hourdata <- fetch(hourquery, n=-1)
        
        #if (dbg) browser()
        
        
        #View(d);
        #plot(aotdata$aot500*100)
        
        #dbClearResult(dbListResults(con)[[1]])
        dbClearResult(hourquery)
        
        
        #where datetime between ","'", date_start, "'"," and '2012/10/31'
#         pm10_24query <- dbSendQuery(con, 
#                                     
#                                     paste(
#                                             "select year(datetime) as year,", 
#                                             "month(datetime) as month, day(datetime) as day,",
#                                             "cast(date_format(date(datetime),'%j') as decimal(5,0)) as julian_day," ,
#                                             "avg(pmptp) as pm10_24 ,date(datetime) as date",
#                                             "from pm10",
#                                             "where datetime between ","'", date_start, "'"," and ","'", date_stop, "'","",
#                                             "and pmptp > 0", 
#                                             "group by date(datetime)",
#                                             "order by date(datetime);"
#                                     )
#         )
#         
#         pm10_24data <- fetch(pm10_24query, n=-1);
#         
#         dbClearResult(pm10_24query)
#         
#         pm10_1016query <- dbSendQuery(con, 
#                                       paste(
#                                               "select year(datetime) as year,", 
#                                               "month(datetime) as month, day(datetime) as day,",
#                                               "cast(date_format(date(datetime),'%j') as decimal(5,0)) as julian_day," ,
#                                               "avg(pmptp) as pm10_1016 ,date(datetime) as date",
#                                               "from pm10",
#                                               "where datetime between ","'", date_start, "'"," and ","'", date_stop, "'","",
#                                               "and pmptp > 0",
#                                               "and hour(datetime) between 10 and 16",
#                                               "group by date(datetime)",
#                                               "order by date(datetime);"
#                                       )
#         )
#         
#         pm10_1016data <- fetch(pm10_1016query, n=-1);
#                 
#         dbClearResult(pm10_1016query)
#         
#         pm10_0817query <- dbSendQuery(con, 
#                                       paste(
#                                               "select year(datetime) as year,", 
#                                               "month(datetime) as month, day(datetime) as day,",
#                                               "cast(date_format(date(datetime),'%j') as decimal(5,0)) as julian_day," ,
#                                               "avg(pmptp) as pm10_0817 ,date(datetime) as date",
#                                               "from pm10",
#                                               "where datetime between ","'", date_start, "'"," and ","'", date_stop, "'","",
#                                               "and pmptp > 0",
#                                               "and hour(datetime) between 8 and 17",
#                                               "group by date(datetime)",
#                                               "order by date(datetime);"
#                                       )
#         )
#         
#         pm10_0817data <- fetch(pm10_0817query, n=-1);
#         
#         dbClearResult(pm10_0817query)
        
        dbDisconnect(con)
        
        # get pm25 data from csv file
        #library(dplyr)
        #hourdata <- mutate(hourdata, datetime = paste(date,time,sep=""))
        #hourlydata <- subset(hourlydata, select = c(date,aot500))
        
        #browser()
        
        # merge is left inner join from sql
#         data2k12 <- merge(aotdata, pm10_24data, by="date",all.x = T, all.y = T)
#         data2k12 <- merge(data2k12, pm10_1016data, by="date",all.x = T, all.y = T)
#         data2k12 <- merge(data2k12, pm10_0817data, by="date",all.x = T, all.y = T)
#         data2k12 <- merge(data2k12, pm25data, by="date", all.x = T, all.y = T)
#         data2k12 <- data2k12[order(data2k12$date),]
        
        fname <- paste(as.character(date_start),
                       "_",
                       as.character(date_stop),
                       "_",
                       "aotnsup4hourly",
                       ".RData",
                       sep="")
        
        save(hourdata, file=fname)
        
        
        hourdata
        
        
}

output = function(data, periode) {
        
        output2(periode, data, "aot500", "pm10")
        #output2(periode, data, "aot500", "pm10_1016")
        #output2(periode, data, "aot500", "pm10_0817")
}

output2 = function(periode, data2k12, x_string, y_string) {
        #         output.cors(Data = data2k12, x= "aot500", y = "pm10_24",
        #                     data.x= data2k12$aot500, data.y= data2k12$pm10_24, periode)
        
        basename <- paste(as.character(date_start),
                       "_",
                       as.character(date_stop),
                       "_",
                       "aotnsup4_exdates_hourly",
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
        
        #browser()
        
        library(ggplot2)
        g <- ggplot(data2k12, aes_string(x= x_string, y= y_string) )
        title = paste(periode,
                      "(cor=",cor2k12,",a=",a2k12,
                      ",b=",b2k12,
                      "n=",nrow(data2k12),")"           
        )
        g <- g + geom_point() + geom_smooth(method="lm") + ggtitle(periode)
        df.cor = data.frame(a = paste("a=",a2k12), 
                            b= paste("b=",b2k12),
                            cor = paste("cor=",cor2k12))
        
        #geom_text(data = df.cors, aes(x = 350, y = 90, label = cor, size = 7), color ="red" , inherit.aes = FALSE, parse = T)
        #g <- g + geom_text(  aes(x =0.4, y = 30, label = corlabel), parse = TRUE)
        g <- g + geom_text(data=df.cor,  aes(x =0.5, y = 25, label = cor), parse = FALSE)
        g <- g + geom_text(data=df.cor,  aes(x =0.5, y = 20, label = a), parse = FALSE)
        g <- g + geom_text(data=df.cor, aes(x =0.5, y = 15, label = b), parse = FALSE)
        #g <- g + geom_point() + geom_smooth(method="lm") + ggtitle(paste(periode,"(cor=",cor10_24,")" ))
        
        browser()
        
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

# plot(data2k12$pm10_24, type='b', col='blue',ylim=c(1,100), 
#      xlab=paste(c("days of",config$lblmonth," ",config$year)))
# title("pm10global(blue), pm10_1016h(violet),aot500(green), pm25(red)")
# points(data2k12$pm10_1016, type='b', col='violet')
# points(data2k12$aot500*100, type='b', col='green')
# points(data2k12$pm25, type='b', col='red')

#         library(lattice)
#         data2k12 <- mutate(data2k12, month= as.numeric(substr(date,6,7)))
#         xyplot(pm10_24 ~ aot500*100 | month, data = data2k12, layout =c(4,3))
#         xyplot(pm10_1016 ~ aot500*100 | month, data = data2k12, layout =c(4,3))
#         library(ggplot2)
#         qplot(pm10_24, aot500 , data = data2k12, color=month, geom = c("point"), method = "lm")
#         # histogram
#         qplot(pm10_24, data = data2k12, fill=month)
#         qplot(log(pm10_24), data= data2k12, geom = "density")

#         qplot(aot500, pm10_24, data= data2k12, geom = c("point", "smooth"), method = "lm", main="2012")
#         qplot(aot500, pm10_1016, data= data2k12, geom = c("point", "smooth"), method = "lm", main="2012")
#         qplot(aot500, pm10_0817, data= data2k12, geom = c("point", "smooth"), method = "lm", main="2012")
#         
#library(ggplot2)
#qplot(pm10_24, data=data)
#         data2 <- na.omit(select(data2k12,pm10_24, aot500))
#         datacor24 <- cor(data2$pm10_24, data2$aot500)
#         # datacor242 <- lm(data2$pm10_24 ~ data2$aot500)
#         #View(datacor24)
#         data3 <- na.omit(select(data2k12,pm10_1016, aot500))
#         datacor1016 <- cor(data3$pm10_1016, data3$aot500)
#View(c(datacor24,datacor1016))
# merge and allow NA values 
# data.all <- merge(aotdata, pm10data, all.x = T, all.y = T)
# plot(data.all$pm10_24, type='b', col='blue')
# points(data.all$aot500*100, type='b', col='green')
