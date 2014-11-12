library(RMySQL)
lblmonths <- c("January","February","March","April","May","June","July","August",
               "September","October","November","December")
# lblmonth <- lblmonths[month]
# config <- list(year = 2012,
#                month = 7,
#                lblmonth = lblmonths[month])

config <- list(year = 2012,
               month = 7, day = 3)

con = dbConnect(dbDriver("MySQL"), user="dbmeteodb", 
                password="dbmeteodb",
                dbname="dbmeteodb",
                host="localhost")

# data available by hour. So aggregate by the day.
aotquery <- dbSendQuery(con,
                        paste( 
                          "select year(date) as year, 
                          month(date) as month, day(date) as day,
                          hour(subtime(time,'04:00:00')) as hour,
                          floor(julian_day) as julian_day,
                          avg(aot_500) as aot500,
                          date
                          from aotv1500",
                          "where year(date) =", config$year,
                          "and month(date) =", config$month,
                          "and day(date) =", config$day,
                          "and aot_500 > 0 and aot_500 < 1 
                          group by date, hour(time);
                          "))

aotdata <- fetch(aotquery, n=-1)

aot2query <- dbSendQuery(con,
                         paste( 
                           "select year(date) as year, 
                           month(date) as month, day(date) as day,
                           hour(subtime(time,'04:00:00')) as hour, 
                           floor(julian_day) as julian_day,
                           avg(aot_500) as aot500,
                           date, time
                           from aotv1500",
                           "where date ='2012-03-07'",
                           "and aot_500 > 0 and aot_500 < 1 
                           group by date, hour(time);
                           "))

aot2data <- fetch(aot2query, n=-1)