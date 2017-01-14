library(tidyquant)

vwData  <- tq_get("VOW.DE", get = "stock.prices", from = " 2005-01-01") #vOW3 preffer. shares
bmwData  <- tq_get("BMW.DE", get = "stock.prices", from = " 2005-01-01")
daiData  <- tq_get("DAI.DE", get = "stock.prices", from = " 2005-01-01")

vwClose <- vwData %>% select(date, vw=adjusted) %>% mutate(date = ymd(date) )
bmwClose <- bmwData %>% select(date, bmw=adjusted) %>% mutate(date = ymd(date) )
daiClose <- daiData %>% select(date, dai=adjusted) %>% mutate(date = ymd(date) )

#---------------------
table(is.na(vwClose$vw))
table(is.na(bmwClose$bmw))
table(is.na(daiClose$dai))

vwClose[which(!complete.cases(vwClose)), ]
vwClose[which(!complete.cases(bmwClose )), ]
vwClose[which(!complete.cases(daiClose)), ]

bmwData %>% filter(date==ymd("2008-08-01"))
daiData %>% filter(date=="2010-08-15")
#---------------------

manufData <- vwClose %>% 
  full_join(bmwClose, by="date") %>%
  full_join(daiClose, by="date") 

manufData[which(!complete.cases(manufData) ), ]

length(c(unique(vwClose$date, bmwClose$date, daiClose$date)))
3127-3112


#---- Missing prices



manufLong <- manufData %>% gather(., manuf, adjClose, -date)

manufLong %>%
 group_by(manuf, year=year(date), month=month(date)) %>%
 summarise(
  nobs = n(),
  minPrice = min(adjClose),
  maxPrice = max(adjClose),
  diff = maxPrice-minPrice,
  volat = sd(adjClose) 
)


#----------
ggplot(manufLong, aes(x=date, y=adjClose, col=manuf) ) + 
 geom_line() + 
 facet_grid(.~manuf) + theme_bw()

min(manufLong$date)
#----------

manufDataScale <- manufData %>% 
 mutate(
  vw = vw/first(vw),
  bmw = bmw/first(bmw),
  dai = dai/first(dai)
 )

manufDataScaleLong <- manufDataScale %>% gather(., manuf, adjClose, -date)
min(manufDataScaleLong$date)

manufDataScaleLong$manuf <- factor(
 manufDataScaleLong$manuf, levels=c("vw", "bmw", "dai"))


ggxbreaks <- as.Date(paste0(2015:2017, "/1/1"), format="%Y/%m/%d" )

as.Date("2015/01/01", format="%Y/%m/%d")

ggplot(manufDataScaleLong, aes(x=date, y=adjClose, col=manuf) ) + 
 geom_line() + theme_bw() + 
 scale_colour_discrete("Manufacturer", labels=c( "VW", "BMW", "Daimler")) + 
 scale_y_continuous("Adjusted price, normed") + 
 scale_x_date("Date", date_labels = "%Y", date_minor_breaks = "1 year")

#----------------
# Returns analysis
# Aritmetic (simple) or Logaritmic returns?
retArit <- function(x) {x/lag(x)}
retLog <- function(x) {log(x)-log(lag(x))}

manufData %>% 
 mutate_each(
  funs(retLog), c(vw, bmw,dai)) -> retData

weights <- c(0.35, 0.35, 0.3) #BMW, DAI, VW

retData[-1, ] %>% # remove NA returns
  gather(., manuf, adjClose, -date) %>%
  group_by(manuf, year=year(date), month=month(date)) %>%
  summarise(
    expect = mean(adjClose),
    varian = var(adjClose)
  ) %>%
  group_by(year, month) %>%
   summarise(
     avg = weighted.mean(x=expect,w=weights)
   )
  

aaa %>% filter(year==2015 & month==10)

#---------------
library(PerformanceAnalytics)
CalculateReturns(prices, method = c("discrete", "log"))

retData[-1, ] %>% data.frame -> dsdata
xtsdata <- xts(dsdata[,-1], order.by = dsdata[, 1])
  %>%
Return.portfolio(xtsdata[complete.cases(xtsdata),], weights)

xtsdata[which(!complete.cases(xtsdata) )]


weights <- c(0.35, 0.35, 0.3)
