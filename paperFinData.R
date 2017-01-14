library(tidyquant)

vwData  <- tq_get("VOW.DE", get = "stock.prices", from = " 2005-01-01")
bmwData  <- tq_get("BMW.DE", get = "stock.prices", from = " 2005-01-01")
daiData  <- tq_get("DAI.DE", get = "stock.prices", from = " 2005-01-01")

vwClose <- vwData %>% select(date, vw=adjusted) %>% mutate(date = ymd(date) )
bmwClose <- bmwData %>% select(date, bmw=adjusted) %>% mutate(date = ymd(date) )
daiClose <- daiData %>% select(date, dai=adjusted) %>% mutate(date = ymd(date) )

manufData <- vwClose %>% 
 left_join(bmwClose, by="date") %>%
 left_join(daiClose, by="date") 

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

weights <- c(0.35, 0.35, 0.3)

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

