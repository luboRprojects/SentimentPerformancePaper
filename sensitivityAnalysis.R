#----------------
# ggPeriods taken from sentimentAnalysis
analyseData <- ggPeriods %>% arrange(start) 
colnames(analyseData)[1] <- "Date"

analyseData[-1, ] %>%
 mutate(
  meanBin = as.numeric(meanBMW>0),
  ZEWBin  = as.numeric(GRZEWI>0)
 ) %>% data.frame() %>%
by(., .$periodTable, function(x){table(x$meanBin, x$ZEWBin)} )

histMonthlyRet <- 
retDataDaily[complete.cases(retDataDaily), ] %>% 
 group_by(year=year(Date), month=month(Date) ) %>%
 summarise(
  BMWret = mean(BMW),
  DAIret = mean(DAI),
  VWret = mean(VW),
  DAXret = mean(DAX)
)

#--- UNUSED SO FAR ----
histMonthlyRet <- histMonthlyRet[-c(1:11), ] %>% filter(year!=2017) 
#---------------------
# 1) Returns in the last one month:  
# 2) Actual Zew:  analyseData$GRZEWI
# 3) Future 3 month Returns:  analyseData$meanBMW

analyseData %>% select(Date, ZEW = GRZEWI, BMWfut=meanBMW) -> dct1

histMonthlyRet$Date <- c(dct1$Date, last(dct1$Date) %m+% months(1:3))
dataCompareThree <- dct1 %>% full_join(histMonthlyRet) %>% 
 select(-year, -month)

dataCompareThree %>%
 select(Date, ZEW, BMWfut, BMWret) %>%
 gather(Index, Value, -Date) %>%
 mutate(
  Index = as.factor(Index),
  Index = fct_relevel(Index, c("BMWret","ZEW","BMWfut"))
 ) -> ggDataThreeSeries

 ggplot(ggDataThreeSeries, aes(x=Date, Value, group=Index) ) +
  geom_line() +
  geom_point() +
  scale_x_date("Date", date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() +
  theme(
   axis.text = element_text(colour = "black"),
   #panel.border = element_blank(),
   strip.text = element_text(colour = "black", face="bold"),
   strip.background = element_rect(colour="grey", fill="white"),
   legend.position = c(0.05, 0.2),
   legend.background = element_rect(size=0.5, linetype="solid", colour="grey80") ) +
  geom_hline(yintercept=0, col="grey60") +
  facet_grid(Index~., scales="free")

rectanglesThreeSeries <- data.frame( 
 xmin = leftShade,
 xmax = rightShade ,
 ymin = -Inf,
 ymax = Inf)

ggplot()+
 geom_rect(data=rectanglesThreeSeries , 
  aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='gray80', alpha=0.8) +
 geom_line(data=ggDataThreeSeries, aes(x=Date, Value, group=Index)) +
 geom_point(data=ggDataThreeSeries, aes(x=Date, Value, group=Index)) +
 geom_hline(yintercept=0, colour="black") + 
  theme_bw() +
  theme(
   axis.text = element_text(colour = "black"),
   #panel.border = element_blank(),
   strip.text = element_text(colour = "black", face="bold"),
   strip.background = element_rect(colour="grey", fill="white"),
   legend.position = c(0.28, 0.14), # do not change, print settings
   legend.background = 
     element_rect(size=0.5, linetype="solid", colour="grey80") ) +
 facet_grid(Index~., scales="free") 

ggsave("threeTimeSeries.png", width=10, height=6)

#=======================
dataCompareThree %>%
 select(Date, ZEW, BMWfut, BMWret) %>%
 mutate(show = ZEW>0) %>%
 gather(Index, Value, -Date, -show) %>%
 mutate(
  Index = as.factor(Index),
  Index = fct_relevel(Index, c("BMWret","ZEW","BMWfut"))
 ) -> ggDataThreeSeriesShow

ggplot()+
 geom_rect(data=rectanglesThreeSeries , 
  aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='gray80', alpha=0.8) +
 geom_line(data=ggDataThreeSeriesShow, aes(x=Date, Value, group=Index)) +
 geom_point(data=ggDataThreeSeriesShow, aes(x=Date, Value, group=Index, colour=show)) +
 geom_hline(yintercept=0, colour="black") + 
  theme_bw() +
  theme(
   axis.text = element_text(colour = "black"),
   #panel.border = element_blank(),
   strip.text = element_text(colour = "black", face="bold"),
   strip.background = element_rect(colour="grey", fill="white"),
   legend.position = c(0.28, 0.14), # do not change, print settings
   legend.background = 
     element_rect(size=0.5, linetype="solid", colour="grey80") ) +
 facet_grid(Index~., scales="free") +
 scale_colour_discrete(guide=FALSE)


#=======================
 

# --- rectangles data in gdpDaxPlot

analyseData %>%
 select(Date, ZEW=GRZEWI, BMW=meanBMW) %>%
 gather(Index, Value, -Date) %>%
 ggplot(., aes(x=Date, y=Value, group=Index) ) +
  geom_line() + 
  geom_point() +
  geom_hline(yintercept=0, colour="black") + 
  theme_bw() +
  theme(
   axis.text = element_text(colour = "black"),
   #panel.border = element_blank(),
   strip.text = element_text(colour = "black", face="bold"),
   strip.background = element_rect(colour="grey", fill="white"),
   legend.position = c(0.28, 0.14), # do not change, print settings
   legend.background = 
     element_rect(size=0.5, linetype="solid", colour="grey80") ) +
 facet_grid(Index~., scales="free")

#---
#  geom_rect(data=rectangles, 
#    aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
#     fill='gray80', alpha=0.8)
#---

# Around-Crisis window
ggPeriods %>% filter(year(start)> 2006 & year(start) < 2010) %>%
 select(Date = start, ZEW = GRZEWI, BMW = meanBMW, Period = period) %>%
 gather(Index, Value, -Date, -Period) %>%
ggplot(., aes(x=Date, y=Value, group=Index) ) +
 geom_line() +
 geom_point() + 
 geom_hline(yintercept=0, colour="black") +
 facet_grid(Index~., scales="free") + theme_bw()

ggPeriods %>% filter(year(start)> 2006 & year(start) < 2010) %>%
 select(Date = start, ZEW = GRZEWI, BMW = meanBMW) %>%
 mutate( ZEW = scale(ZEW), BMW = scale(BMW) ) %>%
 gather(Index, Value, -Date) %>%
ggplot(., aes(x=Date, y=Value, colour=Index, group=Index) ) +
 geom_line() +
 geom_point() + 
 geom_hline(yintercept=0, colour="black") 






