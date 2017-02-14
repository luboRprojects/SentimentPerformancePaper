library(forcats)
source("Scripts/sentimentDataClean.R")
data <- sentimentDataClean()

sentData <- data$sentData %>% as_data_frame()

#--- Data from paperFinDataBloomberg ---
#------- ZEW Germany Expectation of Economic Growth
#--- GRZEWI Index
midPeriod <- months(3)

periods <- data_frame(
 start = sentData$Date,
 end   = sentData$Date %m+% midPeriod,
 GRZEWI = sentData$GRZEWI,
 meanBMW  = NA,
 sdBMW    = NA,
 ndays = NA) %>% 
  filter(end<dmy("01/01/2017"))

#- retDataDaily from paperFinDataBloomberg

for(i in 1:length(periods$start) ){
 retDataDaily[-1, ] %>% 
  filter(Date >=periods$start[i] & Date <=periods$end[i]) %>%
   summarise(
    mean  = mean(DAX),
    sd    = sd(DAX),
    ndays = n()
   ) -> foo
 periods[i,"meanBMW"] <- foo$mean
 periods[i,"sdBMW"] <- foo$sd
 periods[i,"ndays"] <- foo$ndays
}

# --- Annual analysis ---
periods %>%
 select(end, GRZEWI, meanBMW, sdBMW) %>%
 group_by(year=year(end)) %>%
 ggplot(., aes(x=GRZEWI, y=meanBMW, colour=sdBMW, group=year)) +
  geom_point() +
 theme_bw() + 
 geom_vline(xintercept=0, col="red") +
 geom_hline(yintercept=0, col="red") +
 scale_colour_continuous(guide=FALSE) +
 facet_grid(.~year, scales="free") 

# Three Period Analysis
# period - for predictions
# periodTable - for visualisation

periods %>%
 select(start, end, GRZEWI, meanBMW, sdBMW) %>%
 mutate( 
  period = ifelse(
	end < dmy("30-03-2008"), "Pre",
	 ifelse(
	  end > dmy("30-06-2009"),"Post","Crisis"
	 )),
  periodTable = ifelse(
	start < dmy("30-03-2008"), "Pre",
	 ifelse(
	  start > dmy("30-06-2009"),"Post","Crisis"
	 )
  )
 )  -> ggPeriods

ggPeriods$periodTable <- as.factor(ggPeriods$periodTable)

levOrder <- c("Pre","Crisis","Post")
ggPeriods$periodTable <- fct_relevel(ggPeriods$periodTable, levOrder)

ggPeriods %>%
 group_by(period) %>%
 summarise(
  n()
 )

ggPeriods %>%
 filter(year(start)>2005) %>% 
 filter(periodTable == "Crisis") %>%
 ggplot(., aes(x=GRZEWI, y=meanBMW, size=sdBMW ) ) +
  geom_point()

ggPeriods %>%
 filter(year(start)>2005) %>% 
 ggplot(., aes(x=GRZEWI, y=meanBMW, size=sdBMW, group=periodTable)) +
  geom_point(alpha=0.7) +
  scale_x_continuous("ZEW Germany Expectation of Economic Growth") +
  scale_y_continuous(paste0("Mean Value of BMW Returns in ", month(midPeriod), " Months") ) +
 theme_bw() +
 theme(
  axis.text = element_text(colour = "black"),
  #panel.border = element_blank(),
  strip.text = element_text(colour = "black", face="bold"),
  strip.background = element_rect(colour="grey", fill="white"),
  legend.position = c(0.28, 0.14), # do not change, print settings
  legend.background = element_rect(size=0.5, linetype="solid", colour="grey80") ) +
 geom_vline(xintercept=0, col="grey60") +
 geom_hline(yintercept=0, col="grey60") +
 scale_colour_continuous(guide=FALSE) +
 scale_size_continuous("St. Dev") +
 facet_grid(.~periodTable) 

# ggsave("threePeriodsScatter.png", width=10, height=5)
#--------------
# Look at Pre-Crisis Period
ggPeriods[which(ggPeriods$period == "Crisis"), ]

ggPeriods %>% filter(year(start)> 2006 & year(start) < 2010) %>%
 select(Date = start, ZEW = GRZEWI, BMW = meanBMW, Period = period) %>%
 gather(Index, Value, -Date, -Period) %>%
  ggplot(., aes(x=start, y=Value, group=Index) ) +
   geom_line() +
   geom_point()


#---------------

# --- Returns in Periods Plot ---
ggPeriods %>%
 ggplot(., aes(x=end, y=meanBMW, group=period)) +
   geom_line() +
   geom_point(aes(size=sdBMW), alpha=0.7) +
  scale_x_date("Date", date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() +
  theme(
   axis.text = element_text(colour = "black"),
   #panel.border = element_blank(),
   strip.text = element_text(colour = "black", face="bold"),
   strip.background = element_rect(colour="grey", fill="white"),
   legend.position = c(0.05, 0.2),
   legend.background = element_rect(size=0.5, linetype="solid", colour="grey80") ) +
  geom_vline(xintercept=0, col="grey60") +
  geom_hline(yintercept=0, col="grey60") +
  scale_colour_continuous(guide=FALSE) +
  scale_size_continuous("SDev") +
  facet_grid(.~period, scales="free", space="free_x") 

#----------

periods %>%
 ggplot(., aes(x=start, y=meanBMW, colour=sdBMW) ) +
  geom_line(size=1.05)

