# Plot GDP, DAX
library(tidyverse)

# ---- Performance Data -----
# Data taken from paperFinDataBloomberg.R
# Returns analysis - Daily

# ---- Last days in quarters differ ------
retDataDaily %>% filter(year(Date)<2017) %>%
 group_by(year=year(Date), month=month(Date) ) %>%
 summarise(Date=first(Date)) %>% 
 filter(month %in% c(3,6,9,12)) -> lastDays

# --- GDP Data ---
gdpDataIn <- read_tsv("Data/gdpGermany.txt") 

gdpData <- gdpDataIn %>% 
 mutate(Date = dmy(date) ) %>%
 select(-date, -Quarter) 

gdpData$Date <- lastDays[-nrow(lastDays),"Date"]$Date

# ---- Make GDP ~ DAX plot ----
# --- Plot tunning
leftShade  <- c(dmy("30-03-2008") )
rightShade <- c(dmy("30-06-2009") )

rectangles <- data.frame( 
 xmin = leftShade,
 xmax = rightShade,
 ymin = 0,
 ymax = 1200)

# --- Plotting  
ggplot() +
geom_rect(data=rectangles, 
  aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='gray80', alpha=0.8) +
 geom_line(data=perfData, aes(x=Date, y=CDAX) ) +
 geom_line(data=gdpData, aes(x=Date, y=GDP)) + 
 geom_point(data=gdpData, aes(x=Date, y=GDP)) +
 scale_x_date("Date", date_labels = "%Y", date_breaks = "1 year") +
 scale_y_continuous("CDAX in points, GPD in EUR Billion", limits=c(0,1250), 
  sec.axis = sec_axis(~ . * 1) ) +
 theme_bw() +
 theme(
  axis.text = element_text(colour = "black"),
  panel.border = element_blank() ) +
 annotate("text", label = "GDP 16Q3", x = dmy("01-09-2016"), y = 650) +
 annotate("text", label = "DAX", x = dmy("01-01-2017"), y = 1115) -> plot1

# ggsave("daxGDPplot.png", width=10, height=6)
# pdf("daxGDPplot.pdf", width=10, height=6)
#  plot1
# dev.off()

# ---- Manufacturer Time Series  ---------------
# perfData taken from paperFinDataBloomberg.R
manufLong <- perfData %>% gather(., manuf, adjClose, -Date)
manufLong %>%
  group_by(manuf, year=year(Date), month=month(Date)) %>%
  summarise(
    nobs = n(),
    minPrice = min(adjClose),
    maxPrice = max(adjClose),
    diff = maxPrice-minPrice,
    volat = sd(adjClose) 
  )

manufLong %>% filter(manuf!="CDAX") %>%
ggplot(., aes(x=Date, y=adjClose) ) +
  scale_x_date("Date", date_labels = "%Y", date_breaks = "2 years") +
  facet_grid(.~manuf) +
 geom_rect(
  aes(xmin=dmy("30-03-2008") , xmax=dmy("30-06-2009") , ymin=0, ymax=500), fill='gray80', alpha=0.8) + 
 scale_y_continuous("Stock Price, EUR", limits=c(0,500), 
  sec.axis = sec_axis(~ . * 1) ) +
  geom_line() +
 theme_bw() +
 theme(
  axis.text = element_text(colour = "black"),
  strip.text = element_text(colour = "black", face="bold"),
  panel.border = element_blank(),
  strip.background = element_rect(colour="white", fill="white") ) +
 scale_colour_discrete(guide=FALSE) -> plot2

# ggsave("companiesSelection.png", dpi=600)
# pdf("companiesPrices.pdf", width=10, height=6)
#  plot2
# dev.off()


