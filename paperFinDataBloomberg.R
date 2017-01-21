library(tidyverse)
library(lubridate)

#------ Import/munging ---------
file <- "SentimentPerformancePaper/Data/perfmormanceManufEconomy.txt"
perfDataIn <- read_tsv(file) %>%
  mutate(Date = dmy(Date))

perfData <- perfDataIn[complete.cases(perfDataIn), ]
perfData[c(-5:3) + which.max(perfData$VOW), ]

# Spike replaced by average of t+-1 values
perfData[which.max(perfData$VOW),"VOW"] <- 
  mean(perfData[(-1:1)+which.max(perfData$VOW),"VOW"]$VOW[-2])

  
#------ Analysis ---------
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

#---

manufLong %>% filter(manuf!="CDAX") %>%
ggplot(., aes(x=Date, y=adjClose, col=manuf) ) + 
  geom_line() + 
  facet_grid(.~manuf) + theme_bw()

perfDataScale <- perfData %>% 
  arrange(Date) %>%
  mutate(
    VW = VOW/first(VOW),
    BMW = BMW/first(BMW),
    DAI = DAI/first(DAI),
    DAX = CDAX/first(CDAX)
  ) %>% 
  select(Date, VW, BMW, DAI, DAX)

perfDataScaleLong <- perfDataScale %>% gather(., Title, adjClose, -Date)

# --- Tunning ggplot
ggxbreaks <- as.Date(paste0(2015:2017, "/1/1"), format="%Y/%m/%d" )
# relevel to match top-down Time-Series
perfDataScaleLong$Title <- factor(
  perfDataScaleLong$Title, levels=c("VW", "DAX", "BMW", "DAI"))

ggplot(perfDataScaleLong, aes(x=Date, y=adjClose, col=Title) ) + 
  geom_line() + theme_bw()+ 
  scale_colour_discrete("Title", labels=c("VW", "DAX", "BMW", "Daimler")) + 
  scale_y_continuous("Adjusted price, normed") + 
  scale_x_date("Date", date_labels = "%Y", date_minor_breaks = "1 year")

# Todo: Legend inside plot values, print final values, limit to 31/12/2016


#----------------
# Aritmetic (simple) or Logaritmic returns?
retArit <- function(x) {x/lag(x)}
retLog <- function(x) {log(x)-log(lag(x))}

# Returns analysis - Daily
perfData %>% 
  mutate_each(
    funs(retLog), c(VW=VOW, DAX=CDAX, BMW, DAI)) %>%
  select(-VOW, -CDAX) -> retData

weights <- c(0.35, 0.35, 0.3) #BMW, DAI, VW

retData %>% gather(Title, Returns, -Date) %>%
ggplot(., aes(x=Date, y=Returns, colour=Title) ) + 
  geom_line() + theme_bw() + facet_grid(~Title)


### Weekly Returns Analysis
perfData %>% 
  arrange(Date) %>%
  mutate(week = week(Date) ) %>%
  group_by(paste0(year(Date),month(Date),week(Date))) %>%
  do(tail(., n=1)) %>%
  arrange(Date) %>% ungroup() %>%
  mutate_each(
    funs(retLog), c(VOW, CDAX, BMW, DAI)) %>%
  select(Date, VOW, CDAX, BMW, DAI) -> retDataWeekly

colnames(retDataWeekly) <- c("Date","VW","DAX","BMW","DAI")

retDataWeekly[-1, ] %>% gather(Title, Returns, -Date) %>%
  ggplot(., aes(x=Date, y=Returns, colour=Title) ) + 
  geom_line() + theme_bw() + facet_grid(~Title)


