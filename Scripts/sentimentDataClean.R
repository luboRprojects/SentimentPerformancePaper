sentimentDataClean <- function(){

# ----------
library(tidyverse)
library(lubridate)
library(stringr)

# ----------
zewDataIn <- read.csv("Data/zewSeries.txt", sep="\t", header=TRUE)
ifoDataIn <- read.csv("Data/ifoSeries.txt", sep="\t", header=TRUE)
perfDataIn <- read.csv("Data/performanceSeries.txt", sep="\t", header=TRUE)

# ------------
zewData <- zewDataIn
ifoData <- ifoDataIn
perfData <- perfDataIn

colnames(zewData)[1] <- colnames(ifoData)[1] <- colnames(perfData)[1] <- "Date"

colnames(zewData) <- gsub(x=colnames(zewData), pattern=".Index", replace="")
colnames(ifoData) <- gsub(x=colnames(ifoData), pattern=".Index", replace="")
colnames(perfData) <- gsub(x=colnames(perfData), pattern=".Index", replace="")

zewData$Date <- dmy(zewData$Date)
ifoData$Date <- dmy(ifoData$Date)
perfData$Date <- dmy(perfData$Date)

sentData <- zewData %>%
 left_join(ifoData)

#--------

perfData %>% 
 mutate_if(is.factor, as.character) %>% 
 mutate_if(is.character, function(x){gsub(x, pattern="#N/A N/A", replace=NA)}) %>%
 mutate_if(is.character, as.numeric) -> perfData

return(list(sentData=sentData, perfData=perfData))
}