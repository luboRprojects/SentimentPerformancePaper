source("C:\\Users\\homolka\\Desktop\\sentimentDataClean.R")
data <- sentimentDataClean()

perfData <- data$perfData

perfData %>%
 group_by(year(Date), month(Date)) %>%
 selecT()
 summarise( 
  obs = n(),
  ) 