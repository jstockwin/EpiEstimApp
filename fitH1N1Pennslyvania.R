source("main.R")

serialIntervalData <-  read.csv("datasets/PennsylvaniaH1N12009SerialIntervalData.csv", header = FALSE)
casesPerDayData <- read.csv("datasets/PennsylvaniaH1N12009FluData.csv", header = FALSE)


transmissibility_estimation(serialIntervalData, casesPerDayData, 5)

