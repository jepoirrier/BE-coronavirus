# BE-coronavirus analysis
# Hospitalizations

# Source: https://epistat.wiv-isp.be/covid/
# CSV: https://epistat.sciensano.be/Data/COVID19BE_MORT.csv

library(dplyr)
library(ggpubr) # for ggarrange
library(gghighlight)
library(ggplot2)
library(ini)
library(tidyr)
library(zoo) # for rollmean()

print("Entering deaths")

plotWidth <- 10
plotHeight <- 6 # for single graph: 6 (= 2 times 3) + 1
plotHeightLong <- 12 # for multiple graphs: 9 (= 3 times 3) + 1
preventMultipleDownload <- FALSE # because cases.R and tests.R have most likely run already

logScale <- TRUE
logWarning <- ""
if(logScale)
  logWarning = " (log scale!)"

# Tests file ...

DURL <- "https://epistat.sciensano.be/Data/COVID19BE_MORT.csv"
DFILE <- "../data/COVID19BE_deaths.csv"

# Download and import the data
if((as.Date(file.info(DFILE)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(DURL, DFILE, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as DFile already downloaded today")
}
if(file.exists(DFILE)) {
  print(paste("DFILE found, created on:", file.info(DFILE)$ctime))
} else {
  stop(DFILE, " does not exist") # not the best way to stop (if it even stops!)
}

datD <- read.csv(DFILE, sep = ",", colClasses = c("Date", "character", "character", "character", "integer"))
datD$DATE <- as.Date(datD$DATE)
lastDDate <- max(datD$DATE)
print(paste("Latest data point in DFILE:", lastDDate))

# Clean the data
colnames(datD) <- c("Date", "Region", "Age Group", "Sex", "Deaths")
#datH$NReporting <- NULL

# Calculate totals per day
datDt <- na.exclude(datD) %>%
  group_by(Date) %>%
  summarise_at(vars(Deaths),
               sum)

p <- ggplot(datDt, aes(x = Date, y = Deaths)) +
  geom_line() +
  labs(title = "Evolution of daily number of deaths due to COVID-19 in Belgium (2020)",
       x = "Date",
       y = "Number of deaths",
       caption = paste("Explanations at https://jepoirrier.org/becovid19/ ; data from https://epistat.wiv-isp.be/covid/ ; last data:", format(lastDDate, "%b %d, %Y"), " ; last update:", format(Sys.Date(), "%b %d, %Y")))
p
ggsave("../figures/deaths.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")
