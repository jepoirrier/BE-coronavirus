# BE-coronavirus analysis
# Positivity rate (well, sort of ...)

# Source: https://epistat.wiv-isp.be/covid/
# Bringing cases and tests

library(dplyr)
library(ggpubr) # for ggarrange
library(gghighlight)
library(ggplot2)
library(ini)
library(tidyr)
library(zoo) # for rollmean()

print("Entering positivity.R")

plotWidth <- 10
plotHeight <- 6 # for single graph: 6 (= 2 times 3) + 1
plotHeightLong <- 12 # for multiple graphs: 9 (= 3 times 3) + 1
preventMultipleDownload <- TRUE # because cases.R and tests.R have most likely run already

logScale <- TRUE
logWarning <- ""
if(logScale)
  logWarning = " (log scale!)"

# Tests file ...

TURL <- "https://epistat.sciensano.be/Data/COVID19BE_tests.csv"
TFILE <- "../data/COVID19BE_tests.csv"

# Download and import the data
if((as.Date(file.info(TFILE)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(TURL, TFILE, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as TFile already downloaded today")
}
if(file.exists(TFILE)) {
  print(paste("TFILE found, created on:", file.info(TFILE)$ctime))
} else {
  stop(TFILE, " does not exist") # not the best way to stop (if it even stops!)
}

datT <- read.csv(TFILE, sep = ",", colClasses = c("Date", "integer"))
datT$DATE <- as.Date(datT$DATE)
lastTDate <- max(datT$DATE)
print(paste("Latest data point in TFILE:", lastTDate))

# Clean the data
colnames(datT) <- c("Date", "Tests")

# Calculate a 7 day average
datT <- datT %>%
  mutate(tests_7da = zoo::rollmean(Tests, k = 7, fill = NA, align = "right"))
colnames(datT) <- c("Date", "Tests daily", "Tests 7-days average")
datTlast <- tail(datT, n = 1)

# Cases

CURL <- "https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv"
CFILE <- "../data/COVID19BE_cases.csv"

# Download and import the data
if((as.Date(file.info(CFILE)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(CURL, CFILE, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as CFILE already downloaded today")
}
if(file.exists(CFILE)) {
  print(paste("CFILE found, created on:", file.info(CFILE)$ctime))
} else {
  stop(CFILE, " does not exist") # not the best way to stop (if it even stops!)
}

datC <- read.csv(CFILE, sep = ",", colClasses = c("Date", "character", "character", "character", "character", "integer"))
datC$DATE <- as.Date(datC$DATE)
lastCDate <- max(datC$DATE, na.rm = TRUE)
print(paste("Latest data point in CFILE:", lastCDate))

# Clean the data
colnames(datC) <- c("Date", "Province", "Region", "AgeGroup", "Sex", "Cases")

# Calculate totals per day
datCt <- na.exclude(datC) %>%
  group_by(Date) %>%
  summarise_at(vars(Cases),list(Cases = sum))

# Calculate a 7 day average
datCt <- datCt %>%
  mutate(Case7da = zoo::rollmean(Cases, k = 7, fill = NA, align = "right"))
colnames(datCt) <- c("Date", "Cases daily", "Cases 7-days average")
datCtlast <- tail(na.exclude(datCt), n = 1)

# Merging the 2 datasets
superDat <- merge(datT, datCt, by = 'Date', all.y = TRUE) # usually datCt (cases) have less dates

superDat$TestsPositivity <- superDat$`Cases daily` / superDat$`Tests daily` * 100
superDat$Tests7daPositivity <- superDat$`Cases 7-days average` / superDat$`Tests 7-days average` * 100

# Some cleanup
superDat$`Tests daily` <- NULL
superDat$`Tests 7-days average` <- NULL
superDat$`Cases daily` <- NULL
superDat$`Cases 7-days average` <- NULL
colnames(superDat) <- c("Date", "Daily kind-of Positivity", "7-days average kind-of Positivity")
superDatlast <- tail(na.exclude(superDat), n = 1)

# Simply plotting the daily cases
cols2pivot <- colnames(superDat)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"
superdt <- pivot_longer(data = superDat, cols = cols2pivot, names_to = "Positivity", values_to = "Count", values_drop_na = TRUE)

p <- ggplot(superdt, aes(x = Date, y = Count, group = Positivity)) +
  geom_line(aes(color = Positivity)) +
  theme(legend.position = "bottom") + 
  # Annotations
  annotate("text", label = paste("On", superDatlast$Date, ":\n",
                                 "Daily \"positivity\":", format(superDatlast$`Daily kind-of Positivity`, scientific = FALSE, big.mark = " ", digits = 2), "% (**)\n",
                                 "7-day average \"positivity\":", format(superDatlast$`7-days average kind-of Positivity`, scientific = FALSE, big.mark = " ", digits = 2), "%"),
           x = superDatlast$Date - 30,
           y = 20,
           size = 3, fontface = "italic") +
  annotate("text", label = paste("(**)"),
           x = superDatlast$Date,
           y = superDatlast$`Daily kind-of Positivity` + 3,
           size = 3) +
  labs(title = "Evolution of COVID-19 \"positivity rate\" (*) in Belgium (2020)",
       x = "Date",
       y = "% positivity",
       caption = paste("(*) Not a real positivity rate as dates for tests and cases are not matched, just positivity on reported date\n",
                       "(**) The daily \"positivity\" for the last day may be biased by incomplete reporting",
                       "Explanations at https://jepoirrier.org/becovid19/ ; data from https://epistat.wiv-isp.be/covid/ ; last data:", format(datCtlast$Date, "%b %d, %Y"), " ; last update:", format(Sys.Date(), "%b %d, %Y")))
p

ggsave("../figures/positivity.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")
