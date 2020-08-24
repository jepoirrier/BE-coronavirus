# BE-coronavirus analysis
# Total number of tests by date

# Source: https://epistat.wiv-isp.be/covid/
# Data URL: https://epistat.sciensano.be/Data/COVID19BE_tests.csv

library(dplyr)
library(gghighlight)
library(ggplot2)
library(ini)
library(tidyr)
library(zoo) # for rollmean()

print("Entering test-analysis.R")

plotWidth <- 10
plotHeight <- 6 # for single graph: 6 (= 2 times 3) + 1
plotHeightLong <- 12 # for multiple graphs: 9 (= 3 times 3) + 1
preventMultipleDownload <- FALSE

logScale <- TRUE
logWarning <- ""
if(logScale)
  logWarning = " (log scale!)"

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
print(paste("Latest data point in TFILE:", lastTDates))

# Clean the data
colnames(datT) <- c("Date", "Tests")

# Calculate a 7 day average
datT <- datT %>%
  mutate(tests_7da = zoo::rollmean(Tests, k = 7, fill = NA, align = "right"))
colnames(datT) <- c("Date", "Tests daily", "Tests 7-days average")
datTlast <- tail(datT, n = 1)

# Simply plotting the data
cols2pivot <- colnames(datT)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"
dtT <- pivot_longer(data = datT, cols = cols2pivot, names_to = "Tests", values_to = "Count", values_drop_na = TRUE)

p <- ggplot(dtT, aes(x = Date, y = Count, group = Tests)) +
  geom_line(aes(color = Tests), lwd = 1) +
  #geom_point() +
  theme(legend.position = "bottom") + 
  #geom_smooth(mapping = aes(x = Date, y = Tests)) +
  # Annotations
  annotate("text", label = paste("On", lastTDate, ":",
                                 format(datTlast$`Tests daily`, scientific = FALSE, big.mark = " "), "tests,\n",
                                 format(datTlast$`Tests 7-days average`, scientific = FALSE, big.mark = " "), "tests (7-days average)"),
           x = datTlast$Date - 20,
           y = 500,
           size = 3, fontface = "italic") +
  labs(title = "Evolution of daily COVID-19 testing in Belgium (2020)",
       x = "Date",
       y = "# tests",
       caption = paste("Explanations at https://jepoirrier.org/becovid19/ ; data from https://epistat.wiv-isp.be/covid/ ; last data:", format(lastTDate, "%b %d, %Y"), " ; last update:", format(Sys.Date(), "%b %d, %Y")))
p
ggsave("../figures/tests.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")
