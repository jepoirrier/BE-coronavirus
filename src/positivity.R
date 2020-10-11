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

datT <- read.csv(TFILE, sep = ",", colClasses = c("Date", "character", "character", "integer", "integer"))
datT$DATE <- as.Date(datT$DATE)
lastTDate <- max(datT$DATE)
print(paste("Latest data point in TFILE:", lastTDate))

# Rename headers and aggregate data for Belgium
# TODO % positive tests per province / region
colnames(datT) <- c("Date", "Province", "Region", "TestsAll", "TestsPos")
datT <- na.exclude(datT) %>%
  group_by(Date) %>%
  summarise_at(c("TestsAll", "TestsPos"), sum)
datT$TestsPcPos <- datT$TestsPos / datT$TestsAll * 100

# Calculate a 7 day average on tests
datT <- datT %>%
  mutate(testsA_7da = zoo::rollmean(TestsAll, k = 7, fill = NA, align = "right")) %>%
  mutate(testsP_7da = zoo::rollmean(TestsPos, k = 7, fill = NA, align = "right"))
colnames(datT) <- c("Date", "Tests Total", "Tests Positifs", "Tests Pc Positifs", "Tests Total 7-days average",  "Tests Positifs 7-days average")
datTlast <- tail(datT, n = 1)

# Plot total tests and positive tests (raw and 7-days average)
datT1 <- datT
datT1$`Tests Positifs` <- NULL
datT1$`Tests Pc Positifs` <- NULL
datT1$`Tests Positifs 7-days average` <- NULL
colnames(datT1) <- c("Date", "Daily", "7-days average")
cols2pivot <- colnames(datT1)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"
dT1 <- pivot_longer(data = datT1, cols = cols2pivot, names_to = "Tests Total", values_to = "Count", values_drop_na = TRUE)
p <- ggplot(dT1, aes(x = Date, y = Count, group = `Tests Total`)) +
  geom_line(aes(color = `Tests Total`)) +
  annotate("text", label = paste("On", datTlast$Date, ":\n",
                                 "Tests done:", format(datTlast$`Tests Total`, scientific = FALSE, big.mark = " ", digits = 2), "\n",
                                 "7-day average tests done:", format(datTlast$`Tests Total 7-days average`, scientific = FALSE, big.mark = " ", digits = 2)),
           x = datTlast$Date - 90,
           y = 40000,
           size = 3, fontface = "italic") +
  labs(title = "Evolution of daily total COVID-19 tests done in Belgium (2020)",
       x = "Date",
       y = "Daily total tests done")
p

datT2 <- datT
datT2$`Tests Total` <- NULL
datT2$`Tests Pc Positifs` <- NULL
datT2$`Tests Total 7-days average` <- NULL
colnames(datT2) <- c("Date", "Daily", "7-days average")
cols2pivot <- colnames(datT2)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"
dT2 <- pivot_longer(data = datT2, cols = cols2pivot, names_to = "Tests Positive", values_to = "Count", values_drop_na = TRUE)
q <- ggplot(dT2, aes(x = Date, y = Count, group = `Tests Positive`)) +
  geom_line(aes(color = `Tests Positive`)) +
  annotate("text", label = paste("On", datTlast$Date, ":\n",
                                 "Tests positive:", format(datTlast$`Tests Positifs`, scientific = FALSE, big.mark = " ", digits = 2), "\n",
                                 "7-day average tests positive:", format(datTlast$`Tests Positifs 7-days average`, scientific = FALSE, big.mark = " ", digits = 2)),
           x = datTlast$Date - 90,
           y = 4000,
           size = 3, fontface = "italic") +
  labs(title = "Evolution of daily positive COVID-19 tests in Belgium (2020)",
       x = "Date",
       y = "Daily positive tests results",
       caption = paste("Explanations at https://jepoirrier.org/becovid19/ ; data from https://epistat.wiv-isp.be/covid/ ; last data:", format(datTlast$Date, "%b %d, %Y"), " ; last update:", format(Sys.Date(), "%b %d, %Y")))
q
r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")
r
ggsave("../figures/tests.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")
# for tests, I used to do Total tests per 100,000 pop. - but no more

# Plot total tests and positive tests (raw and 7-days average)
datT3 <- datT
datT3$`Tests Total 7-days average` <- NULL
datT3$`Tests Positifs 7-days average` <- NULL
datT3$`Tests Total` <- NULL
datT3$`Tests Positifs` <- NULL
datT3 <- datT3 %>%
  mutate(testsPc_7da = zoo::rollmean(`Tests Pc Positifs`, k = 7, fill = NA, align = "right"))
colnames(datT3) <- c("Date", "Daily", "7-day average")
datT3last <- tail(datT3, n = 1)
cols2pivot <- colnames(datT3)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"
dT3 <- pivot_longer(data = datT3, cols = cols2pivot, names_to = "Positivity", values_to = "Percentage", values_drop_na = TRUE)
s <- ggplot(dT3, aes(x = Date, y = Percentage, group = Positivity)) +
  geom_line(aes(color = Positivity)) +
  annotate("text", label = paste("On", datT3last$Date, ":\n",
                                 "Daily \"positivity\":", format(datT3last$Daily, scientific = FALSE, big.mark = " ", digits = 2), "% (**)\n",
                                 "7-day average \"positivity\":", format(datT3last$`7-day average`, scientific = FALSE, big.mark = " ", digits = 2), "%"),
           x = datT3last$Date - 30,
           y = 20,
           size = 3, fontface = "italic") +
  annotate("text", label = paste("(**)"),
           x = datT3last$Date,
           y = datT3last$Daily + 3,
           size = 3) +
  labs(title = "Evolution of COVID-19 positivity rate (*) in Belgium (2020)",
       x = "Date",
       y = "% tests turning out positive",
       caption = paste("(*) Positivity rate defined as N positive tests / N total tests\n",
                       "(**) The positivity for the last day may be biased by incomplete reporting\n",
                       "Explanations at https://jepoirrier.org/becovid19/ ; data from https://epistat.wiv-isp.be/covid/ ; last data:", format(datT3last$Date, "%b %d, %Y"), " ; last update:", format(Sys.Date(), "%b %d, %Y")))

s

ggsave("../figures/positivity.png", plot = s, device = "png", width = plotWidth, height = plotHeight, units = "in")


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

#superDat$TestsPositivity <- superDat$`Cases daily` / superDat$`Tests daily` * 100
#superDat$Tests7daPositivity <- superDat$`Cases 7-days average` / superDat$`Tests 7-days average` * 100

# Some cleanup
superDat$`Tests Total` <- NULL
superDat$`Tests Positifs` <- NULL # comment this if you want to see small variations
superDat$`Tests Pc Positifs` <- NULL
superDat$`Tests Total 7-days average` <- NULL
superDat$`Cases daily` <- NULL # comment this if you want to see small variations
colnames(superDat) <- c("Date", "Positive tests", "Cases")
superDatlast <- tail(na.exclude(superDat), n = 1)

# Simply plotting the daily cases
cols2pivot <- colnames(superDat)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"
superdt <- pivot_longer(data = superDat, cols = cols2pivot, names_to = "7-day average", values_to = "Count", values_drop_na = TRUE)

t <- ggplot(superdt, aes(x = Date, y = Count, group = `7-day average`)) +
  geom_line(aes(color = `7-day average`)) +
  # Annotations
  annotate("text", label = paste("On", superDatlast$Date, ":\n",
                                 "7-day average positive tests:", format(superDatlast$`Positive tests`, scientific = FALSE, big.mark = " ", digits = 2), "\n",
                                 "7-day average cases:", format(superDatlast$Cases, scientific = FALSE, big.mark = " ", digits = 2)),
           x = superDatlast$Date - 60,
           y = 2000,
           size = 3, fontface = "italic") +
  annotate("text", label = paste("(*)"),
           x = superDatlast$Date,
           y = superDatlast$`Positive tests` + 400,
           size = 3) +
  labs(title = "Evolution of COVID-19 positive tests and cases in Belgium (2020)",
       x = "Date",
       y = "Count",
       caption = paste("(*) The daily count for the last day may be biased by incomplete reporting\n",
                       "Explanations at https://jepoirrier.org/becovid19/ ; data from https://epistat.wiv-isp.be/covid/ ; last data:", format(datCtlast$Date, "%b %d, %Y"), " ; last update:", format(Sys.Date(), "%b %d, %Y")))
t

ggsave("../figures/positivity-cases.png", plot = t, device = "png", width = plotWidth, height = plotHeight, units = "in")

# library(plotly)
# 
# fig <- plot_ly(superDat, x = superDat$Date, y = superDat$`7-days average kind-of Positivity`, type = 'scatter', mode = 'lines')
# fig <- fig %>% 
#   layout(title = "Evolution of COVID-19 \"positivity rate\" (*) in Belgium (2020)\n(*) Not a real positivity rate as dates for tests and cases are not matched, just positivity on reported date",
#          xaxis = list(title = "Date"),
#          yaxis = list (title = "% positivity"))
# fig
# htmlwidgets::saveWidget(config(fig, showLink = T), "../figures/positivity-7-days-average.html")
#plotly_POST(x = fig, sharing = "public")
