# BE-coronavirus analysis
# Confirmed cases by date, age, sex and province

# Source: https://epistat.wiv-isp.be/covid/
# Data URL: https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv

library(dplyr)
library(ggpubr) # for ggarrange
library(gghighlight)
library(ggplot2)
library(ini)
library(tidyr)
library(zoo) # for rollmean()

print("Entering cases.R")

plotWidth <- 10
plotHeight <- 6 # for single graph: 6 (= 2 times 3) + 1
plotHeightLong <- 12 # for multiple graphs: 9 (= 3 times 3) + 1
preventMultipleDownload <- TRUE

logScale <- TRUE
logWarning <- ""
if(logScale)
  logWarning = " (log scale!)"

# Read an .ini file with point data in it
iniFile <- "../data/pointData.ini"
pointData <- read.ini(iniFile)

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

# Save to calculate the cumulative # of cases
datCc <- datCt

# Calculate a 7 day average
datCt <- datCt %>%
  mutate(Case7da = zoo::rollmean(Cases, k = 7, fill = NA, align = "right"))
colnames(datCt) <- c("Date", "Cases daily", "Cases 7-days average")
datCtlast <- tail(na.exclude(datCt), n = 1)

# Simply plotting the daily cases
cols2pivot <- colnames(datCt)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"
dtCt <- pivot_longer(data = datCt, cols = cols2pivot, names_to = "Cases", values_to = "Count", values_drop_na = TRUE)

p <- ggplot(dtCt, aes(x = Date, y = Count, group = Cases)) +
  geom_line(aes(color = Cases), lwd = 1) +
  theme(legend.position = "bottom") + 
  # Annotations
  annotate("text", label = paste("On", datCtlast$Date, ":",
                                 format(datCtlast$`Cases daily`, scientific = FALSE, big.mark = " ", digits = 5), "cases,\n",
                                 format(datCtlast$`Cases 7-days average`, scientific = FALSE, big.mark = " ", digits = 5), "cases (7-days average)"),
           x = datCtlast$Date - 20,
           y = 1500,
           size = 3, fontface = "italic") +
  labs(title = "Evolution of daily confirmed COVID-19 cases in Belgium (2020)",
       x = "Date",
       y = "# confirmed cases")
#p
#ggsave("../figures/cases.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")

# Respective to total population
# Not very insightfull but still there ...

bePopTot <- as.integer(pointData$BEPopulation$Total)

datCt <- datCt %>%
  mutate(casesRel = `Cases daily` / bePopTot * 100000) %>%
  mutate(cases7Rel = `Cases 7-days average` / bePopTot * 100000)

# Simply plotting the test data relative to the Belgian population
datCt$`Cases daily` <- NULL
datCt$`Cases 7-days average` <- NULL
colnames(datCt) <- c("Date", "Cases daily", "Cases 7-days average")
datCtlast <- tail(na.exclude(datCt), n = 1)

cols2pivot <- colnames(datCt)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"
dtCt <- pivot_longer(data = datCt, cols = cols2pivot, names_to = "Relative", values_to = "Count", values_drop_na = TRUE)

q <- ggplot(dtCt, aes(x = Date, y = Count, group = Relative)) +
  geom_line(aes(color = Relative), lwd = 1) +
  theme(legend.position = "bottom") + 
  # Annotations
  annotate("text", label = paste("On", datCtlast$Date, ":",
                                 format(datCtlast$`Cases daily`, digits = 2), "cases per 100 000,\n",
                                 format(datCtlast$`Cases 7-days average`, digits = 3), "cases (7-days average) per 100 000"),
           x = datCtlast$Date - 60,
           y = 10,
           size = 3, fontface = "italic") +
  labs(title = "Evolution of daily confirmed COVID-19 cases relative to Belgian population (2020)",
       x = "Date",
       y = "# cases relative to population (per 100 000 pop.)",
       caption = paste("Explanations at https://jepoirrier.org/becovid19/ ; data from https://epistat.wiv-isp.be/covid/ ; last data:", format(datCtlast$Date, "%b %d, %Y"), " ; last update:", format(Sys.Date(), "%b %d, %Y")))
#q

r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")
#r
#ggsave("../figures/cases.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")

# Calculate and display the cumulative # cases with datCc

datCc <- datCc %>%
  mutate(cumulative_cases = cumsum(Cases))

# cleanup
datCc$Cases <- NULL
colnames(datCc) <- c("Date", "Cumulative Cases")
  
datCclast <- tail(na.exclude(datCc), n = 1)

s <- ggplot(datCc, aes(x = Date, y = `Cumulative Cases`)) +
  {if(logScale) scale_y_log10()} +
  {if(logScale) annotation_logticks()} +
  geom_line(lwd = 1) +
  theme(legend.position = "bottom") + 
  # Annotations
  annotate("text", label = paste("On", datCclast$Date, ":\n",
                                 format(datCclast$`Cumulative Cases`, scientific = FALSE, big.mark = " ", digits = 5), "cases\n",
                                 format(datCclast$`Cumulative Cases` / bePopTot * 100000, scientific = FALSE, big.mark = " ", digits = 5), "cases per 100 000 pop.\n"),
           x = datCclast$Date - 20,
           y = 5000,
           size = 3, fontface = "italic") +
  labs(title = "Evolution of cumulative confirmed COVID-19 cases in Belgium (2020)",
       x = "Date",
       y = paste("Cumulative cases", logWarning),
       caption = paste("Explanations at https://jepoirrier.org/becovid19/ ; data from https://epistat.wiv-isp.be/covid/ ; last data:", format(datCclast$Date, "%b %d, %Y"), " ; last update:", format(Sys.Date(), "%b %d, %Y")))
s
r <- ggarrange(p, s, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")

ggsave("../figures/cases.png", plot = r, device = "png", width = plotWidth, height = plotHeight, units = "in")

# Looking at Brabant Wallon & Liege

datCP <- datC %>%
  na.omit() %>%
  filter(Province == "BrabantWallon" | Province == "Liège") %>%
  select(-Region) %>% # pipe shortcut in RStudio: Ctrl + Shift + M
  # Calculate totals per day
  group_by(Date, Province) %>%
  summarise_at(vars(Cases),list(Cases = sum))

TotPop <- data.frame( # https://statbel.fgov.be/fr/themes/population/structure-de-la-population
  Province = c("Liège", "BrabantWallon"),
  Population = c(1106992, 403599)
)

datCP <- merge(datCP, TotPop, by = 'Province', all.x = TRUE)

datCP$casesRel <- datCP$Cases / datCP$Population * 100000

datCP <- datCP %>% 
  select(-c(Cases, Population))

datCPlast <- tail(datCP, n = 1)

t <- ggplot(datCP, aes(x = Date, y = casesRel, group = Province)) +
  geom_line(aes(color = Province)) +
  #gghighlight(Province == "BrabantWallon") + 
  # Annotations
  labs(title = "Evolution of daily confirmed COVID-19 cases in some Belgium provinces (2020)",
       x = "Date",
       y = "# cases relative to pop. (per 100 000)",
       caption = paste("Explanations at https://jepoirrier.org/becovid19/ ; data from https://epistat.wiv-isp.be/covid/ ; last data:", format(datCPlast$Date, "%b %d, %Y"), " ; last update:", format(Sys.Date(), "%b %d, %Y")))

t

ggsave("../figures/cases-WB-Lg.png", plot = t, device = "png", width = plotWidth, height = plotHeight, units = "in")

# Trying plotly

#library(plotly)

#fig <- plot_ly(datCP, x = datCP$Date, y = datCP$caseRel, type = 'scatter', mode = 'lines')
#fig
