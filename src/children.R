# BE-coronavirus analysis
# Cases & quarantines in schools in Wallonia

# Source: https://www.one.be/public/coronavirus/
# No CSV available: data should be ;manually scraped from press releases in PDF

library(dplyr)
library(ggpubr) # for ggarrange
library(gghighlight)
library(ggplot2)
library(ini)
library(tidyr)
library(zoo) # for rollmean()

print("Entering ONE analysis")

plotWidth <- 10
plotHeight <- 6 # for single graph: 6 (= 2 times 3) + 1
plotHeightLong <- 12 # for multiple graphs: 9 (= 3 times 3) + 1
preventMultipleDownload <- FALSE

logScale <- TRUE
logWarning <- ""
if(logScale)
  logWarning = " (log scale!)"

datO <- read.csv("../data/COVID19BE_ONE.csv", header = TRUE, sep = " ", colClasses = c("Date", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer"))
datO$WeekEnding <- as.Date(datO$WeekEnding)
lastODate <- max(datO$WeekEnding)
print(paste("Latest data point in OFILE:", lastODate))

# Cases, just numbers
datOC <- select(datO, -starts_with("Q"))
colnames(datOC) <- c("Week ending", "Maternelle (2.5-5y)", "Primaire (6-11y)", "Secondaire (12-17y)", "Superieur (18+)", "Personnel", "Inconnu")

cols2pivot <- colnames(datOC)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"
dtOC <- pivot_longer(data = datOC, cols = cols2pivot, names_to = "Type", values_to = "Cases", values_drop_na = TRUE)
dtOC$Type <- factor(dtOC$Type, levels = c("Maternelle (2.5-5y)", "Primaire (6-11y)", "Secondaire (12-17y)", "Superieur (18+)", "Personnel", "Inconnu"))

q <- ggplot(dtOC, aes(x = `Week ending`, y = Cases, group = Type)) +
  geom_line(aes(color = Type)) +
  labs(title = "Evolution of weekly COVID-19 cases in Wallonia schools (2020)",
       x = "Week ending",
       y = "Number of new cases",
       caption = paste("Note: these numbers are most likely underestimated, for several reasons, see link below\n",
         "Explanations at https://jepoirrier.org/becovid19/ ; data from https://www.one.be/public/coronavirus/ ; last data:", format(lastODate, "%b %d, %Y"), " ; last update:", format(Sys.Date(), "%b %d, %Y")))
q

# Cases, cumulative
datOC <- datOC %>%
  mutate(cMat = cumsum(`Maternelle (2.5-5y)`)) %>%
  mutate(cPri = cumsum(`Primaire (6-11y)`)) %>%
  mutate(cSec = cumsum(`Secondaire (12-17y)`)) %>%
  mutate(cSup = cumsum(`Superieur (18+)`)) %>%
  mutate(cPer = cumsum(`Personnel`)) %>%
  mutate(cInc = cumsum(`Inconnu`)) %>%
  select(`Week ending`, starts_with("c"))

colnames(datOC) <- c("Week ending", "Maternelle (2.5-5y)", "Primaire (6-11y)", "Secondaire (12-17y)", "Superieur (18+)", "Personnel", "Inconnu")

cols2pivot <- colnames(datOC)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"
dtOC <- pivot_longer(data = datOC, cols = cols2pivot, names_to = "Type", values_to = "Cases", values_drop_na = TRUE)
dtOC$Type <- factor(dtOC$Type, levels = c("Maternelle (2.5-5y)", "Primaire (6-11y)", "Secondaire (12-17y)", "Superieur (18+)", "Personnel", "Inconnu"))

p <- ggplot(dtOC, aes(x = `Week ending`, y = Cases, group = Type)) +
  geom_line(aes(color = Type)) +
  labs(title = "Evolution of cumulative COVID-19 cases in Wallonia schools (2020)",
       x = "Week ending",
       y = "Number of cumulative cases")
p

r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")
r

ggsave("../figures/children-ONEcases.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")



