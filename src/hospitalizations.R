# BE-coronavirus analysis
# Hospitalizations

# Source: https://epistat.wiv-isp.be/covid/
# CSV: https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv

library(dplyr)
library(ggpubr) # for ggarrange
library(gghighlight)
library(ggplot2)
library(ini)
library(tidyr)
library(zoo) # for rollmean()

print("Entering hospitalizations")

plotWidth <- 10
plotHeight <- 6 # for single graph: 6 (= 2 times 3) + 1
plotHeightLong <- 12 # for multiple graphs: 9 (= 3 times 3) + 1
preventMultipleDownload <- FALSE # because cases.R and tests.R have most likely run already

logScale <- TRUE
logWarning <- ""
if(logScale)
  logWarning = " (log scale!)"

# Tests file ...

HURL <- "https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv"
HFILE <- "../data/COVID19BE_hospitalizations.csv"

# Download and import the data
if((as.Date(file.info(HFILE)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(HURL, HFILE, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as HFile already downloaded today")
}
if(file.exists(HFILE)) {
  print(paste("HFILE found, created on:", file.info(HFILE)$ctime))
} else {
  stop(HFILE, " does not exist") # not the best way to stop (if it even stops!)
}

datH <- read.csv(HFILE, sep = ",", colClasses = c("Date", "character", "character", "integer", "integer", "integer", "integer", "integer", "integer", "integer"))
datH$DATE <- as.Date(datH$DATE)
lastHDate <- max(datH$DATE)
print(paste("Latest data point in HFILE:", lastHDate))

# Clean the data
colnames(datH) <- c("Date", "Province", "Region", "NReporting", "Prevalence", "PrevICU", "PrevResp", "PrevESMO", "Admissions", "Discharges")
#datH$NReporting <- NULL

# Calculate totals per day
datHt <- na.exclude(datH) %>%
  group_by(Date) %>%
  summarise_at(vars(NReporting, Prevalence, PrevICU, PrevResp, PrevESMO, Admissions, Discharges),
               sum)

datHt1 <- datHt %>% 
  select(-c(Admissions, Discharges))
datHt2 <- datHt %>% 
  select(-c(NReporting, Prevalence, PrevICU, PrevResp, PrevESMO))

# Simple graph Admissions and Discharges
cols2pivot <- colnames(datHt2)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"
dtHt2 <- pivot_longer(data = datHt2, cols = cols2pivot, names_to = "InOut", values_to = "Patients", values_drop_na = FALSE)

p <- ggplot(dtHt2, aes(x = Date, y = Patients, group = InOut)) +
  geom_line(aes(color = InOut)) +
  labs(title = "Evolution of COVID-19 admissions and discharges in Belgium (2020)",
       x = "Date",
       y = "Number of patients",
       caption = paste("Explanations at https://jepoirrier.org/becovid19/ ; data from https://epistat.wiv-isp.be/covid/ ; last data:", format(lastHDate, "%b %d, %Y"), " ; last update:", format(Sys.Date(), "%b %d, %Y")))
p
ggsave("../figures/hospit-admdis.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")

# Calculations N patients currently in hospital = cumulative admissions - cumulative discharges

datHt2$AdmissionsSum <- cumsum(datHt2$Admissions)

# Small parenthesis for cumulative Admissions = patients ever hospitalized (Not counting potential doubles)
datHt2tmp <- datHt2 %>% 
  select(-c(Admissions, Discharges))

p <- ggplot(datHt2tmp, aes(x = Date, y = AdmissionsSum)) +
  geom_line() +
  # Annotations
  annotate("text", label = paste("On", format(lastHDate, "%b %d, %Y"), ":\n",
                                 format(max(datHt2tmp$AdmissionsSum), scientific = FALSE, big.mark = " ", digits = 2), "patients ever hospitalized\nfor COVID-19 since the beginning"),
           x = lastHDate - 30,
           y = 15000,
           size = 3, fontface = "italic") +
  labs(title = "Total number of patients ever hospitalized due to COVID-19 in Belgium (2020)",
       x = "Date",
       y = "Cumulative number of patients")
p
#ggsave("../figures/hospit-ever.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")

# Back to calculations ...

datHt2$DischargesSum <- cumsum(datHt2$Discharges)
datHt2$CurrentlyInHospital <- datHt2$AdmissionsSum - datHt2$DischargesSum

datHt2 <- datHt2 %>% 
  select(-c(Admissions, Discharges, AdmissionsSum, DischargesSum))
colnames(datHt2) <- c("Date", "Currently in hospital")
datHt2last <- tail(datHt2, n = 1)

q <- ggplot(datHt2, aes(x = Date, y = `Currently in hospital`)) +
  geom_line() +
  # Annotations
  annotate("text", label = paste("On", format(lastHDate, "%b %d, %Y"), ":\n",
                                 format(datHt2last$`Currently in hospital`, scientific = FALSE, big.mark = " ", digits = 2), "patients currently\nhospitalized for COVID-19"),
           x = lastHDate - 30,
           y = 2000,
           size = 3, fontface = "italic") +
  labs(title = "Number of patients currently hospitalized due to COVID-19 in Belgium (2020)",
       x = "Date",
       y = "Number of patients",
       caption = paste("N patients currently hospitalized, each day = sum of all admissions - sum of all discharges\n",
         "Explanations at https://jepoirrier.org/becovid19/ ; data from https://epistat.wiv-isp.be/covid/ ; last data:", format(lastHDate, "%b %d, %Y"), " ; last update:", format(Sys.Date(), "%b %d, %Y")))
q

r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")
r

ggsave("../figures/hospit-evercurrent.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")

