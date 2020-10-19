# AVIQ clusters - just graphical representation so far
# 
# URL: https://covid.aviq.be/fr/les-actualites
# PDF direct link: https://covid.aviq.be/sites/default/files/fichiers-upload/15%2010%202020%20CP%20lieux%20des%20clusters%20Covid-19.pdf

library(ggplot2)
library(tidyr)

print("Entering AVIQ Clusters")

plotWidth <- 10
plotHeight <- 6 # for single graph: 6 (= 2 times 3) + 1
plotHeightLong <- 12 # for multiple graphs: 9 (= 3 times 3) + 1
preventMultipleDownload <- FALSE # because cases.R and tests.R have most likely run already

logScale <- TRUE
logWarning <- ""
if(logScale)
  logWarning = " (log scale!)"

# Tests file ...
AFILE <- "../data/AVIQ-clusters-201015.csv"

if(file.exists(AFILE)) {
  print(paste("AFILE found, created on:", file.info(AFILE)$ctime))
} else {
  stop(AFILE, " does not exist") # not the best way to stop (if it even stops!)
}

datA <- read.csv(AFILE, sep = ",", colClasses = c("character", "numeric"))

datA$Cluster <- factor(datA$Cluster,
                       levels = datA$Cluster[order(datA$Percent, decreasing = FALSE)])

p <- ggplot(data = datA, aes(x = Cluster, y = Percent, label = sprintf("%1.1f%%", Percent))) +
  geom_bar(stat="identity", fill = "darkgreen") +
  coord_flip() +
  geom_text(size = 3, hjust = -0.2)  +
  labs(title = "COVID-19 clusters in Wallonia",
       subtitle = "AVIQ poll made public on October 15, 2020",
       x = "Percent",
       y = "Cluster location",
       caption = paste("Explanations at https://jepoirrier.org/becovid19/ ; data from https://covid.aviq.be/fr/les-actualites ; last update:", format(Sys.Date(), "%b %d, %Y")))

p

ggsave("../figures/AVIQ-clusters.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")

