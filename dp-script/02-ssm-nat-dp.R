setwd(paste(Sys.getenv("OneDrive"), "\\GitHub\\ssm-nat-shiny", sep=''))

library(lubridate)
library(data.table)
library(readxl)
library(tidyverse)
library(grid)
library(gridExtra)
library(plotly)
library(ggmap)
library(GGally)
library(sp)
library(rgeos)

version2 <- "20220723185612"
# "20220722170034"
# "20220723120109"
# "20220723185612"

st_out <- paste("data/1-station-",version2,".csv", sep = "")
bk_out <- paste("data/2-booking-",version2,".csv", sep = "")
tp_out <- paste("data/3-throughput-",version2,".csv", sep = "")
station <- read_csv(st_out)
booking <- read_csv(bk_out)
throughput <- read_csv(tp_out)

versionEt <- as.POSIXct(version2,format="%Y%m%d%H%M%S","Asia/Taipei")
versionEtRound <- floor_date(versionEt, "30mins")

### Start generate for map html with leaflet
totalSwabBooking <- sum(booking$SwabCount,na.rm = TRUE)
totalSwabDone <- sum(throughput$SwabCount,na.rm = TRUE)

## Start plotting
options("scipen" = 100, "digits" = 4)

p1 <- booking %>% group_by(Status) %>%
  summarise(NasalSwab.sum = sum(鼻咽拭, na.rm = TRUE), OralSwab.sum = sum(口咽拭, na.rm = TRUE)) %>%
  as.data.table() %>%
  melt(., measure.vars = c("NasalSwab.sum","OralSwab.sum"))
p2 <- booking %>% group_by(area, Status) %>% tally(SwabCount)

g1 <- ggplot(data=p1, aes(x = variable, y = value, fill = Status)) +
  geom_bar(stat="identity", alpha = .7) +
  coord_flip() +
  scale_color_viridis_d(option = 'magma') + scale_fill_viridis_d(option = 'magma') +
  theme_minimal() +
  xlab("採集方法") + ylab("採樣數") +
  # ggtitle("口鼻採集法總計數")
  ggtitle("Total Swabs Done vs Booked by Swab Method")

g2 <- ggplot(data=p2, aes(x = reorder(area,n), y = n, fill = Status)) +
  geom_bar(stat="identity", alpha = .7) +
  coord_flip() +
  scale_color_viridis_d(option = 'magma') + scale_fill_viridis_d(option = 'magma') +
  theme_minimal() +
  xlab("地區") + ylab("採樣數") +
  ggtitle("地區總計數")

g3 <- ggplot(data = booking, aes(x = as.POSIXct(DateTimeRound), y = SwabCount, fill = Status)) +
  geom_bar(stat="identity", alpha = .7) +
  scale_color_viridis_d(option = 'magma') + scale_fill_viridis_d(option = 'magma') +
  theme_minimal() +
  xlab("每小時") + ylab("採樣數") +
  ggtitle("每小時總計數")

g4 <- ggplot(data = booking, aes(x = reorder(factor(DurationDayNumber),SwabCount), y = SwabCount, fill = Status)) +
  geom_bar(stat="identity", alpha = .7) +
  coord_flip() +
  scale_color_viridis_d(option = 'magma') + scale_fill_viridis_d(option = 'magma') +
  theme_minimal() +
  xlab("第N天") + ylab("採樣數") +
  ggtitle("當天總計數")

grid.arrange(g1, g3, g2, g4, ncol = 2,
top = paste("Total Swab Done ", totalSwabDone, " out of ", totalSwabBooking, " bookings as of ", versionEtRound, sep = " "))
