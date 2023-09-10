

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

station <- read_csv("data/1-station.csv")
# filter(station, DateTimeRound == "2022/07/22 12:00:00")

