
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



version <- 20220722
locMaster <- read_csv(paste("data/source/location-master/location-master-",version,".csv", sep = ""))


version2 <- "20220723185612"
# "20220722170034"
# "20220723120109"
# "20220723185612"

StartTimeStr <- "2022-07-22 09:00"
EndTimeStr <- as.POSIXct(version2,format="%Y%m%d%H%M%S","Asia/Taipei")

versionSt <- as.POSIXct(StartTimeStr, "Asia/Taipei")
versionEt <- as.POSIXct(EndTimeStr, "Asia/Taipei") - 1 # Avoid version ended given no result capture
versionEt0 <- as.POSIXct(EndTimeStr, "Asia/Taipei")

versionEtRound <- floor_date(versionEt, "30mins")
versionTi <- interval(versionSt, versionEt)
versionTr <- seq(versionSt, versionEt, "30 mins")
versionTr0 <- seq(versionSt, versionEt0, "30 mins")

versionEtStr <- str_remove_all(str_remove_all(toString(versionEt), "-"),":")




filelist <- dir('data/source/aptmon', full.names=TRUE)
# tailfile <- tail(filelist, 1)
tailfile <- paste("data/source/aptmon/aptmon-",version2,".xlsx", sep = "")

scrp <- data.frame()
  for (file in filelist) {
    filetimestr <- sub(".xlsx", "", sub(".*-", "", file))
    filetime <- strptime(filetimestr,"%Y%m%d%H%M%S")
    tailfileflag <- ifelse(file == tailfile, 1, 0)
    if (filetime %within% versionTi) {
      temp <- read_excel(file, na = "---")
      temp$DateTime <- as.POSIXlt(filetime)
      temp$CurrentFlag <- tailfileflag
      scrp <- rbind(scrp,as.data.frame(temp))
    }
  }

# tail(scrp[order(scrp[,"DateTime"]),], 70)

### Date and time transformation
attr(scrp$DateTime, "tzone") <- "Asia/Taipei"
scrp$DateTimeRound <- floor_date(scrp$DateTime, "30mins")
scrp$WaitingQueue <- as.numeric(as.character(sub("*人", "", scrp$輪候人數)))
scrp$WaitingMinutes <- as.numeric(as.character(sub("分鐘", "",sub(".*>", "", scrp$等候時間))))
scrp$口採樣點 = as.numeric(scrp$口採樣點)
scrp$鼻採樣點 = as.numeric(scrp$鼻採樣點)
scrp$DeskCount <- rowSums(scrp[ ,c("口採樣點", "鼻採樣點")], na.rm=TRUE)
scrp$HourNumber <- sapply(strsplit(substr(scrp$DateTimeRound,12,16),":"),
         function(x) {
           x <- as.numeric(x)
           x[1]+x[2]/60
         }
  )
# str(scrp)


station <- scrp %>% group_by(序號,Location,DateTimeRound,HourNumber) %>%
  summarise(
    DeskCount.mean = mean(DeskCount, na.rm = TRUE),
    DeskCount.median = median(DeskCount, na.rm = TRUE),
    口採樣點.mean = mean(口採樣點, na.rm = TRUE),
    鼻採樣點.mean = mean(鼻採樣點, na.rm = TRUE),
    口採樣點.median = median(口採樣點, na.rm = TRUE),
    鼻採樣點.median = median(鼻採樣點, na.rm = TRUE),
    WaitingQueue.mean = mean(WaitingQueue, na.rm = TRUE),
    WaitingMinutes.mean = mean(WaitingMinutes, na.rm = TRUE),
    WaitingQueue.median = median(WaitingQueue, na.rm = TRUE),
    WaitingMinutes.median = median(WaitingMinutes, na.rm = TRUE),
    WaitingQueue.current = max(ifelse(CurrentFlag == 1, WaitingQueue, 0)),
    WaitingMinutes.current = max(ifelse(CurrentFlag == 1, WaitingMinutes, 0)),
    MaxCurrentFlag = max(CurrentFlag)
  ) %>%
  ungroup() %>%
  as.data.frame()
# str(station)

# filter(scrp, Location == "MGM Cotai")
### Complete for the missing DateTimeRound, note there's no filling result of MaxCurrentFlag
# unique(scrp$Location)
# unique(station$DateTimeRound)

station <- station %>%
  complete(nesting(Location,序號),
           DateTimeRound = seq.POSIXt(versionSt, versionEt, by="30 min")
  ) %>%
  mutate(HourNumber = hour(DateTimeRound) + minute(DateTimeRound) / 60) %>%
  arrange(Location,DateTimeRound) %>%
  group_by(Location) %>%
  fill(`DeskCount.mean`,`DeskCount.median`,
       `口採樣點.mean`,`鼻採樣點.mean`,`口採樣點.median`,`鼻採樣點.median`,
       `WaitingQueue.mean`,`WaitingMinutes.mean`,`WaitingQueue.median`,`WaitingMinutes.median`) %>%
  mutate(DeskCount.ntile = ntile(DeskCount.mean, 5),
         AvgDeskCount = median(DeskCount.mean)) %>%
  ungroup() %>%
  complete(nesting(Location,序號),
           DateTimeRound = seq.POSIXt(versionSt, versionEt, by="30 min")
  ) %>%
  mutate(HourNumber = hour(DateTimeRound) + minute(DateTimeRound) / 60) %>%
  arrange(Location,desc(DateTimeRound)) %>%
  group_by(Location) %>%
  fill(`DeskCount.mean`,`DeskCount.median`,
       `口採樣點.mean`,`鼻採樣點.mean`,`口採樣點.median`,`鼻採樣點.median`,
       `WaitingQueue.mean`,`WaitingMinutes.mean`,`WaitingQueue.median`,`WaitingMinutes.median`) %>%
  mutate(DeskCount.ntile = ntile(DeskCount.mean, 5),
         AvgDeskCount = median(DeskCount.mean)) %>%
  ungroup()


### Inspect filled data via the complete process
# filter(station, DateTimeRound == "2022-06-19 12:00")

### Supply for Location Master data
set1 <- merge(station, locMaster)






### Locate for booking data

file2 <- paste("data/source/RNA010/RNA010-",version2,".xlsx", sep = "")

day1 <- as.Date(StartTimeStr)
day2 <- as.Date(StartTimeStr)+1
day3 <- as.Date(StartTimeStr)+2
sheet1 <- paste(str_remove_all(day1,"-"),"A",sep="")
sheet2 <- paste(str_remove_all(day2,"-"),"A",sep="")
# sheet3 <- paste(str_remove_all(day3,"-"),"A",sep="")

# sheets <- c(sheet1,sheet2,sheet3)
sheets <- c(sheet1,sheet2)

### Loop to ingest for all sheets
df <- data.frame()
for (sheetname in sheets) {
  # sheetname <- "20220710A"
  
  ### Extract first row for location list
  cnames <- read_excel(file2, sheet = sheetname, n_max = 0, na = "---") %>% names()
  cnames <- gsub("\n", "\r\n", cnames)
  cn1 <- cnames[seq(6, length(cnames))]
  lls1 <- sub(".*?-", "",cn1[grepl("^[^...*]", cn1)])
  ### Extract data from 2nd row
  rdf1 <- read_excel(file2, sheet=sheetname, na = "---", skip = ifelse(sheetname == sheet1, 2, 1)) # skip 2 because there exists a hidden row 1 in this spreadsheet
  ### Set date
  rdf1$SwabDate <- as.Date(strptime(str_remove(sheetname, "A"),"%Y%m%d"))
  rdf1$SwabTime <- substr(rdf1$預約時段,1,5)
  ### select columns and rows
  sdf1 <- rdf1 %>% select(c(6:ncol(rdf1))) %>% slice(2:nrow(rdf1)) %>% select(-contains("總人次"))
  ### Repeat Location info for number of rows
  dtls <- as.data.table(lls1)
  setnames(dtls, "lls1", "Location")
  Location <- dtls[, repeats:=ifelse(grepl("^流動核酸採樣車.*", Location), nrow(sdf1), nrow(sdf1) * 2)][rep(1:.N,repeats)]
  ### Melt to pivot
  sdf1 <- as.data.frame(sdf1)
  mdf1 <- reshape::melt(sdf1, id = c("SwabDate", "SwabTime"))
  ### Combine Location with dataset
  df1 <- cbind(Location,mdf1)
  ### Clean away column names with ...
  df1$variable <- sub("\\....*", "", df1$variable)
  df <- rbind(df,as.data.frame(df1))
}

# df %>% filter(Location %like% '流動核酸採樣車')

pdf <- df %>% pivot_wider(names_from = variable, values_from = value)
pdf <- as.data.frame(pdf)
pdf$SwabCount <- rowSums(pdf[ ,c("口咽拭", "鼻咽拭")], na.rm=TRUE)
pdf$DateTimeRound <- as.POSIXlt(paste(pdf$SwabDate, pdf$SwabTime))
attr(pdf$DateTimeRound, "tzone") <- "Asia/Taipei"
pdf$HourNumber <- 
  sapply(strsplit(pdf$SwabTime,":"),
         function(x) {
           x <- as.numeric(x)
           x[1]+x[2]/60
         }
  )

# pdf %>% filter(Location %like% '流動核酸採樣車')


booking <- pdf %>%
  group_by(Location) %>%
  mutate(AvgSwabCount = mean(SwabCount)) %>%
  ungroup() %>%
  mutate(
    DurationHour = as.numeric((DateTimeRound - ymd_hms(versionSt, tz = "Asia/Taipei")),"hours"),
    DurationDay = as.numeric((DateTimeRound - ymd_hms(versionSt, tz = "Asia/Taipei")),"days"),
    DurationDayNumber = as.integer(as.numeric((DateTimeRound - ymd_hms(versionSt, tz = "Asia/Taipei")),"days") + 1),
    Status = ifelse(DateTimeRound <= versionEt, "1.已完成", "2.預約中")
  )
# str(booking)

### Supply for Location Master data
set2 <- merge(booking, locMaster)
# str(set2)





### Combine for bookings and station data into throughput dataframe
mdf <- merge(booking, station, by = c("Location", "DateTimeRound","HourNumber")) #, all.x=TRUE)
mdf <- mdf %>%
  mutate(
    DateTimeRound = as.POSIXct(DateTimeRound),
    SwabPerDesk = mdf$SwabCount / ifelse(is.na(mdf$DeskCount.mean) | mdf$DeskCount.mean == 0, 1,
                                         mdf$DeskCount.mean),
    SwabPerDesk.ntile = ntile(SwabPerDesk, 4),
    SwabPerDesk.color = case_when(SwabPerDesk.ntile == 4 ~ "coral",
                                SwabPerDesk.ntile == 3 ~ "goldenrod",
                                SwabPerDesk.ntile == 2 ~ "steelblue",
                                SwabPerDesk.ntile == 1 ~ "seagreen",
                                TRUE ~ "chocolate"
                                ),
    MouthPerDesk = mdf$口咽拭 / ifelse(is.na(mdf$口採樣點.mean) | mdf$口採樣點.mean == 0 , 1,
                                    mdf$口採樣點.mean),
    NosePerDesk = mdf$鼻咽拭 / ifelse(is.na(mdf$鼻採樣點.mean) | mdf$鼻採樣點.mean == 0, 1,
                                   mdf$鼻採樣點.mean)) %>%
  group_by(Location) %>%
  mutate(AvgSwabPerDesk = mean(SwabPerDesk)) %>%
  ungroup()

throughput <- merge(mdf, locMaster)



st_out <- paste("data/1-station-",version2,".csv", sep = "")
bk_out <- paste("data/2-booking-",version2,".csv", sep = "")
tp_out <- paste("data/3-throughput-",version2,".csv", sep = "")
write_excel_csv(set1, st_out)
write_excel_csv(set2, bk_out)
write_excel_csv(throughput, tp_out)
