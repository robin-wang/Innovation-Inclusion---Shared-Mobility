
loadPkg = function(x) { if (!require(x,character.only=T, quietly =T)) { install.packages(x,dep=T,repos="http://cran.us.r-project.org"); if(!require(x,character.only=T)) stop("Package not found") } }

loadPkg("cluster")
loadPkg("dplyr")
loadPkg("ggplot2")
loadPkg("graphics")
loadPkg("gridExtra")
loadPkg("knitr")
loadPkg("tidyverse")
loadPkg ("pls")
loadPkg("caret")
loadPkg("ggrepel")
loadPkg("modelr")
loadPkg("purrr")
loadPkg("haven")
loadPkg("readxl")
loadPkg("stringr")
loadPkg("factoextra")
loadPkg("caret")
loadPkg("ggpubr")
loadPkg("stringr")
loadPkg("scales")

# Trip Data -----------------------------------------------------------------------------------------------------
wddir <- "D:\\Users\\RWang\\Box Sync\\Inclusion&Innovation\\Data Collection and Process\\Capital Bikeshare"
ACSfiledir <- "D:\\Users\\RWang\\Box Sync\\Inclusion&Innovation\\Data Collection and Process\\DC neighborhoods"
datadir10 <- "D:\\Users\\RWang\\Box Sync\\Inclusion&Innovation\\Data Collection and Process\\Capital Bikeshare\\2010-capitalbikeshare-tripdata"

setwd(datadir10)
tripdata10 <- data.frame(read.csv("2010-capitalbikeshare-tripdata.csv")) #115597 observations
tripdata10$DayWeek_S <- weekdays(as.Date(str_split_fixed(tripdata10$Start.date, " ",2)[,1],'%Y-%m-%d'))
tripdata10$Start_hour <- substr(str_split_fixed(tripdata10$Start.date, " ",2)[,2], start=1, stop=2)
trip_keep <- c("Duration","Start.station.number","End.station.number","Member.type",
               "Start_hour","DayWeek_S")
tripdata10 <- tripdata10[trip_keep]

setwd(wddir)
tract_info <- data.frame(read_excel("station_with_tract_info.xlsx"))
tract_keep <- c("TERMINAL_N","OWNER","TRACTCE","GEOID")
tract_info <- tract_info[tract_keep]

tripdata10 <- subset(merge(x=tripdata10,y=tract_info,
                           by.x = c("Start.station.number"),by.y = c("TERMINAL_N")),OWNER=="DC") #109739 observations
tripdata10 <- subset(tripdata10, select= -c(OWNER))
colnames(tripdata10)[(length(colnames(tripdata10)) - 1):length(colnames(tripdata10))] <- purrr::map_chr(colnames(tripdata10)[(length(colnames(tripdata10)) - 1):length(colnames(tripdata10))],paste0,"_S")
tripdata10 <- subset(merge(x=tripdata10,y=tract_info,
                           by.x = c("End.station.number"),by.y = c("TERMINAL_N")),OWNER=="DC") #109123 observations
tripdata10 <- subset(tripdata10, select= -c(OWNER))
colnames(tripdata10)[(length(colnames(tripdata10)) - 1):length(colnames(tripdata10))] <- purrr::map_chr(colnames(tripdata10)[(length(colnames(tripdata10)) - 1):length(colnames(tripdata10))],paste0,"_E")

setwd(wddir)
Trip_tractcharacteristics <- read_dta("D:/Users/RWang/Box Sync/Inclusion&Innovation/Data Collection and Process/Trip_tractcharacteristics.dta")
Trip_Tract_keep <- c("tractce_s","EOR_s")
Trip_chaS <- Trip_tractcharacteristics[Trip_Tract_keep]
Trip_chaS <- Trip_chaS[!duplicated(Trip_chaS[,1]),]
colnames(Trip_chaS) <- gsub('_s', '', colnames(Trip_chaS))
Trip_chaS$tractce <- stringr::str_pad(as.numeric(Trip_chaS$tractce),6,side = "left",pad=0)

setwd(ACSfiledir)
ACSdata10 <- data.frame(read.csv("ACS_data_tr10_06-10.csv"))
ACSdata10 <- subset(ACSdata10,substr(ACSdata10$Geo2010, start=1, stop=2)=="DC")
ACSdata10$Geo2010 <- stringr::str_pad(as.numeric(gsub('DC Tract ', '', as.character(ACSdata10$Geo2010))) * 100,6,side = "left",pad=0)
ACSdata10$PopShareW <- with(ACSdata10,PopAloneW_2006_10/PopWithRace_2006_10)


# Trip Weighted Pre -----------------------------------------------------------------------------------------------------
keep <- c("TRACTCE_E","TRACTCE_S")
tripdata_in <- tripdata10[, (names(tripdata10) %in% keep)]
rm(keep)
tripdata_in <- tripdata_in %>% group_by(TRACTCE_S,TRACTCE_E) %>% summarise(n = n())
tripdata_in$tot_E <- ave(tripdata_in$n, tripdata_in$TRACTCE_E, FUN=sum) # tot_E: total trips ending in a given tract_E
tripdata_in$tot_S <- ave(tripdata_in$n, tripdata_in$TRACTCE_S, FUN=sum) # tot_S: total trips starting from a given tract_S

tripdata_in <-merge(x=tripdata_in,y=Trip_chaS,by.x=c("TRACTCE_S"),by.y=c("tractce"),all.x = TRUE)
colnames(tripdata_in)[length(colnames(tripdata_in))] <- purrr::map_chr(colnames(tripdata_in)[length(colnames(tripdata_in))],paste0,"_S")
tripdata_in$EOR_S[tripdata_in$EOR_S==9] <- 0
tripdata_in <-merge(x=tripdata_in,y=Trip_chaS,by.x=c("TRACTCE_E"),by.y=c("tractce"),all.x = TRUE)
colnames(tripdata_in)[length(colnames(tripdata_in))] <- purrr::map_chr(colnames(tripdata_in)[length(colnames(tripdata_in))],paste0,"_E")
tripdata_in$EOR_E[tripdata_in$EOR_E==9] <- 0
tripdata_in$Trips_osriver_E <- ifelse(tripdata_in$EOR_E!=tripdata_in$EOR_S,tripdata_in$n,0)
tripdata_in$Trips_osriver_S <- ifelse(tripdata_in$EOR_E!=tripdata_in$EOR_S,tripdata_in$n,0)
# For each ending tract, count starting trips that are from other side of river
tripdata_in$Trips_osriver_S <- ave(tripdata_in$Trips_osriver_S,tripdata_in$TRACTCE_E,FUN=sum)
tripdata_in$Trips_osriver_E <- ave(tripdata_in$Trips_osriver_E,tripdata_in$TRACTCE_S,FUN=sum)
# For each ending tract, share of starting trips from other side of river among total number of trips going into this tract, tot_E
tripdata_in$ShareTrips_osriver_E <- tripdata_in$Trips_osriver_E/tripdata_in$tot_S
tripdata_in$ShareTrips_osriver_S <- tripdata_in$Trips_osriver_S/tripdata_in$tot_E

# S characteristics should be weighted by tot_E, and vice versa
# S trips in EOR trips should be scaled by tot_S, and vice versa
keep <- c("Geo2010","unemploymentrate","PctCol","pctearningover75K","pctcostburden"
          ,"commuteunder45","violentcrimerate","PopShareW")
ACSdata10 <- ACSdata10[, (names(ACSdata10) %in% keep)]
rm(keep)

# Data Quality -----------------------------------------------------------------------------------------------------
colnames(ACSdata10)[colSums(is.na(ACSdata10)) > 0]
#000201 006202 006804 010800 Georgetown/Mall/anacostia park , Kingman island and heritage island, and Congressional cemetery/GWU
unique(ACSdata10$Geo2010[is.na(ACSdata10$unemploymentrate)]) #006202
unique(ACSdata10$Geo2010[is.na(ACSdata10$PctCol)]) #006202 
unique(ACSdata10$Geo2010[is.na(ACSdata10$pctearningover75K)]) #006202
unique(ACSdata10$Geo2010[is.na(ACSdata10$pctcostburden)]) #000201 006202 006804
unique(ACSdata10$Geo2010[is.na(ACSdata10$commuteunder45)]) #006202
unique(ACSdata10$Geo2010[is.na(ACSdata10$PopShareW)]) #006202
#tripdata_in1 <- tripdata_in1[!grepl('pctfamover75',colnames(tripdata_in1))]
#tripdata_in1 <- tripdata_in1[!grepl('pctchildabovepov',colnames(tripdata_in1))]
#rm(tripdata, ACSdata, datadir, wddir, tripdata_in, temp_tripdata,drops,tempdf)


# Missing Value in Weighting -----------------------------------------------------------------------------------------------------
# merge transport index, ignore 006202 006804 000201 when aggregating
temp_tripdata <- ACSdata10[c("Geo2010","pctcostburden")]
colnames(temp_tripdata)[1] <- "TRACTCE"
temp_tripdata <- distinct(temp_tripdata)
temp_tripdata <- temp_tripdata[complete.cases(temp_tripdata),]
#colnames(temp_tripdata) <- gsub('_S', '', colnames(temp_tripdata))
#unique(temp_tripdata$TRACTCE[is.na(temp_tripdata$pctcostburden)])

tripdata_in1 <- tripdata_in

tripdata_in1$save3_E <- 
  ifelse(tripdata_in1$TRACTCE_S %in% c("006202","006804","000201"), tripdata_in1$n,0) %>% ave(.,tripdata_in1$TRACTCE_E,FUN = sum)
tripdata_in1$save3_S <- 
  ifelse(tripdata_in1$TRACTCE_E %in% c("006202","006804","000201"), tripdata_in1$n,0) %>% ave(.,tripdata_in1$TRACTCE_S,FUN = sum)

tripdata_in1 <- merge(x=tripdata_in1,y=temp_tripdata,by.x = c("TRACTCE_S"), by.y=c("TRACTCE"),all.x = TRUE)
tripdata_in1$pctcostburden <- tripdata_in1$pctcostburden * tripdata_in1$n / (tripdata_in1$tot_E - tripdata_in1$save3_E) 
colnames(tripdata_in1)[length(colnames(tripdata_in1))] <- purrr::map_chr(colnames(tripdata_in1)[length(colnames(tripdata_in1))],paste0,"_S")
tempdf <- 
  tripdata_in1[c("TRACTCE_S","TRACTCE_E","pctcostburden_S")] %>%
  group_by(TRACTCE_E) %>% summarise(pctcostburden_S = sum(pctcostburden_S, na.rm = TRUE))
tripdata_in1 <- tripdata_in1[,-ncol(tripdata_in1)]
tripdata_in1 <- merge(x=tripdata_in1,y=tempdf,by=c("TRACTCE_E"),replace=TRUE)
tripdata_in1 <- merge(x=tripdata_in1,y=temp_tripdata,by.x = c("TRACTCE_E"), by.y=c("TRACTCE"),all.x = TRUE)
tripdata_in1$pctcostburden <- tripdata_in1$pctcostburden * tripdata_in1$n / (tripdata_in1$tot_S - tripdata_in1$save3_S) 
colnames(tripdata_in1)[length(colnames(tripdata_in1))] <- purrr::map_chr(colnames(tripdata_in1)[length(colnames(tripdata_in1))],paste0,"_E")
tempdf <- 
  tripdata_in1[c("TRACTCE_S","TRACTCE_E","pctcostburden_E")] %>%
  group_by(TRACTCE_S) %>% summarise(pctcostburden_E = sum(pctcostburden_E, na.rm = TRUE))
tripdata_in1 <- tripdata_in1[,-ncol(tripdata_in1)]
tripdata_in1 <- merge(x=tripdata_in1,y=tempdf,by=c("TRACTCE_S"),replace=TRUE)
drops <- c("save3_S","save3_E")
tripdata_in1 <- tripdata_in1[, !(names(tripdata_in1) %in% drops)]
colnames(tripdata_in1)[(length(colnames(tripdata_in1)) - 1):length(colnames(tripdata_in1))] <- purrr::map_chr(colnames(tripdata_in1)[(length(colnames(tripdata_in1)) - 1):length(colnames(tripdata_in1))],paste0,"weight")

temp_tripdata <- ACSdata10[c("Geo2010","unemploymentrate","PctCol","pctearningover75K","commuteunder45","PopShareW")]
colnames(temp_tripdata)[1] <- "TRACTCE"
temp_tripdata <- distinct(temp_tripdata)
temp_tripdata <- temp_tripdata[complete.cases(temp_tripdata),]
tripdata_in1$save1_E <- 
  ifelse(tripdata_in1$TRACTCE_S %in% c("006202"), tripdata_in1$n,0) %>% ave(.,tripdata_in1$TRACTCE_E,FUN = sum)
tripdata_in1$save1_S <- 
  ifelse(tripdata_in1$TRACTCE_E %in% c("006202"), tripdata_in1$n,0) %>% ave(.,tripdata_in1$TRACTCE_S,FUN = sum)

tripdata_in1 <- merge(x=tripdata_in1,y=temp_tripdata,by.x = c("TRACTCE_S"), by.y=c("TRACTCE"),all.x = TRUE)
tripdata_in1[(length(colnames(tripdata_in1)) - 4):length(colnames(tripdata_in1))] <-
  map(tripdata_in1[(length(colnames(tripdata_in1)) - 4):length(colnames(tripdata_in1))],
      function(x) x * tripdata_in1$n / (tripdata_in1$tot_E - tripdata_in1$save1_E))
colnames(tripdata_in1)[(length(colnames(tripdata_in1))-4):length(colnames(tripdata_in1))] <- 
  purrr::map_chr(colnames(tripdata_in1)[(length(colnames(tripdata_in1))-4):length(colnames(tripdata_in1))],paste0,"_S")
tempdf <- tripdata_in1[c("TRACTCE_S","TRACTCE_E","unemploymentrate_S","PctCol_S","pctearningover75K_S","commuteunder45_S","PopShareW_S")] %>%
  group_by(TRACTCE_E) %>% summarise(unemploymentrate_S = sum(unemploymentrate_S, na.rm = TRUE),
                                    PctCol_S = sum(PctCol_S, na.rm = TRUE),pctearningover75K_S = sum(pctearningover75K_S, na.rm = TRUE),
                                    commuteunder45_S = sum(commuteunder45_S, na.rm = TRUE),PopShareW_S = sum(PopShareW_S, na.rm = TRUE))
tripdata_in1 <- tripdata_in1[,-c((ncol(tripdata_in1)-4):ncol(tripdata_in1))]
tripdata_in1 <- merge(x=tripdata_in1,y=tempdf,by=c("TRACTCE_E"),replace=TRUE)

tripdata_in1 <- merge(x=tripdata_in1,y=temp_tripdata,by.x = c("TRACTCE_E"), by.y=c("TRACTCE"),all.x = TRUE)
tripdata_in1[(length(colnames(tripdata_in1)) - 4):length(colnames(tripdata_in1))] <-
  map(tripdata_in1[(length(colnames(tripdata_in1)) - 4):length(colnames(tripdata_in1))],
      function(x) x * tripdata_in1$n / (tripdata_in1$tot_S - tripdata_in1$save1_S))
colnames(tripdata_in1)[(length(colnames(tripdata_in1))-4):length(colnames(tripdata_in1))] <- 
  purrr::map_chr(colnames(tripdata_in1)[(length(colnames(tripdata_in1))-4):length(colnames(tripdata_in1))],paste0,"_E")
tempdf <- tripdata_in1[c("TRACTCE_S","TRACTCE_E","unemploymentrate_E","PctCol_E","pctearningover75K_E","commuteunder45_E","PopShareW_E")] %>%
  group_by(TRACTCE_S) %>% summarise(unemploymentrate_E = sum(unemploymentrate_E, na.rm = TRUE),
                                    PctCol_E = sum(PctCol_E, na.rm = TRUE),pctearningover75K_E = sum(pctearningover75K_E, na.rm = TRUE),
                                    commuteunder45_E = sum(commuteunder45_E, na.rm = TRUE),PopShareW_E = sum(PopShareW_E, na.rm = TRUE))
tripdata_in1 <- tripdata_in1[,-c((ncol(tripdata_in1)-4):ncol(tripdata_in1))]
tripdata_in1 <- merge(x=tripdata_in1,y=tempdf,by=c("TRACTCE_S"),replace=TRUE)
drops <- c("save1_S","save1_E")
tripdata_in1 <- tripdata_in1[, !(names(tripdata_in1) %in% drops)]
colnames(tripdata_in1)[(length(colnames(tripdata_in1)) - 9):length(colnames(tripdata_in1))] <- purrr::map_chr(colnames(tripdata_in1)[(length(colnames(tripdata_in1)) - 9):length(colnames(tripdata_in1))],paste0,"weight")

# Trip Weighted 1 -----------------------------------------------------------------------------------------------------
temp_tripdata <- ACSdata10[c("Geo2010","violentcrimerate")]
colnames(temp_tripdata)[1] <- "TRACTCE"
temp_tripdata <- distinct(temp_tripdata)
temp_tripdata <- temp_tripdata[complete.cases(temp_tripdata),]

# if merge by _S, then it is for average intrip features, calculate by _E
tripdata_in1 <- merge(x=tripdata_in1,y=temp_tripdata,by.x=c("TRACTCE_S"), by.y=c("TRACTCE"))
colnames(tripdata_in1)[length(colnames(tripdata_in1))] <- purrr::map_chr(colnames(tripdata_in1)[length(colnames(tripdata_in1))],paste0,"_S")
tripdata_in1[length(colnames(tripdata_in1))] <-
  map(tripdata_in1[length(colnames(tripdata_in1))],function(x) x * tripdata_in1$n / tripdata_in1$tot_E) 
tripdata_in1[length(colnames(tripdata_in1))] <-
  map(tripdata_in1[length(colnames(tripdata_in1))],function(x) ave(x,tripdata_in1$TRACTCE_E,FUN=sum))
colnames(tripdata_in1)[length(colnames(tripdata_in1))] <- purrr::map_chr(colnames(tripdata_in1)[length(colnames(tripdata_in1))],paste0,"weight")
#length(unique(tripdata_in1$TRACTCE_E))
#length(unique(tripdata_in1$PopWithRace_2006_10_Sweight))

# merge by _E, then it is for average outtrip features, calculate by _S
tripdata_in1 <- merge(x=tripdata_in1,y=temp_tripdata,by.x=c("TRACTCE_E"), by.y=c("TRACTCE"))
colnames(tripdata_in1)[length(colnames(tripdata_in1))] <- purrr::map_chr(colnames(tripdata_in1)[length(colnames(tripdata_in1))],paste0,"_E")
tripdata_in1[length(colnames(tripdata_in1))] <-
  map(tripdata_in1[length(colnames(tripdata_in1))],function(x) x * tripdata_in1$n / tripdata_in1$tot_S)
tripdata_in1[length(colnames(tripdata_in1))] <-
  map(tripdata_in1[length(colnames(tripdata_in1))],function(x) ave(x,tripdata_in1$TRACTCE_S,FUN=sum))
colnames(tripdata_in1)[length(colnames(tripdata_in1))] <- purrr::map_chr(colnames(tripdata_in1)[length(colnames(tripdata_in1))],paste0,"weight")
#length(unique(tripdata_in1$TRACTCE_S))
#length(unique(tripdata_in1$PopWithRace_2006_10_Eweight))

# Merge Tract Chracteristics for S and E -----------------------------------------------------------------------------------------------------
S_stationcount <- tripdata10[,(names(tripdata10) %in% c("TRACTCE_S","Start.station.number"))]
E_stationcount <- tripdata10[,(names(tripdata10) %in% c("TRACTCE_E","End.station.number"))]
S_stationcount$TRACTCE_S <- as.factor(S_stationcount$TRACTCE_S)
S_stationcount$Start.station.number <- as.factor(S_stationcount$Start.station.number)
E_stationcount$TRACTCE_E <- as.factor(E_stationcount$TRACTCE_E)
E_stationcount$End.station.number <- as.factor(E_stationcount$End.station.number)

S_stationcount <- S_stationcount %>% group_by(TRACTCE_S) %>% mutate(n_station = n_distinct(Start.station.number))
S_stationcount <- distinct(S_stationcount[,c("TRACTCE_S","n_station")])
E_stationcount <- E_stationcount %>% group_by(TRACTCE_E) %>% mutate(n_station = n_distinct(End.station.number))
E_stationcount <- distinct(E_stationcount[,c("TRACTCE_E","n_station")])


tripdata10_tr <- merge(x=tripdata_in1,y=ACSdata10,by.x=c("TRACTCE_S"), by.y=c("Geo2010"),all.x = TRUE)
colnames(tripdata10_tr)[(length(colnames(tripdata10_tr)) - 6):length(colnames(tripdata10_tr))] <- purrr::map_chr(colnames(tripdata10_tr)[(length(colnames(tripdata10_tr)) - 6):length(colnames(tripdata10_tr))],paste0,"_S")
tripdata10_tr <- merge(x=tripdata10_tr,y=ACSdata10,by.x=c("TRACTCE_E"), by.y=c("Geo2010"),all.x = TRUE)
colnames(tripdata10_tr)[(length(colnames(tripdata10_tr)) - 6):length(colnames(tripdata10_tr))] <- purrr::map_chr(colnames(tripdata10_tr)[(length(colnames(tripdata10_tr)) - 6):length(colnames(tripdata10_tr))],paste0,"_E")

tripdata10_tr <- merge(x=tripdata10_tr,y=S_stationcount,by=c("TRACTCE_S"))
colnames(tripdata10_tr)[length(colnames(tripdata10_tr))] <- "S_n_station"
tripdata10_tr <- merge(x=tripdata10_tr,y=E_stationcount,by=c("TRACTCE_E"))
colnames(tripdata10_tr)[length(colnames(tripdata10_tr))]  <- "E_n_station"
tripdata10_tr$n_perStation_S <- tripdata10_tr$n / tripdata10_tr$S_n_station
tripdata10_tr$n_perStation_E <- tripdata10_tr$n / tripdata10_tr$E_n_station

tripdata10 <- merge(x=tripdata10,y=tripdata10_tr,by=c("TRACTCE_S","TRACTCE_E"))

# Map Component -----------------------------------------------------------------------------------------------------
setwd("D:\\SharedMobility-BikeShare-sarah")
source("code/00_define-functions.R")

bikes10 <- get_bikeshare_data(2010) %>% geocode_bikeshare()

dc_bikeshare <- get_bikeshare_coords(clip_dc = TRUE)

topstarts <- bikes10 %>% 
  filter(membertype == "Member",
         start_stationno %in% dc_bikeshare$station_id,
         end_stationno %in% dc_bikeshare$station_id) %>% 
  group_by(start_stationno, start_address, start_lat, start_long) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  mutate(ridesperday = n/365)

topdest <- bikes10 %>% 
  filter(membertype == "Member",
         start_stationno %in% dc_bikeshare$station_id,
         end_stationno %in% dc_bikeshare$station_id) %>% 
  group_by(end_stationno, end_address, end_lat, end_long) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  mutate(ridesperday = n/365)

avgstarts <- bikes10 %>% 
  mutate(dayofweek = lubridate::wday(start_date),
         date = substr(start_date, 1, 10)) %>% 
  filter(membertype == "Member",
         start_stationno %in% dc_bikeshare$station_id,
         end_stationno %in% dc_bikeshare$station_id,
         dayofweek %in% 2:6) %>% 
  group_by(date, start_stationno, start_address, start_lat, start_long) %>% 
  count() %>% 
  arrange(desc(n))

avgdailystarts <- avgstarts %>% 
  group_by(start_stationno, start_address, start_lat, start_long) %>% 
  summarize(avgstarts = mean(n))



# Chracteristics -----------------------------------------------------------------------------------------------------
keep <- c("TRACTCE_S","total_jobs_S","total_jobs_Eweight",
          "EOR_S.y","EOR_E.y","Tot_E","Tot_S","ShareTrips_osriver_S","ShareTrips_osriver_Eweight",
          "unemploymentrate_S","unemploymentrate_Eweight","pctearningover75k_S","pctearningover75k_Eweight",
          "pctcostburden_S","pctcostburden_Eweight","commuteunder45_S","commuteunder45_Eweight",
          "violentcrimerate_S","violentcrimerate_Eweight","PctCol_S","PctCol_Eweight",
          "PopShareW_S","PopShareW_Eweight","newname","n_perStation_S")
mapdata0 <- tripdata10[, (names(tripdata10) %in% keep)]
mapdata_factory <- function(df,var) {
  byvar <- enquo(var)
  data_count <- count(df,!!byvar)
  data_aggre <- df %>% group_by(!!!byvar) %>% 
    summarise_all(funs(if(is.numeric(.)) mean(.,na.rm = TRUE) else first(.))) 
  data_output <- cbind(data_count, 
                       data_aggre[,!(names(data_aggre) %in% c(as.character(byvar)))])
  return(data_output)
}
#using adjusted per station weight
mapdata1 <- mapdata_factory(mapdata0,TRACTCE_S)
#only for tracts with bikestations
acsgeo1 <- right_join(mapdata1, ctracts, by = c("TRACTCE_S" = "TRACTCE")) 

# Clustering Analysis and Mapping - Member Commuting Hours 2010 -----------------------------------------------------------------------------------------------------
drops <- c("GEOID_E","GEOID_S","Index","End_hour","Duration")
clus_data0 <- tripdata10[, !(names(tripdata10) %in% drops)]
keep <- c("n_perStation_S","TRACTCE_S","TRACTCE_E","Member.type","DayWeek_S","Start_hour",
          "EOR_S.y","EOR_E.y","Tot_E","Tot_S","ShareTrips_osriver_S","ShareTrips_osriver_Eweight",
          "unemploymentrate_S","unemploymentrate_Eweight","pctearningover75k_S","pctearningover75k_Eweight",
          "pctcostburden_S","pctcostburden_Eweight","commuteunder45_S","commuteunder45_Eweight",
          "violentcrimerate_S","violentcrimerate_Eweight","PctCol_S","PctCol_Eweight",
          "PopShareW_S","PopShareW_Eweight","newname")
treedata_factory <- function(df) {
  data_count <- df[,(names(df) %in% c("TRACTCE_E","TRACTCE_S"))] %>% count(TRACTCE_E,TRACTCE_S)
  data_aggre <- df %>% group_by(TRACTCE_S,TRACTCE_E) %>% 
    summarise_all(funs(if(is.numeric(.)) mean(.,na.rm = TRUE) else first(.))) 
  data_output <- merge(data_count,data_aggre,by=c("TRACTCE_E","TRACTCE_S"))
  return(data_output)
}

Commute <- c(7,8,9,16,17,18,19)
input <- quote(clus_data0)
cond1 <- quote(eval(input)$Member.type=="Member")
cond2 <- quote(eval(input)$DayWeek_S<6)
cond3 <- quote(eval(input)$Start_hour %in% Commute)
clus_data0$newname <- ifelse(eval(cond1),ifelse(eval(cond2),ifelse(eval(cond3),"M_MF_Commute","M_MF_Other"),"M_Weekend"),"C_trips")

clus_data0_l <- eval(input)[,(names(eval(input)) %in% keep)] %>% split(.,list(.$newname))
clus_data0_l <- 
  map(clus_data0_l, function(x) treedata_factory(x[!(names(x) %in% c("Member.type","DayWeek_S","Start_hour"))]))
for (element in 1:length(clus_data0_l)) {assign(unique(clus_data0_l[[element]]$newname),clus_data0_l[[element]])}

clus_input <- quote(M_MF_Commute)
#keep <- c("TRACTCE_S","n_perStation_S","tcost_idx_Eweight","tcost_idx_S","unemploymentrate_S","PctCol_S","pctcostburden_S","commuteunder45_S","violentcrimerate_S","EOR_S","unemploymentrate_Eweight","PctCol_Eweight","pctcostburden_Eweight","commuteunder45_Eweight","violentcrimerate_Eweight")
keep <- c("TRACTCE_S","n_perStation_S","unemploymentrate_S","PctCol_S","pctcostburden_S","commuteunder45_S","violentcrimerate_S","EOR_S","unemploymentrate_Eweight","PctCol_Eweight","pctcostburden_Eweight","commuteunder45_Eweight","violentcrimerate_Eweight")
cluster_df0 <- na.omit(eval(clus_input)[,(names(eval(clus_input)) %in% keep)])
cluster_df <- cluster_df0[,!(names(cluster_df0)) %in% c("TRACTCE_S") ]
scale_cluster_df <- scale(cluster_df)

set.seed(826)
k.max <- 9
dataclus <- scale_cluster_df

wss <- sapply(1:k.max, 
              function(k){kmeans(dataclus, k, nstart=25,iter.max = 15 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares") 

k2 <- kmeans(dataclus, centers = 2, nstart = 25)
k3 <- kmeans(dataclus, centers = 3, nstart = 25)
k4 <- kmeans(dataclus, centers = 4, nstart = 25)
k5 <- kmeans(dataclus, centers = 5, nstart = 25)
k6 <- kmeans(dataclus, centers = 6, nstart = 25)
k7 <- kmeans(dataclus, centers = 7, nstart = 25)
k8 <- kmeans(dataclus, centers = 8, nstart = 25)
k9 <- kmeans(dataclus, centers = 9, nstart = 25)

p1 <- fviz_cluster(k2, geom = "point",  data = dataclus) + ggtitle("k = 2") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#CEDDE4", 
                                        colour = "#CEDDE4"))
p2 <- fviz_cluster(k3, geom = "point",  data = dataclus) + ggtitle("k = 3") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#CEDDE4", 
                                        colour = "#CEDDE4"))
p3 <- fviz_cluster(k4, geom = "point",  data = dataclus) + ggtitle("k = 4") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#CEDDE4", 
                                        colour = "#CEDDE4"))
p4 <- fviz_cluster(k5, geom = "point",  data = dataclus) + ggtitle("k = 5") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#CEDDE4", 
                                        colour = "#CEDDE4"))
p1
p2
p3
#p4
aggregate(cluster_df, by=list(k2$cluster),mean)
aggregate(cluster_df, by=list(k3$cluster),mean)
aggregate(cluster_df, by=list(k4$cluster),mean)

cluster_df1 <- cbind(k3$cluster, cluster_df0)
colnames(cluster_df1)[1] <- "cluster"
cluster_df1$cluster <- as.factor(cluster_df1$cluster)

cluster_df2 <- cluster_df1
#Three clusters, when including transport cost
#cluster_df2$cluster[cluster_df1$TRACTCE_S %in% c("000702","004701","007100")] <- 3
#test <- cluster_df2 %>% group_by(TRACTCE_S) %>% mutate(n_cluster = n_distinct(cluster))
#test <- test[,c("TRACTCE_S","cluster","n_cluster")]

acsgeo2 <- merge(x=acsgeo1,y=distinct(cluster_df2[,c("TRACTCE_S","cluster")]),by = c("TRACTCE_S"))

ggplot() +
  geom_sf(acsgeo2, mapping = aes(fill = cluster)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)

aggregate(cluster_df, by=list(k3$cluster),mean)


# Scatter Plot -----------------------------------------------------------------------------------------------------
keep <- c("S_n_station","n_perStation_S","n","TRACTCE_S","total_jobs_S","total_jobs_Eweight",
          "EOR_S.y","EOR_E.y","Tot_E","Tot_S","ShareTrips_osriver_S","ShareTrips_osriver_Eweight",
          "unemploymentrate_S","unemploymentrate_Eweight","pctearningover75K_S","pctearningover75K_Eweight",
          "pctcostburden_S","pctcostburden_Eweight","commuteunder45_S","commuteunder45_Eweight",
          "violentcrimerate_S","violentcrimerate_Eweight","PctCol_S","PctCol_Eweight",
          "PopShareW_S","PopShareW_Eweight","newname")
scatterdata0_10 <- tripdata10[, (names(tripdata10) %in% keep)]

data_factory <- function(df,var) {
  byvar <- enquo(var)
  data_count <- count(df,!!byvar)
  data_aggre <- df %>% group_by(!!!byvar) %>% 
    summarise_all(funs(if(is.numeric(.)) mean(.,na.rm = TRUE) else first(.))) 
  data_output <- cbind(data_count, 
                       data_aggre[,!(names(data_aggre) %in% c(as.character(byvar)))])
  return(data_output)
}

#using adjusted per station weight
scatterdata1_10 <- data_factory(scatterdata0_10,TRACTCE_S)

#
ggplot(scatterdata1_10, aes(x=unemploymentrate_S,y=unemploymentrate_Eweight)) + 
  geom_point(aes(size=n_perStation_S)) + 
  coord_fixed(ratio = 1, xlim = c(0,0.35), ylim = c(0,0.35)) +
  geom_text_repel(data=filter(scatterdata1_10, ((abs(unemploymentrate_Eweight/unemploymentrate_S)>1.6|abs(unemploymentrate_S/unemploymentrate_Eweight)>1.6))&unemploymentrate_S > 0.06 & unemploymentrate_Eweight > 0.06), 
                  aes(label=TRACTCE_S))

ggplot(scatterdata1_10, aes(x=PctCol_S,y=PctCol_Eweight)) + 
  geom_point(aes(size=n_perStation_S)) + 
  coord_fixed(ratio = 1, xlim = c(0,1), ylim = c(0,1)) + 
  geom_text_repel(data=filter(scatterdata1_10, ((abs(PctCol_Eweight/PctCol_S)>1.6|abs(PctCol_S/PctCol_Eweight)>1.6))), 
                  aes(label=TRACTCE_S))


ggplot(scatterdata1_10, aes(x=pctcostburden_S,y=pctcostburden_Eweight)) + 
  geom_point(aes(size=n_perStation_S)) + 
  coord_fixed(ratio = 1, xlim = c(0.15,0.75), ylim = c(0.15,0.75)) + 
  geom_text_repel(data=filter(scatterdata1_10, ((abs(pctcostburden_Eweight/pctcostburden_S)>1.6|abs(pctcostburden_S/pctcostburden_Eweight)>1.6))), 
                  aes(label=TRACTCE_S))

ggplot(scatterdata1_10, aes(x=commuteunder45_S,y=commuteunder45_Eweight)) + 
  geom_point(aes(size=n_perStation_S)) + 
  coord_fixed(ratio = 1, xlim = c(0,0.7), ylim = c(0,0.7)) + 
  geom_text_repel(data=filter(scatterdata1_10, ((abs(commuteunder45_Eweight/commuteunder45_S)>1.3|abs(commuteunder45_S/commuteunder45_Eweight)>1.3))), 
                  aes(label=TRACTCE_S))

#
ggplot(scatterdata1_10, aes(x=violentcrimerate_S,y=violentcrimerate_Eweight)) + 
  geom_point(aes(size=n_perStation_S))  + 
  coord_fixed(ratio = 1, xlim = c(0,100), ylim = c(0,100)) + 
  geom_text_repel(data=filter(scatterdata1_10, ((abs(violentcrimerate_Eweight/violentcrimerate_S)>2|abs(violentcrimerate_S/violentcrimerate_Eweight)>2))), 
                  aes(label=TRACTCE_S))

ggplot(scatterdata1_10, aes(x=PopShareW_S,y=PopShareW_Eweight)) + 
  geom_point(aes(size=n_perStation_S))  + 
  coord_fixed(ratio = 1, xlim = c(0,1), ylim = c(0,1)) + 
  geom_text_repel(data=filter(scatterdata1_10, ((abs(PopShareW_Eweight/PopShareW_S)>2|abs(PopShareW_S/PopShareW_Eweight)>2))), 
                  aes(label=TRACTCE_S))

ggplot(scatterdata1_10, aes(x=pctearningover75K_S,y=pctearningover75K_Eweight)) + 
  geom_point(aes(size=n_perStation_S)) + 
  coord_fixed(ratio = 1, xlim = c(0,1), ylim = c(0,1)) + 
  geom_text_repel(data=filter(scatterdata1_10, ((abs(pctearningover75K_Eweight/pctearningover75K_S)>1.6|abs(pctearningover75K_S/pctearningover75K_Eweight)>1.6))), 
                  aes(label=TRACTCE_S))  + 
  labs(title="Bike Trips and Neighborhood Income Level in 2010",
       subtitle = "Measured by Share of Residents With Income over 75,000",
       caption = "Census Tract Label added for neighborhoods deviating from 45 degree line") +
  xlab("Starting Neighborhood") + ylab("Average Ending Neighborhood") +
  scale_size_continuous(name="Bike Trips Per Station") +
  theme(legend.position = "top")

ggplot(scatterdata1_10, aes(x=pctearningover75K_S,y=pctearningover75K_Eweight)) + 
  geom_point(aes(size=n_perStation_S)) + 
  coord_fixed(ratio = 1, xlim = c(0,0.8), ylim = c(0,0.8)) + 
  geom_text_repel(data=filter(scatterdata1_10, pctearningover75K_S > 0.4 | pctearningover75K_Eweight > 0.4), 
                  aes(label=TRACTCE_S))

ggplot(scatterdata1_10, aes(x=pctearningover75K_S,y=pctearningover75K_Eweight)) + 
  geom_point(aes(size=n_perStation_S)) + 
  coord_fixed(ratio = 1, xlim = c(0,0.8), ylim = c(0,0.8)) + 
  geom_text_repel(data=filter(scatterdata1_10, TRACTCE_S %in% c("001100","000801","001001","006800","000100","000600")), 
                  aes(label=TRACTCE_S))

ggplot(scatterdata1_10, aes(x=pctearningover75K_S,y=pctearningover75K_Eweight)) + 
  geom_point(aes(size=n_perStation_S)) + 
  coord_fixed(ratio = 1, xlim = c(0,0.8), ylim = c(0,0.8)) + 
  geom_text_repel(data=filter(scatterdata1_10, TRACTCE_S %in% 
                                c("000201","008904","010800","009501","001804","007601",
                                  "007807","007401","010400","007808","007401","010400",
                                  "007808","009810","009803","007409")), 
                  aes(label=TRACTCE_S))

###   Combined   #########################
scatterdata1_ready <- scatterdata1[c("n_perStation_S","TRACTCE_S","pctearningover75K_S","pctearningover75K_Eweight")]
scatterdata1_ready$year <- 2017
scatterdata1_10_ready <- scatterdata1_10[c("n_perStation_S","TRACTCE_S","pctearningover75K_S","pctearningover75K_Eweight")]
scatterdata1_10_ready$year <- 2010
scatterdata1_10_ready <- scatterdata1_10_ready[!(scatterdata1_10_ready$TRACTCE_S=="006804"),]
scatterdata_combine <- rbind(scatterdata1_ready,scatterdata1_10_ready)
scatterdata_combine$year <- as.factor(scatterdata_combine$year)


ggplot(scatterdata_combine, aes(x=pctearningover75K_S,y=pctearningover75K_Eweight)) + 
  geom_point(aes(size=n_perStation_S,
                 color=scatterdata_combine$year)) + 
  scale_color_manual(values = c('#fdbf11','#1696d2')) + 
  scale_size_continuous(breaks = c(40,100,350,1000,2500,6000)) +
  theme(legend.title=element_blank(),
        plot.caption = element_text(hjust=0),
        panel.background = element_rect(fill = "white"),
        plot.subtitle = element_text(size = 9),
        legend.key = element_rect(colour = "transparent", fill = "white")) + 
  coord_fixed(ratio = 1, 
              xlim = c(0,0.75), 
              ylim = c(0,0.75)) + 
  labs(title="Bike Trips and Neighborhood Income Level \nin 2010 and 2017",
       subtitle = "Measured by Share of Residents With Income over $75,000",
       caption = "Census Tract Label added for neighborhoods deviating \nfrom 45 degree line") +
  xlab("Starting Neighborhood") + 
  ylab("Average Ending Neighborhood") +
  scale_x_continuous(labels=percent) + 
  scale_y_continuous(labels=percent)


toptract <- filter(scatterdata1, pctearningover75K_S>0.6 | pctearningover75K_Eweight>0.6)[,"TRACTCE_S"]
ggplot(scatterdata_combine, aes(x=pctearningover75K_S,y=pctearningover75K_Eweight)) + 
  geom_point(aes(size=n_perStation_S,
                 color=scatterdata_combine$year)) + 
  scale_color_manual(values = c('#fdbf11','#1696d2')) + 
  theme(legend.title=element_blank(),plot.caption = element_text(hjust=0),
        panel.background = element_rect(fill = "white"),
        plot.subtitle = element_text(size = 9)) + 
  coord_fixed(ratio = 1, xlim = c(0,0.75), ylim = c(0,0.75)) + 
  geom_text_repel(data=filter(scatterdata_combine, TRACTCE_S %in% toptract),aes(label=TRACTCE_S)) +
  labs(title="Bike Trips and Neighborhood Income Level \nin 2010 and 2017",
       subtitle = "Measured by Share of Residents With Income over $75,000",
       caption = "Census Tract Label added for neighborhoods deviating \nfrom 45 degree line") +
  xlab("Starting Neighborhood") + ylab("Average Ending Neighborhood") +
  scale_size_continuous(name="Trips Per Station") +
  scale_x_continuous(labels=percent) + scale_y_continuous(labels=percent)


outlier <- filter(scatterdata1, ((pctearningover75K_S/pctearningover75K_Eweight)>2 | (pctearningover75K_Eweight/pctearningover75K_S)>2))[,"TRACTCE_S"]
ggplot(scatterdata_combine, aes(x=pctearningover75K_S,y=pctearningover75K_Eweight)) + 
  geom_point(aes(size=n_perStation_S,
                 color=scatterdata_combine$year)) + 
  scale_color_manual(values = c('#fdbf11','#1696d2')) + 
  theme(legend.title=element_blank(),plot.caption = element_text(hjust=0),
        panel.background = element_rect(fill = "white"),
        plot.subtitle = element_text(size = 9)) + 
  coord_fixed(ratio = 1, xlim = c(0,0.75), ylim = c(0,0.75)) + 
  geom_text_repel(data=filter(scatterdata_combine, TRACTCE_S %in% outlier & year==2017),aes(label=TRACTCE_S)) +
  labs(title="Bike Trips and Neighborhood Income Level \nin 2010 and 2017",
       subtitle = "Measured by Share of Residents With Income over $75,000",
       caption = "Census Tract Label added for neighborhoods deviating \nfrom 45 degree line") +
  xlab("Starting Neighborhood") + ylab("Average Ending Neighborhood") +
  scale_size_continuous(name="Trips Per Station") +
  scale_x_continuous(labels=percent) + scale_y_continuous(labels=percent)



ggplot(scatterdata_combine, aes(x=pctearningover75K_S,y=pctearningover75K_Eweight)) + 
  geom_point(aes(size=n_perStation_S,
                 color=scatterdata_combine$year)) + 
  scale_color_manual(values = c('#fdbf11','#1696d2')) + 
  theme(legend.title=element_blank(),plot.caption = element_text(hjust=0),
        panel.background = element_rect(fill = "white"),
        plot.subtitle = element_text(size = 9)) + 
  coord_fixed(ratio = 1, xlim = c(0,0.75), ylim = c(0,0.75)) + 
  #geom_text_repel(data=filter(scatterdata_combine, TRACTCE_S %in% c("000202","008402")),aes(label=TRACTCE_S)) +
  labs(title="Bike Trips and Neighborhood Income Level \nin 2010 and 2017",
       subtitle = "Measured by Share of Residents With Income over $75,000",
       caption = "Census Tract Label added for neighborhoods deviating \nfrom 45 degree line") +
  xlab("Starting Neighborhood") + ylab("Average Ending Neighborhood") +
  scale_size_continuous(name="Trips Per Station") +
  scale_x_continuous(labels=percent) + scale_y_continuous(labels=percent)

scatterdata_output <- scatterdata_combine
colnames(scatterdata_output)[1] <- "trips_station"
colnames(scatterdata_output)[2] <- "census_tract"
colnames(scatterdata_output)[3] <- "pct_above75k_S"
colnames(scatterdata_output)[4] <- "pct_above75k_Eweight"

write.csv(scatterdata_output,file = "D:\\Users\\RWang\\Box Sync\\Inclusion&Innovation\\Data Collection and Process\\blog2_scatter_data.csv")

