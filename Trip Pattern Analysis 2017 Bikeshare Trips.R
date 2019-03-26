
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


# scale_fill_gradientn(labels = scales::percent) +
# theme_urbn_map() +

# Trip Data -----------------------------------------------------------------------------------------------------
wddir <- "D:\\Users\\RWang\\Box Sync\\Inclusion&Innovation\\Data Collection and Process\\Capital Bikeshare"
ACSfiledir <- "D:\\Users\\RWang\\Box Sync\\Inclusion&Innovation\\Data Collection and Process\\DC neighborhoods"
datadir <- "D:\\Users\\RWang\\Box Sync\\Inclusion&Innovation\\Data Collection and Process\\Capital Bikeshare\\2017-capitalbikeshare-tripdata"

trip_keep <- c("Index","Duration","Start.station.number","End.station.number","Member.type",
               "Start_hour","End_hour","DayWeek_S")
trans_keep <- c("tcost_idx","fips11")
tract_keep <- c("TERMINAL_N","OWNER","TRACTCE","GEOID")
Trip_Tract_keep <- c("tractce_s","tractce_e","EOR_s","EOR_e","total_jobs_s","total_jobs_e")

setwd(datadir)
tripdata <- data.frame(read.csv("Clean21Sep.csv")) #3757777 observations
setwd(ACSfiledir)
weighted_features <- data.frame(read.csv("Weighted_in&out_tract_features.csv"))
setwd(ACSfiledir)
ACSdata <- data.frame(read.csv("ACS_data_tr10.csv"))
trans_index <- data.frame(read_excel("transportationindices_AFFH.xlsx"))
setwd(wddir)
tract_info <- data.frame(read_excel("station_with_tract_info.xlsx"))
Trip_tractcharacteristics <- read_dta("D:/Users/RWang/Box Sync/Inclusion&Innovation/Data Collection and Process/Trip_tractcharacteristics.dta")

#tripdata <- data.frame(read_csv("//tsclient/D/Users/RWang/Box Sync/Inclusion&Innovation/Data Collection and Process/Capital Bikeshare/2017-capitalbikeshare-tripdata/Clean21Sep.csv")) #3757777 observations
#ACSdata <- data.frame(read_csv("//tsclient/D/Users/RWang/Box Sync/Inclusion&Innovation/Data Collection and Process/DC neighborhoods/ACS_data_tr10.csv"))
#trans_index <- data.frame(read_excel("//tsclient/D/Users/RWang/Box Sync/Inclusion&Innovation/Data Collection and Process/DC neighborhoods/transportationindices_AFFH.xlsx"))
#tract_info <- data.frame(read_excel("//tsclient/D/Users/RWang/Box Sync/Inclusion&Innovation/Data Collection and Process/Capital Bikeshare/station_with_tract_info.xlsx"))
#Trip_tractcharacteristics <- data.frame(read.csv("//tsclient/D/Users/RWang/Box Sync/Inclusion&Innovation/Data Collection and Process/Trip_tractcharacteristics.csv"))

tripdata <- tripdata[trip_keep]
trans_index <- trans_index[trans_keep]
tract_info <- tract_info[tract_keep]
Trip_tractcharacteristics <- Trip_tractcharacteristics[Trip_Tract_keep]

tripdata <- merge(x=tripdata,y=tract_info,by.x = c("Start.station.number"),by.y = c("TERMINAL_N"))
tripdata <- subset(tripdata,OWNER=="DC")
tripdata <- subset(tripdata, select= -c(OWNER))
colnames(tripdata)[(length(colnames(tripdata)) - 1):length(colnames(tripdata))] <- purrr::map_chr(colnames(tripdata)[(length(colnames(tripdata)) - 1):length(colnames(tripdata))],paste0,"_S")

tripdata <- merge(x=tripdata,y=tract_info,by.x = c("End.station.number"),by.y = c("TERMINAL_N"))
tripdata <- subset(tripdata,OWNER=="DC")
tripdata <- subset(tripdata, select= -c(OWNER))
colnames(tripdata)[(length(colnames(tripdata)) - 1):length(colnames(tripdata))] <- purrr::map_chr(colnames(tripdata)[(length(colnames(tripdata)) - 1):length(colnames(tripdata))],paste0,"_E") #3228864 observations

tripdata <- merge(x=tripdata,y=trans_index,by.x = c("GEOID_S"),by.y = c("fips11"))
colnames(tripdata)[length(colnames(tripdata))] <- paste0(colnames(tripdata)[length(colnames(tripdata))],"_S")

tripdata <- merge(x=tripdata,y=trans_index,by.x = c("GEOID_E"),by.y = c("fips11"))
colnames(tripdata)[length(colnames(tripdata))] <- paste0(colnames(tripdata)[length(colnames(tripdata))],"_E")

ACSdata$Geo2010 <- stringr::str_pad(as.numeric(gsub('DC Tract ', '', ACSdata$Geo2010)) * 100,
                                    6,side = "left",pad=0)
tripdata <- merge(x=tripdata,y=ACSdata,
                  by.x=c("TRACTCE_S"),by.y=c("Geo2010"))
colnames(tripdata)[(length(colnames(tripdata)) - 15):length(colnames(tripdata))] <- purrr::map_chr(colnames(tripdata)[(length(colnames(tripdata)) - 15):length(colnames(tripdata))],paste0,"_S")
tripdata <- merge(x=tripdata,y=ACSdata,
                  by.x=c("TRACTCE_E"),by.y=c("Geo2010"))
colnames(tripdata)[(length(colnames(tripdata)) - 15):length(colnames(tripdata))] <- purrr::map_chr(colnames(tripdata)[(length(colnames(tripdata)) - 15):length(colnames(tripdata))],paste0,"_E")

Trip_chaS <- Trip_tractcharacteristics[,c(1,3,5)]
colnames(Trip_chaS) <- gsub('_s', '', colnames(Trip_chaS))
Trip_chaS <- Trip_chaS[!duplicated(Trip_chaS[,1]),]
Trip_chaS$tractce <- stringr::str_pad(as.numeric(Trip_chaS$tractce),6,side = "left",pad=0)

tripdata <-merge(x=tripdata,y=Trip_chaS,by.x=c("TRACTCE_S"),by.y=c("tractce"))
colnames(tripdata)[(length(colnames(tripdata)) - 1):length(colnames(tripdata))] <- purrr::map_chr(colnames(tripdata)[(length(colnames(tripdata)) - 1):length(colnames(tripdata))],paste0,"_S")
tripdata <- merge(x=tripdata,y=Trip_chaS,by.x=c("TRACTCE_E"),by.y=c("tractce"))
colnames(tripdata)[(length(colnames(tripdata)) - 1):length(colnames(tripdata))] <- purrr::map_chr(colnames(tripdata)[(length(colnames(tripdata)) - 1):length(colnames(tripdata))],paste0,"_E")

weighted_features$TRACTCE_E <- stringr::str_pad(as.numeric(weighted_features$TRACTCE_E),6,side = "left",pad=0)
weighted_features$TRACTCE_S <- stringr::str_pad(as.numeric(weighted_features$TRACTCE_S),6,side = "left",pad=0)
tripdata <- merge(x=tripdata,y=weighted_features,by = c("TRACTCE_E","TRACTCE_S"))

tripdata$PopShareW_S <- with(tripdata,PopAloneW_2012_16_S/(PopAsianPINonHispBridge_2012_16_S+PopAloneB_2012_16_S+PopAloneW_2012_16_S+PopAloneH_2012_16_S))
tripdata$PopShareW_E <- with(tripdata,PopAloneW_2012_16_E/(PopAsianPINonHispBridge_2012_16_E+PopAloneB_2012_16_E+PopAloneW_2012_16_E+PopAloneH_2012_16_E))

rm(trip_keep,trans_keep,tract_keep,Trip_tractcharacteristics,tract_info, trans_index, ACSdata,Trip_chaS, weighted_features,ACSfiledir,datadir,wddir,Trip_Tract_keep)
#names(which(colSums(is.na(tripdata))>0))

# Map Component -----------------------------------------------------------------------------------------------------
setwd("D:\\SharedMobility-BikeShare-sarah")
source("code/00_define-functions.R")

bikes17 <- get_bikeshare_data(2017) %>% geocode_bikeshare()

dc_bikeshare <- get_bikeshare_coords(clip_dc = TRUE)

topstarts <- bikes17 %>% 
  filter(membertype == "Member",
         start_stationno %in% dc_bikeshare$station_id,
         end_stationno %in% dc_bikeshare$station_id) %>% 
  group_by(start_stationno, start_address, start_lat, start_long) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  mutate(ridesperday = n/365)

topdest <- bikes17 %>% 
  filter(membertype == "Member",
         start_stationno %in% dc_bikeshare$station_id,
         end_stationno %in% dc_bikeshare$station_id) %>% 
  group_by(end_stationno, end_address, end_lat, end_long) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  mutate(ridesperday = n/365)

avgstarts <- bikes17 %>% 
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

# CHARACTERISTICS -----------------------------------------------------------------------------------------------------
keep <- c("TRACTCE_S","total_jobs_S","total_jobs_Eweight",
          "EOR_S.y","EOR_E.y","Tot_E","Tot_S","ShareTrips_osriver_S","ShareTrips_osriver_Eweight",
          "unemploymentrate_S","unemploymentrate_Eweight","pctearningover75k_S","pctearningover75k_Eweight",
          "pctcostburden_S","pctcostburden_Eweight","commuteunder45_S","commuteunder45_Eweight",
          "violentcrimerate_S","violentcrimerate_Eweight","PctCol_S","PctCol_Eweight",
          "PopShareW_S","PopShareW_Eweight","tcost_idx_S","tcost_idx_Eweight","newname")
mapdata0 <- tripdata[, (names(tripdata) %in% keep)]

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

#get difference in characteristics
acsgeo1$tcost_diff <- acsgeo1$tcost_idx_Eweight - acsgeo1$tcost_idx_S
acsgeo1$unemploymentrate_diff <- acsgeo1$unemploymentrate_Eweight - acsgeo1$unemploymentrate_S
acsgeo1$PctCol_diff <- acsgeo1$PctCol_Eweight - acsgeo1$PctCol_S
acsgeo1$pctcostburden_diff <- acsgeo1$pctcostburden_Eweight - acsgeo1$pctcostburden_S
acsgeo1$commuteunder45_diff <- acsgeo1$commuteunder45_Eweight - acsgeo1$commuteunder45_S
acsgeo1$violentcrimerate_diff <- acsgeo1$violentcrimerate_Eweight - acsgeo1$violentcrimerate_S
acsgeo1$PopShareW_diff <- acsgeo1$PopShareW_Eweight - acsgeo1$PopShareW_S
acsgeo1$total_jobs_diff <- acsgeo1$total_jobs_Eweight - acsgeo1$total_jobs_S


### Overall, sending and receiving neighborhoods tend to share socio-economic characteristics
# Transport Cost - Map -----------------------------------------------------------------------------------------------------

#Transport cost index high in city center, for sending neighborhoods as well as their corresponding destinations' average; 
#however, the difference in transport cost between the two ends are small, indicating less interaction between places with different transport cost conditions
ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = tcost_idx_Eweight)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)

ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = tcost_idx_S)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)

ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = tcost_diff)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)


# Unemployment - Map -----------------------------------------------------------------------------------------------------
#Similar patterns for unemployment rate, but the high unemployment areas are mostly east of the river
#With regard to unemployment rate difference, destination averages lower than sending neighborhood, indicating a clear commute tendency (??)
ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = unemploymentrate_Eweight)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)

ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = unemploymentrate_S)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)

ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = unemploymentrate_diff)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)


# PctCol - Map -----------------------------------------------------------------------------------------------------
#Similar patterns to unemployment
#Yet we also see neighborhoods in the east of the city travelling towards places with higher share of college educated people

ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = PctCol_Eweight)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)

ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = PctCol_S)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)

ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = PctCol_diff)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)


# pctcostburden - Map -----------------------------------------------------------------------------------------------------
#Less Obvious with cost burden

ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = pctcostburden_Eweight)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)

ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = pctcostburden_S)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)

ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = pctcostburden_diff)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)


# Commute under 45 mins - Map -----------------------------------------------------------------------------------------------------
#People still travel to places with similar commuting time

ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = commuteunder45_Eweight)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)

ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = commuteunder45_S)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)

ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = commuteunder45_diff)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)



# Violent Crime Rate - Map -----------------------------------------------------------------------------------------------------
#Violent crime rate is probably proxy for something else, which is really high for city center, especially the mall and areas around

ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = violentcrimerate_Eweight)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)

ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = violentcrimerate_S)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)

ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = violentcrimerate_diff)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)



# Population White - Map -----------------------------------------------------------------------------------------------------
#It's known that segregation by race and ethnicity is patterned geographically; yet, people from white neighborhoods also travel to places with higher shares of white residents
#Can be noted that that travels from east of the river are travelling to places with higher shares of white residents; partially explained by the higher shares of travels across river
#But we also see less trips originating from east of river

ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = PopShareW_Eweight)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)

ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = PopShareW_S)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)

ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = PopShareW_diff)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)



# Total Jobs - Map -----------------------------------------------------------------------------------------------------
#jobs concentrate in city center, but from the difference measure, we do not see large shifts measured by total jobs
#due to commute not repliant completely on bikes?

ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = total_jobs_Eweight)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)

ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = total_jobs_S)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)

ggplot() +
  geom_sf(acsgeo1, mapping = aes(fill = total_jobs_diff)) +
  geom_point(data = topstarts, mapping = aes(start_long, start_lat, size = n),
             color = "#ec008b", alpha = .2) +
  scale_color_manual(values = "black",
                     guide = guide_legend()) +
  #scale_fill_gradientn(labels = scales::percent) +
  #theme_urbn_map() +
  #labs(fill = "Unemployment rate", color = NULL) +
  theme(legend.box = "vertical") +
  coord_sf(crs = 4269, datum = NA)





# Multivariate Linear Regression -----------------------------------------------------------------------------------------------------
keep <- c("TRACTCE_S","TRACTCE_E","Member.type","DayWeek_S","Start_hour",
          "EOR_S.y","EOR_E.y","Tot_E","Tot_S","ShareTrips_osriver_S","ShareTrips_osriver_Eweight",
          "unemploymentrate_S","unemploymentrate_Eweight","pctearningover75k_S","pctearningover75k_Eweight",
          "pctcostburden_S","pctcostburden_Eweight","commuteunder45_S","commuteunder45_Eweight",
          "violentcrimerate_S","violentcrimerate_Eweight","PctCol_S","PctCol_Eweight",
          "PopShareW_S","PopShareW_Eweight","tcost_idx_S","tcost_idx_Eweight")
lmdata <- tripdata[, (names(tripdata) %in% keep)]

tractdata_factory <- function(df) {
  data_count <- df[,(names(df) %in% c("TRACTCE_E","TRACTCE_S"))] %>% count(TRACTCE_E,TRACTCE_S)
  data_aggre <- df %>% group_by(TRACTCE_S,TRACTCE_E) %>% 
    summarise_all(funs(if(is.numeric(.)) mean(.,na.rm = TRUE) else first(.))) 
  data_output <- merge(data_count,data_aggre,by=c("TRACTCE_E","TRACTCE_S"))
  return(data_output)
}
lmdata1 <- tractdata_factory(lmdata)

S_stationcount <- tripdata[,(names(tripdata) %in% c("TRACTCE_S","Start.station.number"))]
E_stationcount <- tripdata[,(names(tripdata) %in% c("TRACTCE_E","End.station.number"))]
S_stationcount$TRACTCE_S <- as.factor(S_stationcount$TRACTCE_S)
S_stationcount$Start.station.number <- as.factor(S_stationcount$Start.station.number)
E_stationcount$TRACTCE_E <- as.factor(E_stationcount$TRACTCE_E)
E_stationcount$End.station.number <- as.factor(E_stationcount$End.station.number)

S_stationcount <- 
  S_stationcount %>% 
  group_by(TRACTCE_S) %>%
  mutate(n_station = n_distinct(Start.station.number))
S_stationcount <- distinct(S_stationcount[,c(1,3)])
E_stationcount <- 
  E_stationcount %>% 
  group_by(TRACTCE_E) %>%
  mutate(n_station = n_distinct(End.station.number))
E_stationcount <- distinct(E_stationcount[,c(1,3)])

lmdata1 <- merge(x=lmdata1,y=S_stationcount,by=c("TRACTCE_S"))
colnames(lmdata1)[length(colnames(lmdata1))] <- "S_n_station"
lmdata1 <- merge(x=lmdata1,y=E_stationcount,by=c("TRACTCE_E"))
colnames(lmdata1)[length(colnames(lmdata1))] <- "E_n_station"

lmdata1$n_perStation_S <- lmdata1$n / lmdata1$S_n_station
lmdata1$n_perStation_E <- lmdata1$n / lmdata1$E_n_station

fit <- lm(n_perStation_S ~ tcost_idx_S+tcost_idx_Eweight+unemploymentrate_S+unemploymentrate_Eweight+PctCol_S+PctCol_Eweight+pctcostburden_S+pctcostburden_Eweight+commuteunder45_S+commuteunder45_Eweight+violentcrimerate_S+violentcrimerate_Eweight,
          data=lmdata1)
summary(fit)
plot(fit)
###By no means a comprehensive regression model, nor does the model suggest causal relationship
###Merely suggestive of: tcost_index, college education as well as commute time for start and receiving, and pct cost burden in start


# Bottom and Top Neighborhoods and Characteristics -----------------------------------------------------------------------------------------------------
drops <- c("GEOID_E","GEOID_S","Index","End_hour","Duration")
map_data0 <- tripdata[, !(names(tripdata) %in% drops)]

keep <- c("TRACTCE_S","total_jobs_S","total_jobs_Eweight",
          "EOR_S.y","EOR_E.y","Tot_E","Tot_S","ShareTrips_osriver_S","ShareTrips_osriver_Eweight",
          "unemploymentrate_S","unemploymentrate_Eweight","pctearningover75k_S","pctearningover75k_Eweight",
          "pctcostburden_S","pctcostburden_Eweight","commuteunder45_S","commuteunder45_Eweight",
          "violentcrimerate_S","violentcrimerate_Eweight","PctCol_S","PctCol_Eweight",
          "PopShareW_S","PopShareW_Eweight","tcost_idx_S","tcost_idx_Eweight","newname")
map_data1 <- map_data0[, (names(map_data0) %in% keep)]
rankdata <- mapdata_factory(map_data1,TRACTCE_S)


# Among the 3.2m bike trips within DC undertaken in 2017, we have explored the unequal distribution of trips between stations in different parts of the city, and it is equally important to note that this inequity is rooted in the socioeconomic conditions across the city. We perform this analysis by aggregating trips for stations at areas defined as Census Tract, at which level the socioeconomic characteristics from the American Community Survey are shown.
# First, taking the number of trips from each census tract as a start:
# 10 census tracts  that have the highest number bike trips accounted for over half of all trips (52.93%) in the year, over 1.7m in total. Each census tract, as a start of journey, has over 100,000. In contrast, 10 census tracts  that have the lowest number of trips together had less than 2,000 trips, 0.06% of the full utilization of the system. The fact that nine out of these 10 areas with low capital bikeshare usage are located east of the Anacostia River is indicative of the socioeconomic patterns underlying these preliminary findings .
                                                                                                  
# 1.	National mall - Tidal Basin - White House - Capitol Hill
# 2.	Metro Center-Chinatown
# 3.	Union Station
# 4.	Farragut North - Dupont Circle South
# 5.	Franklin Square
# 6.	West of 14th St - South of S St
# 7.	West End and West Dupont Circle
# 8.	L'Enfant Plaza and Waterfront
# 9.	Foggy Bottom- South of Farragut West
# 10.	Judicial Square Metro

head(rankdata[order(rankdata$n,decreasing=F),],10)

# Second panel of the table, from lowest
# 1.	Shipley
# 2.	Between Bellevue and Congress Heights
# 3.	Bellevue
# 4.	Grant Park and Northeast Boundary (Capitol Heights Metro Station)
# 5.	Burrville
# 6.	North Deanwood
# 7.	Bellevue
# 8.	Lincoln Heights
# 9.	Benning
# 10.	Woodridge (west of river)

head(rankdata[order(rankdata$n,decreasing=T),],10)

# For the 10 neighborhoods with the largest bike trip volumes in 2017
# .	High transport cost index scores, also high weighted scores for the tracts they are connected to; more educated on both ends; more white; higher share of commuting under 45 mins
# .	Low starting and weighted ending unemployment rates; lower cost burden
# .	Importantly, very small share of trips going to the other side of the river, or in other words, going to the East of the river
# For the bottom 10 neighborhoods, 9 of which are located East of the river:
#   .	We note structural differences both in terms of the starting neighborhoods and the receiving end compared with the top 10 neighborhoods
# .	For these neighborhoods, we do note that bikes had taken riders to neighborhoods with higher share of white residents, and larger shares of trips are going to the other side of the river compared to the top 10;
# .	Yet, bike trips may be limited in terms of breaking existing structural differences across the map of DC; in terms of the other aspects of characteristics we looked at, such as transport cost, unemployment and education
# .	Based on characteristics for top and bottom 10 neighborhoods, it's not hard to see sending and receiving (average) neighborhoods have similar levels


# Stations going to the largest number of destination stations -----------------------------------------------------------------------------------------------------
keep <- c("TRACTCE_S","total_jobs_S","total_jobs_Eweight",
          "EOR_S.y","Tot_E","Tot_S","ShareTrips_osriver_S",
          "unemploymentrate_S","unemploymentrate_Eweight","pctearningover75k_S","pctearningover75k_Eweight",
          "pctcostburden_S","pctcostburden_Eweight","commuteunder45_S","commuteunder45_Eweight",
          "violentcrimerate_S","violentcrimerate_Eweight","PctCol_S","PctCol_Eweight",
          "PopShareW_S","PopShareW_Eweight","tcost_idx_S","tcost_idx_Eweight")

#For bar charts that goes with maps showing top stations and bottom stations
## Station 31623, Tract 010600
unique(mapdata1[mapdata1$TRACTCE_S==unique(tripdata[(tripdata$Start.station.number==31623),"TRACTCE_S"]),(names(mapdata1) %in% keep)])
## Station 31101, Tract 004300
unique(mapdata1[mapdata1$TRACTCE_S==unique(tripdata[(tripdata$Start.station.number==31101),"TRACTCE_S"]),(names(mapdata1) %in% keep)])
## Station 31603, Tract 010600
unique(mapdata1[mapdata1$TRACTCE_S==unique(tripdata[(tripdata$Start.station.number==31603),"TRACTCE_S"]),(names(mapdata1) %in% keep)])
## Station 31200, Tract 005500
unique(mapdata1[mapdata1$TRACTCE_S==unique(tripdata[(tripdata$Start.station.number==31200),"TRACTCE_S"]),(names(mapdata1) %in% keep)])
## Station 31232, Tract 005800
unique(mapdata1[mapdata1$TRACTCE_S==unique(tripdata[(tripdata$Start.station.number==31232),"TRACTCE_S"]),(names(mapdata1) %in% keep)])
## Station 31109, Tract 004400
unique(mapdata1[mapdata1$TRACTCE_S==unique(tripdata[(tripdata$Start.station.number==31109),"TRACTCE_S"]),(names(mapdata1) %in% keep)])
## Station 31201, Tract 005201
unique(mapdata1[mapdata1$TRACTCE_S==unique(tripdata[(tripdata$Start.station.number==31201),"TRACTCE_S"]),(names(mapdata1) %in% keep)])
## Station 31203, Tract 005201
unique(mapdata1[mapdata1$TRACTCE_S==unique(tripdata[(tripdata$Start.station.number==31203),"TRACTCE_S"]),(names(mapdata1) %in% keep)])
## Station 31102, Tract 003000
unique(mapdata1[mapdata1$TRACTCE_S==unique(tripdata[(tripdata$Start.station.number==31102),"TRACTCE_S"]),(names(mapdata1) %in% keep)])
## Station 31213, Tract 010700
unique(mapdata1[mapdata1$TRACTCE_S==unique(tripdata[(tripdata$Start.station.number==31213),"TRACTCE_S"]),(names(mapdata1) %in% keep)])







# Clustering Analysis and Mapping - Member Commuting Hours -----------------------------------------------------------------------------------------------------
S_stationcount <- tripdata[,(names(tripdata) %in% c("TRACTCE_S","Start.station.number"))]
E_stationcount <- tripdata[,(names(tripdata) %in% c("TRACTCE_E","End.station.number"))]
S_stationcount$TRACTCE_S <- as.factor(S_stationcount$TRACTCE_S)
S_stationcount$Start.station.number <- as.factor(S_stationcount$Start.station.number)
E_stationcount$TRACTCE_E <- as.factor(E_stationcount$TRACTCE_E)
E_stationcount$End.station.number <- as.factor(E_stationcount$End.station.number)

S_stationcount <- 
  S_stationcount %>% 
  group_by(TRACTCE_S) %>%
  mutate(n_station = n_distinct(Start.station.number))
S_stationcount <- distinct(S_stationcount[,c(1,3)])
E_stationcount <- 
  E_stationcount %>% 
  group_by(TRACTCE_E) %>%
  mutate(n_station = n_distinct(End.station.number))
E_stationcount <- distinct(E_stationcount[,c(1,3)])

tripdata1 <- merge(x=tripdata,y=S_stationcount,by=c("TRACTCE_S"))
colnames(tripdata1)[length(colnames(tripdata1))] <- "S_n_station"
tripdata1 <- merge(x=tripdata1,y=E_stationcount,by=c("TRACTCE_E"))
colnames(tripdata1)[length(colnames(tripdata1))]  <- "E_n_station"
tripdata1$n_perStation_S <- tripdata1$n / tripdata1$S_n_station
tripdata1$n_perStation_E <- tripdata1$n / tripdata1$E_n_station

drops <- c("GEOID_E","GEOID_S","Index","End_hour","Duration")
clus_data0 <- tripdata1[, !(names(tripdata1) %in% drops)]
keep <- c("n_perStation_S","TRACTCE_S","TRACTCE_E","Member.type","DayWeek_S","Start_hour",
          "EOR_S.y","EOR_E.y","Tot_E","Tot_S","ShareTrips_osriver_S","ShareTrips_osriver_Eweight",
          "unemploymentrate_S","unemploymentrate_Eweight","pctearningover75k_S","pctearningover75k_Eweight",
          "pctcostburden_S","pctcostburden_Eweight","commuteunder45_S","commuteunder45_Eweight",
          "violentcrimerate_S","violentcrimerate_Eweight","PctCol_S","PctCol_Eweight",
          "PopShareW_S","PopShareW_Eweight","tcost_idx_S","tcost_idx_Eweight","newname")

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
clus_data0$newname <- ifelse(eval(cond1),ifelse(eval(cond2),ifelse(eval(cond3),"M_MF_Commute","M_MF_Other"),
                                                "M_Weekend"),"C_trips")

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

cluster_df1 <- cbind(k2$cluster, cluster_df0)
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

# Three clusters were identified
# 1. High Utilisation (Per Station Trips): city center, On both ends of Trips - high transport cost index, low unemployment, High College Educated, High share of commuting under 45 mins, low pct cost burden; yet travelling from low crime to high crime 
# 2. Low Utilisation: East and outer suburb Northeast, on both ends - low transport cost index, high unemployment, low college educated, high cost burden, low share of commuting under 45 mins; travelling from high crime to low crime rates areas
# 3. Median Utilisation: Close to city center and North/Northwest, on both ends - high transport cost index, relatively low unemployment and cost burden; travelling from low to high college area, from low to median level commuting under 45mins, from low to relatively low crime rates areas
# Results suggestive of segregated travel pattern, using multivariate clustering analysis


# Clustering Analysis and Mapping - Member Non Commuting Hours -----------------------------------------------------------------------------------------------------

clus_input <- quote(M_MF_Other)
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

cluster_df1 <- cbind(k2$cluster, cluster_df0)
colnames(cluster_df1)[1] <- "cluster"
cluster_df1$cluster <- as.factor(cluster_df1$cluster)

cluster_df2 <- cluster_df1
#Four Clusters
#cluster_df2$cluster[cluster_df1$TRACTCE_S %in% c("002702","007100")] <- 2
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


# Scatter Plot -----------------------------------------------------------------------------------------------------
keep <- c("n","TRACTCE_S","total_jobs_S","total_jobs_Eweight",
          "EOR_S.y","EOR_E.y","Tot_E","Tot_S","ShareTrips_osriver_S","ShareTrips_osriver_Eweight",
          "unemploymentrate_S","unemploymentrate_Eweight","pctearningover75K_S","pctearningover75K_Eweight",
          "pctcostburden_S","pctcostburden_Eweight","commuteunder45_S","commuteunder45_Eweight",
          "violentcrimerate_S","violentcrimerate_Eweight","PctCol_S","PctCol_Eweight",
          "PopShareW_S","PopShareW_Eweight","tcost_idx_S","tcost_idx_Eweight","newname")
scatterdata0 <- tripdata[, (names(tripdata) %in% keep)]


S_stationcount <- tripdata[,(names(tripdata) %in% c("TRACTCE_S","Start.station.number"))]
E_stationcount <- tripdata[,(names(tripdata) %in% c("TRACTCE_E","End.station.number"))]
S_stationcount$TRACTCE_S <- as.factor(S_stationcount$TRACTCE_S)
S_stationcount$Start.station.number <- as.factor(S_stationcount$Start.station.number)
E_stationcount$TRACTCE_E <- as.factor(E_stationcount$TRACTCE_E)
E_stationcount$End.station.number <- as.factor(E_stationcount$End.station.number)

S_stationcount <- 
  S_stationcount %>% 
  group_by(TRACTCE_S) %>%
  mutate(n_station = n_distinct(Start.station.number))
S_stationcount <- distinct(S_stationcount[,c(1,3)])
E_stationcount <- 
  E_stationcount %>% 
  group_by(TRACTCE_E) %>%
  mutate(n_station = n_distinct(End.station.number))
E_stationcount <- distinct(E_stationcount[,c(1,3)])

scatterdata0 <- merge(x=scatterdata0,y=S_stationcount,by=c("TRACTCE_S"))
colnames(scatterdata0)[length(colnames(scatterdata0))] <- "S_n_station"
scatterdata0$n_perStation_S <- scatterdata0$n / scatterdata0$S_n_station

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
scatterdata1 <- data_factory(scatterdata0,TRACTCE_S)

ggplot(scatterdata1, aes(x=tcost_idx_S,y=tcost_idx_Eweight)) + 
  geom_point(aes(size=n_perStation_S)) + 
  coord_fixed(ratio = 1, xlim = c(91,99), ylim = c(91,99)) +
  geom_text_repel(data=filter(scatterdata1, (abs(tcost_idx_Eweight/tcost_idx_S)>1.02|abs(tcost_idx_S/tcost_idx_Eweight)>1.02)), 
                  aes(label=TRACTCE_S))

#
ggplot(scatterdata1, aes(x=unemploymentrate_S,y=unemploymentrate_Eweight)) + 
  geom_point(aes(size=n_perStation_S)) + 
  coord_fixed(ratio = 1, xlim = c(0,0.5), ylim = c(0,0.3)) + 
  geom_text_repel(data=filter(scatterdata1, 
                              ((abs(unemploymentrate_Eweight/unemploymentrate_S)>1.6|abs(unemploymentrate_S/unemploymentrate_Eweight)>1.6)) & unemploymentrate_S > 0.1  ), 
                  aes(label=TRACTCE_S))

ggplot(scatterdata1, aes(x=PctCol_S,y=PctCol_Eweight)) + 
  geom_point(aes(size=n_perStation_S)) + 
  coord_fixed(ratio = 1, xlim = c(0,1), ylim = c(0,1)) +
  geom_text_repel(data=filter(scatterdata1, 
                            ((abs(PctCol_Eweight/PctCol_S)>1.6|abs(PctCol_S/PctCol_Eweight)>1.6)) & PctCol_Eweight > 0.58), 
                aes(label=TRACTCE_S)) +
  labs(title="Bike Trips and Neighborhood Education Level in 2017",
       subtitle = "Measured by Share of Residents College Education",
       caption = "Census Tract Label added for neighborhoods deviating from 45 degree line") +
  xlab("Starting Neighborhood") + ylab("Average Ending Neighborhood") +
  scale_size_continuous(name="Bike Trips Per Station") +
  theme(legend.position = "top")



ggplot(scatterdata1, aes(x=pctcostburden_S,y=pctcostburden_Eweight)) + 
  geom_point(aes(size=n_perStation_S)) + 
  coord_fixed(ratio = 1, xlim = c(0,0.75), ylim = c(0,0.75)) + 
  geom_text_repel(data=filter(scatterdata1, 
                            ((abs(pctcostburden_Eweight/pctcostburden_S)>1.5|abs(pctcostburden_S/pctcostburden_Eweight)>1.5))), 
                aes(label=TRACTCE_S))

ggplot(scatterdata1, aes(x=commuteunder45_S,y=commuteunder45_Eweight)) + 
  geom_point(aes(size=n_perStation_S)) + 
  coord_fixed(ratio = 1, xlim = c(0,0.7), ylim = c(0,0.7)) + 
  geom_text_repel(data=filter(scatterdata1, 
                              ((abs(commuteunder45_Eweight/commuteunder45_S)>1.35|abs(commuteunder45_S/commuteunder45_Eweight)>1.35))), 
                  aes(label=TRACTCE_S))

#
ggplot(scatterdata1, aes(x=violentcrimerate_S,y=violentcrimerate_Eweight)) + 
  geom_point(aes(size=n_perStation_S))  + 
  coord_fixed(ratio = 1, xlim = c(0,150), ylim = c(0,150)) + 
  geom_text_repel(data=filter(scatterdata1, 
                              ((abs(violentcrimerate_Eweight/violentcrimerate_S)>1.8|abs(violentcrimerate_S/violentcrimerate_Eweight)>1.8))), 
                  aes(label=TRACTCE_S))

ggplot(scatterdata1, aes(x=PopShareW_S,y=PopShareW_Eweight)) + 
  geom_point(aes(size=n_perStation_S))  + 
  coord_fixed(ratio = 1, xlim = c(0,1), ylim = c(0,1)) + 
  geom_text_repel(data=filter(scatterdata1, 
                              ((abs(PopShareW_Eweight/PopShareW_S)>2|abs(PopShareW_S/PopShareW_Eweight)>2)) & PopShareW_Eweight>0.1 & PopShareW_S > 0.1), 
                  aes(label=TRACTCE_S)) +
  labs(title="Bike Trips and Neighborhood Demographics in 2017",
       subtitle = "Measured by Share of White Residents",
       caption = "Census Tract Label added for neighborhoods deviating from 45 degree line") +
  xlab("Starting Neighborhood") + ylab("Average Ending Neighborhood") +
  scale_size_continuous(name="Bike Trips Per Station") +
  theme(legend.position = "top")


ggplot(scatterdata1, aes(x=pctearningover75K_S,y=pctearningover75K_Eweight)) + 
  geom_point(aes(size=n_perStation_S)) + 
  coord_fixed(ratio = 1, xlim = c(0,0.8), ylim = c(0,0.8)) + 
  geom_text_repel(data=filter(scatterdata1, 
                              ((abs(pctearningover75K_Eweight/pctearningover75K_S)>1.6|abs(pctearningover75K_S/pctearningover75K_Eweight)>1.6))), 
                  aes(label=TRACTCE_S)) +
  labs(title="Bike Trips and Neighborhood Income Level in 2017",
       subtitle = "Measured by Share of Residents With Income over 75,000",
       caption = "Census Tract Label added for neighborhoods deviating from 45 degree line") +
  xlab("Starting Neighborhood") + ylab("Average Ending Neighborhood") +
  scale_size_continuous(name="Bike Trips Per Station") +
  theme(legend.position = "top")
 
#Top places
ggplot(scatterdata1, aes(x=pctearningover75K_S,y=pctearningover75K_Eweight)) + 
  geom_point(aes(size=n_perStation_S)) + 
  coord_fixed(ratio = 1, xlim = c(0,0.8), ylim = c(0,0.8)) + 
  geom_text_repel(data=filter(scatterdata1, pctearningover75K_S>0.6 | pctearningover75K_Eweight>0.6), 
                  aes(label=TRACTCE_S))

#Bottom places
ggplot(scatterdata1, aes(x=pctearningover75K_S,y=pctearningover75K_Eweight)) + 
  geom_point(aes(size=n_perStation_S)) + 
  coord_fixed(ratio = 1, xlim = c(0,0.8), ylim = c(0,0.8)) + 
  geom_text_repel(data=filter(scatterdata1, pctearningover75K_S<0.12 | pctearningover75K_Eweight<0.12), 
                  aes(label=TRACTCE_S))

#Union
ggplot(scatterdata1, aes(x=pctearningover75K_S,y=pctearningover75K_Eweight)) + 
  geom_point(aes(size=n_perStation_S)) + 
  coord_fixed(ratio = 1, xlim = c(0,0.8), ylim = c(0,0.8)) + 
  geom_text_repel(data=filter(scatterdata1, TRACTCE_S=="010600"), 
                  aes(label=TRACTCE_S))
