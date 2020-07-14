

library(leaflet)
library(sp)
library(mapproj)
library(maps)
library(mapdata)
library(maptools)
library(htmlwidgets)
library(magrittr)
library(XML)
library(plyr)
library(rgdal)
library(WDI)
library(raster)
library(noncensus)
library(stringr)
library(tidyr)
library(tigris)
library(rgeos)
library(ggplot2)
library(scales)


Fortune_500 <- read_csv("Documents/CDocuments/Georgetown/ANLY500/Week 2/CF_HW2_Clean_Fortune_500_Corporate_Headquarters_Data")
popup_F500 <- paste0("<strong>Company Name: </strong>", Fortune_500$name, "<br><strong>Fortune 500 List Rank: </strong>", Fortune_500$rank, "<br><strong>Link: </strong>", Fortune_500$website)
F500_icons <- makeIcon(iconUrl = "https://bt-wpstatic.freetls.fastly.net/wp-content/blogs.dir/9/files/2013/02/Fortune_500.png", iconWidth = 30, iconHeight = 30)
CancerRates <- read_csv("Documents/CDocuments/Georgetown/ANLY500/Week 2/CF_Homework_2_Submission/CancerCountyFIPS.csv")
names(CancerRates) <- tolower(names(CancerRates))
colnames(CancerRates) <- c("location", "GEOID", "rate")
CancerRates$GEOID <- formatC(CancerRates$GEOID, width = 5, format = "d", flag = "0")
CancerRates <- separate(CancerRates, location, into = c("county", "state"), sep = ", ")
CancerRates[] <- lapply(CancerRates, function(x) gsub("\\s*\\([^\\)]+\\)", "", x))
CancerRates$state <- state.abb[match(CancerRates$state,state.name)]
CancerRates$rate <- as.numeric(as.character(CancerRates$rate))
us.map <- tigris::counties(cb = TRUE, year = 2015)
us.map <- us.map[!us.map$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69",
                                        "64", "68", "70", "74"),]
us.map <- us.map[!us.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                        "95", "79"),]

cancermap <- merge(us.map, CancerRates, by=c("GEOID"))


pal <- colorQuantile("YlOrRd", NULL, n = 9)
gmap <- leaflet(data = cancermap) %>%
  addTiles() %>%
  setView(lng = -105, lat = 40, zoom = 4) %>% 
  addMarkers(data=Fortune_500,lat=~lat, lng=~lng, icon = F500_icons, popup=popup_F500, group = "Fortune 500 Corporate Headquarters") %>%
  addLayersControl(
    
    overlayGroups = c("Fortune 500 Corporate Headquaters"),
    options = layersControlOptions(collapsed = FALSE)
  )
gmap

Major_Sport_Venues <- read_csv("Documents/CDocuments/Georgetown/ANLY500/Week 2/CF_Homework_2_Submission/Major_Sport_Venues.csv")
popup_MSV <- paste0("<strong>Stadium Name: </strong>", Major_Sport_Venues$name, "<br> <strong>Stadium Type: </strong>", Major_Sport_Venues$stadium_type, "<br><strong>Link: </strong>", Major_Sport_Venues$url)
stadium_icons <- makeIcon(iconUrl = "http://individual.icons-land.com/IconsPreview/POI/PNG/Plain/128x128/Stadium_Plain_Green.png", iconWidth = 30, iconHeight = 30)

gmap2 <- leaflet(data = cancermap) %>%
  addTiles() %>%
  setView(lng = -105, lat = 40, zoom = 4) %>% 
  addMarkers(data=Major_Sport_Venues,lat=~LATITUDE, lng=~LONGITUDE, icon = stadium_icons, popup=popup_MSV, group = "Major Sports Venues") %>%
  addLayersControl(
    
    overlayGroups = c("Major Sports Venues"),
    options = layersControlOptions(collapsed = FALSE)
  )
gmap2

Colleges_and_Universities <- read.csv("Documents/CDocuments/Georgetown/ANLY503/Colleges_and_Universities.csv")
Colleges_and_Universities_popup <- paste0("<strong>College Name Name: </strong>", Colleges_and_Universities$NAME, "<br> <strong>Total Enrollment: </strong>", Colleges_and_Universities$TOT_ENROLL, "<br><strong>Link: </strong>", Colleges_and_Universities$WEBSITE)
Colleges_and_Universities_icon <- makeIcon(iconUrl = "https://cdn1.iconfinder.com/data/icons/education-e-learning-1/128/UNIVERSITY-512.png", iconWidth = 30, iconHeight = 30)

gmap3 <- leaflet(data = cancermap) %>%
  addTiles() %>%
  setView(lng = -105, lat = 40, zoom = 4) %>% 
  addMarkers(data=Colleges_and_Universities,lat=~LATITUDE, lng=~LONGITUDE, icon = Colleges_and_Universities_icon, popup=popup_MSV, group = "Major Sports Venues") %>%
  addLayersControl(
    
    overlayGroups = c("Colleges and Universities"),
    options = layersControlOptions(collapsed = FALSE)
  )
gmap3
