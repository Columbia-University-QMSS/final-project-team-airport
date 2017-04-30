#################
# Airline Project
# Map Visualizations
# 06/27/2017
#################
  
 
library(tidyverse)
routes <- read.csv(".../.../data/processed/airline_12.csv")
head(routes)


data1 <- routes[,c("UNIQUE_CARRIER", "ORIGIN", "ORIGIN_CITY_NAME", "DEST", "DEST_CITY_NAME","CANCELLED", "ARR_DELAY")]
data1 <- data1 %>%
  group_by(ORIGIN, DEST) %>%
  summarise(Arriving_routes = n()) 


library(ggmap)
library(RSQLite)
setwd("~/Desktop/project")

# Read data directly from URLs
airport <- read.csv(".../.../data/raw/airports.csv", header = F)
airport_US <- filter(airport, V4 == "United States")

# Remove the airports without IATA codes and rename the variables
airport_US <- airport_US[airport_US$V5!='', c('V3', 'V4', 'V5','V7','V8','V9')]
colnames(airport_US) <- c("City", "Country", "IATA", "lantitude", "longitude", "altitude")
colnames(data1) <- c("Departure", "Arrival","Arriving_routes")


library(leaflet)
colnames(airport_US)[3] <- "DEST"
data2 <- routes[,c("UNIQUE_CARRIER", "ORIGIN", "ORIGIN_CITY_NAME", "DEST", "DEST_CITY_NAME","CANCELLED", "ARR_DELAY")]
data2 <- data2 %>%
  group_by(DEST) %>%
  summarise(Arriving_routes = n())
data2 <- merge(x = airport_US, y = data2, by = "DEST", all.x = TRUE)

summary(data2$Arriving_routes)

data2 <- mutate(data2, size = ifelse(data2$Arriving_routes <= 31, 31, ifelse(data2$Arriving_routes >= 162, 162, 300)))
data2.2 <- routes %>%
  group_by(DEST, UNIQUE_CARRIER) %>%
  summarize(countn = n(),
            mean_arrdelay = round(mean(ARR_DELAY,na.rm=T),2)) 
data2.2 <- merge(x = data2.2, y =airport_US, by = "DEST", all.x = TRUE)


content1 <- paste("Airport:",data2$DEST,"<br/>",
                  "City:", data2$City,"<br/>",
                  "flights:", data2$Arriving_routes, "<br/>")
content2 <- paste("Airport:", data2.2$DEST, "<br/>",
                  "City:", data2.2$City, "<br/>",
                  "Airlin:", data2.2$UNIQUE_CARRIER,"<br/>",
                  "Delay:", data2.2$mean_arrdelay,"mm", "<br/>")
m1 <- leaflet() %>% addTiles() %>%
  setView(lng = -95.71289, lat = 37.09024, zoom =5) %>%
  addTiles(group = "Background", 'http://{s}.basemaps.cartocdn.com/dark_nolabels/{z}/{x}/{y}.png') %>%
  addCircles(group = "Flight Amount", data = data2, lng = ~longitude, lat = ~lantitude, radius = ~Arriving_routes*10, fillOpacity=0.7, col="orange", popup = content1) %>%
  addCircles(group = "DL Flight Delay", data = filter(data2.2, UNIQUE_CARRIER == "DL"), lng = ~longitude, lat = ~lantitude, radius = ~mean_arrdelay*1000, color = "red", fillOpacity=0.7, popup = content2)%>%
  addCircles(group = "AA Flight Delay", data = filter(data2.2, UNIQUE_CARRIER == "AA"), lng = ~longitude, lat = ~lantitude, radius = ~mean_arrdelay*1000, color = "blue", fillOpacity=0.7, popup = content2)%>%
  addCircles(group = "UA Flight Delay", data = filter(data2.2, UNIQUE_CARRIER == "UA"), lng = ~longitude, lat = ~lantitude, radius = ~mean_arrdelay*1000, color = "green", fillOpacity=0.7, popup = content2)%>%
  addLayersControl(
    baseGroups = c("Background"),
    overlayGroups = c( "Flight Amount", "DL Flight Delay","AA Flight Delay","UA Flight Delay" ),
    options = layersControlOptions(collapsed = FALSE) )  %>%
  addEasyButton(easyButton(
    icon="fa-globe", title="Zoom to Entire World Map",
    onClick=JS("function(btn, map){ map.setZoom(1.5); }"))) %>%
  addEasyButton(easyButton(
    icon="fa-crosshairs", title="Locate Me",
    onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) 
m1
