#Data Table

stopifnot(require(readr))
data <- read_csv("C:/Users/bhqiz/Desktop/QMSS/Data Visualization/Final Project/Final/airline_12.csv")

stopifnot(require(dplyr))
stopifnot(require(DT))

## Exclude div flights
data_nodiv <- filter(data, DIV_AIRPORT_LANDINGS == 0)
## Remove NAs from dataset
data_cleaned <- filter(data_nodiv, is.na(data_nodiv$ARR_DELAY) == 0)
## Combine flight number with carrier
data_cleaned$unique_flight <- with(data_cleaned, paste0(UNIQUE_CARRIER, FL_NUM))


##I originally thought that I only need to calculate by airport, but later realized that 
##it is important to differentiate departure delay and arrival delay... so I have to calculate arrival       and departure airport seperately.
##sum_byairline <- group_by(data_cleaned, UNIQUE_CARRIER) %>%
##  summarise(avg_dep_delay = round(mean(DEP_DELAY), 0), avg_arr_delay = round(mean(ARR_DELAY), 0), 
##            count = n())
##colnames(sum_byairline) <- c("Carrier", "Average Departure Delay", "Average Arrival Delay", 
##                             "Count of Flights")


sum_byairport1 <- group_by(data_cleaned, ORIGIN) %>%
  summarise(avg_dep_delay = round(mean(DEP_DELAY), 0), avg_taxi_out = round(mean(TAXI_OUT), 0), 
            count = n())
colnames(sum_byairport1) <- c("Airport", "Average Departure Delay", "Average Taxi Out Time", 
                              "Count of Flights")
sum_byairport2 <- group_by(data_cleaned, DEST) %>%
  summarise(avg_arr_delay = round(mean(ARR_DELAY), 0), avg_taxi_in = round(mean(TAXI_IN), 0))
colnames(sum_byairport2) <- c("Airport", "Average Arrival Delay", "Average Taxi In Time")
## Merge two dataset to show more info
sum_byairport <- merge(sum_byairport1, sum_byairport2, by = "Airport")



sum_byflightnum <- group_by(data_cleaned, unique_flight) %>%
  summarise(avg_dep_delay = round(mean(DEP_DELAY), 0), avg_arr_delay = round(mean(ARR_DELAY), 0),
            avg_speed = round(sum(DISTANCE) / (sum(AIR_TIME) / 60), 0), 
            count = n()) ## Convert minutes to hours
## Add information about departure and arrival airpot
temp_data <- data_cleaned[!duplicated(data_cleaned[ , 34]), c(6, 9, 34)]
sum_byflightnum <- merge(sum_byflightnum, temp_data, by = "unique_flight")
colnames(sum_byflightnum) <- c("Flight Number", "Average Departure Delay", "Average Arrival Delay",
                               "Average in-air Speed", "Count of Flights", "Departure Airport", "Arrival Airport")


datatable(sum_byairport)

datatable(sum_byflightnum, filter = 'top')

#Map
library(tidyverse)
routes <- airline_12

data1 <- routes[,c("UNIQUE_CARRIER", "ORIGIN", "ORIGIN_CITY_NAME", "DEST", "DEST_CITY_NAME","CANCELLED", "ARR_DELAY")]
data1 <- data1 %>%
  group_by(ORIGIN, DEST) %>%
  summarise(Arriving_routes = n()) 

library(ggmap)
# Read data directly from URLs
airport <- read.csv("C:/Users/bhqiz/Desktop/QMSS/Data Visualization/Final Project/Final/airports.csv", header = F)
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


data2 <- mutate(data2, size = ifelse(data2$Arriving_routes <= 31, 31, ifelse(data2$Arriving_routes >= 162, 162, 300)))
data2.2 <- routes %>%
  group_by(DEST, UNIQUE_CARRIER) %>%
  summarize(countn = n(),
            mean_arrdelay = round(mean(ARR_DELAY,na.rm=T),2)) 
data2.2 <- merge(x = data2.2, y =airport_US, by = "DEST", all.x = TRUE)

content1 <- paste("Airport:",data2$DEST,"<br/>",
                  "City:", data2$City,"<br/>",
                  "flights:", data2$Arriving_routes, "<br/>")
data2.2_DL <- filter(data2.2, UNIQUE_CARRIER == "DL")
data2.2_AA <- filter(data2.2, UNIQUE_CARRIER == "AA")
data2.2_UA <- filter(data2.2, UNIQUE_CARRIER == "UA")
content2_DL <- paste("Airport:", data2.2_DL$DEST, "<br/>",
                     "City:", data2.2_DL$City, "<br/>",
                     "Airline:", data2.2_DL$UNIQUE_CARRIER,"<br/>",
                     "Delay:", data2.2_DL$mean_arrdelay,"mm", "<br/>")
content2_AA <- paste("Airport:", data2.2_AA$DEST, "<br/>",
                     "City:", data2.2_AA$City, "<br/>",
                     "Airline:", data2.2_AA$UNIQUE_CARRIER,"<br/>",
                     "Delay:", data2.2_AA$mean_arrdelay,"mm", "<br/>")
content2_UA <- paste("Airport:", data2.2_UA$DEST, "<br/>",
                     "City:", data2.2_UA$City, "<br/>",
                     "Airline:", data2.2_UA$UNIQUE_CARRIER,"<br/>",
                     "Delay:", data2.2_UA$mean_arrdelay,"mm", "<br/>")
m1 <- leaflet() %>% addTiles() %>%
  setView(lng = -95.71289, lat = 37.09024, zoom =5) %>%
  addTiles(group = "Background", 'http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png') %>%
  addCircles(group = "Flight Amount", data = data2, lng = ~longitude, lat = ~lantitude, radius = ~Arriving_routes*10, fillOpacity=0.7, col="orange", popup = content1) %>%
  addCircles(group = "AA Flight Delay", data = data2.2_AA, lng = ~longitude, lat = ~lantitude, radius = ~mean_arrdelay*1000, color = "red", fillOpacity=0.7, popup = content2_AA)%>%
  addCircles(group = "DL Flight Delay", data = data2.2_DL, lng = ~longitude, lat = ~lantitude, radius = ~mean_arrdelay*1000, color = "green", fillOpacity=0.7, popup = content2_DL)%>%
  addCircles(group = "UA Flight Delay", data = data2.2_UA, lng = ~longitude, lat = ~lantitude, radius = ~mean_arrdelay*1000, color = "blue", fillOpacity=0.7, popup = content2_UA)%>%
  addLayersControl(
    baseGroups = c("Background"),
    overlayGroups = c( "Flight Amount", "AA Flight Delay","DL Flight Delay","UA Flight Delay"),
    options = layersControlOptions(collapsed = FALSE) )  %>%
  addEasyButton(easyButton(
    icon="fa-globe", title="Zoom to Entire World Map",
    onClick=JS("function(btn, map){ map.setZoom(1.5); }"))) %>%
  addEasyButton(easyButton(
    icon="fa-crosshairs", title="Locate Me",
    onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) 
m1

#ggplot
##Rating Analysis

library(tidyverse)
library(readxl)
Rating <- read_excel("C:/Users/bhqiz/Desktop/QMSS/Data Visualization/Final Project/Final/Airlines_Rating.xlsx")
plot_data8 <- Rating %>%
  gather(Overall_Rating,Seat_Comfort_Rating,Cabin_Staff_Rating,Food_Beverages_Rating,Inflight_Entertainment_Rating,Ground_Service_Rating,Wifi_Connectivity_Rating,Value_Money_Rating,key = "Rating_types", value = "value"  )

p8 <- ggplot(plot_data8, aes(x=Rating_types,y=value)) + 
  geom_boxplot(aes(fill=airline, outlier.colour="transparent"),alpha=0.7)+ 
  coord_flip()+ 
  labs(x="Rating Categories", 
       y="Rating Value",
       fill="Carrier")+
  scale_x_discrete(labels=c("Cabin Staff",  "Food Beverages","Groud Service","Inflight Entertainment","Overall Rating","Seat Comfort","Value Money","Wifi Connectivity")) +
  ggtitle("Rating in Different Aspects") +
  theme_bw()+
  theme(plot.title = element_text(face="bold", color = "black", size=14, vjust = 1,hjust = 0.5))
p8

##Delay Analysis

library(readr)
library(ggplot2)
library(tidyverse)
library(plotly)
airline_12 <- read_csv("C:/Users/bhqiz/Desktop/QMSS/Data Visualization/Final Project/Final/airline_12.csv")

reason <- airline_12 %>%
  gather(CARRIER_DELAY,WEATHER_DELAY,NAS_DELAY, SECURITY_DELAY, LATE_AIRCRAFT_DELAY, DIV_AIRPORT_LANDINGS,DIV_REACHED_DEST,DIV_ACTUAL_ELAPSED_TIME,DIV_ARR_DELAY,DIV_DISTANCE,key = "reason", value = "minutes"  )

a <- filter(reason, minutes > 0)
plot_data <- a %>%
  group_by(reason, UNIQUE_CARRIER) %>%
  summarize(count = n(),
            mean_delay = mean(minutes,na.rm=TRUE))


p3 <- ggplot(plot_data, aes(x=reason,y=mean_delay, color=UNIQUE_CARRIER)) + 
  geom_point(alpha=0.5,size=5)+ 
  coord_flip()+ 
  labs(x="Delay Causes", 
       y="Mean Delay Minutes",
       color="Carrier",
       size="Flight Amounts") +
  scale_x_discrete(labels=c("Carrier Delay",  "Diverted Actual Elapsed","Diverted Airport Landings","Diverted Arrival Delay","Diverted Distance","Diverted Reached Destination","Late Aircraft Delay","National Airspace System", "Security Delay", "Weather Delay"))+
  ggtitle("Delay Causes and Mean Delay Minutes") +
  theme_bw() +
  theme(plot.title = element_text(face="bold", color = "black", size=14, vjust = 1,hjust = 2))
plot3 <- ggplotly(p3)
plot3

p3.5 <- ggplot(plot_data, aes(x=reason,y=count, fill=UNIQUE_CARRIER)) + 
  geom_col(position = "dodge",alpha=0.7)+ 
  coord_flip()+ 
  labs(x="Delay Causes", 
       y="Flight Amounts",
       fill="Carrier")+
  scale_x_discrete(labels=c("Carrier Delay",  "Diverted Actual Elapsed","Diverted Airport Landings","Diverted Arrival Delay","Diverted Distance","Diverted Reached Destination","Late Aircraft Delay","National Airspace System", "Security Delay", "Weather Delay"))+
  ggtitle("Delay Causes and Flight Amounts")+
  theme_bw()+
  theme(plot.title = element_text(face="bold", color = "black", size=14, vjust = 1,hjust = 0.5))
plot3.5 <- ggplotly(p3.5)
plot3.5

airline_12$ARR_TIME <- as.numeric(airline_12$ARR_TIME)
airline_12$ARR_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", airline_12$ARR_TIME), format="%H%M"), 12, 16), '%H:%M'), '%H:%M')
airline_12$ARR_TIME <- as.POSIXct(airline_12$ARR_TIME,format='%H:%M')

plot_data4 <- airline_12 %>%
  group_by(ARR_TIME, UNIQUE_CARRIER) %>%
  summarize(count = n(),
            mean_delay = mean(ARR_DELAY))

p4 <- ggplot(plot_data4, aes(x=ARR_TIME,y=mean_delay, color=UNIQUE_CARRIER)) + 
  scale_x_datetime(date_labels = "%H:00")+
  scale_y_continuous(limits = c(-50, 600))+ 
  geom_point(alpha=0.2)+
  geom_smooth(se=FALSE, size=2) + 
  labs(x="Arrival Time", 
       y="Mean Delay Minutes",
       color="Carrier")+
  ggtitle("Arrival Delay Time during One Day")+
  theme_bw()+
  theme(plot.title = element_text(face="bold", color = "black", size=14, vjust = 1,hjust = 0.5))

plot4 <- ggplotly(p4)
plot4

airline_12$DEP_TIME <- as.numeric(airline_12$DEP_TIME)
airline_12$DEP_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", airline_12$DEP_TIME), format="%H%M"), 12, 16), '%H:%M'), '%H:%M')
airline_12$DEP_TIME <- as.POSIXct(airline_12$DEP_TIME,format='%H:%M')

plot_data5 <- airline_12 %>%
  group_by(DEP_TIME, UNIQUE_CARRIER) %>%
  summarize(count = n(),
            mean_delay = mean(DEP_DELAY))

p5 <- ggplot(plot_data5, aes(x=DEP_TIME,y=mean_delay, color=UNIQUE_CARRIER)) + 
  scale_x_datetime(date_labels = "%H:00")+
  scale_y_continuous(limits = c(-50, 600))+ 
  geom_point(alpha=0.2)+
  geom_smooth(se=FALSE, size=2) + 
  labs(x="Departure Time", 
       y="Mean Delay Minutes",
       color="Carrier")+
  ggtitle("Departure Delay Time during One Day")+
  theme_bw()+
  theme(plot.title = element_text(face="bold", color = "black", size=14, vjust = 1,hjust = 0.5))

plot5 <- ggplotly(p5)

plot_data6 <- airline_12 %>%
  group_by(DAY_OF_WEEK, UNIQUE_CARRIER) %>%
  summarize(count = n(),
            mean_dep_delay = mean(DEP_DELAY,na.rm=T),
            mean_arr_delay = mean(ARR_DELAY,na.rm=T)) %>%
  gather(mean_dep_delay, mean_arr_delay, key="Delay_Type", value="Mean_Delay_Time")

p6 <- ggplot(plot_data6, aes(x=DAY_OF_WEEK,y=Mean_Delay_Time, color=UNIQUE_CARRIER, linetype = Delay_Type)) + 
  geom_point(alpha=0.5,size=2)+
  geom_smooth(se=FALSE, size=1.5) + 
  labs(x="Day of Week", 
       y="Mean Delay Minutes",
       color="Carrier",
       linetype="Departure or Arrival")+
  ggtitle("Delay Time during One Week") +
  theme_bw()+
  theme(plot.title = element_text(face="bold", color = "black", size=14, vjust = 1,hjust = 0.5))

p6

plot_data7 <- airline_12 %>%
  group_by(DEST, UNIQUE_CARRIER) %>%
  summarize(countn = n(),
            mean_arr_delay = mean(ARR_DELAY,na.rm=T)) 

plot_data7 <- plot_data7[order(-plot_data7$countn),]
plot_data7 <- plot_data7[1:30,]

p7 <- ggplot(plot_data7, aes(x=DEST,y=mean_arr_delay, color=UNIQUE_CARRIER, size=countn)) + 
  geom_point(alpha=0.5) + 
  scale_size(range=c(2,8)) +
  coord_flip()+ 
  labs(x="Destination Airport", 
       y="Mean Arrival Delay Minutes",
       color="Carrier",
       size="Flight Amounts")+
  ggtitle("Mean Arrival Delay Minutes to Different Destination")+
  theme_bw()+
  theme(plot.title = element_text(face="bold", color = "black", size=14, vjust = 1,hjust = 0.5))


p7

##Overbooking Analysis

library(readxl)
Space_Denied_2016_Quarterly <- read_excel("C:/Users/bhqiz/Desktop/QMSS/Data Visualization/Final Project/Final/Space_Denied_2016_Quarterly.xlsx")

overbook <- filter(Space_Denied_2016_Quarterly, CARRIER=="American Airlines"|CARRIER=="Delta Air Lines"|CARRIER=="United Air Lines")
overbook <- mutate(overbook, ratio_invol=involunter/Total_Boardings)
overbook <- mutate(overbook, ratio_vol=volunteered/Total_Boardings)

p9 <- ggplot(overbook, aes(x=Season,y=ratio_invol,fill=CARRIER)) +
  geom_col(position = "dodge", alpha=0.7) + 
  labs(x="Season", 
       y="Ratio",
       fill="Carrier")+
  ggtitle("Ratio of People Denied Boarding Involuntarily")+
  theme_bw()+
  theme(plot.title = element_text(face="bold", color = "black", size=14, vjust = 1,hjust = 0.5))

plot9 <- ggplotly(p9)
plot9

p10 <- ggplot(overbook, aes(x=Season,y=ratio_vol,fill=CARRIER)) +
  geom_col(position = "dodge", alpha=0.7) + 
  labs(x="Season", 
       y="Ratio",
       fill="Carrier")+
  ggtitle("Ratio of People Denied Boarding Voluntarily")+
  theme_bw()+
  theme(plot.title = element_text(face="bold", color = "black", size=14, vjust = 1,hjust = 0.5))

plot10 <- ggplotly(p10)
plot10

#Text Analysis
library(tm)       
library(qdap)     
library(qdapDictionaries)
library(tidytext)
library(quanteda)
library(readxl)
Airlines_Review <- read_excel("C:/Users/bhqiz/Desktop/QMSS/Data Visualization/Final Project/Final/Airlines_Review.xlsx")
Airlines_Review<-as.data.frame(Airlines_Review)
Review <- Airlines_Review[,3]
review<- VectorSource(Review)
review_corpus <- VCorpus(review)
meta(review_corpus, type="local", tag="country") <- Airlines_Review$Author_country
meta(review_corpus, type="local", tag="class") <- Airlines_Review$Cabin_Flown
meta(review_corpus, type="local", tag="airline") <- Airlines_Review$airline

removeNumPunct <- function(x){gsub("[^[:alpha:][:space:]]*", "", x)}
#clean
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(replace_symbol))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, c(stopwords("english")))  
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  #corpus <- tm_map(corpus, content_transformer(replace_number))
  corpus <- tm_map(corpus, content_transformer(removeNumPunct))
  return(corpus)
}
review_clean<-clean_corpus(review_corpus)
#stem
library(SnowballC)    
review_stemmed <- tm_map(review_clean, stemDocument)
stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}
review_comp <- lapply( review_clean, stemCompletion2, 
                       dictionary=review_clean)
#tdm
for(i in 1:1993){
  Airlines_Review$ID[i]<-i
}
Airlines_Review$ID<-as.character(Airlines_Review$ID)
review_comp_corp <- as.VCorpus(review_comp)
meta(review_comp_corp, type="local", tag="id")<-Airlines_Review$ID
review_tdm <- TermDocumentMatrix(review_comp_corp)

#td
library(dplyr)
library(tidytext)
review_td <- tidy(review_tdm)

review_tdd<- review_td %>%
  inner_join(Airlines_Review, by = c(document = "ID"))

library(ggplot2)

library(wordcloud)
library(tidyr)
set.seed(2103)
RdBu <- brewer.pal(10, "RdBu")
set1 <- brewer.pal(5, "Set1")
cloud_tf <- review_tdd %>%
  group_by(airline,term) %>%
  summarise(num = sum(count))%>%
  spread(airline, num)

cloud_tf <- as.data.frame(cloud_tf)
rownames(cloud_tf) <- cloud_tf$term
cloud_tf <- cloud_tf[ , -1]
cloud_tf[is.na(cloud_tf)] <- 0

## Frequency Analysis


commonality.cloud(cloud_tf, color = RdBu, max.words = 200)

comparison.cloud(cloud_tf, color = set1, max.words = 200, title.size = 1)

##Sentiment Analysis

pos <- read.table("dictionaries/positive-words.txt", as.is=T)
neg <- read.table("dictionaries/negative-words.txt", as.is=T)
sentiment <- function(words){
  require(quanteda)
  tok <- quanteda::tokenize(words)
  pos.count <- sum(tok[[1]]%in%pos[,1])
  neg.count <- sum(tok[[1]]%in%neg[,1])
  out <- (pos.count - neg.count)/(pos.count+neg.count)
  return(out)
}
for(i in 1:1993){
  Airlines_Review$senti[i]<-sentiment(Airlines_Review$Review[i])
}
Airlines_Review$citizen<-"US"
Airlines_Review$Author_country[is.na(Airlines_Review$Author_country)]<-"United States"
Airlines_Review$citizen[Airlines_Review$Author_country !="United States"]<-"non-US"
Airlines_Review$Cabin_Flown[is.na(Airlines_Review$Cabin_Flown)]<-"Economy"

ggplot(data=Airlines_Review, aes(x=citizen, y=senti, color=airline))  +
  geom_boxplot() +
  xlab("Citizen") +
  ylab("Attitude")+
  ggtitle("Customers' Attitude to Airlines by Citizen") +
  theme_bw() +
  theme(plot.title = element_text(face="bold", color = "black", size=14, vjust = 1,hjust = 0.5))


ggplot(data=Airlines_Review,
       aes(x=Cabin_Flown, y=senti, color=airline))  +
  geom_boxplot() +
  scale_x_discrete(limit=c("Economy","Premium Economy","Business Class","First Class")) +
  xlab("Cabin") +
  ylab("Attitude") +
  ggtitle("Customers' Attitude to Airlines by Cabin") +
  theme_bw()+
  theme(plot.title = element_text(face="bold", color = "black", size=14, vjust = 1,hjust = 0.5))


# Regressive Imagery dictionary
# primordial / conceptual thinking
RID_dictionary <- dictionary(file="dictionaries/RID.cat",
                             format = "wordstat")
review_corpus_corp<-corpus(review_corpus)
# make a dfm based on the dictionary
DTM_RIDdict <- dfm(review_corpus_corp, dictionary=RID_dictionary)
library(reshape2)
library(stringr)
RIDdf <- melt(as.matrix(DTM_RIDdict))
RIDdf$id <- str_sub(RIDdf$docs,5)
RIDdff<- RIDdf %>%
  inner_join(Airlines_Review, by = c(id = "ID"))

library(ggrepel)
# Has politics become more aggressive over time?
RIDdff$idd<-as.numeric(RIDdff$id)
ggplot(filter(RIDdff, features=="EMOTIONS.SADNESS._",value>0), 
       aes(x=idd, y=value, color=airline)) + geom_point() + 
  ylab("Sadness Level")+xlab(NULL)  +
  ggtitle("Sadness Level Analysis") +
  theme_bw() +
  theme(plot.title = element_text(face="bold", color = "black", size=14, vjust = 1,hjust = 0.5))