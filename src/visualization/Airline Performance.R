#################
# Airline Project
# Performance Visualizations
# 06/27/2017
#################

library(tidyverse)
library(plotly)
Delta_12$ORIGIN_STATE_ABR <- as.factor(Delta_12$ORIGIN_STATE_ABR)
DLAAUA_12 <- filter(Delta_12, UNIQUE_CARRIER=="DL"| UNIQUE_CARRIER=="AA"| UNIQUE_CARRIER=="UA")

airline_12 <- select(DLAAUA_12,-YEAR,-QUARTER,-MONTH,-FL_DATE,-AIRLINE_ID,-CARRIER,-ORIGIN_AIRPORT_ID,-ORIGIN_STATE_NM,-DEST_AIRPORT_ID,-DEST_AIRPORT_SEQ_ID,-DEST_CITY_MARKET_ID,-DEST_STATE_NM,-CRS_DEP_TIME,-DEP_DELAY_NEW,-DEP_DEL15,-WHEELS_OFF,-WHEELS_ON,-CRS_ARR_TIME,-ARR_DELAY_NEW,-ARR_DEL15,-DIVERTED,-CRS_ELAPSED_TIME,-ACTUAL_ELAPSED_TIME,-FLIGHTS,-X57)
library(xlsx)
write.csv(airline_12, ".../.../data/processed/airline_12.csv")


p1 <- ggplot(plot_data, aes(x=DAY_OF_MONTH,y=mean_delay, color=UNIQUE_CARRIER)) +
  scale_y_continuous(limits = c(-50, 250)) + 
  geom_point() +
  theme_bw()

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
  labs(x="Delay Reasons", 
       y="Mean Delay Minutes",
       color="Unique Carrier",
       size="Flight Amounts")+
  theme_bw()
ggsave(filename='p3.jpg', width=8, height=4)

p3.5 <- ggplot(plot_data, aes(x=reason,y=count, fill=UNIQUE_CARRIER)) + 
  geom_col(position = "dodge")+ 
  coord_flip()+ 
  labs(x="Delay Reasons", 
       y="Flight Amounts",
       color="Unique Carrier")+
  theme_bw()
ggsave(filename='p3.5.jpg', width=8, height=4)

colnames(airline_12)
ggplotly(p3)
p3
p3.5



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
  labs(x="Arrive Time", 
       y="Mean Delay Minutes",
       color="Unique Carrier")+
  theme_bw()
ggsave(filename='p4.jpg', width=8, height=4)
p4
ggplotly(p4)



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
       color="Unique Carrier")+
  theme_bw()
ggsave(filename='p5.jpg', width=8, height=4)
p5
ggplotly(p5)


plot_data6 <- airline_12 %>%
  group_by(DAY_OF_WEEK, UNIQUE_CARRIER) %>%
  summarize(count = n(),
            mean_depdelay = mean(DEP_DELAY,na.rm=T),
            mean_arrdelay = mean(ARR_DELAY,na.rm=T)) %>%
  gather(mean_depdelay, mean_arrdelay, key="Delay_Type", value="Mean_Delay_Time")

p6 <- ggplot(plot_data6, aes(x=DAY_OF_WEEK,y=Mean_Delay_Time, color=UNIQUE_CARRIER, linetype = Delay_Type)) + 
  geom_point(alpha=0.5,size=2)+
  geom_smooth(se=FALSE, size=1.5) + 
  labs(x="Day of Week", 
       y="Mean Delay Minutes",
       color="Unique Carrier",
       linetype="Departure or Arrive")+
  theme_bw()
ggsave(filename='p6.jpg', width=8, height=4)
p6
ggplotly(p6)


plot_data7 <- airline_12 %>%
  group_by(DEST, UNIQUE_CARRIER) %>%
  summarize(countn = n(),
            mean_arrdelay = mean(ARR_DELAY,na.rm=T)) 

plot_data7 <- plot_data7[order(-plot_data7$countn),]
plot_data7 <- plot_data7[1:30,]

p7 <- ggplot(plot_data7, aes(x=DEST,y=mean_arrdelay, color=UNIQUE_CARRIER, size=countn)) + 
  geom_point(alpha=0.5) + 
  scale_size(range=c(2,8)) +
  coord_flip()+ 
  labs(x="Destination Airport", 
       y="Mean Arrive Minutes",
       color="Unique Carrier",
       size="Flight Amounts")+
  theme_bw()
ggsave(filename='p7.jpg', width=8, height=4)
p7
ggplotly(p7)


###Rating

plot_data8 <- Rating %>%
  gather(Overall_Rating,Seat_Comfort_Rating,Cabin_Staff_Rating,Food_Beverages_Rating,Inflight_Entertainment_Rating,Ground_Service_Rating,Wifi_Connectivity_Rating,Value_Money_Rating,key = "Rating_types", value = "value"  )

p8 <- ggplot(plot_data8, aes(x=Rating_types,y=value)) + 
  geom_boxplot(aes(fill=airline, outlier.colour="transparent"))+ 
  coord_flip()+ 
  labs(x="Rating Type", 
       y="Rating Value",
       color="Unique Carrier")+
  theme_bw()
ggsave(filename='p8.jpg', width=8, height=4)
p8


