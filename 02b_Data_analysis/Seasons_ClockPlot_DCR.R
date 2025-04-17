#Libraries####
if (!require(tidyverse)) {install.packages("tidyverse")}
if (!require(ggpubr)) {install.packages("ggpubr")}
if (!require(geosphere)) {install.packages("geosphere")}


library(tidyverse)
library(ggpubr)
library(geosphere) #Gets daylength based on latitude and doy
library(ggh4x)
library(ShellChron)

#Id latitude of our study lakes####
LAKES <- c("Lake Erken (2018)", #59.83917
           "Arendsee (2020)", #52.89099
           "Lake Ägeri (2023)", #47.1213
           "Midway Pond (2023)", #44.9312
           "Lake Sunapee (2008)", #43.3802
           "Mohonk Lake (2017)", #41.76598
           "Beaverdam Reservoir (2021)", #37.3164
           "Lake Rerewhakaaitu (2022-2023)") #-38.2936


#Get in the lake information####
#Load data
#BVR
bvr_catwalk_format <- read.csv("01b_Processed_data/BVR_catwalk.csv")
bvr_met_format <- read.csv("01b_Processed_data/BVR_met.csv")
bvr_strat <- read.csv("01b_Processed_data/BVR_stratification_continuous.csv")
bvr_ice<-read_csv("01b_Processed_data/BVR_ice.csv")
bvr_ice2<-tibble(doy=c(21,22,23,41,42,55,56,57,58))
#Rerewhakaaitu
rere_in_lake <- read.csv("01b_Processed_data/Rerewhakaaitu_continuous.csv")
rere_strat <- read.csv("01b_Processed_data/Rerewhakaaitu_stratification_continuous.csv")
rere_met <- read.csv("01b_Processed_data/Rerewhakaaitu_met.csv")
#Sunapee
sun_in_lake <- read.csv("01b_Processed_data/Sunapee_continuous.csv")
sun_strat <- read.csv("01b_Processed_data/Sunapee_stratification_continuous.csv")
sun_met <- read.csv("01b_Processed_data/Sunapee_met.csv")
#Erken
erken_in_lake <- read.csv("01b_Processed_data/Erken_continuous.csv")
erken_strat <- read.csv("01b_Processed_data/Erken_stratification_continuous.csv")
erken_met <- read.csv("01b_Processed_data/Erken_met.csv")
#Mohonk
mohonk_in_lake <- read.csv("01b_Processed_data/Mohonk_continuous.csv")
mohonk_strat <- read.csv("01b_Processed_data/Mohonk_stratification_continuous.csv")
mohk_ice<-read_csv("01b_Processed_data/Mohonk_ice.csv")
#Arendsee
arendsee_in_lake <- read.csv("01b_Processed_data/Arendsee_continuous.csv")
arendsee_strat <- read.csv("01b_Processed_data/Arendsee_stratification_continuous.csv")
arendsee_met <- read.csv("01b_Processed_data/Arendsee_met.csv")
#Midway
midway_in_lake <- read.csv("01b_Processed_data/Midway_continuous.csv")
midway_strat <- read.csv("01b_Processed_data/Midway_stratification_continuous.csv")
#Ägeri
Ägeri_in_lake <- read.csv("01b_Processed_data/Ägeri_continuous.csv")
Ägeri_strat <- read.csv("01b_Processed_data/Ägeri_stratification_continuous.csv")
Ägeri_met <- read.csv("01b_Processed_data/Ägeri_met.csv")
#Format
#A bit of magic to get explicit NAs
all <- bvr_catwalk_format %>%
  full_join(bvr_met_format) %>%
  full_join(bvr_strat) %>%
  full_join(rere_in_lake) %>%
  full_join(rere_strat) %>%
  full_join(rere_met) %>%
  full_join(sun_in_lake) %>%
  full_join(sun_strat) %>%
  full_join(sun_met) %>%
  full_join(erken_in_lake) %>%
  full_join(erken_strat) %>%
  full_join(erken_met) %>%
  full_join(mohonk_in_lake) %>%
  full_join(mohonk_strat) %>%
  full_join(arendsee_in_lake) %>%
  full_join(arendsee_strat) %>%
  full_join(arendsee_met) %>%
  full_join(midway_in_lake) %>%
  full_join(midway_strat) %>%
  full_join(Ägeri_in_lake) %>%
  full_join(Ägeri_strat) %>%
  full_join(Ägeri_met)

full <- all %>%
  expand(Variable, yday, Lake)


#Function for day length####
getDayLength <- function(date, latitude, longitude){
  time_origin <- strptime("01/01/2000 12:00", "%d/%m/%Y %H:%M")
  n <- as.numeric(difftime(date, time_origin, units = "days"))
  Jstar <- n - longitude / 360
  M <- (357.5291 + 0.98560028 * Jstar) %% 360
  C <- 1.9148 * sin(M * 2*pi/360) + 
    0.0200 * sin(2*M * 2*pi/360) + 
    0.0003 * sin(3*M * 2*pi/360)
  lambda <- (M + C + 180 + 102.9372) %% 360
  Jtransit <- as.double(2451545.000) + 
    as.double(Jstar) + 
    as.double(0.0053 * sin(M * 2*pi/360)) - 
    as.double(0.0069 * sin(lambda * 4*pi/360))
  sindelta <- sin(lambda * 2*pi/360) * sin(23.44 * 2*pi/360)
  delta <- asin(sindelta) * 360/(2*pi)
  cos_omega <- (sin(-0.83 * 2*pi/360) - (sindelta * sin(latitude * 2*pi/360)))/
    (cos(latitude * 2*pi/360) * cos(delta * 2*pi/360))
  omega <- acos(cos_omega) * 360 / (2*pi)
  Jrise <- Jtransit - omega / 360
  Jset <- Jtransit + omega /360
  return((Jset - Jrise) * 24)
}

# Lake info
for_map <- read_csv("01b_Processed_data/lake_metadata.csv")

#Merge in the different data and get the day lengths####
dayLength <- full %>%
  select(Lake, yday) %>%
  left_join(for_map) %>%
  mutate(Year = str_extract(Lake, "\\d{4}"),
         Date = Solstice+yday) %>%
  mutate(DayLength = getDayLength(Date, Latitude_DD, Longitude_DD)) %>%
  pivot_longer(cols = DayLength, 
               names_to = "Variable", values_to = "Value") %>%
  select(Lake, Variable, Value, yday)

continuous_data2 <- all %>%
  full_join(dayLength) %>%
  mutate(Variable = factor(Variable, 
                           levels = c("DayLength",
                                      "Shortwave",
                                      "AirTemp_C_Average", 
                                      "Surface_temperature",
                                      "Diff_Dens_surf_bot", 
                                      "Bottom_DO",
                                      "Surface_DO",
                                      "Surface_chla"),
                           labels = c("Day length",
                                      "Solar radiation",
                                      "Air temperature",
                                      "Surface temperature",
                                      "Epi-hypo dens. difference",
                                      "Bottom-water DO",
                                      "Surface DO", 
                                      "Surface chlorophyll-a"))) %>%
  filter(!is.na(Variable)) %>%
  mutate(Lake = factor(Lake, levels = LAKES))



#Composite clock figure here#####
#https://docs.google.com/presentation/d/18RpGled962TMjXDMLeXbHc7AHqc8q4t86c3t2Zy3NlU/edit?slide=id.p#slide=id.p####

#Get the lake names out####
continuous_data2%>%dplyr::select(Lake)%>%distinct()


#Get out beaverdam reservoir data####
mohk_cont<-continuous_data2%>%filter(Lake=="Mohonk Lake (2017)")

#Check the variables here####
unique(mohk_cont$Variable)


#Get out mohk day length####
mohk_daylength<-mohk_cont%>%filter(Variable=="Day length")%>%distinct()

#Get out the stratification metric
mohk_strat<-mohk_cont%>%filter(Variable=="Epi-hypo dens. difference")

#Set up the annual data to get the template
annual_data_mohk<-tibble(yday=seq(1,365,by=1),y=min(mohk_daylength$Value),y2=max(mohk_daylength$Value))

#Mohonk ice####
#Join them all together, add Mohonk Ice####
all_mohk<-annual_data_mohk%>%
          left_join(.,mohk_strat%>%mutate(delta_density=Value)%>%dplyr::select(yday,delta_density))%>%
          left_join(.,mohk_daylength%>%mutate(day_length=Value)%>%dplyr::select(yday,day_length))%>%
          mutate(Ice=ifelse(yday<=108,"ice","open"),
                 )

all_mohk<-all_mohk%>%
        bind_rows(all_mohk%>%slice(1)%>%mutate(yday=0),.)%>% #add in a duplicate first row
        bind_rows(.,all_mohk%>%slice(365)%>%mutate(yday=366)) #add in a duplicate first row


#set the inner and outer limits to the figure
y_limit_lower<-7
y_limit_upper<-20
scalar_month_labels<-1.2
scalar_season_labels<-1.3

#plot the seasons as a clock####
ggplot(data=all_mohk)+
  #Put the polar coordinates on with breaks at each of the month starts####
  coord_polar(start=0*2*pi/365)+
  scale_x_continuous(breaks=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365)+
  #Inner white circle####
  geom_ribbon(aes(x=yday*2*pi/365,ymin=0,ymax=y),color="white",fill="white")+ #Set up inner circle
  
  #Seasons colors for each 1/4####
  geom_ribbon(data=tibble(rad=c(-1:80)*2*pi/365),aes(x=rad,ymin=min(mohk_daylength$Value)*0.9,ymax=max(mohk_daylength$Value)*1.05),color="black",fill=rgb(215,228,255,maxColorValue = 255))+ #winter
  geom_ribbon(data=tibble(rad=c(355:366)*2*pi/365),aes(x=rad,ymin=min(mohk_daylength$Value)*0.9,ymax=max(mohk_daylength$Value)*1.05),color="black",fill=rgb(215,228,255,maxColorValue = 255))+ #winter2
  geom_ribbon(data=all_mohk%>%filter(yday>=80&yday<=172),aes(x=yday*2*pi/365,ymin=y*0.9,ymax=y2*1.05),color="black",fill=rgb(183,234,123,maxColorValue = 255))+ #spring
  geom_ribbon(data=all_mohk%>%filter(yday>=172&yday<=264),aes(x=yday*2*pi/365,ymin=y*0.9,ymax=y2*1.05),color="black",fill=rgb(250,251,114,maxColorValue = 255))+ #summer
  geom_ribbon(data=all_mohk%>%filter(yday>=264&yday<=355),aes(x=yday*2*pi/365,ymin=y*0.9,ymax=y2*1.05),color="black",fill=rgb(228,141,44,maxColorValue = 255))+ #fall
  
  #Zoe deerling colors####
  #geom_ribbon(aes(x=yday*2*pi/365,ymin=y*0.9,ymax=y2*1.05),color="black",fill=rgb(212,143,85,maxColorValue = 255))+
  #geom_ribbon(data=all_mohk%>%filter(yday>=80&yday<=172),aes(x=yday*2*pi/365,ymin=y*0.9,ymax=y2*1.05),color="black",fill=rgb(255, 162, 204,maxColorValue = 255))+
  #geom_ribbon(data=all_mohk%>%filter(yday>=172&yday<=264),aes(x=yday*2*pi/365,ymin=y*0.9,ymax=y2*1.05),color="black",fill=rgb(228,141,44,maxColorValue = 255))+
  #geom_ribbon(data=all_mohk%>%filter(yday>=264&yday<=355),aes(x=yday*2*pi/365,ymin=y*0.9,ymax=y2*1.05),color="black",fill=rgb(228, 141, 44,maxColorValue = 255))+
  
  
  #Geometric segments for the months spindles coming out from the middle####
  geom_segment(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,y=2.3)%>%mutate(xend=x,y=y_limit_lower,yend=all_mohk$y2[1]*1.1),aes(x=x,xend=xend,y=y,yend=yend),color="lightgrey")+  
  
  #Rectangles for the center####
  geom_rect(data=tibble(xmin=0*2*pi/365,xmax=365*2*pi/365,ymin=y_limit_lower,ymax=all_mohk$y[1]*0.9),aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="white",color="white")+
  
  #Rectangles for the school year####
  #fall####
  geom_rect(data=tibble(xmin=237*2*pi/365,xmax=365*2*pi/365,ymin=all_mohk$y[1]*0.9,ymax=all_mohk$y[1]*1),aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill=alpha("grey",0.85),color=alpha("grey",0.85))+
  #spring####  
  geom_rect(data=tibble(xmin=-1*2*pi/365,xmax=136*2*pi/365,ymin=all_mohk$y[1]*0.9,ymax=all_mohk$y[1]*1),aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill=alpha("grey",0.85),color=alpha("grey",0.85))+
  
  #Set up ribbon for ice data
  geom_ribbon(aes(x=yday*2*pi/365,ymin=y2,ymax=y2*1.08),color="black",fill="black")+
  #Rectangles for the stratification####
  geom_rect(aes(xmin=(yday-1)*2*pi/365,xmax=yday*2*pi/365,ymin=all_mohk$y2[1],ymax=all_mohk$y2[1]*1.08,fill=delta_density),color=NA)+
  
  #Points for ice data
  geom_point(data=all_mohk%>%filter(Ice=="ice"),aes(x=yday*2*pi/365,y=(all_mohk$y2[1]+all_mohk$y2[1]*1.08)/2),shape=21,color=rgb(215,228,255,maxColorValue = 255,alpha=200),fill=rgb(215,228,255,maxColorValue = 255,alpha=200),size=4.5)+ #put blue dots for ice days in the outer ring

  #Day length as a line####
  geom_line(aes(x=yday*2*pi/365,y=day_length),color="black",size=1.5)+
  
  #circles overtop
  geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=all_mohk$y[1]*0.9,yend=all_mohk$y[1]*0.9),aes(x=x,xend=xend,y=y,yend=yend),color="black")+
  geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=all_mohk$y[1]*1,yend=all_mohk$y[1]*1),aes(x=x,xend=xend,y=y,yend=yend),color="black")+
  geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=all_mohk$y2[1],yend=all_mohk$y2[1]),aes(x=x,xend=xend,y=y,yend=yend),color="black")+
  geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=all_mohk$y2[1]*1.08,yend=all_mohk$y2[1]*1.08),aes(x=x,xend=xend,y=y,yend=yend),color="black")+
  
  
  #Geometric segments for the months spindles coming out from the middle####
  #geom_segment(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,y=2.3)%>%mutate(xend=x,y=y_limit_lower,yend=all_mohk$y2[1]*1.1),aes(x=x,xend=xend,y=y,yend=yend),color="lightgrey")+  
  
  #geom_rect(aes(xmin=doy,xmax=doy,ymin=1,ymax=2),color="black",fill="grey")+
  geom_label(aes(x=(80-46)*2*pi/365,y=max(day_length)*scalar_season_labels),label="winter",label.size=NA)+
  geom_label(aes(x=(172-46)*2*pi/365,y=max(day_length)*scalar_season_labels),label="spring",label.size=NA)+
  geom_label(aes(x=(264-46)*2*pi/365,y=max(day_length)*scalar_season_labels),label="summer",label.size=NA)+
  geom_label(aes(x=(355-46)*2*pi/365,y=max(day_length)*scalar_season_labels),label="fall",label.size=NA)+
  geom_text(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,y=2.3,label=month.abb),aes(x=x,y=max(all_mohk$day_length)*scalar_month_labels,label=label))+
  #scale_x_continuous(breaks=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365)+
  #coord_polar(start=0)+
  scale_y_continuous(limits=c(y_limit_lower,y_limit_upper))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(title="Astronomical seasons")
