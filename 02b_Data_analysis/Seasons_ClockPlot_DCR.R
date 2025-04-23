#Libraries####
if (!require(tidyverse)) {install.packages("tidyverse")}
if (!require(ggpubr)) {install.packages("ggpubr")}
if (!require(geosphere)) {install.packages("geosphere")}


library(zoo) #install zoo for na.approx
library(tidyverse)
library(ggpubr)
library(geosphere) #Gets daylength based on latitude and doy
library(ggh4x)
library(ShellChron)
library(rLakeAnalyzer) #water.density function
library(patchwork) #plot panels

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

getDayLength(date=as.POSIXct("2017-03-21 00:00:00",tz="EST"),latitude=41.8,longitude=-74.2)

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

#####MOHONK LAKE DATA#####

#Get out Mohonk Lake data####
mohk_cont<-continuous_data2%>%filter(Lake=="Mohonk Lake (2017)")

#Check the variables here####
unique(mohk_cont$Variable)


#Create the correct day length for Mohonk####
daylength_Mohonk<-tibble(date=seq.POSIXt(as.POSIXct("2017-01-01 00:00:00", tz ="EST"),as.POSIXct("2017-12-31 00:00:00",tz="EST"), by = 'day'))%>%
  mutate(DayLength=getDayLength(date,latitude = 41.8,longitude = -74.2))%>%
  mutate(yday=yday(date))
ggplot(data=daylength_Mohonk,aes(x=yday,y=DayLength))+geom_point()


#Get out the stratification metric
mohk_strat<-mohk_cont%>%filter(Variable=="Epi-hypo dens. difference")

#Set up the annual data to get the template
annual_data_mohk<-tibble(yday=seq(1,365,by=1),y=min(daylength_Mohonk$DayLength),y2=max(daylength_Mohonk$DayLength))

#Mohonk ice####
#Join them all together, add Mohonk Ice####
all_mohk<-annual_data_mohk%>%
          left_join(.,mohk_strat%>%mutate(delta_density=Value)%>%dplyr::select(yday,delta_density))%>%
          left_join(.,daylength_Mohonk%>%mutate(day_length=DayLength)%>%dplyr::select(yday,day_length))%>%
          mutate(Ice=ifelse(yday<=108,"ice","open"),
                 )
#Add on extra values to complete teh circle####
all_mohk<-all_mohk%>%
        bind_rows(all_mohk%>%slice(1)%>%mutate(yday=0),.)%>% #add in a duplicate first row
        bind_rows(.,all_mohk%>%slice(365)%>%mutate(yday=366)) #add in a duplicate first row

#Fill in the missing data with 0.1
all_mohk<-all_mohk%>%mutate(delta_density=na.approx(delta_density))%>% #linearly interpolate 
          mutate(delta_density=ifelse(delta_density<0,0,delta_density)) #get rid of negatives

#set the inner and outer limits to the figure
y_limit_lower<-7
y_limit_upper<-20
scalar_month_labels<-1.18
scalar_season_labels<-1.3

#Colors for seasons#####
winter_color<-rgb(215,228,255,maxColorValue = 255)
spring_color<-rgb(183,234,123,maxColorValue = 255)
summer_color<-rgb(250,251,114,maxColorValue = 255)
autumn_color<-rgb(228,141,44,maxColorValue = 255)
  
#plot the seasons as a clock####
(gg.clock.mohonk<-ggplot(data=all_mohk)+
  #Put the polar coordinates on with breaks at each of the month starts####
  coord_polar(start=0*2*pi/365,clip="off")+
  scale_x_continuous(breaks=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,limits=c(0,2*pi),oob = scales::oob_keep)+ #the limits and oob indicates to connect the circle
  #Inner white circle####
  geom_ribbon(aes(x=yday*2*pi/365,ymin=0,ymax=y),color="white",fill="white")+ #Set up inner circle
  #Weird points to establish the legend for the colors of the seasons#
  geom_point(data=tibble(x=c(1:4)*2*pi/365,y=y_limit_lower,color=c(winter_color,spring_color,summer_color,autumn_color)),aes(x=x,y=y,color=color))+
  scale_color_manual(name = "Astronomical\nseasons", values = c(winter_color,spring_color,summer_color,autumn_color), labels = c("Winter", "Spring","Summer","Autumn"))+
  
  #Seasons colors for each 1/4####
  geom_ribbon(data=tibble(rad=c(-1:80)*2*pi/365),aes(x=rad,ymin=min(daylength_Mohonk$DayLength)*0.9,ymax=max(daylength_Mohonk$DayLength)*1.05),color="black",fill=winter_color)+ #winter
  geom_ribbon(data=tibble(rad=c(355:366)*2*pi/365),aes(x=rad,ymin=min(daylength_Mohonk$DayLength)*0.9,ymax=max(daylength_Mohonk$DayLength)*1.05),color="black",fill=winter_color)+ #winter2
  geom_ribbon(data=all_mohk%>%filter(yday>=80&yday<=172),aes(x=yday*2*pi/365,ymin=y*0.9,ymax=y2*1.05),color="black",fill=spring_color)+ #spring
  geom_ribbon(data=all_mohk%>%filter(yday>=172&yday<=264),aes(x=yday*2*pi/365,ymin=y*0.9,ymax=y2*1.05),color="black",fill=summer_color)+ #summer
  geom_ribbon(data=all_mohk%>%filter(yday>=264&yday<=355),aes(x=yday*2*pi/365,ymin=y*0.9,ymax=y2*1.05),color="black",fill=autumn_color)+ #fall
  
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
  geom_rect(data=tibble(xmin=237*2*pi/365,xmax=353*2*pi/365,ymin=all_mohk$y[1]*0.9,ymax=all_mohk$y[1]*1),aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill=alpha("grey",0.85),color=alpha("grey",0.85))+
  #spring####  
  geom_rect(data=tibble(xmin=20*2*pi/365,xmax=136*2*pi/365,ymin=all_mohk$y[1]*0.9,ymax=all_mohk$y[1]*1),aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill=alpha("grey",0.85),color=alpha("grey",0.85))+
  
  #Set up ribbon for ice data
  geom_ribbon(aes(x=yday*2*pi/365,ymin=y2,ymax=y2*1.08),color="black",fill="black")+
  #Rectangles for the stratification####
  geom_rect(aes(xmin=(yday-1)*2*pi/365,xmax=yday*2*pi/365,ymin=all_mohk$y2[1],ymax=all_mohk$y2[1]*1.08,fill=delta_density),color=NA)+
  scale_fill_gradientn(limits=c(0,3.4),colors = c("red","red","white","blue"),values=c(3.4,1,0.24,0),breaks=c(0.2,3.1),labels=c("Mixed","Stratified"), name = "Stratification")+
  #Points for ice data
  geom_point(data=all_mohk%>%filter(Ice=="ice"),aes(x=yday*2*pi/365,y=(all_mohk$y2[1]+all_mohk$y2[1]*1.08)/2),shape=21,color=rgb(215,228,255,maxColorValue = 255,alpha=200),fill=rgb(215,228,255,maxColorValue = 255,alpha=200),size=4.5)+ #put blue dots for ice days in the outer ring

  #Day length as a line####
  geom_line(data=daylength_Mohonk,aes(x=yday*2*pi/365,y=DayLength),color="black",size=1)+
  
  #circles overtop
  geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=all_mohk$y[1]*0.9,yend=all_mohk$y[1]*0.9),aes(x=x,xend=xend,y=y,yend=yend),color="black")+
  geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=all_mohk$y[1]*1,yend=all_mohk$y[1]*1),aes(x=x,xend=xend,y=y,yend=yend),color="black")+
  geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=all_mohk$y2[1],yend=all_mohk$y2[1]),aes(x=x,xend=xend,y=y,yend=yend),color="black")+
  geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=all_mohk$y2[1]*1.08,yend=all_mohk$y2[1]*1.08),aes(x=x,xend=xend,y=y,yend=yend),color="black")+
  
  #arrows for labels####
  #Ice arrow
  geom_segment(data=tibble(x=(c(45))*2*pi/365,y=(all_mohk$y2[1]+all_mohk$y2[1]*1.08)/2)%>%mutate(xend=x,yend=y_limit_upper),aes(x=x,xend=xend,y=y,yend=yend),color="black")+  
  #school year arrow
  geom_segment(data=tibble(x=(c(131))*2*pi/365,y=all_mohk$y[1]*0.95)%>%mutate(xend=133*2*pi/365,yend=y_limit_upper),aes(x=x,xend=xend,y=y,yend=yend),color="black")+  
  #day length arrow
  geom_segment(data=tibble(x=(c(24))*2*pi/365,y=daylength_Mohonk$DayLength[daylength_Mohonk$yday==24])%>%mutate(xend=x,yend=y_limit_upper),aes(x=x,xend=xend,y=y,yend=yend),color="black")+  
   
     
  #labels for the arrows
  #Ice label
  geom_label(data=tibble(x=(c(45))*2*pi/365,y=y_limit_upper),aes(x=x,y=y),label="Ice",size=9*(5/14))+ #The size= in element_text is 14/5 of the size= in geom_text  
  #School year label
  geom_label(data=tibble(x=(c(130))*2*pi/365,y=y_limit_upper),aes(x=x,y=y),label="University",size=9*(5/14))+ 
  #day length label
  geom_label(data=tibble(x=(c(24))*2*pi/365,y=y_limit_upper),aes(x=x,y=y),label="Day length",size=9*(5/14))+ 
   
   
  #Geometric segments for the months spindles coming out from the middle####
  #geom_segment(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,y=2.3)%>%mutate(xend=x,y=y_limit_lower,yend=all_mohk$y2[1]*1.1),aes(x=x,xend=xend,y=y,yend=yend),color="lightgrey")+  
  
  #geom_rect(aes(xmin=doy,xmax=doy,ymin=1,ymax=2),color="black",fill="grey")+
  #geom_label(aes(x=(80-46)*2*pi/365,y=max(day_length)*scalar_season_labels),label="winter",label.size=NA)+
  #geom_label(aes(x=(172-46)*2*pi/365,y=max(day_length)*scalar_season_labels),label="spring",label.size=NA)+
  #geom_label(aes(x=(264-46)*2*pi/365,y=max(day_length)*scalar_season_labels),label="summer",label.size=NA)+
  #geom_label(aes(x=(355-46)*2*pi/365,y=max(day_length)*scalar_season_labels),label="fall",label.size=NA)+
  geom_text(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,y=2.3,label=month.abb),aes(x=x,y=max(all_mohk$day_length)*scalar_month_labels,label=label))+
  
   #text at the bottom for label####
  geom_text(data=tibble(x=182*2*pi/365,y=y_limit_upper),aes(x=x,y=y),label=deparse(bquote(a*"."~Mohonk~Lake~USA*","~41.8*degree*N)),parse=TRUE)+
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
        axis.ticks.y=element_blank(),
        legend.title=element_text(size=9), #change the legend title size
        legend.text=element_text(size=9), #change the legend text size
        legend.position = c(0.16,0.7),
        legend.spacing.y = unit(0.2, "cm"), #decrease the space between the two legends
        legend.margin = margin(3.4,0,0,0, unit="cm"),
        legend.background = element_rect(color = NA, fill = NA), #make the background of the legend box blank
        legend.key.size = unit(0.6,"line"), #increase the size of the legend points
        plot.margin = unit(c(-0.5,-0.5,-0.5,-0.2),"cm"), #spread out the plot a bit to minimize the white space
        panel.border = element_blank() #get rid of line around plot
        )+
  labs(fill="Stratification")+
  guides(color = guide_legend(order = 1,override.aes = list(size=3)), #reorder the legends so seasons goes first
         fill = guide_colorbar(order=2)    #reorder the legends so stratification goes second
         )
) #end of the clock plot

ggsave(filename="03a_Figures/SeasonsMohonk_Clock.jpg",plot=gg.clock.mohonk,width=3.3,height=3.3,units="in",dpi=300)


#####Rerewhakaaitu LAKE DATA#####

#Get out Mohonk Lake data####
rere_cont<-continuous_data2%>%filter(Lake=="Lake Rerewhakaaitu (2022-2023)")%>%mutate(yday2=yday-194)

#Check the variables here####
unique(rere_cont$Variable)

#Get in rere raw data####
rere_dates <- read_csv("01a_Raw_data/Rerewhakaaitu_buoy_wq.csv")%>%
               dplyr::select(day)%>%
               distinct()%>%
               mutate(day2 = as.Date(day) + 194)%>%
               filter(year(day2)==2023)
                
head(rere_dates)
tail(rere_dates)



#Recreate stratification for Rere####
rere_strat <- read_csv("01a_Raw_data/Rerewhakaaitu_buoy_wq.csv")%>%
                filter(variable=="TmpWtr"&depth_rnd%in%c(1,11,12))%>%
                filter(day>=as.Date("2022-06-21")&day<=as.Date("2023-06-20"))%>%
                pivot_wider(names_from=depth_rnd,values_from=avg_value,names_prefix="Temp_")%>%
                mutate(delta_density=ifelse(!is.na(Temp_12),water.density(Temp_12) - water.density(Temp_1),water.density(Temp_11) - water.density(Temp_1)))%>%
                rename(Date=day)%>%
                left_join(tibble(Date=as.Date(seq.POSIXt(as.POSIXct("2022-06-22 00:00:00", tz ="Pacific/Auckland"),as.POSIXct("2023-06-21 00:00:00",tz="Pacific/Auckland"), by = 'day'))),.)%>%
                mutate(delta_density=na.approx(delta_density))%>% #linearly interpolate 
                mutate(delta_density=ifelse(delta_density<0,0,delta_density)) #get rid of negatives
ggplot(data=rere_strat,aes(x=Date,y=delta_density))+geom_point()  
  
#Create the correct day length for Rere####
rere_daylength<-tibble(Date=seq.POSIXt(as.POSIXct("2022-06-21 00:00:00", tz ="Pacific/Auckland"),as.POSIXct("2023-06-20 00:00:00",tz="Pacific/Auckland"), by = 'day'))%>%
  mutate(DayLength=getDayLength(Date,latitude = -38.3,longitude = 177.))%>%
  mutate(yday=yday(Date))%>%
  mutate(Value=DayLength)
ggplot(data=rere_daylength,aes(x=yday,y=DayLength))+geom_point()

#Set up the annual data to get the template
annual_data_rere<-tibble(yday=rere_daylength$yday,y=min(rere_daylength$DayLength),y2=max(rere_daylength$DayLength))

#Mohonk ice####
#Join them all together, add Mohonk Ice####
all_rere<-annual_data_rere%>%
  left_join(.,rere_strat%>%mutate(yday=yday(Date))%>%dplyr::select(yday,delta_density))%>%
  left_join(.,rere_daylength%>%mutate(day_length=DayLength)%>%dplyr::select(yday,day_length))%>%
  add_row(.,.before=195,yday=0,y=9.49,y2=14.8,delta_density=0.9,day_length=14.8)%>% #add some rows to complete the circle
  add_row(.,.before=195,yday=366,y=9.49,y2=14.8,delta_density=0.9,day_length=14.8)%>% #add some rows to complete the circle
  mutate(Ice=ifelse(yday<=500,"open",NA))

#Add on extra values to complete teh circle####
all_rere<-all_rere%>%
  bind_rows(all_rere%>%slice(1)%>%mutate(yday=171),.)%>% #add in a duplicate first row
  bind_rows(.,all_rere%>%slice(365)%>%mutate(yday=172)) #add in a duplicate first row

#set the inner and outer limits to the figure
y_limit_lower<-7
y_limit_upper<-20
scalar_month_labels<-1.18
scalar_season_labels<-1.3

#Colors for seasons#####
winter_color<-rgb(215,228,255,maxColorValue = 255)
spring_color<-rgb(183,234,123,maxColorValue = 255)
summer_color<-rgb(250,251,114,maxColorValue = 255)
autumn_color<-rgb(228,141,44,maxColorValue = 255)

#plot the seasons as a clock####
(gg.clock.rere<-ggplot(data=all_rere)+
   #Put the polar coordinates on with breaks at each of the month starts####
 coord_polar(start=0*2*pi/365,clip="off")+
   scale_x_continuous(breaks=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,limits=c(0,2*pi),oob = scales::oob_keep)+
   #Inner white circle####
 geom_ribbon(data=all_mohk,aes(x=yday*2*pi/365,ymin=0,ymax=y),color="white",fill="white")+ #Set up inner circle
   #Weird points to establish the legend for the colors of the seasons#
   #geom_point(data=tibble(x=c(1:4)*2*pi/365,y=y_limit_lower,color=c(winter_color,spring_color,summer_color,autumn_color)),aes(x=x,y=y,color=color))+
   scale_color_manual(name = "Astronomical\nseasons", values = c(winter_color,spring_color,summer_color,autumn_color), labels = c("Winter", "Spring","Summer","Autumn"))+
   
   #Seasons colors for each 1/4####
   geom_ribbon(data=tibble(rad=c(-1:80)*2*pi/365),aes(x=rad,ymin=min(daylength_Mohonk$DayLength)*0.9,ymax=max(daylength_Mohonk$DayLength)*1.05),color="black",fill=summer_color)+ #summer
   geom_ribbon(data=tibble(rad=c(355:366)*2*pi/365),aes(x=rad,ymin=min(daylength_Mohonk$DayLength)*0.9,ymax=max(daylength_Mohonk$DayLength)*1.05),color="black",fill=summer_color)+ #summer2
   geom_ribbon(data=all_mohk%>%filter(yday>=80&yday<=172),aes(x=yday*2*pi/365,ymin=y*0.9,ymax=y2*1.05),color="black",fill=autumn_color)+ #fall
   geom_ribbon(data=all_mohk%>%filter(yday>=172&yday<=264),aes(x=yday*2*pi/365,ymin=y*0.9,ymax=y2*1.05),color="black",fill=winter_color)+ #winter
   geom_ribbon(data=all_mohk%>%filter(yday>=264&yday<=355),aes(x=yday*2*pi/365,ymin=y*0.9,ymax=y2*1.05),color="black",fill=spring_color)+ #spring
   
   #Zoe deerling colors####
 #geom_ribbon(aes(x=yday*2*pi/365,ymin=y*0.9,ymax=y2*1.05),color="black",fill=rgb(212,143,85,maxColorValue = 255))+
 #geom_ribbon(data=all_mohk%>%filter(yday>=80&yday<=172),aes(x=yday*2*pi/365,ymin=y*0.9,ymax=y2*1.05),color="black",fill=rgb(255, 162, 204,maxColorValue = 255))+
 #geom_ribbon(data=all_mohk%>%filter(yday>=172&yday<=264),aes(x=yday*2*pi/365,ymin=y*0.9,ymax=y2*1.05),color="black",fill=rgb(228,141,44,maxColorValue = 255))+
 #geom_ribbon(data=all_mohk%>%filter(yday>=264&yday<=355),aes(x=yday*2*pi/365,ymin=y*0.9,ymax=y2*1.05),color="black",fill=rgb(228, 141, 44,maxColorValue = 255))+
 
 
 #Geometric segments for the months spindles coming out from the middle####
 geom_segment(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,y=1)%>%mutate(xend=x,y=y_limit_lower,yend=all_mohk$y2[1]*1.1),aes(x=x,xend=xend,y=y,yend=yend),color="lightgrey")+  
   
   #Rectangles for the center####
 geom_rect(data=tibble(xmin=0*2*pi/365,xmax=365*2*pi/365,ymin=y_limit_lower,ymax=all_mohk$y[1]*0.9),aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="white",color="white")+
   
   #Rectangles for the school year####
    #Waiting on Whitney, for now used this: https://study-in-new-zealand.org/semester-dates/#:~:text=Semester%201%20runs%20from%20April,that%20the%20students%20should%20attend.
 #Semester 1####
 geom_rect(data=tibble(xmin=55*2*pi/365,xmax=170*2*pi/365,ymin=all_mohk$y[1]*0.9,ymax=all_mohk$y[1]*1),aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill=alpha("grey",0.85),color=alpha("grey",0.85))+
   #spring####  
 geom_rect(data=tibble(xmin=188*2*pi/365,xmax=304*2*pi/365,ymin=all_mohk$y[1]*0.9,ymax=all_mohk$y[1]*1),aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill=alpha("grey",0.85),color=alpha("grey",0.85))+
   
   #Set up ribbon for ice data
   #geom_ribbon(aes(x=yday*2*pi/365,ymin=y2,ymax=y2*1.08),color="black",fill="black")+
   #Rectangles for the stratification####
   geom_rect(aes(xmin=(yday-1)*2*pi/365,xmax=yday*2*pi/365,ymin=all_mohk$y2[1],ymax=all_mohk$y2[1]*1.08,fill=delta_density),color=NA)+
   scale_fill_gradientn(limits=c(0,3.4),colors = c("red","red","white","blue"),values=c(3.4,1,0.24,0),breaks=c(0.2,3.1),labels=c("Mixed","Stratified"), name = "Stratification")+
   #Points for ice data (no ice in NZ)
   #geom_point(data=all_mohk%>%filter(Ice=="ice"),aes(x=yday*2*pi/365,y=(all_mohk$y2[1]+all_mohk$y2[1]*1.08)/2),shape=21,color=rgb(215,228,255,maxColorValue = 255,alpha=200),fill=rgb(215,228,255,maxColorValue = 255,alpha=200),size=4.5)+ #put blue dots for ice days in the outer ring
   
   #Day length as a line####
    geom_line(aes(x=yday*2*pi/365,y=day_length),color="black",size=1)+
   
   #circles overtop
   geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=all_mohk$y[1]*0.9,yend=all_mohk$y[1]*0.9),aes(x=x,xend=xend,y=y,yend=yend),color="black")+
   geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=all_mohk$y[1]*1,yend=all_mohk$y[1]*1),aes(x=x,xend=xend,y=y,yend=yend),color="black")+
   geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=all_mohk$y2[1],yend=all_mohk$y2[1]),aes(x=x,xend=xend,y=y,yend=yend),color="black")+
   geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=all_mohk$y2[1]*1.08,yend=all_mohk$y2[1]*1.08),aes(x=x,xend=xend,y=y,yend=yend),color="black")+

   #arrows for labels####
 #Ice arrow
 #geom_segment(data=tibble(x=(c(45))*2*pi/365,y=(all_rere$y2[1]+all_rere$y2[1]*1.08)/2)%>%mutate(xend=x,yend=y_limit_upper),aes(x=x,xend=xend,y=y,yend=yend),color="black")+  
   #school year arrow
   geom_segment(data=tibble(x=(c(131))*2*pi/365,y=all_rere$y[1]*0.925)%>%mutate(xend=130*2*pi/365,yend=y_limit_upper),aes(x=x,xend=xend,y=y,yend=yend),color="black")+  
   #day length arrow
   geom_segment(data=tibble(x=(c(24))*2*pi/365,y=rere_daylength$DayLength[rere_daylength$yday==24])%>%mutate(xend=x,yend=y_limit_upper),aes(x=x,xend=xend,y=y,yend=yend),color="black")+  
   
   
   #labels for the arrows
   #Ice label
   #geom_label(data=tibble(x=(c(45))*2*pi/365,y=y_limit_upper),aes(x=x,y=y),label="Ice")+ 
   #School year label
   geom_label(data=tibble(x=(c(130))*2*pi/365,y=y_limit_upper),aes(x=x,y=y),label="University",size=9*(5/14))+ 
   #day length label
   geom_label(data=tibble(x=(c(24))*2*pi/365,y=y_limit_upper),aes(x=x,y=y),label="Day length",size=9*(5/14))+ 
   
   
   #Geometric segments for the months spindles coming out from the middle####
 #geom_segment(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,y=2.3)%>%mutate(xend=x,y=y_limit_lower,yend=all_mohk$y2[1]*1.1),aes(x=x,xend=xend,y=y,yend=yend),color="lightgrey")+  
 
 #geom_rect(aes(xmin=doy,xmax=doy,ymin=1,ymax=2),color="black",fill="grey")+
 #geom_label(aes(x=(80-46)*2*pi/365,y=max(day_length)*scalar_season_labels),label="winter",label.size=NA)+
 #geom_label(aes(x=(172-46)*2*pi/365,y=max(day_length)*scalar_season_labels),label="spring",label.size=NA)+
 #geom_label(aes(x=(264-46)*2*pi/365,y=max(day_length)*scalar_season_labels),label="summer",label.size=NA)+
 #geom_label(aes(x=(355-46)*2*pi/365,y=max(day_length)*scalar_season_labels),label="fall",label.size=NA)+
 geom_text(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,y=2.3,label=month.abb),aes(x=x,y=max(all_mohk$day_length)*scalar_month_labels,label=label))+
   
   #text at the bottom for label####
 geom_text(data=tibble(x=182*2*pi/365,y=y_limit_upper),aes(x=x,y=y),label=deparse(bquote(b*"."~Lake~Rerewhakaaitu~NZ*","~38.3*degree*S)),parse=TRUE)+
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
         axis.ticks.y=element_blank(),
         legend.position = c(0.14,0.7),
         legend.spacing.y = unit(0.2, "cm"), #decrease the space between the two legends
         legend.margin = margin(3.5,0,0,0, unit="cm"),
         legend.background = element_rect(color = NA, fill = NA), #make the background of the legend box blank
         legend.key.size = unit(0.6,"line"), #increase the size of the legend points
         plot.margin = unit(c(-0.5,-0.5,-0.5,-0.2),"cm"), #spread out the plot a bit to minimize the white space
         #plot.background = element_blank(), 
         panel.border = element_blank() #get rid of line around plot
   )+
   labs(fill="Stratification")+
   #guides(color = guide_legend(order = 1,override.aes = list(size=3)), #reorder the legends so seasons goes first
   #        fill = guide_colorbar(order=2)    #reorder the legends so stratification goes second
   #          )
   guides(color="none",fill="none") #remove legends
) #end of the clock plot

ggsave(filename="03a_Figures/SeasonsWaikato_Clock.jpg",plot=gg.clock.rere,width=3.3,height=3.3,units="in",dpi=300)


#Make a composite figure
gg.2panel<-wrap_plots(list(gg.clock.mohonk,gg.clock.rere),nrow=1)
print(gg.2panel)
ggsave(filename="03a_Figures/2panel_Clock.jpg",plot=gg.2panel,width=6.6,height=3.3,units="in",dpi=300)

