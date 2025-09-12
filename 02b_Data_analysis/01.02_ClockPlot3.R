#Libraries####
if (!require(tidyverse)) {install.packages("tidyverse")}
if (!require(ggpubr)) {install.packages("ggpubr")}
if (!require(geosphere)) {install.packages("geosphere")}
if (!require(ggnewscale)) {install.packages("ggnewscale")}
if (!require(tinter)) {install.packages("tinter")}
if (!require(maps)) {install.packages("maps")}
if (!require(ggmap)) {install.packages("ggmap")}
if (!require(ggspatial)) {install.packages("ggspatial")}
if (!require(grid)) {install.packages("grid")}



library(zoo) #install zoo for na.approx
library(tidyverse)
library(ggpubr)
library(geosphere) #Gets daylength based on latitude and doy
library(ggh4x)
library(ShellChron)
library(rLakeAnalyzer) #water.density function
library(patchwork) #plot panels
library(ggnewscale)
library(tinter)
library(ggmap)
library(maps)
library(ggspatial) #For the scale bar and compass on the map
library(grid) #This extracts the legends from the previous plots and fits them on a new blank plot - export has them all overlaid.

#Id latitude of our study lakes####
LAKES <- c("Mohonk Lake (2017)", #41.76598
           "Lake Rerewhakaaitu (2022-2023)",
           "West Lake Bonney (2023)") #-38.2936


#Get in the lake information####
#Load data

#Rerewhakaaitu
rere_in_lake <- read.csv("01b_Processed_data/Rerewhakaaitu_continuous.csv")
rere_strat <- read.csv("01b_Processed_data/Rerewhakaaitu_stratification_continuous.csv")
rere_met <- read.csv("01b_Processed_data/Rerewhakaaitu_met.csv")

#Mohonk
mohonk_in_lake <- read.csv("01b_Processed_data/Mohonk_continuous.csv")
mohonk_strat <- read.csv("01b_Processed_data/Mohonk_stratification_continuous.csv")
mohk_ice<-read_csv("01b_Processed_data/Mohonk_ice.csv")

#WestLakeBonney
westlakebonney_in_lake <- read.csv("01b_Processed_data/WestLakeBonney_continuous.csv")
westlakebonney_strat <- read.csv("01b_Processed_data/WestLakeBonney_stratification_continuous.csv")
westlakebonney_met <- read.csv("01b_Processed_data/westlakebonney_met.csv")

#kinneret
kin_strat<-read.csv("01b_Processed_data/Kinneret_stratification_continuous.csv")


#Format
#A bit of magic to get explicit NAs
all <- rere_strat %>%
  full_join(rere_met) %>%
  full_join(mohonk_in_lake) %>%
  full_join(mohonk_strat) 

full <- all %>%
  expand(Variable, yday, Lake)


date=as.POSIXct("2023-06-21 00:00:00", tz ="Pacific/Auckland")
latitude=-77.72
longitude=162.2993

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
#Add on extra values to complete the circle####
all_mohk<-all_mohk%>%
  bind_rows(all_mohk%>%slice(1)%>%mutate(yday=0),.)%>% #add in a duplicate first row
  bind_rows(.,all_mohk%>%slice(365)%>%mutate(yday=366)) #add in a duplicate first row

#Fill in the missing data with 0.1
all_mohk<-all_mohk%>%mutate(delta_density=na.approx(delta_density))%>% #linearly interpolate 
  mutate(delta_density=ifelse(delta_density<0,0,delta_density)) #get rid of negatives

#set the inner and outer limits to the figure
y_limit_lower<-0
y_limit_upper<-6
scalar_month_labels<-1.18
scalar_season_labels<-1.3

#Colors for seasons#####
winter_color<-rgb(215,228,255,maxColorValue = 255)
spring_color<-rgb(183,234,123,maxColorValue = 255)
summer_color<-rgb(250,251,114,maxColorValue = 255)
autumn_color<-rgb(228,141,44,maxColorValue = 255)

#Stratification threshold
strat_thresh = 0.1

#Set all the parameters for the daylights, colors should match breaks
daylight_colors<-c(tinter::darken("#FFC233",0.95),
                   tinter::darken("#FFC233",0.6),
                   tinter::lighten("#FFC233",0.8),
                   tinter::lighten("#FFC233",0.1),
                   tinter::lighten("#FFC233",0.05)
                  )
daylight_breaks<-c(0,6,12,18,24)
daylight_limits<-c(0,24)


#plot the seasons as a clock####
(gg.clock.mohonk<-ggplot(data=all_mohk)+
   #Put the polar coordinates on with breaks at each of the month starts####
 coord_polar(start=0*2*pi/365,clip="off")+
   scale_x_continuous(breaks=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,limits=c(0,2*pi),oob = scales::oob_keep)+ #the limits and oob indicates to connect the circle
   
   #Inner white circle####
 geom_ribbon(aes(x=yday*2*pi/365,ymin=0,ymax=1),color="white",fill="white")+ #Set up inner circle
   
   #Weird points to establish the legend for the colors of the seasons#
   geom_point(data=tibble(x=c(1:4)*2*pi/365,y=y_limit_lower,color=c(winter_color,spring_color,summer_color,autumn_color)),
              aes(x=x,y=y,fill=color), shape = 21, color = "black")+
    scale_fill_manual(name = "Astronomical seasons", values = c(winter_color,spring_color,summer_color,autumn_color), labels = c("Winter", "Spring","Summer","Autumn"))+
    guides(fill = guide_legend(title.position="top", 
                                 title.hjust = 0.5, ncol=2,
                                 override.aes = list(shape = 21, color = "black", size = 3)))+
    
   #Seasons colors for each 1/4####
 geom_ribbon(data=tibble(rad=c(-1:80)*2*pi/365),aes(x=rad,ymin=1,ymax=2),color="black",fill=winter_color)+ #winter
   geom_ribbon(data=tibble(rad=c(355:366)*2*pi/365),aes(x=rad,ymin=1,ymax=2),color="black",fill=winter_color)+ #winter2
   geom_ribbon(data=all_mohk%>%filter(yday>=80&yday<=172),aes(x=yday*2*pi/365,ymin=1,ymax=2),color="black",fill=spring_color)+ #spring
   geom_ribbon(data=all_mohk%>%filter(yday>=172&yday<=264),aes(x=yday*2*pi/365,ymin=1,ymax=2),color="black",fill=summer_color)+ #summer
   geom_ribbon(data=all_mohk%>%filter(yday>=264&yday<=355),aes(x=yday*2*pi/365,ymin=1,ymax=2),color="black",fill=autumn_color)+ #fall
   geom_segment(data=tibble(x=(c(355, 80, 172, 264))*2*pi/365,y=4)%>%mutate(xend=x,y=y_limit_lower,yend=4),aes(x=x,xend=xend,y=y,yend=yend),color="black")+  
   
   #Rectangles for the center####
 geom_rect(data=tibble(xmin=0*2*pi/365,xmax=365*2*pi/365,ymin=y_limit_lower,ymax=1),aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="white",color="white")+
   
   #Rectangle for the daylength as the inner circle####
 ggnewscale::new_scale_color()+
    ggnewscale::new_scale_fill()+
   geom_rect(aes(xmin=(yday-1)*2*pi/365,xmax=yday*2*pi/365,ymin=2,ymax=3,fill=day_length, color = day_length),show.legend = FALSE)+ 
   scale_fill_gradientn(limits=c(0,24),
                        colors = daylight_colors,
                        breaks=daylight_breaks,labels=daylight_breaks, name = "Day\nlength (hr)",
                        guide = "none")+
   scale_color_gradientn(limits=daylight_limits,
                         colors = daylight_colors,
                         breaks=daylight_breaks,labels=daylight_breaks, name = "Day\nlength (hr)",
                         guide = "none")+
   geom_line(aes(x=yday*2*pi/365,y=1),color="black",size=0.2)+
   geom_line(aes(x=yday*2*pi/365,y=2),color="black",size=0.2)+ #inner and outer lines
   
   #Set up ribbon for ice data
   geom_ribbon(aes(x=yday*2*pi/365,ymin=y2,ymax=y2*1.08),color="black",fill="black")+
   ggnewscale::new_scale_fill()+
   ggnewscale::new_scale_color()+
   
   #Rectangles for the stratification####
 geom_rect(aes(xmin=(yday-1)*2*pi/365,xmax=yday*2*pi/365,ymin=3,ymax=4,fill=delta_density, color=delta_density))+
    scale_fill_gradientn(limits=c(0,3.4),
                         colors = c("#DB8080","#DB8080","white","#7CA2CB"),
                         values=scales::rescale(c(3.4, 1, strat_thresh, 0)),
                         breaks=c(strat_thresh,3.1),labels=c("Mixed","Strat."),
                         guide = "none",
                         name = "Stratification",
                         )+
    scale_color_gradientn(limits=c(0,3.4),colors = c("#DB8080","#DB8080","white","#7CA2CB"),
                          values=scales::rescale(c(3.4, 1, strat_thresh, 0)),
                          breaks=c(strat_thresh,3.1),labels=c("Mixed","Strat."), 
                          name = "Stratification",
                          guide = "none")+
   #guides(fill = guide_colorbar(title.position="top", title.hjust = 0.5))+
   
   #circles overtop
   geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=1,yend=1),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
   geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=2,yend=2),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
   geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=3,yend=3),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
   geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=4,yend=4),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
   
   #Ice
   geom_point(data=all_mohk%>%filter(Ice=="ice"),aes(x=yday*2*pi/365,y=(3+4)/2),shape=21,color="#8AB4E3",fill="#8AB4E3",size=1.1)+ #put blue dots for ice days in the outer ring
   geom_point(data=all_mohk%>%filter(Ice=="ice"),aes(x=yday*2*pi/365,y=(3+4)/2),shape=21,color="white",fill="white",size=0.9)+ #put blue dots for ice days in the outer ring
   
   #Geometric segments for the months spindles coming out from the middle####
 geom_segment(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,y=4)%>%mutate(xend=x,y=y_limit_lower,yend=4),aes(x=x,xend=xend,y=y,yend=yend),color="white", alpha = 0.3)+  
   
    #Month labels
    geom_text(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,label=month.abb),aes(x=x,y=4.7,label=label), color = "black", size = 2.8)+
    
   #arrows for labels####
 #Ice arrow
 geom_segment(data=tibble(x=(c(45))*2*pi/365,y=3.5)%>%mutate(xend=x,yend=y_limit_upper),aes(x=x,xend=xend,y=y,yend=yend),color="black")+  
    
    #Daylength arrow
    geom_segment(data=tibble(x=(c(131))*2*pi/365,y=2.5)%>%mutate(xend=x,yend=y_limit_upper),aes(x=x,xend=xend,y=y,yend=yend),color="black")+  
    
    #Ice label
    geom_label(data=tibble(x=(c(45))*2*pi/365,y=y_limit_upper),aes(x=x,y=y),label="Ice",size=9*(5/14))+ #The size= in element_text is 14/5 of the size= in geom_text  
   
    #Daylength label
    geom_label(data=tibble(x=(c(135))*2*pi/365,y=y_limit_upper),aes(x=x,y=y),label="Day len.\n(hrs)",size=9*(5/14))+ #The size= in element_text is 14/5 of the size= in geom_text  
    
   #Strat arrow
   geom_segment(data=tibble(x=(c(228))*2*pi/365,y=3.5)%>%mutate(xend=x,yend=y_limit_upper*0.8),aes(x=x,xend=xend,y=y,yend=yend),color="black")+  
   
   #Strat label
   geom_label(data=tibble(x=(c(228))*2*pi/365,y=y_limit_upper*0.9),
              aes(x=x,y=y),label="Stratified",size=9*(5/14),
              hjust = .8)+ #The size= in element_text is 14/5 of the size= in geom_text  
   
   #text at the top for label####
    geom_text(data=tibble(x=2*2*pi/365,y=y_limit_upper), aes(x=x,y=y), label=deparse(bquote(Mohonk~Lake~USA*","~41.8*degree*N)),size = 4.2,parse=TRUE)+
   
   #Letter label in middle
   #geom_text(data=tibble(x=(c(1))*2*pi/365,label="A."),aes(x=x,y=0,label=label), color = "black", size = 2.8)+
     
   scale_y_continuous(limits=c(y_limit_lower,y_limit_upper))+
   
   theme_bw()+
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.title.x=element_blank(),
         axis.title.y=element_blank(),
         axis.ticks.y=element_blank(),
         #legend.position="none", #get rid of legends
         legend.title=element_text(size=11), #change the legend title size
         legend.text=element_text(size=10), #change the legend text size
         legend.margin = margin(0,0,0,0, unit="cm"),
         legend.background = element_rect(color = NA, fill = NA), #make the background of the legend box blank
         legend.key.size = unit(1,"line"), #increase the size of the legend points
         panel.border = element_blank(), #get rid of line around plot
         plot.margin = unit(c(-0.6,-0.7,-0.6,-0.7),"cm"), #spread out the plot a bit to minimize the white space
         panel.background = element_rect(fill = "transparent", colour = NA),
         plot.background = element_rect(fill = "transparent", colour = NA)
   )
) #end of the clock plot

ggsave(filename="03a_Figures/SeasonsMohonk_Clock_v2.png",plot=gg.clock.mohonk,width=1.75,height=1.75,units="in",dpi=300,bg="transparent")


#####Rerewhakaaitu LAKE DATA#####

#Get out rere data####
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

#Rere ice####
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

#plot the seasons as a clock####
(gg.clock.rere<-ggplot(data=all_rere)+
   #Put the polar coordinates on with breaks at each of the month starts####
 coord_polar(start=0*2*pi/365,clip="off")+
   scale_x_continuous(breaks=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,limits=c(0,2*pi),oob = scales::oob_keep)+
   
   #Inner white circle####
 geom_ribbon(data=all_mohk,aes(x=yday*2*pi/365,ymin=0,ymax=1),color="white",fill="white")+ #Set up inner circle
   
   scale_color_manual(name = "Astronomical\nseasons", 
                      values = c(winter_color,spring_color,summer_color,autumn_color), 
                      labels = c("Winter", "Spring","Summer","Autumn"), guide="none")+
   
   #Seasons colors for each 1/4####
   geom_ribbon(data=tibble(rad=c(-1:80)*2*pi/365),aes(x=rad,ymin=1,ymax=2),color="black",fill=summer_color)+ #summer
   geom_ribbon(data=tibble(rad=c(355:366)*2*pi/365),aes(x=rad,ymin=1,ymax=2),color="black",fill=summer_color)+ #summer2
   geom_ribbon(data=all_mohk%>%filter(yday>=80&yday<=172),aes(x=yday*2*pi/365,ymin=1,ymax=2),color="black",fill=autumn_color)+ #fall
   geom_ribbon(data=all_mohk%>%filter(yday>=172&yday<=264),aes(x=yday*2*pi/365,ymin=1,ymax=2),color="black",fill=winter_color)+ #winter
   geom_ribbon(data=all_mohk%>%filter(yday>=264&yday<=355),aes(x=yday*2*pi/365,ymin=1,ymax=2),color="black",fill=spring_color)+ #spring
   geom_segment(data=tibble(x=(c(355, 80, 172, 264))*2*pi/365,y=4)%>%mutate(xend=x,y=y_limit_lower,yend=4),aes(x=x,xend=xend,y=y,yend=yend),color="black")+  
   
   #Rectangles for the center####
 geom_rect(data=tibble(xmin=0*2*pi/365,xmax=365*2*pi/365,
                       ymin=y_limit_lower,ymax=1),
           aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
           fill="white",color="white")+
   
   #Rectangles for the stratification####
 ggnewscale::new_scale_fill()+
   ggnewscale::new_scale_color()+
   geom_rect(aes(xmin=(yday-1)*2*pi/365,xmax=yday*2*pi/365,ymin=3,ymax=4,fill=delta_density, color = delta_density))+
    scale_fill_gradientn(limits=c(0,3.4),
                         colors = c("#DB8080","#DB8080","white","#7CA2CB"),
                         values=scales::rescale(c(3.4, 1, strat_thresh, 0)),
                         breaks=c(strat_thresh,3.4),labels=c("Mixed","Stratified"), 
                         name = "Stratification",
                         guide_colourbar(frame.colour="black",label.position="bottom")
                         #guide_colourbar(frame.colour = "blue") #gets a frame around the gradient bar
                          
                         )+
    scale_color_gradientn(limits=c(0,3.4),colors = c("#DB8080","#DB8080","white","#7CA2CB"),
                          values=scales::rescale(c(3.4, 1, strat_thresh, 0)),
                          breaks=c(strat_thresh,3.4),
                          labels=c("Mixed","Stratified"), 
                          name = "Stratification",
                          guide_colourbar(frame.colour="black",label.position="bottom")
                          #guide = "none"
                          )+
   

   
   
   #Rectangle for the daylength as the inner circle####
 ggnewscale::new_scale_color()+
   ggnewscale::new_scale_fill()+
   geom_rect(aes(xmin=(yday-1)*2*pi/365,xmax=yday*2*pi/365,ymin=2,ymax=3,fill=day_length, color = day_length),show.legend = TRUE)+ 
   scale_fill_gradientn(limits=daylight_limits,
                        colors = daylight_colors,
                        breaks=daylight_breaks,
                        labels=daylight_breaks, name = "Day length (hr)",
                        guide = "none")+
   scale_color_gradientn(limits=daylight_limits,
                         colors = daylight_colors,
                         breaks=daylight_breaks,labels=daylight_breaks, name = "Day length (hr)",
                         guide = "none")+
   #guides(fill = guide_colorbar(title.position="top", title.hjust = 0.5))+
   
   geom_line(aes(x=yday*2*pi/365,y=1),color="black",size=0.2)+
   geom_line(aes(x=yday*2*pi/365,y=2),color="black",size=0.2)+ #inner and outer lines
   
   #Points for ice data (no ice in NZ)
   
   #circles overtop
   geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=1,yend=1),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
   geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=2,yend=2),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
   geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=3,yend=3),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
   geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=4,yend=4),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+

   #Geometric segments for the months spindles coming out from the middle####
  geom_segment(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,y=4)%>%mutate(xend=x,y=y_limit_lower,yend=4),aes(x=x,xend=xend,y=y,yend=yend),color="white", alpha = 0.3)+  
   
   #Month names
   geom_text(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,label=month.abb),aes(x=x,y=4.7,label=label), color = "black", size = 2.8)+
   
    #Strat arrow
    geom_segment(data=tibble(x=(c(132))*2*pi/365,y=3.5)%>%mutate(xend=x,yend=y_limit_upper*0.9),aes(x=x,xend=xend,y=y,yend=yend),color="black")+  
    
    #Strat label
    geom_label(data=tibble(x=(c(132))*2*pi/365,y=y_limit_upper*0.9),
               aes(x=x,y=y),label="Mixed",size=9*(5/14),
               hjust = 0.4)+ #The size= in element_text is 14/5 of the size= in geom_text  
    
    
    
   #text at the top for label####
    #geom_text(data=tibble(x=2*2*pi/365,y=y_limit_upper),aes(x=x,y=y),label=deparse(bquote(b*"."~Lake~Rerewhakaaitu~NZ*","~38.3*degree*S)), size = 4.2, parse=TRUE)+
   
   #Letter label in middle
   #geom_text(data=tibble(x=(c(1))*2*pi/365,label="B."),aes(x=x,y=0,label=label), color = "black", size = 2.8)+
     
   
   scale_y_continuous(limits=c(y_limit_lower,y_limit_upper))+
   #text at the top for label####
    geom_text(data=tibble(x=2*2*pi/365,y=y_limit_upper),aes(x=x,y=y),
           label=deparse(bquote(Lake~Rerewhakaaitu~NZ*","~38.3*degree*S)),
           size = 4.2, parse=TRUE)+
   
   theme_bw()+
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.title.x=element_blank(),
         axis.title.y=element_blank(),
         axis.ticks.y=element_blank(),
         legend.position = "top",
         legend.title=element_text(size=11), #change the legend title size
         legend.text=element_text(size=10), #change the legend text size
         legend.background = element_rect(color = NA, fill = NA), #make the background of the legend box blank
         legend.key.size = unit(1,"line"), #increase the size of the legend points
         panel.border = element_blank(), #get rid of line around plot
         plot.margin = unit(c(-0.6,-0.7,-0.6,-0.7),"cm"), #spread out the plot a bit to minimize the white space
         panel.background = element_rect(fill = "transparent", colour = NA),
         plot.background = element_rect(fill = "transparent", colour = NA)
   )+
   labs(fill="Stratification") 
) #end of the clock plot

ggsave(filename="03a_Figures/SeasonsWaikato_Clock_v2.png",plot=gg.clock.rere,width=1.75,height=1.75,units="in",dpi=300,bg="transparent")


################################################################################
#####West Lake Bonney LAKE DATA#####

#Get out rere data####
wlb_cont<-read_csv("01b_Processed_data/WestLakeBonney_stratification_continuous.csv")%>%mutate(yday2=yday-194)

#Check the variables here####
unique(wlb_cont$Variable)

#Get in rere raw data####
wlb_dates <- tibble(day=seq(as.Date("2023-06-21"),as.Date("2024-06-20"),by=1))%>%
  mutate(day2 = day + 194)
  

head(wlb_dates)
tail(wlb_dates)


#Recreate stratification for WLB####
wlb_strat <- wlb_cont%>%mutate(Date=wlb_dates$day,
                               delta_density=Value)
ggplot(data=wlb_strat,aes(x=Date,y=delta_density))+geom_point()  

#Create the correct day length for Rere####
wlb_daylength<-tibble(Date=seq.POSIXt(as.POSIXct("2023-06-21 00:00:00", tz ="Pacific/Auckland"),as.POSIXct("2024-06-20 00:00:00",tz="Pacific/Auckland"), by = 'day'))%>%
  mutate(DayLength=geosphere::daylength(yday(Date),lat = -77.72))%>%
  mutate(yday=yday(Date))%>%
  mutate(Value=DayLength)
ggplot(data=wlb_daylength,aes(x=yday,y=DayLength))+geom_point()

#Set up the annual data to get the template
annual_data_wlb<-tibble(yday=wlb_daylength$yday,y=min(wlb_daylength$DayLength),y2=max(wlb_daylength$DayLength))

#WLB ice####
#Join them all together, add Mohonk Ice####
all_wlb<-annual_data_wlb%>%
  left_join(.,wlb_strat%>%mutate(yday=yday(Date))%>%dplyr::select(yday,delta_density))%>%
  left_join(.,wlb_daylength%>%mutate(day_length=DayLength)%>%dplyr::select(yday,day_length))%>%
  add_row(.,.before=195,yday=0,y=9.49,y2=14.8,delta_density=90,day_length=24)%>% #add some rows to complete the circle
  add_row(.,.before=195,yday=366,y=9.49,y2=14.8,delta_density=90,day_length=24)%>% #add some rows to complete the circle
  mutate(Ice="ice")%>%
  mutate(delta_density=3.4) #max out delta_density because it is always stratified

#Add on extra values to complete teh circle####
all_wlb<-all_wlb%>%
  bind_rows(all_wlb%>%slice(1)%>%mutate(yday=171),.)%>% #add in a duplicate first row
  bind_rows(.,all_wlb%>%slice(365)%>%mutate(yday=172)) #add in a duplicate first row

#plot the seasons as a clock####
(gg.clock.wlb<-ggplot(data=all_wlb)+
   #Put the polar coordinates on with breaks at each of the month starts####
 coord_polar(start=0*2*pi/365,clip="off")+
   scale_x_continuous(breaks=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,limits=c(0,2*pi),oob = scales::oob_keep)+
   
   #Inner white circle####
 geom_ribbon(data=all_wlb,aes(x=yday*2*pi/365,ymin=0,ymax=1),color="white",fill="white")+ #Set up inner circle
   
   scale_color_manual(name = "Astronomical\nseasons", 
                      values = c(winter_color,spring_color,summer_color,autumn_color), 
                      labels = c("Winter", "Spring","Summer","Autumn"),
                      guide = "none")+
   
   #Seasons colors for each 1/4####
 geom_ribbon(data=tibble(rad=c(-1:80)*2*pi/365),aes(x=rad,ymin=1,ymax=2),color="black",fill=summer_color)+ #summer
   geom_ribbon(data=tibble(rad=c(355:366)*2*pi/365),aes(x=rad,ymin=1,ymax=2),color="black",fill=summer_color)+ #summer2
   geom_ribbon(data=all_mohk%>%filter(yday>=80&yday<=172),aes(x=yday*2*pi/365,ymin=1,ymax=2),color="black",fill=autumn_color)+ #fall
   geom_ribbon(data=all_mohk%>%filter(yday>=172&yday<=264),aes(x=yday*2*pi/365,ymin=1,ymax=2),color="black",fill=winter_color)+ #winter
   geom_ribbon(data=all_mohk%>%filter(yday>=264&yday<=355),aes(x=yday*2*pi/365,ymin=1,ymax=2),color="black",fill=spring_color)+ #spring
   geom_segment(data=tibble(x=(c(355, 80, 172, 264))*2*pi/365,y=4)%>%mutate(xend=x,y=y_limit_lower,yend=4),aes(x=x,xend=xend,y=y,yend=yend),color="black")+  
   
   #Rectangles for the center####
 geom_rect(data=tibble(xmin=0*2*pi/365,xmax=365*2*pi/365,
                       ymin=y_limit_lower,ymax=1),
           aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
           fill="white",color="white")+
   
   #Rectangles for the stratification####
 ggnewscale::new_scale_fill()+
   ggnewscale::new_scale_color()+
   geom_rect(aes(xmin=(yday-1)*2*pi/365,xmax=yday*2*pi/365,ymin=3,ymax=4,fill=delta_density, color = delta_density))+
   scale_fill_gradientn(limits=c(0,3.4),
                        colors = c("#DB8080","#DB8080","white","#7CA2CB"),
                        values=scales::rescale(c(3.4, 1, strat_thresh, 0)),
                        breaks=c(strat_thresh,3.4),labels=c("Mixed","Strat."), 
                        name = "Stratification",
                        guide = "none")+
   scale_color_gradientn(limits=c(0,3.4),colors = c("#DB8080","#DB8080","white","#7CA2CB"),
                         values=scales::rescale(c(3.4, 1, strat_thresh, 0)),
                         breaks=c(strat_thresh,3.4),labels=c("Mixed","Strat."), 
                         name = "Stratification",
                         guide = "none")+
   
   #Rectangle for the daylength as the inner circle####
 ggnewscale::new_scale_color()+
   ggnewscale::new_scale_fill()+
   geom_rect(aes(xmin=(yday-1)*2*pi/365,xmax=yday*2*pi/365,ymin=2,ymax=3,fill=day_length, color = day_length),show.legend = TRUE)+ 
   scale_fill_gradientn(limits=daylight_limits,
                        colors = daylight_colors,
                        breaks=daylight_breaks,
                        labels=daylight_breaks, name = "Day length (hr)",
                        guide_colorbar(frame.colour="black") #gets a grame around the gradient bar
                        )+
   scale_color_gradientn(limits=daylight_limits,
                         colors = daylight_colors,
                         breaks=daylight_breaks,labels=daylight_breaks, name = "Day length (hr)",
                         guide_colorbar(frame.colour="black") #gets a grame around the gradient bar
                         )+
   guides(fill = guide_colorbar(title.position="top", title.hjust = 0.5))+
   
   geom_line(aes(x=yday*2*pi/365,y=1),color="black",size=0.2)+
   geom_line(aes(x=yday*2*pi/365,y=2),color="black",size=0.2)+ #inner and outer lines
   
   #Ice
   geom_point(data=all_wlb%>%filter(Ice=="ice"),aes(x=yday*2*pi/365,y=(3+4)/2),shape=21,color="#8AB4E3",fill="#8AB4E3",size=1.1)+ #put blue dots for ice days in the outer ring
   geom_point(data=all_wlb%>%filter(Ice=="ice"),aes(x=yday*2*pi/365,y=(3+4)/2),shape=21,color="white",fill="white",size=0.9)+ #put blue dots for ice days in the outer ring
   
   
   #circles overtop
   geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=1,yend=1),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
   geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=2,yend=2),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
   geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=3,yend=3),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
   geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=4,yend=4),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
   
   #Geometric segments for the months spindles coming out from the middle####
 geom_segment(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,y=4)%>%mutate(xend=x,y=y_limit_lower,yend=4),aes(x=x,xend=xend,y=y,yend=yend),color="white", alpha = 0.3)+  
   
   #Month names
   geom_text(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,label=month.abb),aes(x=x,y=4.7,label=label), color = "black", size = 2.8)+
   
   #Strat arrow
   geom_segment(data=tibble(x=(c(290))*2*pi/365,y=3.5)%>%mutate(xend=x,yend=y_limit_upper*0.8),aes(x=x,xend=xend,y=y,yend=yend),color="black")+  
   
   #Strat label
   geom_label(data=tibble(x=(c(290))*2*pi/365,y=y_limit_upper*0.8),
              aes(x=x,y=y),label="Ice",size=9*(5/14),
              hjust = .8)+ #The size= in element_text is 14/5 of the size= in geom_text  
   
   
   
   #text at the top for label####
   geom_text(data=tibble(x=2*2*pi/365,y=y_limit_upper),aes(x=x,y=y), label=deparse(bquote(WLB~Antarctica*","~77.7*degree*S)), size = 4.2, parse=TRUE)+
   
    #Letter label in middle
    #geom_text(data=tibble(x=(c(1))*2*pi/365,label="A."),aes(x=x,y=0,label=label), color = "black", size = 2.8)+
 
   scale_y_continuous(limits=c(y_limit_lower,y_limit_upper))+
   
   theme_bw()+
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.title.x=element_blank(),
         axis.title.y=element_blank(),
         axis.ticks.y=element_blank(),
         legend.position = "none", #get rid of the legend
         legend.title=element_text(size=11), #change the legend title size
         legend.text=element_text(size=10), #change the legend text size
         legend.background = element_rect(color = NA, fill = NA), #make the background of the legend box blank
         legend.key.size = unit(1,"line"), #increase the size of the legend points
         panel.border = element_blank(), #get rid of line around plot
         plot.margin = unit(c(-0.6,-0.7,-0.6,-0.7),"cm"), #spread out the plot a bit to minimize the white space
         panel.background = element_rect(fill = "transparent", colour = NA),
         plot.background = element_rect(fill = "transparent", colour = NA)
   )+
   labs(fill="Stratification")#+
 
) #end of the clock plot

ggsave(filename="03a_Figures/SeasonsMcMurdo_Clock_v2.png",plot=gg.clock.wlb,width=1.75,height=1.75,units="in",dpi=300,bg="transparent")

################################################################################
#####KINNERET LAKE DATA#####

#Get in kinneret raw data####
kin_dates <- tibble(day=seq(as.Date("1993-01-01"),as.Date("1993-12-31"),by=1))

head(kin_dates)
tail(kin_dates)


#Recreate stratification for WLB####
kin_strat <- kin_strat%>%mutate(
                               delta_density=Value,
                               Date=make_date(1993, 1, 1) + days(yday - 1))
ggplot(data=kin_strat,aes(x=Date,y=delta_density))+geom_point()  

#Create the correct day length for Kin####
kin_daylength<-tibble(Date=seq.POSIXt(as.POSIXct("1993-01-01 00:00:00", tz ="Asia/Jerusalem"),as.POSIXct("1993-12-31 00:00:00",tz="Asia/Jerusalem"), by = 'day'))%>%
  mutate(DayLength=geosphere::daylength(yday(Date),lat = 32.72))%>%
  mutate(yday=yday(Date))%>%
  mutate(Value=DayLength)
ggplot(data=kin_daylength,aes(x=yday,y=DayLength))+geom_point()

#Set up the annual data to get the template
annual_data_kin<-tibble(yday=kin_daylength$yday,y=min(kin_daylength$DayLength),y2=max(kin_daylength$DayLength))

#kin ice####
#Join them all together, add Mohonk Ice####
all_kin<-annual_data_kin%>%
  left_join(.,kin_strat%>%mutate(yday=yday(Date))%>%dplyr::select(yday,delta_density))%>%
  left_join(.,kin_daylength%>%mutate(day_length=DayLength)%>%dplyr::select(yday,day_length))%>%
  add_row(.,.before=0,yday=0,y=9.99,y2=14.3,delta_density=0.001,day_length=10)%>% #add some rows to complete the circle
  add_row(.,.before=355,yday=366,y=9.49,y2=14.8,delta_density=0.001,day_length=10)%>% #add some rows to complete the circle
  mutate(Ice="open water")%>%
  arrange(yday)%>%
  mutate(delta_density=na.approx(delta_density))%>% #linearly interpolate weekly stratification to daily
  mutate(delta_density=ifelse(delta_density<0,0,delta_density))%>% #set negative values to 0
  mutate(delta_density=ifelse(delta_density>3.4,3.4,delta_density)) #set all large values to 3.4 
  
ggplot(data=all_kin,aes(x=yday,y=delta_density))+geom_point() 

#Add on extra values to complete teh circle####
# all_kin<-all_kin%>%
#   bind_rows(all_kin%>%slice(1)%>%mutate(yday=171),.)%>% #add in a duplicate first row
#   bind_rows(.,all_wlb%>%slice(365)%>%mutate(yday=172)) #add in a duplicate first row

#plot the seasons as a clock####
(gg.clock.kin<-ggplot(data=all_kin)+
   #Put the polar coordinates on with breaks at each of the month starts####
 coord_polar(start=0*2*pi/365,clip="off")+
   scale_x_continuous(breaks=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,limits=c(0,2*pi),oob = scales::oob_keep)+
   
   #Inner white circle####
 geom_ribbon(data=all_kin,aes(x=yday*2*pi/365,ymin=0,ymax=1),color="white",fill="white")+ #Set up inner circle
   
   scale_color_manual(name = "Astronomical\nseasons", 
                      values = c(summer_color,autumn_color,winter_color,spring_color), 
                      labels = c("Summer", "Fall","Winter","Spring"),
                      guide = "none")+
   
   #Seasons colors for each 1/4####
 geom_ribbon(data=tibble(rad=c(-1:80)*2*pi/365),aes(x=rad,ymin=1,ymax=2),color="black",fill=winter_color)+ #winter
   geom_ribbon(data=tibble(rad=c(355:366)*2*pi/365),aes(x=rad,ymin=1,ymax=2),color="black",fill=winter_color)+ #winter2
   geom_ribbon(data=all_mohk%>%filter(yday>=80&yday<=172),aes(x=yday*2*pi/365,ymin=1,ymax=2),color="black",fill=spring_color)+ #spring
   geom_ribbon(data=all_mohk%>%filter(yday>=172&yday<=264),aes(x=yday*2*pi/365,ymin=1,ymax=2),color="black",fill=summer_color)+ #summer
   geom_ribbon(data=all_mohk%>%filter(yday>=264&yday<=355),aes(x=yday*2*pi/365,ymin=1,ymax=2),color="black",fill=autumn_color)+ #fall
   geom_segment(data=tibble(x=(c(355, 80, 172, 264))*2*pi/365,y=4)%>%mutate(xend=x,y=y_limit_lower,yend=4),aes(x=x,xend=xend,y=y,yend=yend),color="black")+  
   
   #Rectangles for the center####
 geom_rect(data=tibble(xmin=0*2*pi/365,xmax=365*2*pi/365,
                       ymin=y_limit_lower,ymax=1),
           aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
           fill="white",color="white")+
   
   #Rectangles for the stratification####
 ggnewscale::new_scale_fill()+
   ggnewscale::new_scale_color()+
   geom_rect(aes(xmin=(yday-1)*2*pi/365,xmax=yday*2*pi/365,ymin=3,ymax=4,fill=delta_density, color = delta_density))+
   scale_fill_gradientn(limits=c(0,3.4),
                        colors = c("#DB8080","#DB8080","white","#7CA2CB"),
                        values=scales::rescale(c(3.4, 1, strat_thresh, 0)),
                        breaks=c(strat_thresh,3.4),labels=c("Mixed","Strat."), 
                        name = "Stratification",
                        guide = "none")+
   scale_color_gradientn(limits=c(0,3.4),colors = c("#DB8080","#DB8080","white","#7CA2CB"),
                         values=scales::rescale(c(3.4, 1, strat_thresh, 0)),
                         breaks=c(strat_thresh,3.4),labels=c("Mixed","Strat."), 
                         name = "Stratification",
                         guide = "none")+
   
   #Rectangle for the daylength as the inner circle####
 ggnewscale::new_scale_color()+
   ggnewscale::new_scale_fill()+
   geom_rect(aes(xmin=(yday-1)*2*pi/365,xmax=yday*2*pi/365,ymin=2,ymax=3,fill=day_length, color = day_length),show.legend = TRUE)+ 
   scale_fill_gradientn(limits=daylight_limits,
                        colors = daylight_colors,
                        breaks=daylight_breaks,
                        labels=daylight_breaks, name = "Day length (hr)",
                        guide_colorbar(frame.colour="black") #gets a grame around the gradient bar
   )+
   scale_color_gradientn(limits=daylight_limits,
                         colors = daylight_colors,
                         breaks=daylight_breaks,labels=daylight_breaks, name = "Day length (hr)",
                         guide_colorbar(frame.colour="black") #gets a grame around the gradient bar
   )+
   guides(fill = guide_colorbar(title.position="top", title.hjust = 0.5))+
   
   geom_line(aes(x=yday*2*pi/365,y=1),color="black",size=0.2)+
   geom_line(aes(x=yday*2*pi/365,y=2),color="black",size=0.2)+ #inner and outer lines
   
   #NO Ice
   #geom_point(data=all_wlb%>%filter(Ice=="ice"),aes(x=yday*2*pi/365,y=(3+4)/2),shape=21,color="#8AB4E3",fill="#8AB4E3",size=1.1)+ #put blue dots for ice days in the outer ring
   #geom_point(data=all_wlb%>%filter(Ice=="ice"),aes(x=yday*2*pi/365,y=(3+4)/2),shape=21,color="white",fill="white",size=0.9)+ #put blue dots for ice days in the outer ring
   
   
   #circles overtop
   geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=1,yend=1),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
   geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=2,yend=2),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
   geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=3,yend=3),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
   geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=4,yend=4),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
   
   #Geometric segments for the months spindles coming out from the middle####
 geom_segment(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,y=4)%>%mutate(xend=x,y=y_limit_lower,yend=4),aes(x=x,xend=xend,y=y,yend=yend),color="white", alpha = 0.3)+  
   
   #Month names
   geom_text(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,label=month.abb),aes(x=x,y=4.7,label=label), color = "black", size = 2.8)+
   
   #Strat arrow
   geom_segment(data=tibble(x=(c(290))*2*pi/365,y=3.5)%>%mutate(xend=x,yend=y_limit_upper*0.8),aes(x=x,xend=xend,y=y,yend=yend),color="black")+  
   
   #Strat label
   geom_label(data=tibble(x=(c(290))*2*pi/365,y=y_limit_upper*0.8),
              aes(x=x,y=y),label="Stratified",size=9*(5/14),
              hjust = .8)+ #The size= in element_text is 14/5 of the size= in geom_text  
   
   
   
   #text at the top for label####
 geom_text(data=tibble(x=2*2*pi/365,y=y_limit_upper),aes(x=x,y=y), label=deparse(bquote(Kinneret~ISR*","~32.7*degree*N)), size = 4.2, parse=TRUE)+
   
   #Letter label in middle
   #geom_text(data=tibble(x=(c(1))*2*pi/365,label="A."),aes(x=x,y=0,label=label), color = "black", size = 2.8)+
   
   scale_y_continuous(limits=c(y_limit_lower,y_limit_upper))+
   
   theme_bw()+
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.title.x=element_blank(),
         axis.title.y=element_blank(),
         axis.ticks.y=element_blank(),
         legend.position = "none", #get rid of the legend
         legend.title=element_text(size=11), #change the legend title size
         legend.text=element_text(size=10), #change the legend text size
         legend.background = element_rect(color = NA, fill = NA), #make the background of the legend box blank
         legend.key.size = unit(1,"line"), #increase the size of the legend points
         panel.border = element_blank(), #get rid of line around plot
         plot.margin = unit(c(-0.6,-0.7,-0.6,-0.7),"cm"), #spread out the plot a bit to minimize the white space
         panel.background = element_rect(fill = "transparent", colour = NA),
         plot.background = element_rect(fill = "transparent", colour = NA)
   )+
   labs(fill="Stratification")#+
 
) #end of the clock plot

ggsave(filename="03a_Figures/SeasonsKinneret_Clock_v2.png",plot=gg.clock.kin,width=1.75,height=1.75,units="in",dpi=300,bg="transparent")





#Read in country data
world<-map_data('world')
#Set the x and y limits
lat_lim <- c(-82, 82)
lon_lim <- c(-182, 182)


(gg.WorldMap<-ggplot()+
    #geom_polygon(data=state,aes(x=long,y=lat,group=group),fill='white',color='dark grey')+
    geom_polygon(data=world,aes(x=long,y=lat,group=group),color='black',fill="#9fc164",linewidth=0.2)+ #6b93d6 EDEDED
    #annotation_scale(location = "br", width_hint = 0.5) +
    geom_point(data=tibble(Latitude=c(-77.7,-38.29,41.77,32.8),Longitude=c(162.3,176.5,-74.16,35.5)),aes(x=Longitude,y=Latitude),shape=21,size=3,fill="red")+
    coord_sf(crs = 4326,xlim = lon_lim,ylim=lat_lim,expand=FALSE)+
    geom_hline(
      yintercept = 0, # yintercept = 0 corresponds to the equator
      color = "black",
      linetype = "dashed",
      linewidth = 0.5
    )+
    #ggspatial::annotation_north_arrow(location = "bl")+
    #geom_label_repel(data=waterChemDF_trophic%>%ungroup()%>%filter(parameterType=="TP")%>%dplyr::select(MULakeNumber,samplingSiteLatitude,samplingSiteLongitude)%>%distinct(),aes(x=samplingSiteLongitude,y=samplingSiteLatitude,label=MULakeNumber),fill=alpha("white",0.9),size=2,min.segment.length = 0)+
    #coord_equal()+
    scale_y_continuous(breaks=c(-45,0,45))+
    #scale_y_continuous(breaks=c(-45,0,45),labels=c(bquote(45*degree*S),bquote(0*degree),bquote(45*degree*N)))+
    scale_x_continuous(breaks=c(-120,-60,0,60,120),labels=c(bquote(120*degree*W),bquote(60*degree*W),bquote(0*degree),bquote(60*degree*E),bquote(120*degree*E)))+
    #xlab(bquote(Longitude~(degree*W)))+
    #ylab(bquote(Latitude~(degree*N)))+
    theme_bw()+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#6b93d6"), #D3E6F5
          plot.margin = unit(c(0.2,0.2,0,1),"cm") #spread out the plot a bit to minimize the white space
          #panel.background = element_rect(fill = "white")) #D3E6F5
        )
    )

# Let coord_quickmap figure out the correct aspect ratio for export:####
#https://community.rstudio.com/t/aspect-ratio-of-a-plot-produced-with-ggplot2-coord-quickmap/9282/2
coord <- coord_quickmap(xlim = lon_lim, ylim = lat_lim, expand = F)
asp <- coord$aspect(list(x.range = lon_lim, y.range = lat_lim))
asp

#Desired plot width in inches
plot.width<-4.4
# Calculate height
plot.height.new <- plot.width * asp

#Export the plot as a jpg####
ggsave(gg.WorldMap,file=paste0("03a_Figures/WorldMap.jpg"),width=plot.width,height=plot.height.new,units="in",dpi=400)

  

library(rnaturalearth)
#library(rnaturaldata)

(gg.worldMap.naturalEarth<-ggplot() +
  geom_sf(data = ne_countries(returnclass = "sf"),
          fill = "#9fc164", color = NA) +
  geom_sf(data = ne_coastline(returnclass = "sf"),color="black",fill="#9fc164",linewidth=0.2) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed",  linewidth = 0.5)+ # yintercept = 0 corresponds to the equator
  geom_point(data=tibble(Latitude=c(-77.7,-38.29,41.77,32.8),Longitude=c(162.3,176.5,-74.16,35.5)),aes(x=Longitude,y=Latitude),shape=21,size=3,fill="red")+
  ylab("") +
  xlab("") +
  theme_bw() +
  coord_sf(expand = FALSE) +
  scale_y_continuous(breaks=c(-45,0,45),labels=c(bquote(45*degree*S),bquote(0*degree),bquote(45*degree*N)))+
  scale_x_continuous(breaks=c(-120,-60,0,60,120),labels=c(bquote(120*degree*W),bquote(60*degree*W),bquote(0*degree),bquote(60*degree*E),bquote(120*degree*E)))+
  theme(axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#6b93d6"), #D3E6F5
      #plot.margin = unit(c(0.2,0.2,0,1),"cm") #spread out the plot a bit to minimize the white space
      #panel.background = element_rect(fill = "white")) #D3E6F5
  )
) #print out plot

#Desired plot width in inches
plot.width<-4.2
# Calculate height
plot.height.new <- plot.width * asp
#Export the plot as a jpg####
ggsave(gg.worldMap.naturalEarth,file=paste0("03a_Figures/WorldMap2.jpg"),width=plot.width,height=plot.height.new,units="in",dpi=400)


#Dummy plot to get stratification legend####
dummy.stratification<-ggplot(data=tibble(x=seq(0,3.4,by=0.2),y=seq(0,3.4,by=0.2),fill=seq(0,3.4,by=0.2)),aes(x=x,y=y,fill=fill)) +
  geom_point(shape=21)+
  # Set the coordinate limits for the x-axis from 0 to 10
  xlim(0, 4) +
  # Set the coordinate limits for the y-axis from 0 to 10
  ylim(0, 4) +
  scale_fill_gradientn(limits=c(0,3.4),
                       colors = c("#DB8080","#DB8080","white","#7CA2CB"),
                       values=scales::rescale(c(3.4, 1, strat_thresh, 0)),
                       breaks=c(strat_thresh,3.4),labels=c("Mixed","Stratified"), 
                       name = "Stratification",
                       guide_colourbar(frame.colour="black",title.position="top")
                       #guide_colourbar(frame.colour = "blue") #gets a frame around the gradient bar
                       
  )+guides(fill=guide_colorbar(title.position="top",title.hjust = 0.5,ticks.colour=NA))+
  theme(legend.position="bottom")

#Get legend only####

g_source <- ggplot_gtable(ggplot_build(gg.clock.mohonk+theme(legend.position="bottom")+theme(legend.title=element_text(size=9),legend.text=element_text(size=9))))
legend_grob_Seasons <- g_source$grobs[[which(sapply(g_source$grobs, function(x) x$name == "guide-box"))]]

g_source <- ggplot_gtable(ggplot_build(dummy.stratification+theme(legend.title=element_text(size=9),legend.text=element_text(size=9))))
legend_grob_Stratification <- g_source$grobs[[which(sapply(g_source$grobs, function(x) x$name == "guide-box"))]]

g_source <- ggplot_gtable(ggplot_build(gg.clock.wlb+theme(legend.position="bottom",legend.title=element_text(size=9),legend.text=element_text(size=9))))
legend_grob_dayLength <- g_source$grobs[[which(sapply(g_source$grobs, function(x) x$name == "guide-box"))]]

p_empty <- # Create a blank plot
  p <- ggplot() +
  # Set the coordinate limits for the x-axis from 0 to 10
  xlim(0, 10) +
  # Set the coordinate limits for the y-axis from 0 to 10
  ylim(0, 10) +
  # Remove the axis ticks, labels, and plot background
  theme_void()

(p_final_vertical <- p_empty + 
  annotation_custom(legend_grob_Seasons, xmin = 4, xmax = 6, ymin = 8.8, ymax =8.8)+
  annotation_custom(legend_grob_dayLength, xmin = 4, xmax = 6, ymin = 5.1, ymax = 5.1)+
  annotation_custom(legend_grob_Stratification, xmin = 4, xmax = 6, ymin = 1.3, ymax = 1.3)
  )
ggsave(filename="03a_Figures/SeasonsScales_vertical.jpg",plot=p_final_vertical,width=1.9,height=1.9,units="in",dpi=300)






#Northern hemisphere: Make a composite figure
(gg.NorthernHemisphere_panel<-wrap_plots(
                                list(
                                  gg.clock.mohonk+theme(legend.position = "none"),
                                  gg.clock.kin+theme(legend.position = "none") 
                                  ),
                                  nrow=1
                                  )&
  theme(legend.position = "none",
        #legend.spacing.x = unit(3, "cm"),
        #legend.box.margin=margin(-55, 0,-10,0),
        plot.margin = unit(c(-1,0,-1.75,0),"cm") #spread out the plot a bit to minimize the white space
  )
)

ggsave(filename="03a_Figures/2panel_Clock_v3_northernHemisphere.jpg",plot=gg.NorthernHemisphere_panel,width=6,height=2.5,units="in",dpi=300)

#Southern hemisphere: Make a composite figure
(gg.SouthernHemisphere_panel<-wrap_plots(
  list(
    gg.clock.wlb+theme(legend.position = "none"),
    gg.clock.rere+theme(legend.position = "none") 
  ),
  nrow=1
)&
    theme(legend.position = "none",
          #legend.spacing.x = unit(3, "cm"),
          #legend.box.margin=margin(-55, 0,-10,0),
          plot.margin = unit(c(-1,0,-1.75,0),"cm") #spread out the plot a bit to minimize the white space
    )
)

ggsave(filename="03a_Figures/2panel_Clock_v3_southernHemisphere.jpg",plot=gg.SouthernHemisphere_panel,width=6,height=2.5,units="in",dpi=300)


