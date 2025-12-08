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
           "Lake Tanganyika (2012-2013)",
           "West Lake Bonney (2023-2024)") #-38.2936


#Get in the lake information####
#Load data

#tanganyika
tanganyika_in_lake <- read.csv("01b_Processed_data/Tanganyika_continuous.csv")
tanganyika_strat <- read.csv("01b_Processed_data/Tanganyika_stratification_continuous.csv")
tanganyika_met <- read.csv("01b_Processed_data/Tanganyika_met.csv")

#Mohonk
mohonk_in_lake <- read.csv("01b_Processed_data/Mohonk_continuous.csv")
mohonk_strat <- read.csv("01b_Processed_data/Mohonk_stratification_continuous.csv")
mohk_ice<-read_csv("01b_Processed_data/Mohonk_ice.csv")

#WestLakeBonney
westlakebonney_in_lake <- read.csv("01b_Processed_data/WestLakeBonney_continuous.csv")
westlakebonney_strat <- read.csv("01b_Processed_data/WestLakeBonney_stratification_continuous.csv")
westlakebonney_met <- read.csv("01b_Processed_data/westlakebonney_met.csv")

#Format
#A bit of magic to get explicit NAs
all <- tanganyika_strat %>%
   full_join(tanganyika_met) %>%
   full_join(mohonk_in_lake) %>%
   full_join(mohonk_strat) %>%
   full_join(westlakebonney_in_lake) %>%
   full_join(westlakebonney_strat) 

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
    scale_x_continuous(breaks=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,
                       limits=c(0,2*pi),
                       oob = scales::oob_keep)+ #the limits and oob indicates to connect the circle
    
    #Inner white circle####
 geom_ribbon(aes(x=yday*2*pi/365,ymin=0,ymax=1),color="white",fill="white")+ #Set up inner circle
    
    #Weird points to establish the legend for the colors of the seasons#
    geom_point(data=tibble(x=c(1:4)*2*pi/365,y=y_limit_lower,color=c(winter_color,spring_color,summer_color,autumn_color)),
               aes(x=x,y=y,fill=color), shape = 22, color = "black")+
    scale_fill_manual(name = "Astronomical seasons", values = c(winter_color,spring_color,summer_color,autumn_color), labels = c("Winter", "Spring","Summer","Autumn"))+
    guides(fill = guide_legend(title.position="top", 
                               title.hjust = 0.5, ncol=2,
                               override.aes = list(shape = 22, color = "black", size = 3)))+
    
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
                         breaks=daylight_breaks,labels=daylight_breaks, name = "Day\nlength (h)",
                         guide = "none")+
    scale_color_gradientn(limits=daylight_limits,
                          colors = daylight_colors,
                          breaks=daylight_breaks,labels=daylight_breaks, name = "Day\nlength (h)",
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
    geom_point(data=all_mohk%>%filter(Ice=="ice"),aes(x=yday*2*pi/365,y=(3+4)/2),
               shape=21,color="black",fill="black",size=1.7)+ #put blue dots for ice days in the outer ring
    geom_point(data=all_mohk%>%filter(Ice=="ice"),aes(x=yday*2*pi/365,y=(3+4)/2),
               shape=21,color="white",fill="white",size=1.5)+ #put blue dots for ice days in the outer ring
    
    #Geometric segments for the months spindles coming out from the middle####
 geom_segment(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,y=4)%>%mutate(xend=x,y=y_limit_lower,yend=4),aes(x=x,xend=xend,y=y,yend=yend),color="white", alpha = 0.3)+  
    
    #Month labels
    geom_text(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,label=month.abb),aes(x=x,y=4.7,label=label), color = "black", size = 2.8)+
    
    #arrows for labels####
 #Ice arrow
 geom_segment(data=tibble(x=(c(45))*2*pi/365,y=3.5)%>%
                mutate(xend=x,yend=y_limit_upper*.8),
              aes(x=x,xend=xend,y=y,yend=yend),color="black")+  
    
    #Ice label
    geom_label(data=tibble(x=(c(45))*2*pi/365,y=y_limit_upper*.8),
               aes(x=x,y=y),label="Ice",size=9*(5/14),
               hjust = 0, vjust = 0.2)+ #The size= in element_text is 14/5 of the size= in geom_text  
    
    #Mixed arrow
    geom_segment(data=tibble(x=(c(347))*2*pi/365,y=3.5)%>%
                   mutate(xend=x,yend=y_limit_upper*0.8),
                 aes(x=x,xend=xend,y=y,yend=yend),color="black")+  
    
    #Mixed label
    geom_label(data=tibble(x=(c(347))*2*pi/365,y=y_limit_upper*.8),
               aes(x=x,y=y),label="Mixed",size=9*(5/14),
               hjust = 0.8, vjust = 0)+ #The size= in element_text is 14/5 of the size= in geom_text  
    
    #Strat arrow
    geom_segment(data=tibble(x=(c(197))*2*pi/365,y=3.5)%>%
                   mutate(xend=x,yend=y_limit_upper*0.9),
                 aes(x=x,xend=xend,y=y,yend=yend),color="black")+  
    
    #Strat label
    geom_label(data=tibble(x=(c(197))*2*pi/365,y=y_limit_upper*.9),
               aes(x=x,y=y),label="Stratified",size=9*(5/14),
               hjust = .9)+ #The size= in element_text is 14/5 of the size= in geom_text 
    
    #text at the top for label####
 geom_text(data=tibble(x=2*2*pi/365,y=y_limit_upper), aes(x=x,y=y), 
           label="a) Mohonk Lake",size = 4.2,
           vjust = 0)+
    
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
          legend.title=element_text(size=10, color = "grey30"), #change the legend title size
          legend.text=element_text(size=9, color = "grey50"), #change the legend text size
          legend.background = element_rect(color = NA, fill = NA), #make the background of the legend box blank
          legend.key.size = unit(1,"line"), #increase the size of the legend points
          panel.border = element_blank(), #get rid of line around plot
          plot.margin = unit(c(-0.6,-0.7,-0.6,-0.7),"cm"), #spread out the plot a bit to minimize the white space
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA)
    )
) #end of the clock plot

#####tanganyika LAKE DATA#####

#Get out tanganyika data####
tanganyika_cont<-continuous_data2%>%filter(Lake=="Lake Tanganyika (2012-2013)")%>%mutate(yday2=yday-194)

#Check the variables here####
unique(tanganyika_cont$Variable)

#Get out the stratification metric
tanganyika_strat<-tanganyika_cont%>%
   filter(Variable=="Epi-hypo dens. difference")%>%
   mutate(delta_density=Value)%>%
   dplyr::select(yday,delta_density)

#Get in tanganyika raw data####
#Create the correct day length for tanganyika####
tanganyika_daylength<-tibble(Date=seq.POSIXt(as.POSIXct("2012-06-21 00:00:00", tz ="Africa/Lusaka"),as.POSIXct("2013-06-20 00:00:00",tz="Africa/Lusaka"), by = 'day'))%>%
   mutate(DayLength=getDayLength(Date,latitude = -6.2,longitude = 29.5))%>%
   mutate(yday=yday(Date))%>%
   mutate(Value=DayLength)
ggplot(data=tanganyika_daylength,aes(x=yday,y=DayLength))+geom_point()

#Set up the annual data to get the template
annual_data_tanganyika<-tibble(yday=tanganyika_daylength$yday,y=min(tanganyika_daylength$DayLength),y2=max(tanganyika_daylength$DayLength))

#tanganyika ice####
#Join them all together, add Mohonk Ice####
all_tanganyika<-annual_data_tanganyika%>%
   left_join(.,tanganyika_strat)%>%
   left_join(.,tanganyika_daylength%>%mutate(day_length=DayLength)%>%dplyr::select(yday,day_length))%>%
   mutate(Ice=ifelse(yday<=500,"open",NA))

all_tanganyika<-all_tanganyika%>%
   mutate(delta_density=na.approx(delta_density, rule = 2))%>% #linearly interpolate 
   mutate(delta_density=ifelse(delta_density<0,0,delta_density)) #get rid of negatives

#Add on extra values to complete the circle####
all_tanganyika<-all_tanganyika%>%
   bind_rows(all_tanganyika%>%slice(1)%>%mutate(yday=171),.)%>% #add in a duplicate first row
   bind_rows(.,all_tanganyika%>%slice(365)%>%mutate(yday=172)) #add in a duplicate first row

#plot the seasons as a clock####
(gg.clock.tanganyika<-ggplot(data=all_tanganyika)+
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
    #Rectangle for the daylength as the inner circle####
 ggnewscale::new_scale_color()+
    ggnewscale::new_scale_fill()+
    geom_rect(aes(xmin=(yday-1)*2*pi/365,xmax=yday*2*pi/365,ymin=2,ymax=3,fill=day_length, color = day_length),show.legend = TRUE)+ 
    scale_fill_gradientn(limits=daylight_limits,
                         colors = daylight_colors,
                         breaks=daylight_breaks,
                         labels=daylight_breaks, name = "Day length (h)",
                         guide = "none")+
    scale_color_gradientn(limits=daylight_limits,
                          colors = daylight_colors,
                          breaks=daylight_breaks,labels=daylight_breaks, name = "Day length (h)",
                          guide = "none")+
    #guides(fill = guide_colorbar(title.position="top", title.hjust = 0.5))+
    
    geom_line(aes(x=yday*2*pi/365,y=1),color="black",size=0.2)+
    geom_line(aes(x=yday*2*pi/365,y=2),color="black",size=0.2)+ #inner and outer lines
    
    #Points for ice data (no ice in NZ)
    
    #Rectangles for the stratification####
 ggnewscale::new_scale_fill()+
    ggnewscale::new_scale_color()+
    geom_rect(aes(xmin=(yday-1)*2*pi/365,xmax=yday*2*pi/365,ymin=3,ymax=4,fill=delta_density, color = delta_density))+
    scale_fill_gradientn(limits=c(0,3.4),
                         colors = c("#DB8080","#DB8080","white","#7CA2CB"),
                         values=scales::rescale(c(3.4, 1, strat_thresh, 0)),
                         breaks=c(strat_thresh,3.4),labels=c("Mixed","Stratified"), 
                         name = "Stratification",
                         guide_colourbar(label.position="bottom")
                         
    )+
    scale_color_gradientn(limits=c(0,3.4),colors = c("#DB8080","#DB8080","white","#7CA2CB"),
                          values=scales::rescale(c(3.4, 1, strat_thresh, 0)),
                          breaks=c(strat_thresh,3.4),
                          labels=c("Mixed","Stratified"), 
                          name = "Stratification",
                          guide_colourbar(label.position="bottom")
    )+
    guides(fill=guide_colorbar(title.position="top",
                               title.hjust = 0.5,
                               frame.colour="grey50",
                               frame.linewidth = 0.1,
                               ticks.colour=NA))+
    
    #circles overtop
    geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=1,yend=1),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
    geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=2,yend=2),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
    geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=3,yend=3),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
    geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=4,yend=4),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
    
    #Geometric segments for the months spindles coming out from the middle####
 geom_segment(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,y=4)%>%mutate(xend=x,y=y_limit_lower,yend=4),aes(x=x,xend=xend,y=y,yend=yend),color="white", alpha = 0.3)+  
    
    #Month names
    geom_text(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,label=month.abb),
              aes(x=x,y=4.7,label=label), 
              color = "black", size = 2.8)+
    
    #Daylength arrow
    geom_segment(data=tibble(x=(c(75))*2*pi/365,y=2.5)%>%
                    mutate(xend=x,yend=y_limit_upper*.75),
                 aes(x=x,xend=xend,y=y,yend=yend),color="black")+  
    
    #Daylength label
    geom_label(data=tibble(x=(c(75))*2*pi/365,y=y_limit_upper*.75),
               aes(x=x,y=y),
               label="Day length", vjust = 0.5, hjust = 0,
               size=9*(5/14))+ #The size= in element_text is 14/5 of the size= in geom_text 
    
    
    #text at the top for label####
 #geom_text(data=tibble(x=2*2*pi/365,y=y_limit_upper),aes(x=x,y=y),label=deparse(bquote(b*"."~Lake~Rerewhakaaitu~NZ*","~38.3*degree*S)), size = 4.2, parse=TRUE)+
 
 #Letter label in middle
 #geom_text(data=tibble(x=(c(1))*2*pi/365,label="B."),aes(x=x,y=0,label=label), color = "black", size = 2.8)+
 
 
 scale_y_continuous(limits=c(y_limit_lower,y_limit_upper))+
    #text at the top for label####
 geom_text(data=tibble(x=2*2*pi/365,y=y_limit_upper),aes(x=x,y=y),
           label="b) Tanganyika Lake",
           size = 4.2, 
           vjust = 1)+
    
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position = "top",
          legend.title=element_text(size=10, color = "grey30"), #change the legend title size
          legend.text=element_text(size=9, color = "grey50"), #change the legend text size
          legend.background = element_rect(color = NA, fill = NA), #make the background of the legend box blank
          legend.key.size = unit(1,"line"), #increase the size of the legend points
          panel.border = element_blank(), #get rid of line around plot
          plot.margin = unit(c(-0.6,-0.7,-0.6,-0.7),"cm"), #spread out the plot a bit to minimize the white space
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA)
    )+
    labs(fill="Stratification") 
) #end of the clock plot


################################################################################
#####West Lake Bonney LAKE DATA#####

#Get out wlb data####
wlb_cont<-read_csv("01b_Processed_data/WestLakeBonney_stratification_continuous.csv")%>%mutate(yday2=yday-194)

#Check the variables here####
unique(wlb_cont$Variable)

#Get in wlb raw data####
wlb_dates <- tibble(day=seq(as.Date("2023-06-21"),as.Date("2024-06-20"),by=1))%>%
   mutate(day2 = day + 194)


head(wlb_dates)
tail(wlb_dates)


#Recreate stratification for WLB####
wlb_strat <- wlb_cont%>%mutate(Date=wlb_dates$day,
                               delta_density=Value)
ggplot(data=wlb_strat,aes(x=Date,y=delta_density))+geom_point()  

#Create the correct day length for wlb####
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

#Add on extra values to complete the circle####
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
                         labels=daylight_breaks, name = "Day length (h)")+
    scale_color_gradientn(limits=daylight_limits,
                          colors = daylight_colors,
                          breaks=daylight_breaks,labels=daylight_breaks, name = "Day length (h)")+
    guides(fill = guide_colorbar(title.position="top", 
                                 title.hjust = 0.5, 
                                 frame.colour="grey50",
                                 frame.linewidth = 0.1,
                                 ticks.colour = "grey50"))+
    
    geom_line(aes(x=yday*2*pi/365,y=1),color="black",size=0.2)+
    geom_line(aes(x=yday*2*pi/365,y=2),color="black",size=0.2)+ #inner and outer lines
    
    #Ice
    geom_point(data=all_wlb%>%filter(Ice=="ice"),aes(x=yday*2*pi/365,y=(3+4)/2),
               shape=21,color="black",fill="black",size=1.7)+ #put blue dots for ice days in the outer ring
    geom_point(data=all_wlb%>%filter(Ice=="ice"),aes(x=yday*2*pi/365,y=(3+4)/2),
               shape=21,color="white",fill="white",size=1.5)+ #put blue dots for ice days in the outer ring
    
    
    #circles overtop
    geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=1,yend=1),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
    geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=2,yend=2),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
    geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=3,yend=3),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
    geom_segment(data=tibble(x=0*2*pi/365,xend=366*2*pi/365,y=4,yend=4),aes(x=x,xend=xend,y=y,yend=yend),color="black", size = 1)+
    
    #Geometric segments for the months spindles coming out from the middle####
 geom_segment(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,y=4)%>%mutate(xend=x,y=y_limit_lower,yend=4),aes(x=x,xend=xend,y=y,yend=yend),color="white", alpha = 0.3)+  
    
    #Month names
    geom_text(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,
                          label=month.abb),
              aes(x=x,y=4.7,label=label), 
              color = "black", size = 2.8)+
    
    #Ice arrow
    geom_segment(data=tibble(x=(c(259))*2*pi/365,y=3.5)%>%
                   mutate(xend=x,yend=y_limit_upper*0.75),
                 aes(x=x,xend=xend,y=y,yend=yend),color="black")+  
    
    #Ice label
    geom_label(data=tibble(x=(c(259))*2*pi/365,y=y_limit_upper*0.75),
               aes(x=x,y=y),label="Ice",size=9*(5/14),
               hjust = 1)+ #The size= in element_text is 14/5 of the size= in geom_text  
    
    
    
    #text at the top for label####
 geom_text(data=tibble(x=2*2*pi/365,y=y_limit_upper),aes(x=x,y=y), 
           label="c) West Lake Bonney", size = 4.2, 
           vjust = 1)+
    
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
          legend.position = "bottom", 
          legend.title=element_text(size=10, color = "grey30"), #change the legend title size
          legend.text=element_text(size=9, color = "grey50"), #change the legend text size
          legend.background = element_rect(color = NA, fill = NA), #make the background of the legend box blank
          legend.key.size = unit(1,"line"), #increase the size of the legend points
          panel.border = element_blank(), #get rid of line around plot
          plot.margin = unit(c(-0.6,-0.7,-0.6,-0.7),"cm"), #spread out the plot a bit to minimize the white space
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA)
    )+
    labs(fill="Stratification")#+
 
) #end of the clock plot

#Set the x and y limits
lat_lim <- c(-82, 82)
lon_lim <- c(-182, 182)

library(rnaturalearth)
#library(rnaturaldata)

(gg.worldMap.naturalEarth<-ggplot() +
      geom_sf(data = ne_countries(returnclass = "sf"),
              color= NA,fill="grey92",linewidth=0.2) +
      geom_sf(data = ne_coastline(returnclass = "sf"),
              color= NA,fill="grey92",linewidth=0.2) +
      #geom_hline(yintercept = 0, lwd = 0.1, alpha = 0.2) +
      geom_point(data=tibble(Latitude=c(-77.720192,41.77,-6.243461),
                             Longitude=c(162.305069,-74.16, 29.545176)),
                 aes(x=Longitude,y=Latitude),shape=21,size=5,fill="grey98", color = "grey70")+
    geom_text(data=tibble(Latitude=c(-77.720192,41.77,-6.243461),
                           Longitude=c(162.305069,-74.16, 29.545176),
                          Label = c("c","a","b")),
               aes(x=Longitude,y=Latitude, label = Label), 
              color = "grey70", hjust = 0.5, vjust = 0.5, size = 2.9)+
    coord_sf(expand = FALSE) +
      theme_void()+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.background = element_rect(fill = "white"))
) #print out plot

# Let coord_quickmap figure out the correct aspect ratio for export:####
#https://community.rstudio.com/t/aspect-ratio-of-a-plot-produced-with-ggplot2-coord-quickmap/9282/2
coord <- coord_quickmap(xlim = lon_lim, ylim = lat_lim, expand = F)
asp <- coord$aspect(list(x.range = lon_lim, y.range = lat_lim))
asp

#Desired plot width in inches
plot.width<-6
# Calculate height
plot.height.new <- plot.width * asp

legend_grob_Seasons <- ggpubr::get_legend(gg.clock.mohonk)
legend_grob_Stratification <- ggpubr::get_legend(gg.clock.tanganyika)
legend_grob_dayLength <- ggpubr::get_legend(gg.clock.wlb)
mhk <- gg.clock.mohonk + theme(legend.position = "none")
wlb <- gg.clock.wlb + theme(legend.position = "none")
tan <- gg.clock.tanganyika + theme(legend.position = "none")
map <- gg.worldMap.naturalEarth + 
   theme(plot.margin = margin(t = -2.5, b = 3, r = 0.5, l = 4, "cm"))
leg1 <- cowplot::plot_grid(legend_grob_dayLength)
leg2 <- cowplot::plot_grid(legend_grob_Stratification)
leg3 <- cowplot::plot_grid(legend_grob_Seasons)
   

final_plot <- map %>% 
   cowplot::ggdraw() +
   cowplot::draw_plot(mhk,
             x = .177, 
             y = .7,
             width = 0.55, 
             height = 0.55,
             vjust = .5, hjust = .5) +
   cowplot::draw_plot(tan,
                      x = .502, 
                      y = .475,
                      width = 0.55, 
                      height = 0.55,
                      vjust = .5, hjust = .5) +
   cowplot::draw_plot(wlb,
                      x = .827, 
                      y = .23,
                      width = 0.55, 
                      height = 0.55,
                      vjust = .5, hjust = .5) +
   cowplot::draw_plot(leg1,
                      x = .14, 
                      y = .1,
                      width = 0.3, 
                      height = 0.2,
                      vjust = .5, hjust = .5)+
   cowplot::draw_plot(leg2,
                      x = .14, 
                      y = .26,
                      width = 0.3, 
                      height = 0.2,
                      vjust = .5, hjust = .5)+
   cowplot::draw_plot(leg3,
                      x = .39, 
                      y = .1,
                      width = 0.3, 
                      height = 0.2,
                      vjust = .5, hjust = .5)
ggsave(filename="03a_Figures/ClockPlotFull-ASL.jpg",plot=final_plot,width=6,height=4.5,units="in",dpi=400)
