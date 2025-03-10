
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

#Daylength for a variety of latitudes
sample_lats<-tibble(doy=rep(seq(1,365),19),
       latitude=rep(seq(0,180,by=10)-90,each=365))%>%
        mutate(daylength=geosphere::daylength(latitude,doy))

#plot them all####
ggplot(data=sample_lats,aes(x=doy,y=daylength,color=latitude))+geom_point()

#plot a few select ones
ggplot(data=sample_lats%>%filter(abs(latitude)<70),aes(x=doy,y=daylength,color=latitude))+geom_point()

#Get out day length 40 and plot the seasons



ggplot(data=sample_lats%>%filter(latitude==40),aes(x=doy,y=daylength))+
  geom_rect(aes(xmin=80,xmax=172,ymin=-Inf,ymax=Inf),fill="yellowgreen",color="black")+ #spring box
  geom_rect(aes(xmin=172,xmax=264,ymin=-Inf,ymax=Inf),fill="green",color="black")+ #summer box
  geom_rect(aes(xmin=264,xmax=355,ymin=-Inf,ymax=Inf),fill="orange",color="black")+ #fall box
  geom_point()+
  geom_vline(xintercept=c(80,172,264,355))+
  geom_text(data=tibble(doy=c(80-46,172-46,264-46,355-46),height=c(16,16,16,16),label=c("winter","spring","summer","fall")),aes(x=doy,y=height,label=label))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(title="standard",x="day of year",y="Daylength (hrs)")

#Get out day length 40 and plot the seasons based on ecology
ggplot(data=sample_lats%>%filter(latitude==40),aes(x=doy,y=daylength))+
  geom_rect(aes(xmin=80-46,xmax=172-46,ymin=-Inf,ymax=Inf),fill="yellowgreen",color="black")+ #spring box
  geom_rect(aes(xmin=172-46,xmax=264-46,ymin=-Inf,ymax=Inf),fill="green",color="black")+ #summer box
  geom_rect(aes(xmin=264-46,xmax=355-46,ymin=-Inf,ymax=Inf),fill="orange",color="black")+ #fall box
  geom_point()+
  geom_vline(xintercept=c(80-46,172-46,264-46,355-46))+
  geom_text(data=tibble(doy=c(0,91.5,91.5*2,91.5*3,91.5*3.85),height=c(16,16,16,16,16),label=c("Nadir:\nwinter","Shoulder:\nspring","Peak:\nsummer","Shoulder:\nfall","Nadir:\nwinter")),aes(x=doy,y=height,label=label))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(title="Ecological seasons?",x="day of year",y="Daylength (hrs)")


annual_data<-tibble(doy=seq(1,365,by=1),y=(1),y2=2)
#plot the seasons as a clock####
ggplot(data=annual_data)+
  geom_ribbon(aes(x=doy*2*pi/365,ymin=0,ymax=y),color="white",fill="white")+
  geom_ribbon(aes(x=doy*2*pi/365,ymin=y,ymax=y2),color="black",fill="white")+
  geom_ribbon(data=annual_data%>%filter(doy>=80&doy<=172),aes(x=doy*2*pi/365,ymin=y,ymax=y2),color="black",fill="yellowgreen")+
  geom_ribbon(data=annual_data%>%filter(doy>=172&doy<=264),aes(x=doy*2*pi/365,ymin=y,ymax=y2),color="black",fill="green")+
  geom_ribbon(data=annual_data%>%filter(doy>=264&doy<=355),aes(x=doy*2*pi/365,ymin=y,ymax=y2),color="black",fill="orange")+
  #geom_rect(aes(xmin=doy,xmax=doy,ymin=1,ymax=2),color="black",fill="grey")+
  geom_label(aes(x=(80-46)*2*pi/365,y=1.5),label="winter")+
  geom_label(aes(x=(172-46)*2*pi/365,y=1.5),label="spring")+
  geom_label(aes(x=(264-46)*2*pi/365,y=1.5),label="summer")+
  geom_label(aes(x=(355-46)*2*pi/365,y=1.5),label="fall")+
  geom_text(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,y=2.3,label=month.abb),aes(x=x,y=y,label=label))+
  scale_x_continuous(breaks=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365)+
  coord_polar(start=0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(title="Astronomical seasons")

#plot the ecological seasons as a clock####
ggplot(data=annual_data)+
  geom_ribbon(aes(x=doy*2*pi/365,ymin=0,ymax=y),color="white",fill="white")+
  geom_ribbon(aes(x=doy*2*pi/365,ymin=y,ymax=y2),color="black",fill="white")+
  geom_ribbon(data=annual_data%>%filter(doy>=(80-46)&doy<=(172-46)),aes(x=doy*2*pi/365,ymin=y,ymax=y2),color="black",fill="yellowgreen")+
  geom_ribbon(data=annual_data%>%filter(doy>=(172-46)&doy<=(264-46)),aes(x=doy*2*pi/365,ymin=y,ymax=y2),color="black",fill="green")+
  geom_ribbon(data=annual_data%>%filter(doy>=(264-46)&doy<=(355-46)),aes(x=doy*2*pi/365,ymin=y,ymax=y2),color="black",fill="orange")+
  geom_label(aes(x=(355)*2*pi/365,y=1.5),label="winter")+
  geom_label(aes(x=(80)*2*pi/365,y=1.5),label="spring")+
  geom_label(aes(x=(172)*2*pi/365,y=1.5),label="summer")+
  geom_label(aes(x=(264)*2*pi/365,y=1.5),label="fall")+
  geom_text(data=tibble(x=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365,y=2.3,label=month.abb),aes(x=x,y=y,label=label))+
  scale_x_continuous(breaks=(c(1,32,60,91,121,152,182,213,244,274,305,335))*2*pi/365)+
  coord_polar(start=0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(title="Ecological seasons")


  
#STOPPED HERE####
#fit the sin curve to the day length
####

#####################################################################################################
#Lakes####
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

#Summarize with sin regression from ShellChron package####
#sinreg fits using a lm that breaks down into sin + cos function: 
#https://rdrr.io/cran/ShellChron/src/R/sinreg.r
#sinreg is a sine regression that has the following output:
#I = mean annual value of sinusoid (height)
#A= amplitude of the sinusoid
#Dper = period of sinusoid in x domain
#peak = location of the peak in the sinuosid
#R2adj=adjusted r2
#p = p-value of the fit
sum <- continuous_data2 %>%
  filter(!is.na(Value), !is.na(yday)) %>%
  mutate(yday = yday) %>%
  group_by(Lake, Variable) %>%
  summarize(vertical = sinreg(yday, Value, fixed_period = 365.25)[[1]][[1]],
            amp = sinreg(yday, Value, fixed_period = 365.25)[[1]][[2]],
            period = sinreg(yday, Value, fixed_period = 365.25)[[1]][[3]],
            peak = sinreg(yday, Value, fixed_period = 365.25)[[1]][[4]],
            R2 = sinreg(yday, Value, fixed_period = 365.25)[[1]][[5]],
            p = sinreg(yday, Value, fixed_period = 365.25)[[1]][[6]],
            fit = amp*sin((2*pi*yday/period+((peak/period-.25)*2*pi)))+vertical,
            fit = ifelse(R2 < 0.5, NA, fit),
            fit = ifelse(p > 0.05, NA, fit)
  ) %>%
  unique()

temp5<-continuous_data2%>%group_by(Lake, Variable) %>%do(lm.fit.model=lm(data=temp,Value~cos(2*pi*yday/365.25)+sin(2*pi*yday/365.25)))

#Plot only the day legnth for our lakes####
#plot them all - use some magic to reverse the southern hemisphere back to the opposite scale
ggplot(data=left_join(continuous_data2,for_map,by="Lake")%>%
             filter(Variable=="Day length")%>%
             mutate(Value=case_when(Lake=="Lake Rerewhakaaitu (2022-2023)"~24-Value,
                                   .default=Value))
       ,aes(x=yday,y=Value,color=Latitude_DD))+geom_point()

#Plot only the day legnth for our lakes####
#plot them all using absolute value of latitude
ggplot(data=left_join(continuous_data2,for_map,by="Lake")%>%
         filter(Variable=="Day length")%>%
         arrange(Latitude_DD,yday)%>%
         group_by(Latitude_DD),
          aes(x=yday,y=Value,color=abs(Latitude_DD),group=abs(Latitude_DD)))+geom_line()

ggplot(data=left_join(continuous_data2,for_map,by="Lake")%>%
         filter(Variable=="Solar radiation")%>%
         arrange(Latitude_DD,yday)%>%
         group_by(Latitude_DD),
       aes(x=yday,y=Value,color=abs(Latitude_DD),group=abs(Latitude_DD)))+geom_point()

#Facet wrap by lake with points####
#Day length####
ggplot(data=left_join(continuous_data2,for_map,by="Lake")%>%
         filter(Variable=="Day length")%>%
         arrange(abs(Latitude_DD))%>%
         mutate(Lake_factor=factor(Lake,levels=for_map$Lake[order(abs(for_map$Latitude_DD),decreasing = TRUE)])),
       aes(x=yday,y=Value,color=abs(Latitude_DD)))+geom_point()+facet_wrap(~Lake_factor)

#Solar radiation####
ggplot(data=left_join(continuous_data2,for_map,by="Lake")%>%
         filter(Variable=="Solar radiation")%>%
         arrange(abs(Latitude_DD))%>%
         mutate(Lake_factor=factor(Lake,levels=for_map$Lake[order(abs(for_map$Latitude_DD),decreasing = TRUE)])),
       aes(x=yday,y=Value,color=abs(Latitude_DD)))+geom_point()+facet_wrap(~Lake_factor)

#Air temperature####
ggplot(data=left_join(continuous_data2,for_map,by="Lake")%>%
         filter(Variable=="Air temperature")%>%
         arrange(abs(Latitude_DD))%>%
         mutate(Lake_factor=factor(Lake,levels=for_map$Lake[order(abs(for_map$Latitude_DD),decreasing = TRUE)])),
       aes(x=yday,y=Value,color=abs(Latitude_DD)))+geom_point()+facet_wrap(~Lake_factor)


#Surface temperature####
ggplot(data=left_join(continuous_data2,for_map,by="Lake")%>%
         filter(Variable=="Surface temperature")%>%
         arrange(abs(Latitude_DD))%>%
         mutate(Lake_factor=factor(Lake,levels=for_map$Lake[order(abs(for_map$Latitude_DD),decreasing = TRUE)])),
       aes(x=yday,y=Value,color=abs(Latitude_DD)))+geom_point()+facet_wrap(~Lake_factor)

#Epi-hypo dens. difference####
ggplot(data=left_join(continuous_data2,for_map,by="Lake")%>%
         filter(Variable=="Epi-hypo dens. difference")%>%
         arrange(abs(Latitude_DD))%>%
         mutate(Lake_factor=factor(Lake,levels=for_map$Lake[order(abs(for_map$Latitude_DD),decreasing = TRUE)])),
       aes(x=yday,y=Value,color=abs(Latitude_DD)))+geom_point()+facet_wrap(~Lake_factor)

##############################################
#Trial of sin fitting####
temp<-continuous_data2%>%filter(Lake=="Mohonk Lake (2017)"&Variable=="Day length")%>%distinct()

#Do the modeling with linear model breakdown - both using lm directly and sinreg (which also uses that)
#more direct case using trig identity - see answer here for the breakdown
#https://stats.stackexchange.com/questions/60500/how-to-find-a-good-fit-for-semi-sinusoidal-model-in-r
fit.lm<-lm(data=temp,Value~cos(2*pi*yday/365.25)+sin(2*pi*yday/365.25))
summary(fit.lm)
temp<-temp%>%mutate(predict_lm_Value=predict(fit.lm,newdata=list(yday=1:366)))%>%
      mutate(predict_sinReg_Value=sinreg(temp$yday, temp$Value, fixed_period = 365.25)[[2]])

#nonlinear version using sine directly
nls.mod <-nls(Value ~ a + b*sin(2*pi*(1/365.25)*yday+c), start=list(a = 12, b = 10,c=1),data=temp)
coeff<-coef(nls.mod)
f<-function(x,a,b,c){a+b*sin(2*pi*(1/365.25)*x+c)}

#plot out the three different versions for proof of concept
ggplot(data=temp,aes(x=yday,y=Value))+geom_point()+geom_line(aes(y=predictSin_Value),color="blue")+
  geom_line(aes(y=predict_sinReg_Value),color="red")+
  geom_line(aes(y=f(1:366,a=coeff["a"],b=coeff["b"],c=coeff["c"])),color="purple")

###################################
###STOPPED HERE - NEED TO ADD IN FITS TO THE CURVES####
#merge the fits from Abby back together to get a fit curve####
#y=a*sin(bx+c)+d
  #where a is amplitude (amp here)
  #where b is the frequency ()
left_join(continuous_data2,for_map,by="Lake")%>%
  left_join(.,sum,by=c("Lake", "Variable"))%>%
  mutate(sinreg_fit_Value=)
