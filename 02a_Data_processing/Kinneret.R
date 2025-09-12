library(tidyverse)
library(rLakeAnalyzer)

#Reads in data from Lewis et al. edi.1530.1, https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1530.1
#Gets out Kinneret data
kinneret_data<-read_csv("https://pasta.lternet.edu/package/data/eml/edi/1530/1/caf59a118a1490e7cb1a219c55b920f9")%>%filter(LakeID=="aba03")

#Plot the data
ggplot(data=kinneret_data,aes(x=Date,y=Temp_C,color=Depth_m))+geom_point()

#Most years have weekly data
kinneret_data%>%distinct(Date)%>%mutate(year=year(Date))%>%group_by(year)%>%summarize(counts=n())%>%print(n=Inf)

#Get out just 1993 - year with the most data####
kinneret_data_1993<-kinneret_data%>%filter(year(Date)==1993)%>%filter(!is.na(Temp_C))

#Get out the 0 meter data
kinneret_data_1993_surface<-kinneret_data_1993%>%filter(Depth_m==0)%>%
  mutate(Surface_temperature=Temp_C)%>%
  dplyr::select(Date,Surface_temperature)

#Deep is 40m (there are 51 of those)
kinneret_data_1993_deep<-kinneret_data_1993%>%filter(Depth_m==40)%>%
  mutate(Bottom_temperature=Temp_C)%>%
  dplyr::select(Date,Bottom_temperature)

#Merge them together####
kinneret_1993_temp<-left_join(kinneret_data_1993_surface,kinneret_data_1993_deep)%>%
  mutate(Diff_Dens_surf_bot=water.density(Bottom_temperature)-water.density(Surface_temperature))%>%
  mutate(Strat = ifelse(Diff_Dens_surf_bot > 0.1, "Stratified", "Mixed"))%>%
  mutate(yday=yday(Date),
         variable="Diff_Dens_surf_bot",
         Value=Diff_Dens_surf_bot,
         Lake="Kinneret (1993)")%>%
  dplyr::select(yday:Lake)

write_csv(kinneret_1993_temp,file="01b_Processed_data/Kinneret_stratification_continuous.csv")


# #Find if other data is more frequent
# annie_data<-read_csv("https://pasta.lternet.edu/package/data/eml/edi/1530/1/caf59a118a1490e7cb1a219c55b920f9")%>%filter(LakeID=="55")
# 
# #Plot the data
# ggplot(data=annie_data,aes(x=Date,y=Temp_C,color=Depth_m))+geom_point()
# 
# #Most years have monthly data
# annie_data%>%distinct(Date)%>%mutate(year=year(Date))%>%group_by(year)%>%summarize(counts=n())%>%print(n=Inf)
# 
# #Find if other data is more frequent from Kivu
# kivu_data<-read_csv("https://pasta.lternet.edu/package/data/eml/edi/1530/1/caf59a118a1490e7cb1a219c55b920f9")%>%filter(LakeID=="123")
# 
# #Plot the data
# ggplot(data=kivu_data,aes(x=Date,y=Temp_C,color=Depth_m))+geom_point()
# 
# #Most years have infrequent data
# kivu_data%>%distinct(Date)%>%mutate(year=year(Date))%>%group_by(year)%>%summarize(counts=n())%>%print(n=Inf)
# 
# #Find if other data is more frequent from Nkuruba
# nkuruba_data<-read_csv("https://pasta.lternet.edu/package/data/eml/edi/1530/1/caf59a118a1490e7cb1a219c55b920f9")%>%filter(LakeID=="442")
# 
# #Plot the data
# ggplot(data=nkuruba_data,aes(x=Date,y=Temp_C,color=Depth_m))+geom_point()
# 
# #Most years have infrequent data
# nkuruba_data%>%distinct(Date)%>%mutate(year=year(Date))%>%group_by(year)%>%summarize(counts=n())%>%print(n=Inf)

