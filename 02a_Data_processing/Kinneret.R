library(tidyverse)
library(rLakeAnalyzer)

#Reads in data from Lewis et al. edi.1530.1, https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1530.1
#Gets out Kinneret data
kinneret_data<-read_csv("https://pasta.lternet.edu/package/data/eml/edi/1530/1/caf59a118a1490e7cb1a219c55b920f9") %>%
  filter(LakeID=="aba03")
#No chla data at Kinneret
#kinneret_chla <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/1530/1/0ece9d7b67cd49741ed7ee60192832e4") %>%
#  filter(LakeID=="aba03")

#Plot the data
ggplot(data=kinneret_data,aes(x=Date,y=Temp_C,color=Depth_m))+geom_point()

#Most years have weekly data
kinneret_data %>% 
  distinct(Date) %>% 
  mutate(year=year(Date)) %>% 
  group_by(year) %>% 
  summarize(counts=n()) %>% 
  print(n=Inf)

#Get out just 1993 - year with the most data####
kinneret_data_1993 <- kinneret_data %>%
  filter(year(Date)==1993) %>%
  filter(!is.na(Temp_C))

#Get out the 0 meter data
kinneret_data_1993_surface <- kinneret_data_1993 %>% 
  filter(Depth_m==0)%>%
  mutate(Surface_temperature=Temp_C) %>%
  dplyr::select(Date,Surface_temperature)

#Deep is 40m (there are 51 of those)
kinneret_data_1993_deep<-kinneret_data_1993 %>% 
  filter(Depth_m==40)%>%
  mutate(Bottom_temperature=Temp_C,
         Bottom_DO = DO_mgL)%>%
  dplyr::select(Date,Bottom_temperature, Bottom_DO)

LAKE = "Kinneret (1993)"

#Merge them together####
in_lake_formatted<-left_join(kinneret_data_1993_surface,kinneret_data_1993_deep)%>%
  select(Date, Surface_temperature, Bottom_temperature, Bottom_DO) %>%
  pivot_longer(c(Surface_temperature, Bottom_temperature, Bottom_DO),
               names_to = "Variable",
               values_to = "Value") %>%
  mutate(yday=yday(Date),
         Lake=LAKE)

write.csv(in_lake_formatted, "../01b_Processed_data/Kinneret_continuous.csv", row.names = F)

# Stratification metric calcs (both discrete and continuous)
strat_sum <- in_lake_formatted %>%
  filter(Variable %in% c("Surface_temperature", "Bottom_temperature")) %>%
  pivot_wider(names_from = "Variable", values_from = "Value") %>%
  mutate(Diff_Dens_surf_bot = water.density(Bottom_temperature) - 
           water.density(Surface_temperature),
         Inverse = ifelse(Bottom_temperature > Surface_temperature,
                          T, F))

strat_export_discrete <- strat_sum %>%
  mutate(Strat = ifelse(Diff_Dens_surf_bot > 0.1, "Stratified", "Mixed"),
         Strat = ifelse(Inverse & Strat == "Stratified", "Inversely\nstratified", Strat)) %>%
  filter(!is.na(Strat)) %>%
  ungroup() %>%
  filter(Strat != lag(Strat)) %>%
  full_join(data.frame(Variable = "Diff_Dens_surf_bot", yday = 1, Strat = "Mixed")) %>%
  arrange(yday) %>%
  mutate(end = lead(yday, default = 365)) %>%
  rename(start = yday, Season = Strat) %>%
  select(start, end, Season) %>%
  mutate(Lake = LAKE)

strat_export_cont <- strat_sum %>%
  select(yday, Diff_Dens_surf_bot) %>%
  pivot_longer(Diff_Dens_surf_bot, names_to = "Variable", values_to = "Value") %>%
  mutate(Lake = LAKE)

write.csv(strat_export_discrete, 
          "../01b_Processed_data/Kinneret_stratification_discrete.csv", 
          row.names = F)
write.csv(strat_export_cont, 
          "../01b_Processed_data/Kinneret_stratification_continuous.csv", 
          row.names = F)

ice_formatted <- tibble(Lake=LAKE,Season="Open water",Method="Ice",start=0,end=365)

write.csv(ice_formatted, "../01b_Processed_data/Kinneret_ice.csv", row.names = F)

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

