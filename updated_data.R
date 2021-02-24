library(sf)
library(dplyr)
library(readr)

NAACC_points<-st_read("D://Erin/Documents/ArcGIS/Projects/NAACC/NAACC_2021_01_14/export.shp")


subwatersheds<-st_read("D://Erin/Documents/ArcGIS/vanwbd6p_v6.shp") %>% 
  st_transform(., crs=st_crs(NAACC_points)) %>% 
  select(HUC12, geometry)

new_data<-st_join(NAACC_points, subwatersheds)

names<-foreign::read.dbf("D://Erin/Documents/ArcGIS/master12_v6.dbf", as.is=FALSE) %>% 
  select(HUC12, VaName, FedName)

sub_data<-left_join(new_data,names)

tnc<-readxl::read_xlsx("tnc_huc12_priority.xlsx") %>% 
  mutate(HUC12=paste0("0",as.character(HUC12)))

NAACC_tnc<-left_join(sub_data,tnc)

sub_stats<-NAACC_tnc %>% 
  group_by(gTown, HUC12,TNC_Priority) %>% 
  summarise(points=n())

NAACC_tnc_cut<-NAACC_tnc %>% 
  filter(TNC_Priority<5)

st_write(NAACC_tnc_cut,"D://Erin/Documents/ArcGIS/Projects/NAACC/NAACC_tnc_2021_01.shp",
         append=FALSE)
st_write(NAACC_tnc,"D://Erin/Documents/ArcGIS/Projects/NAACC/NAACC_tnc_all_2021_01.shp")
