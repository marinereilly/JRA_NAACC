library(dplyr)
library(stringr)
library(sf)

NAACC_points<-st_read("C://Documents and Settings/Erin/Documents/ArcGIS/Projects/NAACC/NAACC_rd_ws.shp") %>% 
  filter(is.na(Odm_cmm) | Odm_cmm != -1)

tnc<-readxl::read_xlsx("tnc_huc12_priority.xlsx") %>% 
  mutate(HUC12=paste0("0",as.character(HUC12)))

NAACC_tnc<-left_join(NAACC_points,tnc)

sub_stats<-NAACC_tnc %>% 
  group_by(VaName, HUC12,TNC_Priority) %>% 
  summarise(points=n())

NAACC_tnc_cut<-NAACC_tnc %>% 
  filter(TNC_Priority<5)

st_write(NAACC_tnc_cut,"C://Documents and Settings/Erin/Documents/ArcGIS/Projects/NAACC/NAACC_tnc.shp")
st_write(NAACC_tnc,"C://Documents and Settings/Erin/Documents/ArcGIS/Projects/NAACC/NAACC_tnc_all.shp")
