library(dplyr)
library(stringr)
library(readr)
library(sf)


hr<-read_csv("VDOTprojectlist_hamptonrds.csv")

hr_filter<-filter(hr, grepl("BRIDGE|CULVERT",DESCRIPTION))

rich<-read_csv("VDOTprojectlist_richmond.csv")
rich_filter<-filter(rich,grepl("BRIDGE|CULVERT",DESCRIPTION))

vdot_projects<-rbind(hr_filter,rich_filter)
write.csv(vdot_projects,"vdot_projects.csv")


vdot<-read.csv("VDOT Bridges and Culverts.csv") %>% 
  rename(Latitud=y, Longitd=x)
vdot_sf<-st_as_sf(vdot, coords = c("Longitd","Latitud"), crs=3857) %>% 
  st_transform(., crs=st_crs(NAACC_points))
write_sf(vdot_sf,"C://Documents and Settings/Erin/Documents/ArcGIS/Projects/NAACC/vdot_culverts.shp")

NAACC_points<-st_read("C://Documents and Settings/Erin/Documents/ArcGIS/Projects/NAACC/NAACC_rd_ws.shp") %>% 
  filter(is.na(Odm_cmm) | Odm_cmm != -1)
  

try_data<-st_join(NAACC_points,vdot_sf,left=FALSE)

culverts<-




sub_stats<-NAACC_points %>% 
  group_by(VaName) %>% 
  summarise(points=n())

