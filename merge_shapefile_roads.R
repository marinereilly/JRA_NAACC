library(sf)
library(dplyr)
library(readr)

roads<-read_csv("NAACC_road_names.csv")
shp<-st_read(dsn="C://Documents and Settings/Erin/Documents/ArcGIS/Projects/NAACC/NAACC.gdb",
             layer = "NAACC_cut2")

all_data<-full_join(shp,roads) %>% 
  select(-X1,-OBJECTID)
st_write(all_data, "C://Documents and Settings/Erin/Documents/ArcGIS/Projects/NAACC/unsurveyed_rds.shp")


subwatersheds<-st_read("C://Documents and Settings/Erin/Documents/ArcGIS/vanwbd6p_v6.shp") %>% 
  st_transform(., crs=st_crs(all_data)) %>% 
  select(HUC12, geometry)

new_data<-st_join(all_data, subwatersheds)

names<-foreign::read.dbf("C://Documents and Settings/Erin/Documents/ArcGIS/master12_v6.dbf", as.is=FALSE) %>% 
  select(HUC12, VaName, FedName)

sub_data<-left_join(new_data,names)
st_write(sub_data,"C://Documents and Settings/Erin/Documents/ArcGIS/Projects/NAACC/NAACC_rd_ws.shp") 
