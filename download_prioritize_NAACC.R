#Most of the code comes from mhpob (Thanks for your help Mike!)

library(httr)

POST('https://naacc.org/naacc_search_crossing_action.cfm',
     body = list(
       sceReset = 'false',
       stateSelect = 12,
       town = '',
       stream = 'wvhqw4vju2vh',
       watershedID = 143,
       observerID = '',
       coordinatorID = '',
       SurveyId = '',
       CrossingCode = '',
       standardID = '',
       num = '25',
       datasetID = 1,
       lastupdated_from = 'All',
       lastupdated_to = 'All',
       date_observed_from = 'All',
       date_observed_to = 'All',
       Submit =  'Search'
     ))


GET('https://naacc.org/naacc_export_begin.cfm?ds=naacc&r=shapefile')


response <- GET('https://naacc.org/excel/0/shapefile.zip', write_disk(
                (paste0('NAACC_shapefile_',Sys.Date(), '.zip')
                )
))

unzip(file.path(response$content),
      exdir = gsub('.zip', '', file.path(response$content))
)

shapefile_name<-paste0('NAACC_shapefile_',Sys.Date(), '/export.shp')

library(sf)
library(dplyr)
library(readr)

NAACC_points<-st_read(paste0("D://Erin/Documents/ArcGIS/Projects/NAACC/",
                             shapefile_name))


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

NAACC_tnc_cut<-NAACC_tnc %>% 
  filter(TNC_Priority<5)

st_write(NAACC_tnc_cut,
         paste0("D://Erin/Documents/ArcGIS/Projects/NAACC/NAACC_tnc_",Sys.Date(),".shp"),
         append=FALSE)
st_write(NAACC_tnc,
         paste0("D://Erin/Documents/ArcGIS/Projects/NAACC/NAACC_tnc_all_",Sys.Date(),".shp"))

