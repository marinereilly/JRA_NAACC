install.packages("revgeo")

library(dplyr)
library(revgeo)
library(readr)

start <- Sys.time()
data<-read_csv("lower_james_NAACC - split.csv") %>% 
  rename(latitude=Latitude, longitude=Longitud) 

data_all = data.frame()

while (nrow(data)>0) {
  data_sub <-  data[1:200,]
  latlong <- data_sub %>% 
    select(latitude, longitude) %>% 
    mutate(index=row_number())


result <- revgeo2(latlong$longitude, latlong$latitude,
                 provider =  'photon',
                 output="frame") %>%
  mutate(index = row_number()) %>% 
  left_join(latlong, by="index") %>% 
  select(street, city,latitude,longitude)

rm(latlong)

data_new <- data_sub %>% 
  left_join(result, by=c("latitude","longitude")) 

data_all <- rbind(data_all,data_new) %>% 
  na.omit()

data <- anti_join(data, data_sub, by=c("OBJECTID"))
print(nrow(data))

rm(data_sub)
rm(data_new)
rm(result)

print('Sleeping for 10 seconds')
Sys.sleep(10)

}

End <- Sys.time()

time_elapsed<-start-End
print(time_elapsed)

write.csv(data_all, "NAACC_road_names.csv")
