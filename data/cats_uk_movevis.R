cats_uk <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk.csv')
View(cats_uk)

cats_uk_reference <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk_reference.csv')
View(cats_uk_reference)

library(dplyr)
cats_uk_join<-full_join(cats_uk,cats_uk_reference,by="tag_id")

move_felid <- filter(cats_uk_join, 
                     animal_id=="Bear" |
                       animal_id=="Coco" |
                       animal_id=="Binky" |
                       animal_id=="Sid" | 
                       animal_id=="Beanie" |
                       animal_id=="Ebby" |
                       animal_id=="Chloe" )

View(move_felid)

library (move)
library (moveVis)
library (maptools) # shapefiles
library(magrittr)
library(ggplot2)

head(move_felid)

move_felid$dt <-as.POSIXct(strptime(move_felid$timestamp, "%Y-%m-%d %H:%M:%OS", tz ="GMT"),  
                           proj=CRS("+proj=longlat +datum=WGS84 +no_defs"))

head(move_felid)

m <-df2move(move_felid,
            proj=CRS("+proj=longlat +datum=WGS84 +no_defs"),
            x = "location_long", y = "location_lat", 
            time = "timestamp", track_id = "animal_id")

head(m)

# resolution of 1 day (86400seconds) at digit 0 (:00 seconds) per timestamp:
am <- align_move(m, res = 10800, digit = 0, unit = "secs")
unique(unlist(timeLag(am, units = "secs")))

get_maptypes()

frames <- frames_spatial(am, path_legend_title = c("Cat name"), path_colours = NA,
                         map_service = "osm", map_type ="topographic", map_res=1) %>% 
  
  add_labels(x = "Longitude", y = "Latitude", 
             title="Pet Cats UK" , 
             subtitle="Movebank Data Repository") %>% # add some customizations, such as axis labels
  add_northarrow(x = -5.082, y = 50.1482) %>% 
  add_scalebar(x = -5.081, y = 50.1482) %>% 
  add_timestamps(am, type = "label") %>% 
  add_progress()

frames <- add_text(frames, "@fblpalmeira",  x = -5.084, y = 50.148,
                   colour = "grey", size = 3.5)

frames[[30]] # preview one of the frames, e.g. the 10th frame

#suggest_formats()

animate_frames(frames, out_file = "cats_uk_movevis.gif", width = 800, height = 550, res = 95)
