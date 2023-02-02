cats_uk <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk.csv')
cats_uk_reference <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk_reference.csv')
View(cats_uk)
View(cats_uk_reference)

cats_uk_join<-full_join(cats_uk,cats_uk_reference,by="tag_id")
View(cats_uk_join)

y4 <- filter(cats_uk_join, tag_id=="Roger-Tag" |
              tag_id=="Morpheus-Tag" |
              tag_id=="Nettle-Tag" |
              tag_id=="Magic-Tag" |
              tag_id=="Tigger-Tag" |
              tag_id=="Smudge-Tag" |
              tag_id=="Neva-Tag" |
              tag_id=="Moscow-Tag" |
              tag_id=="Friday" |
              tag_id=="Bear-Tag" |
              tag_id=="Carbonel-Tag" |
              tag_id=="Coco" |
              tag_id=="Jago" |
              tag_id=="Pants-Tag" | 
              tag_id=="Jim-Tag" |
              tag_id=="Binky-Tag" |
              tag_id=="Charlie" |
              tag_id=="Sid-Tag" | 
              tag_id=="Beanie-Tag" |
              tag_id=="Gracie-Tag" |
              tag_id=="Ebby-Tag" |
              tag_id=="Chloe-Tag" |
              tag_id=="Bumbles-Tag" |
              tag_id=="Maverick" | 
              tag_id=="Lola" |
              tag_id=="Rusty-Tag" |
              tag_id=="Indie-Tag" )
View(y4)

library(janitor)
y4<-y4[-c(70,142,516,595,718,729,730,
          1156,1157,1161,1162,1712,1809,
          1840,1876,1877,1878,1939,
          1940,1941,1942,1943,1942,
          2054,2228,2339,3022,3289), ]

library(dplyr)
#Rename some columns to run Adehabitat
y4 <- y4 %>%
  rename(id = "animal_id",
         x = "location_long",
         y = "location_lat") 

#Plot locations of all individuals
plot(y4$x~y4$y, col = as.factor(y4$id), pch = 16, 
     xlab="Longitude", ylab="Latitude")

# Calculate the trajectory among locations for each individual
library(adehabitatLT)  
y4.ltraj <- as.ltraj(xy = y4[,c("x", "y")], id = y4$id, typeII=FALSE)
plot(y4.ltraj)
y4.ltraj 

t1<-y4.ltraj[[1]]# The first six locations of the first animal
t2<-y4.ltraj[[2]]
t3<-y4.ltraj[[3]]
t4<-y4.ltraj[[4]]
t5<-y4.ltraj[[5]]
t6<-y4.ltraj[[6]]
t7<-y4.ltraj[[7]]
t8<-y4.ltraj[[8]]
t9<-y4.ltraj[[9]]
t10<-y4.ltraj[[10]]
t11<-y4.ltraj[[11]]
t12<-y4.ltraj[[12]]
t13<-y4.ltraj[[13]]
t14<-y4.ltraj[[14]]
t15<-y4.ltraj[[15]]
t16<-y4.ltraj[[16]]
t17<-y4.ltraj[[17]]
t18<-y4.ltraj[[18]]
t19<-y4.ltraj[[19]]
t20<-y4.ltraj[[20]]
t21<-y4.ltraj[[21]]
t22<-y4.ltraj[[22]]
t23<-y4.ltraj[[23]]
t24<-y4.ltraj[[24]]
t25<-y4.ltraj[[25]]
t26<-y4.ltraj[[26]]
t27<-y4.ltraj[[27]]

y4.ltraj_all<-rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,
                    t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,
                    t21,t22,t23,t24,t25,t26,t27) #combine all trajectories
summary(y4.ltraj_all)

y4.traj<-merge(x=y4,y=y4.ltraj_all,by=c("x","y"),all.x=TRUE,all.y=TRUE)

# Create a dataframe to hold all of the contents of bltu.paths with a column for id. 
# Put first element into the dataframe
total.path.df <- data.frame(y4.ltraj[[1]], id = attr(y4.ltraj[[1]], "id"))
# Use a 'for' loop to fill the larger dataframe with the rest of the trajectories.
for(i in 2:length(y4.ltraj)) {
  total.path.df <- rbind(total.path.df, 
                         data.frame(y4.ltraj[[i]], id = attr(y4.ltraj[[i]], "id")))
}

# Calculate distance travelled per interval and add it to the dataframe
total.path.df$distperweek <- total.path.df$dist / (total.path.df$dt/60/60/24)

# Aggregate to show mean distance per interval for each turtle
path.summary <- aggregate(distperweek~id, data = total.path.df, FUN = mean)
path.summary$sd <- aggregate(distperweek~id, data = total.path.df, FUN = sd)$distperweek

# Look at summmary dataframe
path.summary
summary(path.summary)
# Make a graph to visualize data using ggplot
library(ggplot2)
# Create limits used for error bars in graph
limits <- aes(ymax = path.summary$distperweek + path.summary$sd, 
              ymin = path.summary$distperweek - path.summary$sd)

# Make plot. Choose the dataframe (data) and aesthetics (aes; for the x and y)
path.plot <- ggplot(data = path.summary, aes(x = id, y = distperweek, colour = id)) + 
  geom_point(size = 3) + # add points
  geom_errorbar(limits, width = 0.2) + # adds error bars
  labs(x = "Animal number", 
       y = "Mean distance travelled per week (m)" ) + # Axis labels
  theme_classic() + # Make plot black and white with no background grid
  theme(legend.position = "none")
path.plot # call plot

# Only include three columns (id, x, and y coordinates) for making MCP's
y4.sp <- y4[, c("id", "x", "y")] 

# Create a SpatialPointsDataFrame by defining the coordinates
library(sp)
coordinates(y4.sp) <- c("x", "y")
# Set the coordinate reference system (CRS)
proj4string(y4.sp) <- CRS( "+proj=longlat +ellps=airy +no_defs" )
#proj4string(y4.sp) <- CRS( "+proj=longlat +datum=WGS84 +no_defs")

library(adehabitatHR) # Load library
y4.mcp1 <- mcp(y4.sp, percent = 95)## estimates the MCP 95%
y4.mcp1
plot(y4.mcp1)## Plot the home ranges  MCP 95%
plot(y4.mcp1, col = alpha(1:11, 0.5), add = TRUE)
y4.mcp95<-as.data.frame(y4.mcp1)## Store the home-range size as dataframe
y4.mcp95<- y4.mcp95 %>%
  rename(MCP95 = "area") 

y4.mcp2 <- mcp(y4.sp, percent = 100) ## estimates the MCP 100%
y4.mcp2
plot(y4.mcp2)## Plot the home ranges MCP 100%
plot(y4.mcp2, col = alpha(1:11, 0.5), add = TRUE)
y4.mcp100<-as.data.frame(y4.mcp2)## Store the home-range size as dataframe
y4.mcp100<- y4.mcp100 %>%
  rename(MCP100 = "area") 

y4.mcp<-merge(x=y4.mcp95,y=y4.mcp100,by=c("id"),all.x=TRUE,all.y=TRUE)
summary(y4.mcp)

y4.metrics<-merge(x=path.summary,y=y4.mcp,by=c("id"),all.x=TRUE,all.y=TRUE)

library(scales) # Helps make polygons partly transparent using the alpha argument below
plot(y4.sp, col = as.factor(y4.sp@data$id), pch = 20)
plot(y4.mcp1, col = alpha(1:11, 0.5), add = TRUE)

library(leaflet)
library(leaflet.extras)
library(leafem)
library(htmlwidgets)

labs <- lapply(seq(nrow(y4.mcp1)), function(i) {
  paste0( '<p>', y4.mcp1[i, "id"], '<p></p>', 
          y4.mcp1[i, "area"], '</p>' ) 
})

labs <- lapply(seq(nrow(y4)), function(i) {
  paste0( '<p>', y4[i, "id"], '<p></p>', 
          y4[i, "animal_sex"], '<p></p>',
          y4[i, "age_years"], '<p></p>',
          y4[i, "animal_reproductive_condition"], '<p></p>', 
          y4[i, "timestamp"], '<p></p>', 
          y4[i, "x"],'</p><p>',
          y4[i, "y"], '<p></p>',
          y4[i, "study_name"], '</p>' ) 
})

img <- "http://phylopic.org/assets/images/submissions/f79384d6-2cee-47fb-bdae-25d491f82f9e.512.png"

fct <- factor(y4.mcp1$id)
pal <- colorFactor(ggthemes::gdocs_pal()(5), y4.mcp1$id)
leaflet(y4.mcp1) %>% addTiles() %>% 
  addCircles(lng = ~y4.sp$x, lat = ~y4.sp$y, color = ~pal(y4.sp$id), 
             radius = 1, opacity = 0.2, fillOpacity = .1, group ="Locations",
             popup=paste("<b>Cat name:</b>", y4$id, "<br>",
                         "<b>Animal sex:</b>", y4$animal_sex, "<br>", 
                         "<b>Age years:</b>", y4$age_years, "<br>", 
                         "<b>Reproductive condition:</b>", y4$animal_reproductive_condition, "<br>", 
                         "<b>Timestamp:</b>", y4$timestamp, "<br>", 
                         "<b>Longitude:</b>", y4$x, "<br>",
                         "<b>Latitude:</b>", y4$y, "<br>",
                         "<b>Study name:</b>", y4$study_name)) %>%
  addPolygons(weight = 1, opacity = 0.9, fillOpacity = .4, color = ~pal(id), group="95% MCP",
              popup=paste("<b>Cat name:</b>", y4.mcp1$id, "<br>")) %>%
  addLegend('bottomleft', pal = pal, values = ~id, title = 'Cat Name') %>%
  addLayersControl(overlayGroups = c("id")) %>%
  addResetMapButton() %>% 
  addMeasure(position="topleft", primaryLengthUnit = "meters", primaryAreaUnit = "hectares") %>%
  addLogo(img, position="topleft", width = 70, height = 60) %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
  addProviderTiles(providers$OpenStreetMap, group = "Open Street Map") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") %>%
  addLayersControl(
    baseGroups = c("CartoDB Positron", "Open Street Map", "ESRI World Imagery"),
    overlayGroups = c("Polygon (MCP95%)",  "Locations"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup("Locations") %>% 
  addMiniMap(toggleDisplay = TRUE) 

