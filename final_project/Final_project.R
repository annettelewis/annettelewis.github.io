# A bunch of different packages here
library(tidyverse)
library(janitor)
library(leaflet) #interactive street maps, this might even be more helpful (it works like google maps and you can add leaflets)
library(easystats)
library(modelr)


# library(plotly)
library(ggmap) # It connects with google earth, it'll be helpful for my work
# look up different packages and give it a shot ()
# sf package and even rgdal might be helpful for this, renderings and tiff files can be done with this
library(sf)
library(GGally)

# data from tornadoes: New Orleans December 2022
# clean data
df <- read.csv("clean_data.csv") %>% 
  janitor::clean_names() %>% 
  mutate(roofsolar = case_when(roofsolar == "SOLAR PANEL" ~ TRUE)) %>% # This column was just "yes" or "no"
  mutate(roofshape = ifelse(roofshascr < 0.80, NA, roofshape)) %>% # I only want to include roofshapes that the computer was highly confident about
  select(-c(roofshascr, roofcondit_discolordetect, roofcondit_discolorscore, roofcondit_discolorpercen, 
          trampscr, roofcondit_tarppercen))

df$rooftopgeo <- gsub("POINT \\(|\\)", "", df$rooftopgeo) # clear unnecessary words to obtain long and lat

df <- df %>% # Lat -> y-axis; Long -> x-axis
  separate(rooftopgeo, into = c("long", "lat"), sep = " ", convert = TRUE) # separate long and lat

df$damage_level <- ifelse(df$damage_level == "", NA, df$damage_level) # Turn blank observations into NA

df %>% 
  select(-buildings_ids, -geometry, -damage_level) %>% 
  ggpairs(cardinality_threshold = 45000)

#----------------------------------------------------------------
# This would potentially allow me to look into geological points: 
song <- read.csv("clean_data.csv") %>% 
  janitor::clean_names() %>% 
  mutate(roofsolar = case_when(roofsolar == "SOLAR PANEL" ~ TRUE)) %>% # This column was just "yes" or "no"
  mutate(roofshape = ifelse(roofshascr < 0.80, NA, roofshape)) %>% # I only want to include roofshapes that the computer was highly confident about
  select(-c(roofshascr, roofcondit_discolordetect, roofcondit_discolorscore, roofcondit_discolorpercen, 
            trampscr, roofcondit_tarppercen))
# song %>% 
#   filter(-buildings_ids, -geometry, -damage_level)) %>%  
#   ggpairs(song, cardinality_threshold = 45000)
#----------------------------------------------------------------
# These are the points with the most damage
mostdamage <- df %>% 
  filter(catastrophescore >= 50)
df$catastrophescore %>% unique()
leaflet(mostdamage) %>% addTiles() %>%
  addPopups(-122.327298, 47.597131, content,
            options = popupOptions(closeButton = FALSE)
  )

damage <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=mostdamage$long, lat=mostdamage$lat) %>% 
  addMarkers(lng=leastdamage$long, lat=leastdamage$lat)
damage  # Print the map

leastdamage <- df %>% 
  filter(catastrophescore < 80)

leastdamage <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=leastdamage$long, lat=leastdamage$lat)
leastdamage


leaflet() %>% 
  addProviderTiles("Stamen.Toner") %>% 
  addPolygons(data = shape, fill = NA, color = "grey") %>% 
  addCircleMarkers(data = points, color = "red", radius = 3)


df$catastrophescore %>% unique()

# This is a reduced version of the dataset for faster loading times
practice <- df[1:20,]
m <- leaflet(data = practice) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=practice$long, lat=practice$lat, popup="Example")
m  # Print the map

# This will take the longest to load (FULL DATASET)
n <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=df$long, lat=df$lat)
n  # Print the map


mod1 <- df %>% glm(formula = catastrophescore ~ long*lat + roofshape, family = "binomial")
# I think my next goal is to separate the damage levels and characterization of the state

mutate

df %>% 
  ggplot(aes(x=b_imgdate, y=roofcondit_structuraldamagepercen, color = damage_level)) +
  geom_point() +
  facet_wrap(~roofmateri)


df$damage_level %>% unique()

roofcondit <- df %>% 
  select(starts_with("roofcondit_"), buildings_ids, damage_level)
names(df$roofcondit) <- c(paste0("visit",1:(ncol(bp)-1)), "pat_id")
#-------example code for what I want to modify (separate roofconditions to )
bp <- df %>% 
  select(starts_with("bp_"), pat_id)
names(bp) <- c(paste0("visit",1:(ncol(bp)-1)), "pat_id")

bp <- bp %>% 
  pivot_longer(starts_with("visit"),
               names_to = "visit",
               names_prefix = "visit",
               names_transform = as.numeric) %>% 
  separate(value, into = c('systolic', 'diastolic'), convert = TRUE)
#---------------------------

roofcondit <- df %>% 
  select(starts_with("roofcondit_"), buildings_ids)
names(bp) <- c(paste0("visit",1:(ncol(bp)-1)), "pat_id")

# This shows correlations of the most damage caused (trying to see if there's any common threads)
ggpairs(mostdamage, columns = , cardinality_threshold = 500)

count <- mostdamage[,16:30]
ggpairs(count, cardinality_threshold = 500)
ggpairs(roofcondit, cardinality_threshold = 40000)




df %>% 
  select(long, lat, roofmateri, roofshape, catastrophescore) %>% 
  ggpairs(cardinality_threshold = 40000)

df %>% 
  ggplot(aes(x=c(lat,long), y=catastrophescore, color = damage_level)) +
  geom_line() +
  facet_wrap(~roofshape)

df_grouped <- df %>%
  group_by(lat, long)

ggplot(df_grouped, aes(x = roofcondit, y = catastrophescore, color = roofshape)) +
  geom_point() +
  facet_wrap(~ roofmateri)





df$b_imgdate %>%  unique()
  

#Replace na values with blank using is.na()
df[is.na()] <- ""

#Display the dataframe
print(my_dataframe)


df$damage_level %>% class()

# first standardize the spatial objects
points <- sf::st_as_sf(points, coords = c("x", "y"), crs = 4326)
shape <- sf::st_transform(shape, crs = 4326)

# and then plot them
leaflet() %>% 
  addProviderTiles("Stamen.Toner") %>% 
  addPolygons(data = shape, fill = NA, color = "grey") %>% 
  addCircleMarkers(data = points, color = "red", radius = 3)

df$roofsolar %>%unique()

df %>% ggmap(rooftopgeo)


# library(GGally)
# ggpairs(df, columns = !c(building_ids, b_imgdate, ), cardinality_threshold = 45000)

df %>%
  leaflet(data = rooftopgeo)

m <- leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12)
m %>% addTiles()

ggmap()




pls <- df[,2:30]
ggpairs(pls, cardinality_threshold = 4500)

pls <- pls %>% select(trampscr, long, lat, roofshascr, roofcondit, 
                      rooftree, catastrophescore, roofcondit_structuraldamagepercen, 
                      roofcondit_missingmaterialpercen, roofcondit_debrispercent, 
                      roofcondit_discolorpercen, roofcondit_discolorscore, damage_level)
names(df)
df %>% select(lat, long, poolarea, trampoline, deck, pool, enclosure, divingboar, waterslide, playground, sportcourt, primarystr, catastrophescore) %>% 
  ggpairs()

song %>% select(rooftopgeo, poolarea, trampoline, deck, pool, enclosure, divingboar, waterslide, playground, sportcourt, primarystr, catastrophescore) %>% 
  ggpairs(cardinality_threshold = 45000)

# group long and lat together
grouped_pls <- pls %>% 
  group_by(long, lat) %>% 
  summarise_all(mean) %>% 
  ungroup()

# plot pairwise scatter plots
df %>% 
  select(-buildings_ids) %>% 
  ggpairs(cardinality_threshold = 4500)


ggpairs(pls, cardinality_threshold = 4500)
