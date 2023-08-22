library(tidyverse)
library(janitor)
library(leaflet) #interactive street maps, this might even be more helpful (it works like google maps and you can add leaflets)
library(easystats)
library(modelr)
library(ggmap)
library(sf)
library(GGally)
library(plotly)
library(kableExtra)
library(patchwork)
library(stringr)
library(htmltools)
library(devtools)
library(htmlwidgets)
library(webshot)
# I NEED TO HAVE A MYFUNCTIONS FOLDER IF I WANT TO USE IT LIKE THIS :/
# source("../myfunctions.R") # allows to pull functions from another script (I could use this, but I would want to change my markdown file to do this...)
theme_set(theme_minimal())
# sf package and even rgdal might be helpful for this, renderings and tiff files can be done with this
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
df$roofshape <- factor(df$roofshape, levels = c("gable", "hip", "flat"))
levels_roofmateri <- c("metal", "shingle", "membrane", "shake", "tile")
df$roofmateri <- factor(df$roofmateri, levels = c("gravel", levels_roofmateri))
df$roofmateri <- factor(df$roofmateri, levels = levels_roofmateri)


colors <- c("gray", "blue", "#990099", "#990066", "#CC0099", "#CC0066", "#CC0033", "red")

color_map <- setNames(colors, c(NA, "FEMA 0 / Minor", "FEMA 1 / Moderate", "FEMA 2 / Moderate", "FEMA 3 / Moderate",  "FEMA 4 / Major", "FEMA 5 / Major", "FEMA 6 / Destroyed"))

df %>%
  ggplot(aes(x=long, y=catastrophescore, color = damage_level)) +
  geom_point() + facet_wrap(~roofshape) +
  labs(title = "Roof Shapes", x = "Longitude", y = "Catastrophe Score", color = "Damage Level") +
  scale_color_manual(values = color_map)
ggsave("./maps/roof_shapes.png", width = 6.75, height = 4.5, dpi = 300)

df %>%
  ggplot(aes(x=long, y=catastrophescore, color = damage_level)) +
  geom_point() + facet_wrap(~roofmateri) +
  labs(title = "Roof Materials", x = "Longitude", y = "Catastrophe Score", color = "Damage Level") +
  scale_color_manual(values = color_map)
ggsave("./maps/roof_materials.png", width = 6.75, height = 4.5, dpi = 300)

# df %>%
#   kable() %>%
#   kable_classic(lightable_options = "hover") %>%
#   scroll_box(height = "200px")

# df %>%
#   mutate(building_id_short = substr(buildings_ids, 1, 30)) %>%
#   select(building_id_short, everything()) %>%
#   kable() %>%
#   kable_classic(lightable_options = "hover") %>%
#   scroll_box(height = "200px")

df %>%
  mutate(building_id_short = substr(buildings_ids, 1, 30)) %>%
  select(building_id_short, everything()) %>%
  kable() %>%
  kable_classic(lightable_options = "hover") %>%
  scroll_box(height = "200px")

# Define damage categories for mapping
mostdamage <- df %>% filter(catastrophescore >= 50)
nodamage <- df %>% filter(catastrophescore == 0)
decimated <-df %>% filter(catastrophescore == 100)
middamage <- df %>% filter(catastrophescore < 50 & catastrophescore >= 15)
leastdamage <- df %>% filter(catastrophescore < 15 & catastrophescore >= 2)
minimaldamage <- df %>% filter(catastrophescore == 1)

# I COULD PROBABLY GET RID OF THIS 
# Function for adding labels (it seems like popup is the only option for circleMarkers, at least out of what I tried)
create_popup <- function(data) {
  paste("<b>Location</b><br>",
        "&nbsp;&nbsp;&nbsp;Longitude: ", data$long, "<br>",
        "&nbsp;&nbsp;&nbsp;Latitude: ", data$lat, "<br>",
        "<b>Catastrophe Score</b><br>",
        "&nbsp;&nbsp;&nbsp;Score: ", data$catastrophescore, "<br>",
        "<b>Roof Shape</b><br>",
        "&nbsp;&nbsp;&nbsp;Shape: ", str_to_title(data$roofshape), "<br>",
        "<b>Roof Material</b><br>",
        "&nbsp;&nbsp;&nbsp;Material: ", str_to_title(data$roofmateri), "<br>")
}

# # Function for root median square error
r_med_sq_err <- function(model, absolute = FALSE){ # adding in the option for an absolute error
  if(sum(class(model) %in% c("glm","lm")) > 0){
    if(absolute == TRUE){
      median(abs(residuals(model)))
    }
    sqrt(median(residuals(model)^2))
  } else {
    if(class(model) == "list"){
      stop("Did you provide a list of models? Use map() instead.")
    }
    stop("'model' must be either a glm or lm object.")
  }
}

# Maps: First, representing data points
#### In my maps, I probably want to use a legend for the ones that show multiple points
alldamage <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng=mostdamage$long, lat=mostdamage$lat, color = "red", radius = 2, popup = create_popup(mostdamage)) %>%
  addCircleMarkers(lng=middamage$long, lat=middamage$lat, color = "orange", radius = 2, popup = create_popup(middamage)) %>%
  addCircleMarkers(lng=leastdamage$long, lat=leastdamage$lat, color = "blue", radius = 2, popup = create_popup(leastdamage)) %>%
  addCircleMarkers(lng=nodamage$long, lat=nodamage$lat, color = "gray", radius = .5, popup = create_popup(nodamage))

# # Save as HTML file
# saveWidget(alldamage, "alldamage_map.html", selfcontained = FALSE)

# # Generate the iframe tag
# iframe_tag <- tags$iframe(src = "alldamage_map.html", width = "100%", height = 500)

# # Display the iframe tag
# iframe_tag

damage <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng=mostdamage$long, lat=mostdamage$lat, color = "red", radius = 2, popup = create_popup(mostdamage)) %>% 
  addCircleMarkers(lng=middamage$long, lat=middamage$lat, color = "orange", radius = 2, popup = create_popup(middamage)) %>% 
  addCircleMarkers(lng=leastdamage$long, lat=leastdamage$lat, color = "blue", radius = 2, popup = create_popup(leastdamage))

alldamage
damage

# Individual damage categories
high <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng = mostdamage$long, lat = mostdamage$lat, color = "red", radius = 2, 
                   popup = create_popup(mostdamage))

mid <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng=middamage$long, lat=middamage$lat, color = "orange", radius = 2,
                   popup = create_popup(middamage))

least <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng=leastdamage$long, lat=leastdamage$lat, color = "blue", radius = 2,
                   popup = create_popup(leastdamage))

none <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng=nodamage$long, lat=nodamage$lat, color = "gray", radius = .5,
                   popup = create_popup(nodamage))

destroyed <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng=decimated$long, lat=decimated$lat, color = "maroon", radius = 2,
                   popup = create_popup(decimated))

high
mid
least
none
destroyed

# Models:
mod1 <- df %>% glm(formula = catastrophescore ~ long + lat + rooftree + roofshape, family = "gaussian")
mod2 <- df %>% glm(formula = catastrophescore ~ long + lat + roofshape + rooftree + roofmateri, family = "gaussian")
mod3 <- df %>% glm(formula = catastrophescore ~ long + lat + trampoline + deck + pool + enclosure + divingboar + waterslide + playground + sportcourt + primarystr + roofsolar + rooftree + roofmateri * roofshape, family = "gaussian")
mod3 <- df %>% glm(formula = catastrophescore ~ long + lat + enclosure + roofmateri + roofsolar + rooftree + roofshape, family = "gaussian")
mod4 <- df %>% glm(formula = catastrophescore ~ long + lat + roofshape + rooftree + roofmateri + trampoline * deck * pool * enclosure * divingboar * waterslide * playground * sportcourt * primarystr * roofsolar, family = "gaussian")
mod5 <- df %>% glm(formula = catastrophescore ~ long + lat + trampoline * deck * pool * enclosure * divingboar * waterslide * playground * sportcourt * primarystr * roofsolar + roofmateri + roofshape + rooftree, family = "gaussian")

compare_performance(mod1, mod2, mod3, mod4, mod5) %>% plot()
compare_performance(mod3, mod4, mod5) %>% plot()

# check_model(mod1)
# check_model(mod2)
check_model(mod3) # shows a decent level of collinearity.... I don't know though fam...
check_model(mod4)
check_model(mod5)

# It might be worth putting this in my cleaning code:
  # this doesn't fix my rmse values after 3 (it actually even just makes it worse)
df$roofmateri <- factor(df$roofmateri, levels = c("metal", "shingle", "membrane", "shake", "tile", "gravel"))
df$roofshape <- factor(df$roofshape, levels = c("gable", "hip", "flat"))
# 
# # create a factor with the same levels as in the model
df$roofmateri <- factor(df$roofmateri, levels = levels(mod3$model$roofmateri))

# NEXT step is to figure out what's going wrong with this...
rmse(mod1, df)
rmse(mod2, df)
rmse(mod3, df) # Here is where I encounter an error (need to fix up the factors, but it needs to fit in for the other models I have)
rmse(mod4, df)
rmse(mod5, df)
rmse(mod6, df)

# Median square error for all of the models:
r_med_sq_err(mod1)
r_med_sq_err(mod2)
r_med_sq_err(mod3) # appears to have the lowest med sqr error out ove everything
r_med_sq_err(mod4)
r_med_sq_err(mod5)

compare_performance(mod1, mod2, mod3, mod4, mod5) %>% plot()
compare_performance(mod4, mod5, mod6) %>% plot()
compare_performance(mod3, mod4, mod5, mod6) %>% plot()

compare_performance(mod1, mod2, mod3, mod4, modm1) %>% plot() # adding chaos here...

# Now, this is a basic setup for testing models ;)
testing <- sample(1:nrow(df), size = round(nrow(df)*.2))

#Now, if I just reallly messed wiht this... I could get this:
messing <- df %>% filter(df$catastrophescore != 0) # I actually probably do want to use this.... 

library(GGally)

# may be even worth making some models for even just the prescence of damage and not jsut its magnitude... there is so much that isn't accounted for
messing %>% 
  select(-buildings_ids) %>% 
  ggpairs()

# This shows another representation of the data points (map w/o leaflet)
# I would want to change the colors (they don't match well)
messing %>% 
  ggplot(aes(x=long, y=lat, color = damage_level)) +
  geom_point()
# based on long and lat, respectively. i would want to remove the NA
df %>% 
  ggplot(aes(x=long, y=catastrophescore, color = damage_level)) +
  geom_point() + facet_wrap(~roofmateri)

df %>% 
  ggplot(aes(x=lat, y=catastrophescore, color = damage_level)) +
  geom_point() + facet_wrap(~roofmateri)

df %>% 
  ggplot(aes(x=long, y= damage_level, color = catastrophescore) +
           geom_point() + facet_wrap(~roofmateri) +
           labs(title = "Roof materials", x = "Longitude", y = "Catastrophe Score") +
           scale_color_gradient(low = "blue", high = "red")
         

         
#assume appropriate colors
         library(RColorBrewer)
         
         # Set colors for each category
colors <- c("gray", "blue", "#990099", "#990066", "#CC0099", "#CC0066", "#CC0033", "red")
         
         # Create a named vector to map colors to categories
color_map <- setNames(colors, c(NA, "FEMA 0 / Minor", "FEMA 1 / Moderate", "FEMA 2 / Moderate", "FEMA 3 / Moderate",  "FEMA 4 / Major", "FEMA 5 / Major", "FEMA 6 / Destroyed"))
         
         # Use scale_color_manual to specify the color scale
         df %>%
           ggplot(aes(x=long, y=catastrophescore, color = damage_level)) +
           geom_point() + facet_wrap(~roofmateri) +
           labs(title = "Roof materials", x = "Longitude", y = "Catastrophe Score", color = "Damage Level") +
           scale_color_manual(values = color_map)
         
         
        
         df %>%
           mutate(damage_level_numeric = as.numeric(damage_level)) %>%
           ggplot(aes(x=long, y=catastrophescore, color = damage_level_numeric)) +
           geom_point() + facet_wrap(~roofmateri) +
           labs(title = "Roof materials", x = "Longitude", y = "Catastrophe Score") +
           scale_color_gradient(low = "blue", high = "red", na.value = "gray")
         

df %>% 
  ggplot(aes(x=lat, y=catastrophescore, color = damage_level)) +
  geom_point() + facet_wrap(~roofshape)

df %>% 
  ggplot(aes(x=rooftree, y=catastrophescore, color = damage_level)) +
  geom_point()

df %>% 
  filter(na.rm(df$poolarea)) %>% 
  ggplot(aes(x=poolarea, y=catastrophescore)) +
  geom_point()

df %>% 
  ggplot(aes(x=catastrophescore, y=roofcondit_missingmaterialpercen, color = damage_level)) +
  geom_point()

messing %>% 
  ggplot(aes(x=catastrophescore, y=roofcondit_structuraldamagepercen, color = damage_level)) +
  geom_point()

# messing %>% 
#   ggplot(aes(x=rooftree, y=roofcondit_debrispercent, color = damage_level)) +
#   geom_point()

testing <- (1:nrow(messing), size = round(nrow(messing)*.2))


test <- df[testing,]
train <- df[-testing,]
# Actually not horrible... it def gets messy when i add more points that likely dont relate
modm1 <- glm(messing, family = gaussian, formula = catastrophescore ~ roofmateri + roofshape + long + lat + roofsolar + trampoline + rooftree + waterslide + deck + pool + divingboar + playground + primarystr + enclosure + sportcourt)
check_model(modm1)
names(messing)
performance(modm1)
# mod1 <- glm(data = train,
#             formula = mod1$formula)
# mod2 <- glm(data = train,
#             formula = mod2$formula)
mod3 <- glm(data = train,
            formula = mod3$formula) # let's see what this does....
# mod4 <- glm(data = train,
#             formula = mod4$formula)
# mod5 <- glm(data = train,
#             formula = mod5$formula)

# I probably want to make a graph... 
df %>% 
  # filter(catastrophescore != 0) %>% 
  ggplot(aes(x=rooftree, y=catastrophescore, color = damage_level)) +
  geom_point() +
  facet_wrap(~roofmateri)

df %>% 
  ggplot(aes(x=lat, catastrophescore, color = )) +
  geom_point()
unique(df$damage_level)
#---------------------------
roofcondit <- df %>% 
  select(starts_with("roofcondit_"), buildings_ids)
names(bp) <- c(paste0("visit",1:(ncol(bp)-1)), "pat_id")

# df %>% 
#   ggplot(aes(x=c(lat,long), y=catastrophescore, color = damage_level)) +
#   geom_line() +
#   facet_wrap(~roofshape)

# This is funtional, but I would actually need to find something that works better
df_grouped <- df %>%
  group_by(lat, long)

ggplot(df_grouped, aes(x = roofcondit, y = catastrophescore, color = roofshape)) +
  geom_() +
  facet_wrap(~ roofmateri)

# pls <- df[,2:30]
# 
#  group long and lat together
# grouped_pls <- pls %>% 
#   group_by(long, lat) %>% 
#   summarise_all(mean) %>% 
#   ungroup()

# Give a story from the data and present it as a report (with background and why its interesting, talk about data, talk about conclusion, use data and point)

# Predictions:

# I'm planning to add predictions and additional plots for the completed final project.

# Interpretations of predictions and models used:

# The predictions: 

pred <- add_predictions(df, mod, type = "response")

#adding tidy in front of gather predictons like this or something... im not sure...
gather_predictions(tidy,mod1,mod2,mod3)
df %>% add_predictions(mod1, mod2, mod3)

add_predictions(tidy, mod1, mod2, mod3) %>%
  ggplot(aes(x=gpa, y=pred, color =admit)) + 
  geom_point(size = 3, alpha =.5)

add_residuals(test, mod2) %>% 
  pluck("resid") %>% 
  .^2 %>% 
  mean(na.rm = TRUE) %>% 
  sqrt()
rmse(mod) # Same thing as above, but just for the new testing prediction :)

# Example of analysis I could do
df <- add_predictions(penguins, mod)
df <- add_residuals(df,mod)
# to decide if the model fits well with reality (check model allows us to check the model assumptions)
# use easystats
library(easystats)
performance(mod) # the R2 value is pretty high (about 270g off)
# RMSE is only useful in comparing with other models
# The R2 of 0.885 is only on model 2, but this value is based on main information

# Make a new penguin
# This is for if you find a new observation/do another experiment with the same base as the model
# make a new dataframe
names(penguins) 

new_penguin <- data.frame(sex = "male", 
                          species = "Gentoo",
                          island = "Biscoe",
                          bill_length_mm = 1000,
                          bill_depth_mm = 1000,
                          flipper_length_mm = 1)
add_predictions(new_penguin, model = mod)
# Here, we are extrapolating. We are predicting data that's outside of the scope of our model
# The largest body mass in the model is 6300g but our prediction says the penguin is going to be 93044g
# Make sure that the predictions that I am feeding in match the values I've already given to the model
# The model is only good within its limited scope

# Crossvalidation (remove some of the data, take it away, and save it, comparing it to the testing dataset)
# Testing dataset gets set aside until the end
# Training dataset is for model building

penguins # we want to randomly choose 20% of the rows (testing dataset)
set.seed(69) # allowed us to have the same random selection in class

add_residuals(test, mod2) %>% 
  pluck("resid") %>% 
  .^2 %>% 
  mean(na.rm = TRUE) %>% 
  sqrt()
rmse(mod2, df) # Same thing as above, but just for the new testing prediction :)

# Which is the model that we should report in the model
# How believable is our model based on data it hasn't seen before
# performance(mod2, newdata = test) there was a mistake in this
rsquare(mod2,test) # test model: the model is this good when we see new things
rsquare(mod, penguins)# full model 

performance(mod)

list(mod1, mod2, mod3, mod4) %>% 
  map_dbl(r_med_sq_err) # dbl gives the four models, like dbl, gives the rmse for everything
source("../myfunctions.R") # allows to pull functions from another script


mods1 <- glm(formula = catastrophescore ~ roofshape + rooftree + enclosure, data = extra, family = gaussian(link = "identity"))
mods2 <- glm(formula = catastrophescore ~ roofshape + enclosure + rooftree, data = extra, family = gaussian(link = "identity"))
mods3 <- glm(formula = catastrophescore ~ roofshape + enclosure + roofmateri, data = extra, family = gaussian(link = "identity"))
mods4 <- glm(formula = catastrophescore ~ roofmateri, data = extra, family = gaussian(link = "identity"))
mods5 <- glm(formula = catastrophescore ~ roofmateri + rooftree + enclosure, data = extra, family = gaussian(link = "identity"))
compare_performance(mods1, mods2, mods3, mods4, mods5) %>% plot()
compare_performance(mods1, mods2, mods3, mods4, mods5)





# Coinflip
rbinom(prob = 0.5, size =100, n =10) %>% # 100 trials, 10 times each
  hist() # and this shows it


#---------------------------------------------------
# Even with the removal of catastrophe scores equal to 0, there is still a 
 # significant skew with most catastrophe scores being between 2 and 25.
# This is shown in the representative histogram:
extra %>% 
  ggplot(aes(x= catastrophescore)) + 
  geom_histogram()
# In an effort to combat this issue, I decided to use the inverse gaussian family
mods1 <- glm(formula = catastrophescore ~ long + roofshape + rooftree + enclosure, 
             data = extra, family = inverse.gaussian(link = "identity"))
mods2 <- glm(formula = catastrophescore ~ long + roofmateri + enclosure + rooftree, 
             data = extra, family = inverse.gaussian(link = "identity"))
mods3 <- glm(formula = catastrophescore ~ long + roofshape + roofmateri + enclosure + roofmateri, 
             data = extra, family = inverse.gaussian(link = "identity"))
mods4 <- glm(formula = catastrophescore ~ long + enclosure + rooftree + roofshape + roofmateri, 
             data = extra, family = inverse.gaussian(link = "identity"))
mods5 <- glm(formula = catastrophescore ~ long + roofshape + roofmateri + rooftree, 
             data = extra, family = inverse.gaussian(link = "identity"))
#-----
mods1 <- glm(formula = catastrophescore ~ long + roofshape + rooftree + enclosure, data = extra, family = gaussian(link = "identity"))
mods2 <- glm(formula = catastrophescore ~ long + roofshape + enclosure + rooftree, data = extra, family = gaussian(link = "identity"))
mods3 <- glm(formula = catastrophescore ~ long + roofshape + enclosure + roofmateri, data = extra, family = gaussian(link = "identity"))
mods4 <- glm(formula = catastrophescore ~ long + roofmateri, data = extra, family = gaussian(link = "identity"))
mods5 <- glm(formula = catastrophescore ~ long + roofmateri + rooftree + enclosure, data = extra, family = gaussian(link = "identity"))
#-------

# In comparing these updated models, it appears that 4 works best
compare_performance(mods1, mods2, mods3, mods4, mods5) %>% plot()
compare_performance(mods1, mods2, mods3, mods4, mods5)

check_model(mods1) # homogeneity falls off (not flat and horiz.) and normality of residuals doesn't match line
check_model(mods2) # basically same deal
check_model(mods3)
check_model(mods4) # Follows decently well, but has an inherent issue in the normality fo residuals (not following line)
check_model(mods5)

check_model(mods4)

```{r}
summary(mods4)
performance(mods4)
summary(pred)


#Root median squared error for Model 3
```{r}
r_med_sq_err(mods4)

```{r, fig.align='center'}
prediction <- add_predictions(extra, mods4, type = "response") %>% select(pred)
# extra %>% add_column(prediciton)

pred <- add_predictions(extra, mods4, type = "response") %>% select(pred)
extra <- extra %>% add_column(pred)

comparison <- extra %>% select(catastrophescore, pred)


p1 <- ggplot(extra, aes(x = long, y = lat, color = as.numeric(unlist(pred)))) +
  geom_point() +
  geom_point(data = extra, aes(x = long, y = lat)) +
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "Predicted Points",
       color = "Predicted Catastrophe Score") +
  scale_color_gradient(low = "blue", high = "red", limits = c(0, 100))

p2 <- ggplot(extra, aes(x = long, y = lat, color = as.numeric(catastrophescore))) +
  geom_point() +
  geom_point(data = extra, aes(x = long, y = lat)) +
  scale_color_gradient(low = "blue", high = "red", limits = c(0, 100)) +
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "Actual Points",
       color = "Catastrophe Score")
p1/p2