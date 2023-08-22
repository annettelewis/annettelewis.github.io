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
library(car)
# source("../myfunctions.R") # I NEED TO HAVE A MYFUNCTIONS FOLDER IF I WANT TO USE IT LIKE THIS :/ # allows to pull functions from another script (I could use this, but I would want to change my markdown file to do this...)
theme_set(theme_minimal())

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

extra <- df %>% filter(catastrophescore != 0)

df %>% filter(catastrophescore < 15 & catastrophescore >= 2) %>% select(catastrophescore)
extra %>% select(catastrophescore) %>% summary()

# # Get rid of primary structure:
# noprim <- df %>% 
#   filter(primarystr != FALSE)

#----------------------
# Filter based on a specific catastrophe score (shortcut) roofmateri and cat score () cat score 60 to 80 (mess with it)
mods1 <- glm(formula = catastrophescore ~ long + lat + enclosure + rooftree + roofshape, data = extra, family = "gaussian")
mods2 <- glm(formula = catastrophescore ~ long + lat + enclosure + roofmateri + rooftree + roofshape, data = extra, family = "gaussian")
mods3 <- glm(formula = catastrophescore ~ long + lat + roofmateri, data = extra, family = "gaussian")
mods4 <- glm(formula = catastrophescore ~ long + lat, data = extra, family = "gaussian")
mods5 <- glm(formula = catastrophescore ~ long + roofmateri, data = extra, family = "gaussian")
mods6 <- glm(formula = catastrophescore ~ long + lat + roofmateri + enclosure + rooftree, data = extra, family = "gaussian")
mods7 <- glm(formula = catastrophescore ~ roofmateri, data = extra, family = "gaussian")


# Fit quadratic polynomial model
model <- glm(formula = catastrophescore ~ poly(long + lat + roofmateri, 2), data = extra, family = gaussian(link = "identity"))

# Calculate the mean of the roofmateri variable
# Remove rows with missing values
extra <- na.omit(extra)

# Fit the model with separate quadratic terms
model2 <- glm(formula = catastrophescore ~ poly(long, 2) + poly(lat, 2) + poly(roofshape, 2) + poly(rooftree, 2) + poly(enclosure, 2) + poly(I(roofmateri^2), 2), data = extra, family = gaussian(link = "identity"))


extra <- na.omit(extra)
model <- glm(formula = catastrophescore ~ poly(long, 2) + poly(lat, 2) + poly(I(roofmateri^2), 2), data = extra, family = gaussian(link = "identity"))
# model2 <- glm(formula = catastrophescore ~ poly(long, 2) + poly(lat, 2) + poly(roofshape, 2) + poly(rooftree, 2) + poly(enclosure, 2) + poly(I(roofmateri^2), 2), data = extra, family = gaussian(link = "identity"))
# # Calculate the VIF values for the predictor variables
# library(car)
# vif(model)
# roofmateri + enclosure + rooftree

mods1 <- glm(formula = catastrophescore ~ long + roofshape + rooftree + enclosure, data = extra, family = gaussian(link = "identity"))
mods2 <- glm(formula = catastrophescore ~ long + roofshape + enclosure + rooftree, data = extra, family = gaussian(link = "identity"))
mods3 <- glm(formula = catastrophescore ~ long + roofshape + enclosure + roofmateri, data = extra, family = gaussian(link = "identity"))
mods4 <- glm(formula = catastrophescore ~ long + roofmateri, data = extra, family = gaussian(link = "identity"))
mods5 <- glm(formula = catastrophescore ~ long + roofmateri + rooftree + enclosure, data = extra, family = gaussian(link = "identity"))
compare_performance(mods1, mods2, mods3, mods4, mods5) %>% plot()
compare_performance(mods1, mods2, mods3, mods4, mods5)

# check_model(mods1) # Kinda matches, some irregularities
# check_model(mods2) # fucked homogeneity
check_model(mods3) # not horrible, but linearity and homogeneity of variance don't fit too well
# check_model(mods4) # Actually kinda works.. but very questionable
check_model(mods5) # might work
vif(mods5)
check_model(mods3, type = "pearson")
check_model(mods5, type = "pearson")


pred <- add_predictions(extra, mods5, type = "response") %>% select(pred)
extra <- extra %>% add_column(pred)

comparison <- extra %>% select(catastrophescore, pred)

# Add the predictions to the data frame)

# Side by side predicted damage scores in comparsion to the model (not working)
ggplot(extra, aes(x = long, y = lat, color = as.numeric(unlist(pred)))) +
  geom_point() +
  geom_point(data = extra, aes(x = long, y = lat)) +
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "Predicted Points",
       color = "Predicted Catastrophe Score") +
  scale_color_gradient(low = "blue", high = "red", limits = c(0, 100)) +
ggplot(extra, aes(x = long, y = lat, color = as.numeric(catastrophescore))) +
  geom_point() +
  geom_point(data = extra, aes(x = long, y = lat)) +
  scale_color_gradient(low = "blue", high = "red", limits = c(0, 100)) +
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "Actual Points",
       color = "Catastrophe Score")

library(plotly)

# create the ggplot object
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
subplot(p1, p2, nrows = 1)
# convert the ggplot object to a plotly object
library(plotly)
library(ggpl




library(plotly)

# plot for predicted values
p1 <- ggplot(extra, aes(x = long, y = lat, color = as.numeric(pred4))) +
  geom_point() +
  geom_point(data = extra, aes(x = long, y = lat)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "Predicted points")

# plot for actual values
p2 <- ggplot(extra, aes(x = long, y = lat, color = as.numeric(catastrophescore))) +
  geom_point() +
  geom_point(data = extra, aes(x = long, y = lat)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "Actual points")

# create subplot with two plots side by side

plotly::add_heatmap(x=extra$long, y=extra$lat)




extra$pred4 <- as.numeric(extra$pred4)



p2 <- ggplot(extra, aes(x = long, y = lat, color = as.numeric(catastrophescore))) +
  geom_point() +
  geom_point(data = extra, aes(x = long, y = lat)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "Actual points")

p1 + p2




library(ggplot2)
library(gridExtra)

# Create prediction plot
p1 <- ggplot(extra, aes(x = long, y = lat, color = pred4)) +
  geom_point() +
  geom_line() +
  geom_point(data = extra, aes(x = long, y = lat), color = "blue") +
  scale_color_hue(h = c(0, 180))

# Create catastrophe score plot
p2 <- ggplot(extra, aes(x = long, y = lat, color = as.numeric(catastrophescore))) +
  geom_point() +
  geom_point(data = extra, aes(x = long, y = lat), color = "orange") +
  scale_color_gradient(low = "blue", high = "red")

# Combine plots using grid.arrange()
grid.arrange(p1, p2, ncol=2, widths=c(3, 1))








ggplot(aes(x=long, y=lat, color = damage_level)) +
  geom_point()
# Print the summary of the model
summary(model)
check_model(model)
vif(model)
alias(model)

Fit the model without the aliased variable
model <- lm(catastrophescore ~ long + roofshape + rooftree, data = extra)






# Check for collinearity using vif()
library(car)
vif(model)
summary(model)
summary(model1)
summary(model2)
summary(mods1)
summary(mods2)
summary(mods3)
summary(mods4)
summary(mods5)
summary(mods6)

alias(model1)
alias(model2)
alias(mods1)
alias(mods2)
alias(mods3)
alias(mods4)
alias(mods5)
alias(mods6)

cor(model1)

# Extract predictor variables from data frame
predictors <- extra[, c("long", "roofshape", "rooftree", "enclosure", "roofmateri")]

# Calculate correlation matrix
cor(predictors)
var(predictors)





names(extra)
compare_models(mods1, mods2,mods3, mods4,mods5, mods6) %>% plot()
check_model(mods2)
check_model(mods6)

rmse(mods1, extra)
rmse(mods2, extra)
rmse(mods3, extra)
rmse(mods4, extra)
rmse(mods5, extra)
rmse(mods6, extra)


#-------------------------------------------
mods1 <- extra %>% glm(formula = catastrophescore ~ long + lat + enclosure + roofmateri + roofsolar + rooftree + roofshape, family = "gaussian")#--------------------

# Median square error for all of the models:
r_med_sq_err(mod1)
r_med_sq_err(mod2)
r_med_sq_err(mod3) # appears to have the lowest med sqr error out ove everything
r_med_sq_err(mod4)
r_med_sq_err(mod5)

head(df)

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







# The predictions: 
pred4 <- add_predictions(df, mod4, type = "response") %>% select(pred, catastrophescore) 
pred5 <- add_predictions(df, mod5, type = "response") %>% select(pred, catastrophescore)

head(pred4)

head(pred5)

#adding tidy in front of gather predictons like this or something... im not sure...
gather_predictions(mod4,mod5)

testing <- sample(1:nrow(df), size = round(nrow(df)*.2))
test <- df[testing,]
train <- df[-testing,]

# mod4: the full model has a higher R2 value than the test (as expected)
rsquare(mod4,test) # test model
rsquare(mod4, df) # full model 

# mod5: the full model has a higher R2 value than the test (as expected)
rsquare(mod5,test) # test model
rsquare(mod5, df) # full model

performance(mod1)
performance(mod2)
performance(mod3) # mod3 appears to work well, but the issue of collinearity negatively impacts my view of the model

compare_performance(mod1, mod2, mod3) %>% plot # Here, it appears that mod 3 accounts for more of what is seen, this will need to be analyzed further

# Comment: Out of the three models, mod2 appears to be the most accurate as it accounts for the variables year and continent, while not having a high amount of collinearity (unlike mod3) 

# 9. Plot the 3 models’ predictions like so: (10 pts)
mod1_pred <- add_predictions(df, model = mod1) %>% select("pred")
mod2_pred <- add_predictions(df, model = mod2) %>% select("pred")
mod3_pred <- add_predictions(df, model = mod3) %>% select("pred")
pred_df <- bind_cols(mod1_pred, mod2_pred, mod3_pred)

df <- bind_cols(df, pred_df) %>% 
  rename(mod1_pred = pred...6) %>% 
  rename(mod2_pred = pred...7) %>% 
  rename(mod3_pred = pred...8) %>% 
  select(-ends_with("...9"), -ends_with("...10"), -ends_with("...11")) %>% 
  pivot_longer(cols = c(mod1_pred, mod2_pred, mod3_pred), 
               names_to = "model", 
               values_to = "pred") %>% 
  mutate(model = sub("_pred", "", model))

df %>%
  ggplot(aes(x = as.numeric(year), y = pred, color = continent)) +
  facet_grid(rows = vars(year), cols = vars(continent)) +
  facet_wrap(~model) +
  scale_x_continuous(breaks = c(1960, 1980, 2000), 
                     labels = c("1960", "1980", "2000"), 
                     expand = c(0,0)) +
  labs(x = "Year", 
       y = "Predicted U5MR", 
       title = "Model Predictions",
       color = "Continent") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  stat_smooth(method = "lm", se = FALSE)

#-------------------

# Combine the tidy predictions into a single data frame
tidy_preds <- bind_rows(predictions)

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
df <- add_predictions(df, mod4)
df <- add_residuals(df,mod5)






# Actually not horrible... it def gets messy when i add more points that likely dont relate
modm1 <- glm(messing, family = gaussian, formula = catastrophescore ~ roofmateri + roofshape + long + lat + roofsolar + trampoline + rooftree + waterslide + deck + pool + divingboar + playground + primarystr + enclosure + sportcourt)
check_model(modm1)
names(messing)
performance(modm1)
# mod1 <- glm(data = train,
#             formula = mod1$formula)
# mod2 <- glm(data = train,
#             formula = mod2$formula)
mod4 <- glm(data = train,
            formula = mod4$formula) # let's see what this does....
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

# Coinflip
rbinom(prob = 0.5, size =100, n =10) %>% # 100 trials, 10 times each
  hist() # and this shows it



# It might be worth putting this in my cleaning code:
# this doesn't fix my rmse values after 3 (it actually even just makes it worse)
