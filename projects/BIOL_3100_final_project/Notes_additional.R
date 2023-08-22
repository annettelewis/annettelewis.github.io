# I'm making this for additional notes just in case :)

library(tidyverse)
mod <- iris %>% 
  glm(data=., formula = Sepal.Length ~ Sepal.Width)

