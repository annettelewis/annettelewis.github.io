library(ARIMA) #useful for time series

#spitting a dataframe
library(tidyverse)
x <- dplyr::group_split(iris,Species)
map(x, "Species") %>% 
  unique()
names(x)


# Phylogenetics
phangorn:: # this can be really help
ape::
ggtree::
  # all of these could work
  
  cran r project. org task view gives a ton of packages within their categories

rds to save as an image can be super useful


# on google, shift control c (and even some work in the terminal: using "curl") to see rmarkdown code (this can help)

# janitor: make_clean_names: cleans the names in a vector

# wakefield (fake datasets can be used making this package)

# Separating sex:
df$sex <- df$Mouse_ID %>% str_sub(1,1)

# generation of random distributions of data based on distribution...
runif(100, max=10) %>% round(0) #uniform distribution
# or even
rnorm(1000, mean = 10, sd =10) # normal dist. 


