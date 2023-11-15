# Phangorn
# Ggtree (visualization)
# Ape (library - analysis of phylogenetic… calculates branch length distance) Branch measurement is based on average phylogenetic distance (calculation)

# Load in libraries
library(ape)
library(phangorn)
library(ggplot2)
library(tidyverse)
library(kableExtra)
library(plotly)

### Phylogenetic tree
alignment_file <- "./projects/BOT_3500/3_muscle_alignment.fasta"
fungi <- read.phyDat(alignment_file, format = "fasta")
dist_matrix <- dist.ml(fungi, model = "JC69")
nj_tree <- nj(dist_matrix)
# Extract species names from the tip labels
species_names <- sapply(strsplit(nj_tree$tip.label, " "), function(x) paste(x[1:2], collapse = " "))

# Update the labels of the rooted tree
nj_tree$tip.label <- species_names

# Replace these placeholders with the actual species names in the same order as your tip labels
species_names <- c(
  "Aspergillus niger",
  "Fusarium keratoplasticum",
  "Pleurotus ostreatus",
  "Pericharax heteroraphis"
)
nj_tree$tip.label <- species_names
outgroup_name <- "Pericharax heteroraphis"
outgroup_index <- which(nj_tree$tip.label == outgroup_name)
nj_tree_rooted <- root(nj_tree, outgroup_index)

# Phylogenetic tree rooted on *Pericharax heteroraphis*, a sea sponge, including species names.
plot(nj_tree_rooted)

# Combinations
species_combinations <- list(
  "Aspergillus niger", #A
  "Fusarium keratoplasticum", #B
  "Pleurotus ostreatus", #C
  c("Aspergillus niger", "Fusarium keratoplasticum"), #A+B
  c("Aspergillus niger", "Pleurotus ostreatus"), #A+C
  c("Fusarium keratoplasticum", "Pleurotus ostreatus"), #B+C
  c("Aspergillus niger", "Fusarium keratoplasticum", "Pleurotus ostreatus") #A+B+C
)

df <- data.frame(
  Species =  c("Aspergillus niger", #A
               "Fusarium keratoplasticum", #B
               "Pleurotus ostreatus", #C
               "Aspergillus niger and Fusarium keratoplasticum", #A+B
               "Aspergillus niger and Pleurotus ostreatus", #A+C
               "Fusarium keratoplasticum and Pleurotus ostreatus", #B+C
               "Aspergillus niger, Fusarium keratoplasticum, and Pleurotus ostreatus"), #A+B+C 
  Combination = c("A", "B", "C", "A+B", "A+C", "B+C", "A+B+C")
)

df %>% kable() %>% 
  kable_classic(lightable_options = "hover") %>%
  scroll_box(height = "200px")

### Branch lengths
# Function to calculate branch lengths based on the sea sponge
compute_branch_length <- function(species_combination, nj_tree_rooted, outgroup) {
  species_to_keep <- union(species_combination, outgroup)
  sub_tree <- drop.tip(nj_tree_rooted, setdiff(nj_tree_rooted$tip.label, species_to_keep))
  sub_tree <- drop.tip(sub_tree, outgroup)
  sum(sub_tree$edge.length)
}

# Function to calculate branch lengths (general)
compute_branch_length_gen <- function(species_combination, nj_tree_rooted, outgroup) {
  species_to_keep <- union(species_combination, outgroup)
  sub_tree <- drop.tip(nj_tree_rooted, setdiff(nj_tree_rooted$tip.label, species_to_keep))
  return(sum(sub_tree$edge.length))
}

results <- sapply(species_combinations, FUN = compute_branch_length, nj_tree_rooted = nj_tree_rooted, outgroup = outgroup_name)

species_combinations_str <- sapply(species_combinations, function(x) paste(x, collapse=", "))

results_gen <- sapply(species_combinations, FUN = compute_branch_length_gen, nj_tree_rooted = nj_tree_rooted, outgroup = outgroup_name)

# Main. Compute branch lengths based on our sea sponge, *Pleurotus ostreatus*. In this, the branch lengths associated with the outgroup are excluded here after using it to root the tree.
results_tbl <- tibble(Species=species_combinations_str, Combination=df$Combination, Branch_Length=results)
# Table with branch lengths where branch lengths associated with the outgroup "Pericharax heteroraphis" were removed
results_tbl %>% kable() %>%
  kable_classic(lightable_options = "hover") %>%
  scroll_box(height = "200px")
summary(results_tbl$Branch_Length)
sd(results_tbl$Branch_Length)

#  **General**. Below shows the general branch length values where the outgroup's branch length is kept in the final calculation.
results_gen_tbl <- tibble(Species=species_combinations_str, Combination=df$Combination, Branch_Length=results_gen)
# Table with branch lengths based on the outgroup "Pericharax heteroraphis"
results_gen_tbl %>% 
  kable() %>%
  kable_classic(lightable_options = "hover") %>%
  scroll_box(height = "200px")
summary(results_gen_tbl$Branch_Length)
sd(results_gen_tbl$Branch_Length)
# *Our plan will be to stick with this one as the branch lengths here have a slightly larger standard deviation, but this difference is negligible*

### Experimental results
# General Results: 
new <- data.frame(
  Species =  c("Aspergillus niger", #A
  "Fusarium keratoplasticum", #B
  "Pleurotus ostreatus", #C
  "Aspergillus niger and Fusarium keratoplasticum", #A+B
  "Aspergillus niger and Pleurotus ostreatus", #A+C
  "Fusarium keratoplasticum and Pleurotus ostreatus", #B+C
  "Aspergillus niger, Fusarium keratoplasticum, and Pleurotus ostreatus"),
Species_short =  c("A. niger", #A
  "F. keratoplasticum", #B
  "P. ostreatus", #C
  "A. niger and F. keratoplasticum", #A+B
  "A. niger and P. ostreatus", #A+C
  "F. keratoplasticum and P. ostreatus", #B+C
  "A. niger, F. keratoplasticum, and P. ostreatus"))
merged_data <- merge(results_gen_tbl, new, by = "Species")

df <- results_gen_tbl %>%
  ggplot(aes(x = reorder(Combination, Branch_Length, increasing = TRUE), y = Branch_Length)) +
  geom_point() +
  theme_classic() +
  labs(title = "Branch length based on species combination",
       x = "Species combination",
       y = "Branch length", 
       legend = labs(title = "Legend Title",
                     color = "Combination",
                     shape = "Species")) +
  theme(axis.text.x = element_text(face = "italic"))

ggplotly(df)

results_gen_tbl %>%
  ggplot(aes(x = reorder(Combination, Branch_Length, increasing = TRUE), y = Branch_Length, color = Combination, shape = Species)) +
  geom_point() +
  theme_classic() +
  labs(title = "Branch length based on species combination",
       x = "Species combination",
       y = "Branch length") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, face = "italic"))

results_gen_tbl %>%
  ggplot(aes(x = reorder(Combination, Branch_Length, increasing = TRUE), y = Branch_Length, color = Species)) +
  geom_point() +
  theme_classic() +
  labs(title = "Branch length based on species combination",
       x = "Species combination",
       y = "Branch length") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, face = "italic"))

# Other results (just representative)
results_tbl %>%
  ggplot(aes(x = reorder(Combination, Branch_Length, increasing = TRUE), y = Branch_Length)) +
  geom_point() +
  theme_classic() +
  labs(title = "Branch length based on species combination",
       x = "Species combination",
       y = "Branch length")
# *NOTE:* This will be compared to the actual experimental setup.




#-------------------------------------------------------

library(janitor)
library(tidyverse)
library(easystats)
library(readr)
library(patchwork)
library(modelr)
library(broom)
library(tidyr)

# 1. Read in the unicef data (10 pts) 
# 2. Get it into tidy format (10 pts)
df <- read.csv("./projects/BOT_3500/Community_Diversity_Phylogeny_Decomposition - calculator(_).csv")

GGally::ggpairs(df)

# My current line of thinking involves 

df %>% ggplot(aes(x = branch_length, y = total_coverage, color = species_coverage)) +
  geom_point() 


df %>% ggplot(aes(x = branch_length, y = species_coverage, fill = combinations)) +
  facet_wrap(~date) +
  geom_bar(stat = "identity")

# This could be a cool representation:
df %>% ggplot(aes(x = date, y = species_coverage, fill = combinations)) +
  facet_wrap(~trial_number) +
  geom_bar(stat = "identity")
df %>%as_factor(df$species)

head(df)

df %>% ggplot(aes(x = date, y = species_coverage, fill = combinations)) +
  facet_wrap(~trial_number) +
  geom_bar(stat = "identity")
In this example, I have the com

# Calculate the mean of species_coverage
mean_species_coverage <- df %>%
  group_by(combinations, species) %>%
  summarise(mean_species_coverage = max(species_coverage, na.rm = TRUE))

mean(df$species_coverage, na.rm = TRUE)

# Create the bar plot
ggplot(mean_species_coverage, aes(x = combinations, y = mean_species_coverage, fill = species)) +
  geom_bar(stat = "identity") +
  labs(y = "Mean Species Coverage")  # Add y-axis label if needed

#here
df %>% ggplot(aes(x = combinations, y = species_coverage, fill = species))  +
  geom_bar(stat = "identity")

df %>% ggplot(aes(x = combinations, y = species_coverage, fill = species))  +
  geom_line()

unique(df$species_coverage)

df %>% ggplot(aes(x = branch_length, y = trial_number)) +
  geom_point()

library(palmerpenguins)
dfs <- penguins
#------------

library(ggplot2)
library(dplyr)
library(tidyr)

data <- read.csv("./projects/BOT_3500/Community_Diversity_Phylogeny_Decomposition - calculator(_).csv")

# Here is a bar chart with the combinations (this is an overall mean, so not too useful):
grouped_data <- data %>%
  group_by(combinations, species) %>%
  summarise(mean_species_coverage = mean(species_coverage, na.rm = TRUE))

ggplot(grouped_data, aes(x = combinations, y = mean_species_coverage, fill = species)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 100) +
  labs(title = "Mean Species Coverage by Combinations", y = "Mean Species Coverage (%)", x = "Combinations") +
  theme_minimal()

# Unique dates (generates all 4 (you have to go back to view them all):
unique_dates <- unique(data$date)

for(date in unique_dates){
  date_data <- filter(data, date == !!date)
  grouped_date_data <- date_data %>%
    group_by(combinations, species) %>%
    summarise(mean_species_coverage = mean(species_coverage, na.rm = TRUE))
  
  p <- ggplot(grouped_date_data, aes(x = combinations, y = mean_species_coverage, fill = species)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    ylim(0, 100) +
    labs(title = paste0("Mean Species Coverage by Combinations on ", date), y = "Mean Species Coverage (%)", x = "Combinations") +
    theme_minimal()
  
  print(p)
}


# Line plot over time:
time_series_data <- data %>%
  group_by(date, combinations, species) %>%
  summarise(mean_species_coverage = mean(species_coverage, na.rm = TRUE))

ggplot(time_series_data, aes(x = date, y = mean_species_coverage, color = combinations, linetype = species)) +
  geom_line(aes(group = interaction(combinations, species))) +
  ylim(0, 100) +
  labs(title = "Mean Species Coverage Over Time", y = "Mean Species Coverage (%)", x = "Date") +
  theme_minimal()

# Oct 31st
oct_31_data <- filter(data, date == "2023-10-31")
grouped_oct_31_data <- oct_31_data %>%
  group_by(combinations, species) %>%
  summarise(mean_species_coverage = mean(species_coverage, na.rm = TRUE))

ggplot(grouped_oct_31_data, aes(x = combinations, y = mean_species_coverage, fill = species)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 100) +
  labs(title = "Mean Species Coverage by Combinations on October 31st, 2023", y = "Mean Species Coverage (%)", x = "Combinations") +
  theme_minimal()

# Comparison of each date:
grouped_date_data <- data %>%
  group_by(date, combinations, species) %>%
  summarise(mean_species_coverage = mean(species_coverage, na.rm = TRUE))

ggplot(grouped_date_data, aes(x = combinations, y = mean_species_coverage, fill = species)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 100) +
  labs(y = "Mean Species Coverage (%)", x = "Combinations") +
  theme_minimal() +
  facet_wrap(~ date, ncol = 1)

# box
grouped_data <- data %>%
  group_by(combinations, species) %>%
  summarise(mean_species_coverage = mean(species_coverage, na.rm = TRUE))

ggplot(data) +
  # Adding the "bar-lines"
  geom_segment(data = grouped_data, aes(x = combinations, xend = combinations, y = 0, yend = mean_species_coverage, color = species), position = position_dodge(width = 0.9)) +
  # Adding the individual data points
  geom_point(aes(x = combinations, y = species_coverage, color = species), position = position_dodge(width = 0.9)) +
  ylim(0, 100) +
  labs(title = "Species Coverage by Combinations", y = "Species Coverage (%)", x = "Combinations") +
  theme_minimal()

#----
library(dplyr)
library(ggplot2)

# Assuming data is already loaded and named 'data'
combination_data <- data %>%
  group_by(combinations) %>%
  summarise(branch_length = mean(branch_length, na.rm = TRUE),
            total_coverage = mean(total_coverage, na.rm = TRUE))
ggplot(combination_data, aes(x = branch_length, y = total_coverage, label = combinations)) +
  geom_point(aes(color = combinations), size = 4) +
  geom_text(nudge_y = 1, check_overlap = TRUE) +  # Adjust nudge_y as needed to avoid overlap
  labs(title = "Total Species Coverage by Branch Length",
       x = "Branch Length",
       y = "Total Species Coverage (%)") +
  theme_minimal() +
  theme(legend.title = element_blank())


library(ggplot2)
library(dplyr)

# Calculate the mean of total coverage and branch length for each combination
combination_data <- data %>%
  group_by(combinations) %>%
  summarise(branch_length = mean(branch_length, na.rm = TRUE),
            mean_species_coverage = mean(species_coverage, na.rm = TRUE),
            total_coverage = mean(total_coverage, na.rm = TRUE))

# Create a scatter plot
ggplot(combination_data, aes(x = branch_length, y = mean_species_coverage, size = total_coverage)) +
  geom_point(aes(color = combinations), alpha = 0.7) +
  scale_size_continuous(name = "Total Coverage", range = c(3, 10)) +
  labs(title = "Branch Length and Species Coverage by Combinations",
       x = "Branch Length",
       y = "Mean Species Coverage (%)") +
  theme_minimal() +
  theme(legend.title = element_blank())

#--------- Phylogenetic tree (italicized)
 # Function to plot the tree and highlight the path to each species/combination
plot_tree_with_paths <- function(nj_tree_rooted, species_combinations, outgroup_name) {
  plot(nj_tree_rooted, show.tip.label = TRUE, cex = 0.6)
  
  # Function to find edges leading to a node
  find_edges_to_node <- function(tree, node) {
    if (is.na(match(node, tree$tip.label))) {
      # Node is an internal node
      which(apply(tree$edge, 1, function(x) x[2] == node))
    } else {
      # Node is a tip
      which(apply(tree$edge, 1, function(x) any(x == match(node, tree$tip.label))))
    }
  }

  for (combination in species_combinations) {
    species_to_keep <- c(combination, outgroup_name)
    sub_tree <- drop.tip(nj_tree_rooted, setdiff(nj_tree_rooted$tip.label, species_to_keep))
    nodes_to_highlight <- sub_tree$tip.label
    
    # Highlight each edge leading to the species or combination
    for (node in nodes_to_highlight) {
      edges <- find_edges_to_node(nj_tree_rooted, node)
      for (edge in edges) {
        edge_coords <- nj_tree_rooted$edge[edge, ]
        x <- nj_tree_rooted$xx[edge_coords]
        y <- nj_tree_rooted$yy[edge_coords]
        lines(x, y, col = "red", lwd = 2)
      }
    }
  }
}

# Call the function to plot the tree with paths
plot_tree_with_paths(nj_tree_rooted, species_combinations, outgroup_name)








#-------------
