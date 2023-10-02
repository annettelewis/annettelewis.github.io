# Phangorn
# Ggtree (visualization)
# Ape (library - analysis of phylogenetic… calculates branch length distance) Branch measurement is based on average phylogenetic distance (calculation)

library(ape)
library(phangorn)
library(ggplot2)
library(tidyverse)

alignment_file <- "./projects/BOT_3500/muscle_new_sequence_alignment.fasta"
fungi <- read.phyDat(alignment_file, format = "fasta")
dist_matrix <- dist.ml(fungi, model = "JC69")
nj_tree <- nj(dist_matrix)

plot(nj_tree)

# Extract species names from the tip labels
species_names <- sapply(strsplit(nj_tree$tip.label, " "), function(x) paste(x[1:2], collapse = " "))

# Update the labels of the rooted tree
nj_tree$tip.label <- species_names

# Replace these placeholders with the actual species names in the same order as your tip labels
species_names <- c(
  "Aspergillus niger",
  "Aspergillus fumigatus",
  "Fusarium keratoplasticum",
  "Pleurotus ostreatus",
  "Pericharax heteroraphis"
)
nj_tree$tip.label <- species_names
outgroup_name <- "Pericharax heteroraphis"
outgroup_index <- which(nj_tree$tip.label == outgroup_name)
nj_tree_rooted <- root(nj_tree, outgroup_index)


plot(nj_tree_rooted)

outgroup_index <- which(nj_tree$tip.label == outgroup_name)
nj_tree_rooted <- root(nj_tree, outgroup_index)
# Extract species names from the tip labels
species_names <- sapply(strsplit(nj_tree$tip.label, " "), function(x) paste(x[1:2], collapse = " "))

# Update the labels of the rooted tree
nj_tree$tip.label <- species_names

# Replace these placeholders with the actual species names in the same order as your tip labels
species_names <- c(
  "Aspergillus niger",
  "Aspergillus fumigatus",
  "Fusarium keratoplasticum",
  "Pleurotus ostreatus",
  "Pericharax heteroraphis"
)
nj_tree$tip.label <- species_names
outgroup_name <- "Pericharax heteroraphis"
outgroup_index <- which(nj_tree$tip.label == outgroup_name)
nj_tree_rooted <- root(nj_tree, outgroup_index)

plot(nj_tree_rooted)

# Define a function to compute total branch length for a given species combination
compute_branch_length <- function(nj_tree_rooted, species_combinations, outgroup) {
  # Drop species not in the combination and the outgroup
  species_to_keep <- union(species_combination, outgroup) # Keep the outgroup for proper rooting
  sub_tree <- drop.tip(tree, setdiff(tree$tip.label, species_to_keep))
  

  
  # Return total branch length
  sum(sub_tree$edge.length)
}

  # If you want to exclude outgroup's branch lengths from calculations, drop it now
  sub_tree <- drop.tip(sub_tree, outgroup)

# Test for different combinations
species_combinations <- list(
  "Aspergillus niger", #A
  "Aspergillus fumigatus", #B
  "Fusarium keratoplasticum", #C
  "Pleurotus ostreatus", #D
  c("Aspergillus niger", "Aspergillus fumigatus"), #A+B
  c("Aspergillus niger", "Fusarium keratoplasticum"), #A+C
  c("Aspergillus niger", "Pleurotus ostreatus"), #A+D
  c("Aspergillus fumigatus", "Fusarium keratoplasticum"), #B+C
  c("Aspergillus fumigatus", "Pleurotus ostreatus"), #B+D
  c("Fusarium keratoplasticum", "Pleurotus ostreatus"), #C+D
  c("Aspergillus niger", "Aspergillus fumigatus", "Fusarium keratoplasticum"), #A+B+C
  c("Aspergillus niger", "Aspergillus fumigatus", "Pleurotus ostreatus"), #A+B+D
  c("Aspergillus niger", "Fusarium keratoplasticum", "Pleurotus ostreatus"), #A+C+D
  c("Aspergillus fumigatus", "Fusarium keratoplasticum", "Pleurotus ostreatus"), #B+C+D
  c("Aspergillus niger", "Aspergillus fumigatus", "Fusarium keratoplasticum", "Pleurotus ostreatus") #A+B+C+D
)

# Define a function to compute total branch length for a given species combination
compute_branch_length <- function(species_combination, tree, outgroup) {
  # Drop species not in the combination and the outgroup
  species_to_keep <- union(species_combination, outgroup) # Keep the outgroup for proper rooting
  sub_tree <- drop.tip(tree, setdiff(tree$tip.label, species_to_keep))
  
  # If you want to exclude outgroup's branch lengths from calculations, drop it now
  sub_tree <- drop.tip(sub_tree, outgroup)
  
  # Return total branch length
  return(sum(sub_tree$edge.length))
}


# Define a function to compute total branch length for a given species combination
compute_branch_length <- function(nj_tree_rooted, species_combinations, outgroup) {
  # Drop species not in the combination and the outgroup
  species_to_keep <- union(species_combination, outgroup) # Keep the outgroup for proper rooting
  sub_tree <- drop.tip(tree, setdiff(tree$tip.label, species_to_keep))
  
  
  
  # Return total branch length
  sum(sub_tree$edge.length)
}

# Test for different combinations
results <- sapply(species_combinations, compute_branch_length, tree=nj_tree_rooted, outgroup=outgroup_name)

print(results)

# Convert the species combinations into a character vector for easy writing
species_combinations_str <- sapply(species_combinations, function(x) paste(x, collapse=", "))

# Create a tibble
results_tbl <- tibble(Combination=species_combinations_str, Branch_Length=results)
# Table with branch lengths based on the outgroup "Pericharax heteroraphis"






results <- sapply(species_combinations, compute_branch_length, tree=nj_tree_rooted, outgroup=outgroup_name)

# Print results
for(i in 1:length(species_combinations)) {
  cat(paste0("Combination: ", paste(species_combinations[[i]], collapse=", "), "\nTotal branch length: ", results[i], "\n\n"))
}






--------

# Define a function to compute total branch length for a given species combination
compute_branch_length <- function(nj_tree_rooted, species_combinations, outgroup) {
  # Drop species not in the combination and the outgroup
  species_to_keep <- union(species_combination, outgroup) # Keep the outgroup for proper rooting
  sub_tree <- drop.tip(tree, setdiff(tree$tip.label, species_to_keep))
  
  # If you want to exclude outgroup's branch lengths from calculations, drop it now
  sub_tree <- drop.tip(sub_tree, outgroup)
  
  # Return total branch length
  sum(sub_tree$edge.length)
}


# Test for different combinations
species_combinations <- list(
  "Aspergillus niger", #A
  "Aspergillus fumigatus", #B
  "Fusarium keratoplasticum", #C
  "Pleurotus ostreatus", #D
  c("Aspergillus niger", "Aspergillus fumigatus"), #A+B
  c("Aspergillus niger", "Fusarium keratoplasticum"), #A+C
  c("Aspergillus niger", "Pleurotus ostreatus"), #A+D
  c("Aspergillus fumigatus", "Fusarium keratoplasticum"), #B+C
  c("Aspergillus fumigatus", "Pleurotus ostreatus"), #B+D
  c("Fusarium keratoplasticum", "Pleurotus ostreatus"), #C+D
  c("Aspergillus niger", "Aspergillus fumigatus", "Fusarium keratoplasticum"), #A+B+C
  c("Aspergillus niger", "Aspergillus fumigatus", "Pleurotus ostreatus"), #A+B+D
  c("Aspergillus niger", "Fusarium keratoplasticum", "Pleurotus ostreatus"), #A+C+D
  c("Aspergillus fumigatus", "Fusarium keratoplasticum", "Pleurotus ostreatus"), #B+C+D
  c("Aspergillus niger", "Aspergillus fumigatus", "Fusarium keratoplasticum", "Pleurotus ostreatus") #A+B+C+D
)

results <- sapply(species_combinations, compute_branch_length, tree=nj_tree_rooted, outgroup=outgroup_name)

# Print results
for(i in 1:length(species_combinations)) {
  cat(paste0("Combination: ", paste(species_combinations[[i]], collapse=", "), "\nTotal branch length: ", results[i], "\n\n"))
}
-------





# Cumulative view of branch lengths and phylogenetic distances
total_branch_length <- sum(nj_tree_rooted$edge.length)
print(total_branch_length)

avg_phylo_distance <- mean(dist_matrix)
print(avg_phylo_distance)







cophenetic_distances <- cophenetic(nj_tree)
print(cophenetic_distances)

brlen_tree <- compute.brlen(nj_tree_rooted, method = "Grafen", power = 1)

# Get the sum of branch lengths
sum_branch_lengths <- sum(branching.times(brlen_tree))
#branching times is only useful if tree is ultrametric

# Print or use the sum as needed
print(sum_branch_lengths)









# Assuming brlen_tree is your rooted phylogenetic tree with branch lengths
num_species <- length(brlen_tree$tip.label)
sums <- matrix(NA, nrow = num_species, ncol = num_species)

for (i in 1:(num_species - 1)) {
  for (j in (i + 1):num_species) {
    # Extract the subset of the tree for the current pair of species
    subset_labels <- c(brlen_tree$tip.label[i], brlen_tree$tip.label[j])
    subset_tree <- drop.tip(brlen_tree, brlen_tree$tip.label[!(brlen_tree$tip.label %in% subset_labels)])
    
    # Calculate the sum of branch lengths for this pair of species
    sum_length <- sum(subset_tree$edge.length)
    
    # Store the result in the matrix
    sums[i, j] <- sum_length
    sums[j, i] <- sum_length
  }
}

# Print or use the matrix of sums as needed
print(sums)



















install.packages("ggtree")

#since I don't have access to GGTREE :((
library(phytools)

# Plot the tree with branch length distances
plotTree(nj_tree_rooted, cex = 0.6, edge.width = cophenetic_distances)

# ape package (likely) -> look up how to make a sum of branch lengths


