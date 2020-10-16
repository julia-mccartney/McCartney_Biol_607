#'--------------------------------------
#'@title: Testing co-occurrance script
#'@author: Julia McCartney
#'@date: 12 Oct 2020
#' Following: https://towardsdatascience.com/how-to-create-co-occurrence-networks-with-the-r-packages-cooccur-and-visnetwork-f6e1ceb1c523
#'
#'--------------------------------------

# Libraries ####

library(cooccur)
library(visNetwork)

# Load data ####

data("finches")

str(finches)
summary(finches)

# Data is presence of 13  sp across 17 sites, 0 is absence, 1 is presence

# Calculate co-occurance using a probabilistic model ####
co <- print(cooccur(finches, spp_names = TRUE))


# Visualizing the network ####

# create a data frame for the nodes (species that are co-occuring)

nodes <- data.frame(id = 1:nrow(finches),
                    label = rownames(finches),
                    color = "#606482",
                    shadow = TRUE)

# make an edges data frame using significant values from probabilistic model

edges <- data.frame(from = co$sp1, to = co$sp2,
                    color = ifelse(co$p_lt <= 0.05, "#B0B2C1","#3C3F51"),
                    dashes = ifelse(co$p_lt <= 0.05, TRUE, FALSE))

# visualize using Kamada-Kawai layout algorithm

visNetwork(nodes = nodes, edges = edges) %>% 
  visIgraphLayout(layout = "layout_with_kk")









