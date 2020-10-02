#'-----------------
#'@autnor: Julia
#'@title: Intro to GGplot2 (Lab)
#'# link to lab: https://biol607.github.io/lab/04_ggplot_palmer_intro.html
#'@date: 9/25/2020
#'----------------



# GGplot is created on a grammar of graphics


# library
library(palmerpenguins)
library(ggplot2)
library(dplyr)
library(ggsci)

head(penguins)

# Intro to ggplot2 ####

# Look at the distribution of bill length

bill_dens <- ggplot(penguins, aes(x = bill_length_mm))

bill_dens
# nothing is there! but not really... we have an axis, but nothing
# is on the figure
# need geoms!

bill_dens +
  geom_histogram(bins - 40)

bill_dens + 
  geom_freqpoly()

# what happens if you add geom_density()?

bill_dens +
  geom_density()

# What if we want to look at multiple distributions?

bill_dens_group <- ggplot(data = penguins, 
                          mapping = aes(x = bill_length_mm,
                                        group = species))
bill_dens_group +
  geom_density()

# let's add some fill colors!
bill_dens_group +
  geom_density(mapping = aes(fill = species, 
                             alpha = 0.5)) # let's make all of these density plots a little transparent

# we are going to  specify something about an aesthetic
# but NOT have it mapped to the data
bill_dens_group + 
  geom_density(mapping = aes(fill = species),
               alpha = 0.3) 
# properties of a geom that do not map to the data do not go in mapping!!!!

# other
bill_dens_group + 
  geom_density(mapping = aes(fill = species),
               position = "stack")

# Exercise 1 ####

# Exercise: Now you try geom_histogram. How’s it look? 
# Bad, right? Try different colors, alphas, and fills 
# to see if you can improve. Maybe a different position?
# What works best?

# dodge
bill_dens_group +
  geom_histogram(aes(fill = species),
                 position = "stack",
                 color = "black") +
  ggsci::scale_fill_npg() + 
  labs(x = "Bill Length (mm",
       y = "Count")

# fill
bill_dens_group +
  geom_histogram(aes(fill = species),
                 position = "fill",
                 color = "black",
                 alpha = 0.7) +
  ggsci::scale_fill_npg() + 
  labs(x = "Bill Length (mm",
       y = "Count")

# with alpha
bill_dens_group +
  geom_histogram(aes(fill = species),
                 alpha = 0.5) +
  ggsci::scale_fill_npg() + 
  labs(x = "Bill Length (mm",
       y = "Count")


# facet wrap
bill_dens_group +
  geom_histogram(aes(fill = species)) +
  ggsci::scale_fill_npg() + 
  facet_wrap(~species) + 
  labs(x = "Bill Length (mm",
       y = "Count")



# 2D! ####

# do a little x and y as your two dimensions
pen_plot_base <- ggplot(data = penguins,
                        mapping = aes(x = body_mass_g,
                                      y = species,
                                      color = species))
pen_plot_base + geom_point(size = 3,
                           alpha = 0.3)

pen_plot_base +
  geom_jitter(size = 2,
              alpha = 0.6)

pen_plot_base +
  geom_point(size = 2,
             alpha = 0.6,
             position = position_jitter(width = 0,
                                        height = 0.4))
# why this over a box plot?

# Exercise 2 ####


# 1. Try out the following geoms - geom_boxplot(), 
#    geom_violin(), stat_summary(). Which do you prefer?
pen_plot_base +
  geom_violin() 

pen_plot_base + 
  geom_boxplot()

pen_plot_base + 
  stat_summary()

# 2. Try adding multiple geoms together. 
#     Does order matter?

pen_plot_base +
  geom_boxplot()+
  geom_violin()

pen_plot_base +
  geom_violin()+
  geom_boxplot()

pen_plot_base +
  geom_violin()+
  geom_jitter(size = 0.3)



# EC. If you want to get saucy, install ggridges and 
#   try out geom_density_ridges()
library(ggridges)

pen_plot_base + 
  geom_density_ridges(alpha = 0.6,
                      size = 1)


# Continuous values on both axes 
pen_mass_depth <- ggplot(penguins,
                         aes(x=body_mass_g,
                             y = bill_depth_mm,
                             color = species))
pen_mass_depth + 
  geom_point()

# Multi-panel plots ####

# faceting!

# facet wrap
# let R arrange (or can specify rows and columns)
pen_mass_depth + 
  geom_point() +
  facet_wrap(~species)

pen_mass_depth + 
  geom_point() +
  facet_wrap(~species + island)

# Exercise 3 ####

# 1. Given that we have the same species of penguin on different 
# islands, what do you see if you use facet_grid() with both 
# island and species?

penguins %>% 
  na.omit(na.rm = TRUE) %>% 
ggplot(aes(x=body_mass_g,
                     y = bill_depth_mm,
                     color = sex,
                     alpha = 0.5)) + 
  geom_point() +
  facet_grid(year ~ species) +
  labs(x = "Body Mass (g)",
       y = "Bill Depth (mm)")


# 2. Incorporate other faceting variables - sex, year, etc. Or 
# mix up what is a facet and what is a color. What do you learn?

penguins %>% 
  na.omit(na.rm = TRUE) %>% 
ggplot(aes(x = body_mass_g,
                     y = bill_depth_mm,
                     color = sex,
                     alpha = 0.5)) + 
  geom_point() +
  facet_grid(year ~ island + species) +
  labs(x = "Body Mass (g)",
       y = "Bill Depth (mm)")



# Lets's make our plots sing! ####

pen_scatter <- pen_mass_depth +
  geom_point()

# Labels
pen_scatter <- pen_scatter +
  labs(x = "Body Mass (g)",
       y = "Bill Depth (mm)",
       title = "Penguin Bill Depth vs Body Mass",
       subtitle = "Data from Palmer LTER",
       color = "Species of\nPenguin")

# Themes
pen_scatter + theme_bw(base_size = 14,
                       base_family = "Times")

# Jarrett's favorite themes
library(ggthemes)

pen_scatter + 
  theme_classic()

# to set a theme
theme_set() # call this first, all plots default to this theme

# Colors! ####

#using scale_

pen_scatter +
  scale_color_manual(values = c("orange", "purple", "darkblue"))

# Many pre-built palettes
pen_scatter + 
  scale_color_brewer(palette = "Dark2")

pen_scatter +
  scale_color_viridis_d(option = "A")


pen_scatter +
  scale_color_manual(values = rainbow(3))

# packages with color palettes
library(wesanderson)

# Continuous color scales ####

pen_mass_col <- ggplot(penguins,
                       aes(x = bill_depth_mm,
                           y = bill_length_mm,
                           color = body_mass_g)) +
  geom_point() +
  facet_wrap(~species)

# VIRIDIS

pen_mass_col +
  scale_color_viridis_c(option = "B")

# can create gradient

pen_mass_col +
  scale_color_gradient(low = "blue", high = "red")

pen_mass_col +
  scale_color_gradientn(colors = c("blue", "green", "orange", "red"))

# Exercise 4 ####

# Exercise: OK - let’s look at how flipper length relates to 
# bill length and depth. Feel free to choose what gets to be a 
# color and what gets to be a coordinate. Combine other aspects - 
# sex, year, etc., to find something more interesting. 
# But above all, make this plot have a great color scale with 
# matching theme to make it pop.


flipper_bill <- penguins %>% 
  na.omit(na.rm = TRUE) %>% 
  ggplot(aes(x = bill_length_mm, 
             y = bill_depth_mm,
             color = flipper_length_mm,
             shape = sex)) + 
  geom_point() +
  labs(
    color = "Flipper\nLength (mm)",
    x = "Bill Length (mm)",
    y = "Bill Depth (mm)",
    title = "Flipper Length by Bill Depth and Length",
    shape = "Sex"
  )

flipper_bill + 
  facet_grid(year ~ island + species) +
  scale_color_viridis_c(option = "A") +
  theme_minimal()
  
