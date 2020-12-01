#'------------
#' Friday the 13th Lab!
#' Anovas and such - Looking at models with multiple means
#' ------------

# We're using linear models but with categorical descriptors (lots of 1s and 0s)

# Libraries ####
library(readr)
library(dplyr)
library(ggplot2)
library(brms)
library(tidybayes)
library(ggdist)

#Anova library
library(emmeans)
library(car)


# Load the data ####
knees <- read_csv("data/15e1KneesWhoSayNight.csv")
  

# One way ANOVA ####

head(knees)
skimr::skim(knees)
unique(knees$treatment)

# plot the data
ggplot(knees,
       aes(x = treatment, y = shift)) +
  geom_point() + 
  stat_summary(color = "red")

# Fit the model

knees_lm <- lm(shift ~ treatment, data = knees)
knees_glm <- glm(shift ~ treatment, data = knees,
          family=gaussian(link = "identity"))
knees_brm <- brm(shift~treatment,
                  data = knees,
                  family=gaussian(link = "identity"))

logLik(knees_lm)
logLik(knees_glm)

# check assumptions!!
plot(knees_lm, which = 1)
plot(knees_lm, which = 2)
shapiro.test(residuals(knees_lm)) #All good!

plot(knees_lm, which = 4)
plot(knees_lm, which = 5)

# look at residuals by group

#add residuals to data frame
knees2 <- knees %>% 
  modelr::add_residuals(knees_lm) # now plot 

# plotting with car

residualPlot(knees_lm) # not getting what we want :( (not getting 2 plots)

#do it by hand
ggplot(knees2,
       aes(x = treatment, y = resid)) + 
  geom_boxplot() # reids do not vary by group

#check for HOV (homogeneity of variance)
leveneTest(knees_lm)

pp_check(knees_brm, nsamples = 100)


# Let's Evaluate our model

anova(knees_lm)
anova(knees_glm, test = "LRT")

# function for F tables - car::Anova
Anova(knees_lm)
Anova(knees_glm)

# let's look at some coefficients and other things
#r^2
summary(knees_lm)
bayes_R2(knees_brm)

knees_lm_means <- lm(shift ~ treatment-1, # removes intercept. Tests everything against 0
                     data = knees)
summary(knees_lm_means)

emmeans(knees_lm, ~treatment)
emmeans(knees_glm, ~treatment)
knees_brm_em <- emmeans(knees_brm, ~treatment)

knees_draws <- gather_emmeans_draws(knees_brm_em)

ggplot(knees_draws,
       aes(x = treatment, y = .value)) +
  stat_halfeye() + coord_flip()
# can do this with contrasts(???) as well

# compare means (contrasts)

knees_em <- emmeans(knees_lm, ~treatment)

knees_cont <- contrast(knees_em, method = "tukey", adjust = "none")
plot(knees_cont) +
  geom_vline(xintercept = 0, color = "red")

CLD(knees_em) # gives groups for treatments (sig and not)
pwpp(knees_em)


knees_brm_contrast <- contrast(knees_brm_em, method = "tukey") %>% 
  gather_emmeans_draws()

# contrast with Region of Practical Equivalence
ggplot(knees_brm_contrast, 
       aes(y = contrast,
           x = .value)) +
  stat_halfeye() +
  geom_vline(xintercept = c(-0.5, 0.5), color = "red") # Region of Practical Equivalence
# 0.5 coming from guessing

# bayesian post hoc
knees_brm_contrast %>% 
  group_by(contrast) %>% 
  summarize(rope_density = sum(.value > -0.5 & .value < 0.5)/n())


# Faded Examples ####

# ex 1
## Data
plants <- read_csv("data/15q01PlantPopulationPersistence.csv")

## Visualize
qplot(treatment, generations, data = plants, geom = "boxplot")

## Fit
plant_lm <- lm(generations ~ treatment, data = plants)

## Assumptions
plot(plant_lm, which = c(1, 2, 4, 5))

## ANOVA
Anova(plant_lm)

## Tukey's HSD
contrast(emmeans(plant_lm, ~treatment), method = "tukey")
contrast(emmeans(plant_lm, ~treatment), method = "tukey", adjust = "none") # ls, adjust is for family wise error rate
contrast(emmeans(plant_lm, ~treatment), method = "dunnett", adjust = "none")

# ex 2

worms <- read_csv("data/15q19NematodeLifespan.csv")

# Visualize
qplot(treatment, lifespan, data = worms, geom = "boxplot")

# Fit
worms_lm <- lm(lifespan ~ treatment, data = worms)

# Assumptions
plot(worms_lm, which = c(1,2,4,5))

# ANOVA
anova(worms_lm)
Anova(worms_lm) #same

# Tukey's HSD
contrast(emmeans(worms_lm, ~treatment), method = "tukey")


# ex 3

eelgrass <- read_csv("data/15q05EelgrassGenotypes.csv") %>% 
  mutate(treatment.genotypes = as.character(treatment.genotypes))

# Visualize
qplot(treatment.genotypes, shoots, data = eelgrass, geom = "boxplot")

# Fit
eelgrass_lm <- lm(shoots ~ treatment.genotypes, data = eelgrass)

# Assumptions
plot(eelgrass_lm, which = c(1,2,4,5))

#Anova
Anova(eelgrass_lm)

# Tukey's HSD
contrast(emmeans(eelgrass_lm, ~ treatment.genotypes), method = "tukey") %>% 
  plot() + geom_vline(xintercept=0)


# Multiway Models ####

zoop <- read_csv("data/18e2ZooplanktonDepredation.csv", 
                 col_types = "fnf") # make sure block is a character

ggplot(zoop,
       aes(x = treatment, y = zooplankton)) +
  geom_boxplot() +
  facet_wrap(~block)

ggplot(zoop,
       aes(x = block, y = zooplankton, color = treatment)) +
  geom_point()

# fit a model with lm
zoop_lm <- lm(zooplankton ~ treatment + block, data = zoop)

# Assumption
plot(zoop_lm)

# plot residuals by treatment and test for non-additivity
residualPlots(zoop_lm)

# F-test
Anova(zoop_lm)
anova(zoop_lm)

summary(zoop_lm)

# expected means
zoop_trt_em <- emmeans(zoop_lm, ~treatment)
zoop_trt_em
plot(zoop_trt_em)

# contrasts
contrast(zoop_trt_em, method = "dunnett") # compare the control (well first alphabetical treatment)
contrast(zoop_trt_em, method = "tukey", adjust = "none")

# That's it!



