#'-----------------------------
#' @title: 
#' @date: 20 Nov 2020
#' @author: Julia McCartney
#' ----------------------------


# Libraries ####
library(readr)
library(dplyr)
library(tidyr)
library(modelr)
library(car)
library(emmeans)
library(brms)
library(tidybayes)
library(ggdist)
library(ggplot2)

# Loading Data ####

intertidal <- read_csv("data/18e3IntertidalAlgae.csv")

# Looking at the data ####
ggplot(data = intertidal,
       aes(x = herbivores, y = sqrtarea, color = height)) +
  geom_point(position = position_dodge(width = 0.5))

ggplot(data = intertidal,
       aes(x = herbivores, y = sqrtarea, fill = height)) +
  geom_boxplot(width =  1)

ggplot(data = intertidal,
       aes(x = height, y = sqrtarea, fill = herbivores)) +
  geom_boxplot(width =  0.5, position = position_dodge(width = 0.75))

ggplot(data = intertidal,
       aes(x = herbivores, y = sqrtarea, fill = height)) +
  stat_halfeye(position = position_dodge(width =  1))
# THIS PLOT!!!!
ggplot(data = intertidal,
       aes(x = height, y = sqrtarea, color = herbivores)) +
  stat_summary() +
  stat_summary(fun = mean, geom = "line",
               aes(group = herbivores))

# Looks like there may be an interaction effect!


# Model our factorial design ####

#these two models are the same
intertidal_lm <- lm(sqrtarea ~ herbivores + height + herbivores:height, 
                    data = intertidal)

intertidal_lm <- lm(sqrtarea ~ herbivores*height,
                    data = intertidal)

# note: y ~ (a + b) * c is
# y ~ a + b + c + a:c + b:c

# Evaluate assumptions
plot(intertidal_lm, which = 1) # looks good
plot(intertidal_lm, which = 2) # looks good
plot(intertidal_lm, which = 4) # looks good

residualPlots(intertidal_lm)

# residuals by group
# get residuals and add to data frame
intertidal <- intertidal %>%  modelr::add_residuals(intertidal_lm)

# evaluate assumptions of residuals - no group effect on resids
ggplot(intertidal,
       aes(x = height, y = resid)) +
  geom_boxplot()

ggplot(intertidal,
       aes(x = herbivores, y = resid)) +
  geom_boxplot()

ggplot(intertidal,
       aes(x = herbivores, y = resid, fill = height)) +
  geom_boxplot()
#looks good on all accounts

# Evaluate our model

# start with Anova
Anova(intertidal_lm) # remember, defaults to type 2 ss
Anova(intertidal_lm, type = "III") # SAS defaults to type 3 ss

# post hocs
# can't look at main effects
# we are comparing all possible means
intertidal_em <- emmeans(intertidal_lm,
                         specs = ~ herbivores + height)

contrast(intertidal_em, "tukey",adjust = "none") %>%  
  plot() + geom_vline(xintercept = 0, color = "red")

# does herbivore treatment matter at high or low heights?
intertidal_em_2 <- emmeans(intertidal_lm,
                          specs = ~ herbivores | height) # bar nests things 

contrast(intertidal_em_2, "tukey",adjust = "none") %>%  
  plot() + geom_vline(xintercept = 0, color = "red")

# Faded Example for Factorial ANOVA ####
# interested in predator diversity and trial
# look at how diversity and trial is Prop_change
# maybe treatment if you get bored
kelp <- read_csv("data/kelp_pred_div_byrnesetal2006.csv", col_types = "fccfdddd")

## Check and correct for non-factors
str(kelp)
class(kelp$Trial) # fixed
class(kelp$Predator_Diversity)

# Visualize
qplot(Porp_Change,Treatment, data = kelp, geom = "boxplot", fill = Trial)

ggplot(kelp,
       aes(x = Predator_Diversity, y = Porp_Change, fill = Trial)) +
  geom_boxplot(position = position_dodge())

# Fit
kelp_lm <- lm(Porp_Change ~ Treatment * Trial, data = kelp)

# assumptions 
plot(kelp_lm, which = c(1,2,4,5)) # not great but eh. We got some outliers

residualPlots(kelp_lm)

kelp <- kelp %>%  modelr::add_residuals(kelp_lm)

# evaluate assumptions of residuals - no group effect on resids
ggplot(kelp,
       aes(x = Trial, y = resid)) +
  geom_boxplot()

ggplot(kelp,
       aes(x = Treatment, y = resid)) +
  geom_boxplot()

ggplot(kelp,
       aes(x = Treatment, y = resid, fill = Trial)) +
  geom_boxplot()

# ANOVA
Anova(kelp_lm) # no interaction

# Tukey's HSD for simple effects
contrast(emmeans(kelp_lm, specs = ~Treatment), "tukey", adjust = "none")

#--------------------------
## with pred diversity
# Visualize
qplot(Predator_Diversity, Porp_Change, data = kelp, geom = "boxplot", fill = Trial)

# Fit
kelp_lm2 <- lm(Porp_Change ~ Predator_Diversity * Trial, data = kelp)

# assumptions 
plot(kelp_lm2, which = c(1,2,4,5)) # better than before!

residualPlots(kelp_lm2)

kelp <- kelp %>%  modelr::add_residuals(kelp_lm2)

# evaluate assumptions of residuals - no group effect on resids
ggplot(kelp,
       aes(x = Trial, y = resid)) +
  geom_boxplot()

ggplot(kelp,
       aes(x = Predator_Diversity, y = resid)) +
  geom_boxplot()

ggplot(kelp,
       aes(x = Predator_Diversity, y = resid, fill = Trial)) +
  geom_boxplot()

# ANOVA
Anova(kelp_lm2) # no interaction

# Tukey's HSD for simple effects
contrast(emmeans(kelp_lm2, specs = ~ Predator_Diversity), "tukey", adjust = "none")


# ANCOVA style models ####
# General linear models with both categorical and continuous predictors

# Load the data
neand <- read_csv("data/18q09NeanderthalBrainSize.csv")

ggplot(neand, 
       aes(x = species, y = lnbrain)) +
  geom_boxplot()
#looks like no dif

ggplot(neand,
       aes(x = lnmass, y = lnbrain)) +
  geom_point() +
  stat_smooth(method = "lm") 
# BUT there is a relationship here!

ggplot(neand,
       aes(x = lnmass, y = species)) +
  stat_halfeye()

ggplot(neand,
       aes(x = lnmass, y = lnbrain, color = species)) +
  geom_point() +
  stat_smooth(method = "lm") 

# fit a model
neand_glm <- glm(lnbrain ~ lnmass + species, 
                 data = neand,
                 family = gaussian(link = "identity"))

# assumptions 
plot(neand_glm, which = 1)
plot(neand_glm, which = 2)
plot(neand_glm, which = 4)

residualPlots(neand_glm)

neand <- neand %>% 
  add_residuals(neand_glm)

qplot(species, resid, data = neand, geom = "boxplot")

# test for interaction between mass and species 
neand_int <-  glm(lnbrain ~ lnmass * species, 
                  data = neand,
                  family = gaussian(link = "identity"))
# assumption
Anova(neand_int) # no interaction!

# model
Anova(neand_glm)

# post-hocs
# what is the effect of species at the average level of mass
#neand_em <- emmeans(neand_glm, specs = ~ species | lnmass,
#                    at = list(lnmass = 4))

neand_em <- emmeans(neand_glm, specs = ~ species)
neand_em

contrast(neand_em, method = "tukey")

# looking at slope
emtrends(neand_glm, spec = ~ species, var = "lnmass")

# if we had an interaction
emtrends(neand_int, specs =  = ~ species, var = "lnmass")
emmeans(neand_int, specs = ~ species|lnmass,
        at = list(lnmass = c(2,4,6))) %>% 
  contrast(method = "tukey") %>% 
  plot()

# THE FUCK IS THIS?
# predict
neand_newdat <- modelr::data_grid(neand, 
                                  species = unique(species),
                                  lnmass = seq_range(lnmass, n = 100))
nead_predict <- predict(neand_glm,
                        newdata = neand_newdat,
                        type = "response")


neand_newdat <- neand_newdat %>% 
  mutate(lnbrain = neand_predict)

ggplot(neand,
       aes(x = lnmass, y = lnbrain, color = species)) +
  geom_point() +
  geom_line(data = neand_newdat)


# ok maybe this works
neand_newfit <- emmeans(neand_glm, spec = ~ species + lnmass,
                        at = list(lnmass = seq(4,4.5, length.out = 100))) %>%
  as_tibble() %>% 
  mutate(lnbrain = emmean)

ggplot(neand,
       aes(x = lnmass, y = lnbrain, color = species)) +
  geom_point() +
  geom_line(data = neand_newfit) +
  geom_ribbon(data = neand_newfit,
              aes(ymin = asymp.LCL, ymax = asymp.UCL, group = species),
                  alpha = 0.1, color = "lightgrey") +
  theme_minimal()


#jarrett's code
#https://pad.riseup.net/p/-bhlHCoxxlFPjqyUbutA
