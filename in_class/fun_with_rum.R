#'---------------
#'
#'
#'
#'---------------


# Libraries ####

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# Import data

rum <- read_csv("https://raw.githubusercontent.com/jebyrnes/rum_data/main/data/merged_rum_master.csv")

# look at factors with dif in abv values

rum <- rum %>% 
  mutate(abv_dif =label_abv - hydro_abv)

rum %>% 
  filter(type != "Unknown",
         type != "Cachaca") %>% 
ggplot(
       aes(x = additives_g_l,
           y = abv_dif,
           color = type)) +
  geom_point() +
  labs( x = "Additives (g/L)",
        y = "Difference in Label and Hydrometer ABV",
        color = "Type of Rum"
  ) +
  stat_smooth(method = "lm", fill = NA) +
  facet_wrap(.~type)

abv_dif_lm <- lm(abv_dif ~ additives_g_l + type,
                 data = rum)

summary(abv_dif_lm)


ggplot(rum,
       aes(y = ,
           x = abv_dif)) +
  geom_point() +
  labs( x = "Additives (g/L)",
        y = "Difference in Label and Hydrometer ABV"
  ) +
  stat_smooth(method = "lm")



# look at difs by country
rum_country <- rum %>% 
  drop_na(hydro_rum_name) %>% 
  group_by(country) %>%
  summarise(country_total = length(unique(hydro_rum_name)),
            abv_mean = mean(label_abv),
            score_mean = mean(score),
            score_max = max(score),
            score_min = min(score),
            abv_dif_mean = mean(abv_dif),
            score_dif = score_max - score_min)


ggplot(data = rum_country,
       aes(x = country,
       y = abv_mean)) +
  geom_col()

ggplot(data = rum_country %>%  filter ,
       aes(x = country,
           y = country_total)) +
  geom_col()

ggplot(data = rum_country,
       aes(x = country,
           y = score_dif)) +
  geom_col()



