# Dario Trujano-Ochoa
# individual Analysis

library(pacman)
p_load(tidyverse)
p_load(janitor)
p_load(xtable)
p_load(Hmisc)


rm(list = ls())

load('data/ExperienceRisk_Sessions.RData')

ExperienceRisk

# is people consistent with their own beliefs?
corstars(as.matrix(ExperienceRisk %>% select(max_exp_gamble, guess_gamble_most_chosen, Gamble.1, Gamble.2) ),result = "latex")

plot(ExperienceRisk$max_exp_gamble,ExperienceRisk$guess_gamble_most_chosen)
cor.test(ExperienceRisk$max_exp_gamble,ExperienceRisk$guess_gamble_most_chosen,use = "na.or.complete")

plot(ExperienceRisk$Gamble.2,ExperienceRisk$guess_gamble_most_chosen)
cor.test(ExperienceRisk$Gamble.2,ExperienceRisk$guess_gamble_most_chosen,use = "na.or.complete")

