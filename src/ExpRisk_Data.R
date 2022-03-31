### Effect of experience on elicited risk
### Dario Trujano-Ochoa
rm(list = ls())

# Packages ----
library(pacman)
p_load(tidyverse)
p_load(matrixStats)


# Data set ----

ExperienceRisk_Sessions <- read.csv("Data/ExperienceRisk_Sessions.csv", fileEncoding="UTF-8-BOM")
Payoffs <- read.csv("Data/Payoffs.csv", fileEncoding="UTF-8-BOM")
Payoffs <- Payoffs %>% mutate(Exp_payoff = (A+B)/2)

# number of mistakes ----
## function
gamble_payoff <- function(gamble,event) {
  g_payoff <- Payoffs$A[match(gamble,Payoffs$Gamble)]
  g_payoff[event=="e"] <- Payoffs$B[match(gamble,Payoffs$Gamble)][event=="e"]
  return(g_payoff)
}

# try the code
# gamble_payoff(2,"o")

## correct payoffs written down by participants ----

### first 12 elections (pre selected) ----
sum_correct_payoffs_first <- rep(0,length(ExperienceRisk_Sessions$Session))
sum_payoffs_first <- rep(0,length(ExperienceRisk_Sessions$Session))

for(p in 1:12){
  payoffs <-  gamble_payoff(ExperienceRisk_Sessions[,paste("R",p,sep="")], # choice
                                      ExperienceRisk_Sessions[,paste("E",p,sep="")]) # event
  correct_payoff <- payoffs == ExperienceRisk_Sessions[,paste("P",p,sep="")]
  sum_correct_payoffs_first <-  rowSums(cbind(correct_payoff,sum_correct_payoffs_first),na.rm = T)
  sum_payoffs_first <- sum_payoffs_first + payoffs # cumulative payoffs
}

### second 12 elections (free elections) ----
sum_correct_payoffs_second <- rep(0,length(ExperienceRisk_Sessions$Session))
sum_payoffs_second <- rep(0,length(ExperienceRisk_Sessions$Session))

for(p in 1:12){
  payoffs <-  gamble_payoff(ExperienceRisk_Sessions[,paste("F",p,sep="")],
                            ExperienceRisk_Sessions[,paste("EF",p,sep="")])
  correct_payoff <- payoffs == ExperienceRisk_Sessions[,paste("PF",p,sep="")]
  sum_correct_payoffs_second <-  rowSums(cbind(correct_payoff,sum_correct_payoffs_second),na.rm = T)
  sum_payoffs_second <- sum_payoffs_second + payoffs
}

# add sum of correct responses and create new useful variables ----
ExperienceRisk_Sessions <-
ExperienceRisk_Sessions %>% 
  mutate(
    sum_correct_payoffs_first = sum_correct_payoffs_first, 
    sum_correct_payoffs_second = sum_correct_payoffs_second,
    simple_diff = Gamble.2 - Gamble.1,
    sum_correct_payoffs = sum_correct_payoffs_second+sum_correct_payoffs_first,
    # number of even events
    numEven_all = numE_preselected+numE_freechoice,
    Exp_Payoffs_G1 = Payoffs$Exp_payoff[match(Gamble.1,Payoffs$Gamble)],
    Exp_Payoffs_G2 = Payoffs$Exp_payoff[match(Gamble.2,Payoffs$Gamble)],
    sign_gamble = sign(simple_diff),
    # Are people exploring?
    Free_choice_var = rowVars(as.matrix(select(.data = ExperienceRisk_Sessions,F1:F12)),na.rm = T)
    ) %>% 
  mutate(diff_Exp_Payoffs = Exp_Payoffs_G2-Exp_Payoffs_G1) %>% 
  mutate(
    sign_exp_payoff = sign(diff_Exp_Payoffs),
    # consider selecting always the same choice in the free choice subsection as no exploration
    explore = Free_choice_var!=0,
    male = Gender=="M",
    less_than_12_even = numEven_all<12,
    mean_payoff_periods_first = sum_payoffs_first/12,
    mean_payoff_periods_second = sum_payoffs_second/12,
    mean_payoff_periods = (sum_payoffs_first + sum_payoffs_second)/24,
    middle = Gamble.1-3
    )

sum(ExperienceRisk_Sessions$explore)
# 83 people out of 99 explore

# #To check data 
# View(ExperienceRisk_Sessions %>% select(-starts_with(c("F","R","E","P"))))

# export data 
save(ExperienceRisk_Sessions,file = 'data/ExperienceRisk_Sessions.RData')

