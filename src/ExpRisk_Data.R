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
Payoffs <- Payoffs %>% mutate(Exp_payoff = (A+B)/2, 
                              #Gamble = ï..Gamble # problem with windows
                              )

## number of mistakes

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
for(p in 1:12){
  correct_payoff <- gamble_payoff(ExperienceRisk_Sessions[,paste("R",p,sep="")],
                                  ExperienceRisk_Sessions[,paste("E",p,sep="")]) == ExperienceRisk_Sessions[,paste("P",p,sep="")]
  sum_correct_payoffs_first <-  rowSums(cbind(correct_payoff,sum_correct_payoffs_first),na.rm = T)
}

### second 12 elections (free elections) ----
sum_correct_payoffs_second <- rep(0,length(ExperienceRisk_Sessions$Session))
for(p in 1:12){
  correct_payoff <- gamble_payoff(ExperienceRisk_Sessions[,paste("F",p,sep="")],
                                  ExperienceRisk_Sessions[,paste("EF",p,sep="")])== ExperienceRisk_Sessions[,paste("PF",p,sep="")]
  sum_correct_payoffs_second <-  rowSums(cbind(correct_payoff,sum_correct_payoffs_second),na.rm = T)
}

# add sum of correct responses and create new useful data
ExperienceRisk_Sessions <-
ExperienceRisk_Sessions %>% mutate(sum_correct_payoffs_first = sum_correct_payoffs_first, 
                                  sum_correct_payoffs_second = sum_correct_payoffs_second,
                                  simple_diff = Gamble.2 - Gamble.1,
                                  #Session = ï..Session, # problem in windows
                                  sum_correct_payoffs = sum_correct_payoffs_second+sum_correct_payoffs_first,
                                  # number of even events
                                  numEven_all = numE_preselected+numE_freechoice,
                                  Exp_Payoffs = Payoffs$Exp_payoff[match(Gamble.1,Payoffs$Gamble)])

# Are people exploring?
ExperienceRisk_Sessions <- 
ExperienceRisk_Sessions %>% 
  mutate(Free_choice_var = rowVars(as.matrix(select(.data = ExperienceRisk_Sessions,F1:F12)),na.rm = T)) %>% 
  mutate(explore = Free_choice_var!=0)
# consider selecting always the same choice in the free choice subsection as no exploration
sum(ExperienceRisk_Sessions$explore)
# 83 people out of 99 explore

# export data 
save(ExperienceRisk_Sessions,file = 'data/ExperienceRisk_Sessions.RData')

