### Effect of experience on alicited risk
### Dario Trujano-Ochoa

library(pacman)
p_load(tidyverse)


# Data set ----

ExperienceRisk_Sessions <- read.csv("Data/ExperienceRisk_Sessions.csv")
Payoffs <- read.csv("Data/Payoffs.csv")

## number of mistakes

gamble_payoff <- function(gamble,event) {
  g_payoff <- Payoffs$A[match(gamble,Payoffs$Gamble)]
  g_payoff[event=="e"] <- Payoffs$B[match(gamble,Payoffs$Gamble)][event=="e"]
  return(g_payoff)
}

gamble_payoff(2,"o")

## correct payoffs written down by participants ----

### first 12 elections (preselected) ----
sum_correct_payoffs_first <- rep(0,length(ExperienceRisk_Sessions$Session))
for(p in 1:12){
  correct_payoff <- gamble_payoff(ExperienceRisk_Sessions[,paste("R",p,sep="")],ExperienceRisk_Sessions[,paste("E",p,sep="")])== ExperienceRisk_Sessions[,paste("P",p,sep="")]
  sum_correct_payoffs_first <-  rowSums(cbind(correct_payoff,sum_correct_payoffs_first),na.rm = T)
}

### second 12 elections (free elections) ----
sum_correct_payoffs_second <- rep(0,length(ExperienceRisk_Sessions$Session))
for(p in 1:12){
  correct_payoff <- gamble_payoff(ExperienceRisk_Sessions[,paste("F",p,sep="")],ExperienceRisk_Sessions[,paste("EF",p,sep="")])== ExperienceRisk_Sessions[,paste("PF",p,sep="")]
  sum_correct_payoffs_second <-  rowSums(cbind(correct_payoff,sum_correct_payoffs_second),na.rm = T)
}

ExperienceRisk_Sessions <-
ExperienceRisk_Sessions %>% mutate(sum_correct_payoffs_first = sum_correct_payoffs_first, 
                                  sum_correct_payoffs_second = sum_correct_payoffs_second,
                                  simple_diff = Gamble.2 - Gamble.1)


# Regression ----

lm(ExperienceRisk_Sessions$simple_diff ~ ExperienceRisk_Sessions$CR.Payoff)

lm(ExperienceRisk_Sessions$simple_diff ~ ExperienceRisk_Sessions$CR.Payoff+
     ExperienceRisk_Sessions$numE_freechoice)

lm(ExperienceRisk_Sessions$simple_diff ~ ExperienceRisk_Sessions$CR.Payoff+
     ExperienceRisk_Sessions$numE_preselected)

lm(ExperienceRisk_Sessions$simple_diff ~ 
     ExperienceRisk_Sessions$CR.Payoff+
   ExperienceRisk_Sessions$sum_correct_payoffs_first+
   ExperienceRisk_Sessions$sum_correct_payoffs_second)



