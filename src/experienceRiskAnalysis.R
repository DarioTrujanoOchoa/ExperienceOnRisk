### Effect of experience on alicited risk
### Dario Trujano-Ochoa

rm(list = ls())

library(pacman)
p_load(tidyverse)
p_load(matrixStats)


# Data set ----

ExperienceRisk_Sessions <- read.csv("Data/ExperienceRisk_Sessions.csv")
Payoffs <- read.csv("Data/Payoffs.csv")
Payoffs <- Payoffs %>% mutate(Gamble = ï..Gamble)

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

ExperienceRisk_Sessions <-
ExperienceRisk_Sessions %>% mutate(sum_correct_payoffs_first = sum_correct_payoffs_first, 
                                  sum_correct_payoffs_second = sum_correct_payoffs_second,
                                  simple_diff = Gamble.2 - Gamble.1,
                                  Session = ï..Session)

# Are people exploring?
ExperienceRisk_Sessions <- 
ExperienceRisk_Sessions %>% 
  mutate(Free_choice_var = rowVars(as.matrix(select(.data = ExperienceRisk_Sessions,F1:F12)),na.rm = T)) %>% 
  mutate(explore = Free_choice_var!=0)
sum(ExperienceRisk_Sessions$explore)

# Regressions ----

m_CR <- lm(simple_diff ~ CR.Payoff,
           data = ExperienceRisk_Sessions)
summary(m_CR)

lm( simple_diff ~ 
     CR.Payoff+
   Gender+
   explore,
   data = ExperienceRisk_Sessions)

m0 <- lm(simple_diff ~ CR.Payoff+
     numE_freechoice,
     data = ExperienceRisk_Sessions)
summary(m0)

m1 <- lm(simple_diff ~ CR.Payoff+
     numE_preselected, data = ExperienceRisk_Sessions)
summary(m1)

m2 <- lm(simple_diff ~ 
   CR.Payoff+
   sum_correct_payoffs_second+
     explore,
   data = ExperienceRisk_Sessions)
summary(m2)


# changes in decisions conditional on CR results
table(ExperienceRisk_Sessions$Gamble.1,ExperienceRisk_Sessions$Gamble.2,ExperienceRisk_Sessions$CR.Payoff)

# some exploratory graphs ----

ggplot(data = ExperienceRisk_Sessions) + 
  geom_bar(aes(x = Gamble.1),fill = "red", alpha = 0.2) + 
  geom_bar(aes(x = Gamble.2),fill = "blue", alpha = 0.2) 

ggplot(data = ExperienceRisk_Sessions) + 
  geom_point(aes(x=Gamble.1,y=simple_diff, color=factor(CR.Payoff)), alpha = 0.2)

ggplot(data = ExperienceRisk_Sessions) + 
  geom_point(aes(x=numE_freechoice+numE_preselected,y=simple_diff), alpha = 0.2)

ggplot(data = ExperienceRisk_Sessions) + 
  geom_point(aes(x=numE_freechoice+numE_preselected,y=simple_diff, 
                 color=factor(CR.Payoff)), alpha = 0.2)

ggplot(data = ExperienceRisk_Sessions) + 
  geom_point(aes(x=Gamble.1,y=simple_diff, color=factor(Gender)), alpha = 0.2)

ggplot(data = ExperienceRisk_Sessions) + 
  geom_point(aes(x=numE_freechoice+numE_preselected,y=simple_diff, color=factor(Gender)), alpha = 0.2)


