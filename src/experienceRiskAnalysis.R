### Effect of experience on alicited risk
### Dario Trujano-Ochoa

rm(list = ls())

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
           Gamble.1 +
           Gender+
   CR.Payoff +
     sum_correct_payoffs+
     numE_all+
     explore+
     Session,
   data = ExperienceRisk_Sessions)
summary(m2)


# changes in decisions conditional on CR results
table(ExperienceRisk_Sessions$Gamble.1,ExperienceRisk_Sessions$Gamble.2,ExperienceRisk_Sessions$CR.Payoff)

# some exploratory graphs ----

# ## transparency plot
# ggplot(data = ExperienceRisk_Sessions ) + 
#   geom_bar(aes(x = Gamble.1),fill = "red", alpha = 0.2) + 
#   geom_bar(aes(x = Gamble.2),fill = "blue", alpha = 0.2)

ggplot(data = ExperienceRisk_Sessions %>% 
         select(Gamble.1,Gamble.2) %>% 
         gather("Gamble_Order","Gamble") ) + 
  geom_bar(aes(x = Gamble,fill = Gamble_Order),position="dodge")


ggplot(data = ExperienceRisk_Sessions) + 
  geom_bar(aes(x=simple_diff, fill=factor(Gender)), alpha = 0.2)

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


