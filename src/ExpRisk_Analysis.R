### Effect of experience on elicited risk: Data Analysis
### Dario Trujano-Ochoa
rm(list = ls())

# Packages ----
library(pacman)
p_load(tidyverse)

# tables ----

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
           numEven_all+
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
