library(openxlsx)
library(tidyverse)
library(textreadr)
library(haven)
library(tidycomm)
library(ggplot2)
library(MASS)
library(writexl)
library(dplyr) 

allDATA_Relative <- read.xlsx("Data_relative.xlsx")

#### Relativ endring originalt datasett ####

allDATA_Relative2 <- allDATA_Relative %>%
  mutate(Macro_change = (Macroeconomics - lag(Macroeconomics))/lag(Macroeconomics))

allDATA_Relative2 <- allDATA_Relative2 %>%
  mutate(Civil_Lib_change = (Civil_Liberties - lag(Civil_Liberties))/lag(Civil_Liberties))

allDATA_Relative2 <- allDATA_Relative2 %>%
  mutate(Health_change = (Health - lag(Health))/lag(Health))

allDATA_Relative2 <- allDATA_Relative2 %>%
  mutate(Agriculture_change = (Agriculture - lag(Agriculture))/lag(Agriculture))

allDATA_Relative2 <- allDATA_Relative2 %>%
  mutate(Labour_change = (Labour - lag(Labour))/lag(Labour))

allDATA_Relative2 <- allDATA_Relative2 %>%
  mutate(Education_change = (Education - lag(Education))/lag(Education))

allDATA_Relative2 <- allDATA_Relative2 %>%
  mutate(Environment_change = (Environment - lag(Environment))/lag(Environment))

allDATA_Relative2 <- allDATA_Relative2 %>%
  mutate(Energy_change = (Energy - lag(Energy))/lag(Energy))

allDATA_Relative2 <- allDATA_Relative2 %>%
  mutate(Immigration_change = (Immigration - lag(Immigration))/lag(Immigration))

allDATA_Relative2 <- allDATA_Relative2 %>%
  mutate(Traffic_change = (Traffic - lag(Traffic))/lag(Traffic))

allDATA_Relative2 <- allDATA_Relative2 %>%
  mutate(Legal_change = (Legal_Affairs - lag(Legal_Affairs))/lag(Legal_Affairs))

allDATA_Relative2 <- allDATA_Relative2 %>%
  mutate(Social_change = (Social_Policy - lag(Social_Policy))/lag(Social_Policy))

allDATA_Relative2 <- allDATA_Relative2 %>%
  mutate(Housing_change = (Housing - lag(Housing))/lag(Housing))

allDATA_Relative2 <- allDATA_Relative2 %>%
  mutate(Business_change = (Business - lag(Business))/lag(Business))

allDATA_Relative2 <- allDATA_Relative2 %>%
  mutate(Defence_change = (Defence - lag(Defence))/lag(Defence))

allDATA_Relative2 <- allDATA_Relative2 %>%
  mutate(ResearchTech_change = (Research_Technology - lag(Research_Technology))/lag(Research_Technology))

allDATA_Relative2 <- allDATA_Relative2 %>%
  mutate(FTrade_change = (Foreign_Trade - lag(Foreign_Trade))/lag(Foreign_Trade))

allDATA_Relative2 <- allDATA_Relative2 %>%
  mutate(Foreign_change = (Foreign_Policy - lag(Foreign_Policy))/lag(Foreign_Policy))

allDATA_Relative2 <- allDATA_Relative2 %>%
  mutate(Government_change = (Government_Operations - lag(Government_Operations))/lag(Government_Operations))

allDATA_Relative2 <- allDATA_Relative2 %>%
  mutate(PLand_change = (Public_Land - lag(Public_Land))/lag(Public_Land))

allDATA_Relative2 <- allDATA_Relative2 %>%
  mutate(Culture_change = (Culture - lag(Culture))/lag(Culture))



#### Samlet i en fordeling ####
library(dplyr)
library(tidyverse)
test <- allDATA_Relative2 %>% 
  dplyr::select(ends_with(c("Year", "change"))) %>% 
  pivot_longer(cols = ends_with("change"))

test <- test %>%
  mutate(Percent_Change = value*100)


test$value <- ifelse(is.infinite(test$value) == TRUE, NA, test$value)
#Setter Inf verdiene som missing 

mean(test$Percent_Change, na.rm = TRUE)
sd(test$Percent_Change, na.rm = TRUE)
#Regner ut gjennomsnittet og standardavvik for å kunne
#vise hvordan en normalfordeling hadde sett ut
#Dette inkluderes nedenfor




#### Change with % ####
test$Percent_Change <- sd(test$Percent_Change, na.rm = TRUE)
sd(test$Percent_Change, na.rm = TRUE)
#Regner ut gjennomsnittet og standardavvik for å kunne
#vise hvordan en normalfordeling hadde sett ut
#Dette inkluderes nedenfor


ybreaks <- seq(0,300,25)
#Velger verdier på x-aksen som viser count (antall setninger)

#Frequency distribution
ggplot(test, aes(x = Percent_Change)) +
  geom_histogram(aes(y = ..density..), binwidth = 10) +
  stat_function(fun = dnorm, args = list(mean = mean(test$Percent_Change, na.rm = TRUE),
                                         sd = sd(test$Percent_Change, na.rm = TRUE))) +
  scale_y_continuous("Frequency", labels = ybreaks,
                     breaks = NULL,
                     position = "left") +
  scale_y_continuous("", breaks = NULL,
                     sec.axis = sec_axis(trans = ~ . * 10 * nrow(test),
                                         breaks = ybreaks,
                                         name = "Frequency"),
                     position = "right", expand = c(0, 0)) +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank())+
  labs(x = "Percentage change in percentage attention")+
  scale_x_continuous(breaks = seq(-100, 1000, 100))

max(test$Percent_Change, na.rm = TRUE)
min(test$Percent_Change, na.rm = TRUE)
mean(test$Percent_Change, na.rm = TRUE)
sd(test$Percent_Change, na.rm = TRUE)
count(test)

#### L kurtosis ####
kurtosis(test$Percent_Change, na.rm = TRUE)
test <- as.data.frame(test)
install.packages("lmoments")
library(Lmoments)

test$Percent_Change <- ifelse(is.nan(test$Percent_Change) == TRUE, NA, test$Percent_Change)

Lcoefs(test$Percent_Change, rmax = 4, na.rm = TRUE, trim = c(0, 0))

install.packages("EnvStats")
EnvStats::kurtosis(test$Percent_Change, 
                   na.rm = TRUE,
                   method = "l.moment",
                   l.moment.method = "")


??lmoments
pc<-subset(test, select=c(Year,name,Percent_Change))
pc<-test$Percent_Change
Lmoments(pc)

lmoments<-Lmoments(pc)

#### manuelt ####
test2 <- subset(test, select = Percent_Change)
test2 <- na.omit(test2)
test2 <- t(test2)
n <- length(test2)
y <- sort(test2)

###L1
L1 <- (1/choose(n, 1))*sum(sort(test2))

###L2
sum.t.L2 <- 0

bb <- n

for (i in 1:n)
  
{
  
  t.L2 <- (choose((i-1), 1)- choose((n-i), 1))*y[i]
  
  sum.t.L2[i] <- t.L2
  
}

sum(sum.t.L2)

L2 <- 0.5*(1/choose(n,2))*sum(sum.t.L2)

###L3
sum.t.L3 <- 0

bb <- n

for (i in 1:n)
  
{
  
  t.L3 <- (choose((i-1), 2)-(2*choose((i-1),1)*choose((n-i),1))+choose((n-i), 2))*y[i]
  
  
  
  sum.t.L3[i] <- t.L3
  
}

sum(sum.t.L3)

L3 <- (1/3)*(1/choose(n,3))*sum(sum.t.L3)

###L4
sum.t.L4 <- 0

bb <- n

for (i in 1:n)
  
{
  
  t.L4 <- (choose((i-1), 3)-(3*choose((i-1),2)*choose((n-i),1))+(3*choose((i-1),1)*choose((n-i),2))-choose((n-i), 3))*y[i]
  
  
  
  sum.t.L4[i] <- t.L4
  
}

sum(sum.t.L4)

L4 <- (1/4)*(1/choose(n,4))*sum(sum.t.L4)

#### L kurtosis
L4/L2

