#### Estimating relative change ####

Relative_NA2 <- Relative_NA %>%
  mutate(Macro_change = (Macroeconomics - lag(Macroeconomics))/lag(Macroeconomics))

Relative_NA2 <- Relative_NA2 %>%
  mutate(Civil_Lib_change = (Civil_Liberties - lag(Civil_Liberties))/lag(Civil_Liberties))

Relative_NA2 <- Relative_NA2 %>%
  mutate(Health_change = (Health - lag(Health))/lag(Health))

Relative_NA2 <- Relative_NA2 %>%
  mutate(Agriculture_change = (Agriculture - lag(Agriculture))/lag(Agriculture))

Relative_NA2 <- Relative_NA2 %>%
  mutate(Labour_change = (Labour - lag(Labour))/lag(Labour))

Relative_NA2 <- Relative_NA2 %>%
  mutate(Education_change = (Education - lag(Education))/lag(Education))

Relative_NA2 <- Relative_NA2 %>%
  mutate(Environment_change = (Environment - lag(Environment))/lag(Environment))

Relative_NA2 <- Relative_NA2 %>%
  mutate(Energy_change = (Energy - lag(Energy))/lag(Energy))

Relative_NA2 <- Relative_NA2 %>%
  mutate(Immigration_change = (Immigration - lag(Immigration))/lag(Immigration))

Relative_NA2 <- Relative_NA2 %>%
  mutate(Traffic_change = (Traffic - lag(Traffic))/lag(Traffic))

Relative_NA2 <- Relative_NA2 %>%
  mutate(Legal_change = (Legal_Affairs - lag(Legal_Affairs))/lag(Legal_Affairs))

Relative_NA2 <- Relative_NA2 %>%
  mutate(Social_change = (Social_Policy - lag(Social_Policy))/lag(Social_Policy))

Relative_NA2 <- Relative_NA2 %>%
  mutate(Housing_change = (Housing - lag(Housing))/lag(Housing))

Relative_NA2 <- Relative_NA2 %>%
  mutate(Business_change = (Business - lag(Business))/lag(Business))

Relative_NA2 <- Relative_NA2 %>%
  mutate(Defence_change = (Defence - lag(Defence))/lag(Defence))

Relative_NA2 <- Relative_NA2 %>%
  mutate(ResearchTech_change = (Research_Technology - lag(Research_Technology))/lag(Research_Technology))

Relative_NA2 <- Relative_NA2 %>%
  mutate(FTrade_change = (Foreign_Trade - lag(Foreign_Trade))/lag(Foreign_Trade))

Relative_NA2 <- Relative_NA2 %>%
  mutate(Foreign_change = (Foreign_Policy - lag(Foreign_Policy))/lag(Foreign_Policy))

Relative_NA2 <- Relative_NA2 %>%
  mutate(Government_change = (Government_Operations - lag(Government_Operations))/lag(Government_Operations))

Relative_NA2 <- Relative_NA2 %>%
  mutate(PLand_change = (Public_Land - lag(Public_Land))/lag(Public_Land))

Relative_NA2 <- Relative_NA2 %>%
  mutate(Culture_change = (Culture - lag(Culture))/lag(Culture))


#### Frequency distribution ####

test <- Relative_NA2 %>% 
  dplyr::select(ends_with(c("Year", "change"))) %>% 
  pivot_longer(cols = ends_with("change"))

test <- test %>%
  mutate(Percent_Change = value*100)

#Put inf = missing
test$value <- ifelse(is.infinite(test$value) == TRUE, NA, test$value)
test$Percent_Change <- ifelse(is.infinite(test$Percent_Change) == TRUE, NA, test$Percent_Change)



mean(test$Percent_Change, na.rm = TRUE)
sd(test$Percent_Change, na.rm = TRUE)
#Estimation of mean and standard deviation to illustrate a normal distribution

ybreaks <- seq(0,300,25)
#Velger verdier på x-aksen som viser count (antall setninger)

#Create frqunecy distribution
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

#### Descriptive Frequency distribtuion ####
max(test$Percent_Change, na.rm = TRUE) #1014.286
min(test$Percent_Change, na.rm = TRUE) #-100
mean(test$Percent_Change, na.rm = TRUE) #12.6
median(test$Percent_Change, na.rm = TRUE) #-8.49
sd(test$Percent_Change, na.rm = TRUE) #100.28
count(test) #1617
kurtosis(test$Percent_Change, na.rm = TRUE) #17.71
skewness(test$Percent_Change, na.rm = TRUE) #2.83
var(test$Percent_Change, na.rm = TRUE) #10 056

#### KS test ####
library(stats)
ks.test(test$Percent_Change, "pnorm")
#D = 0.55332, p-value < 2.2e-16
#The p-value is less than 0.05, so we can reject H0 of a normal distribution
#Intuitively, the statistic takes the largest absolute difference between the two distribution functions across all x values


#### SW test ####
library(stats)
shapiro.test(test$Percent_Change)
#W = 0.77887, p-value < 2.2e-16


#### L Kurtosis estimation ####
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
L4/L2 #0.258667


#### zero's in dataset ####
table(Relative_NA$Macroeconomics == 0) #none
table(Relative_NA$Civil_Liberties == 0) #20
table(Relative_NA$Health == 0) #8
table(Relative_NA$Agriculture == 0) #5
table(Relative_NA$Labour == 0) #6
table(Relative_NA$Education == 0) #3
table(Relative_NA$Environment == 0) #17
table(Relative_NA$Energy == 0) #9
table(Relative_NA$Immigration == 0) #44
table(Relative_NA$Traffic == 0) #5
table(Relative_NA$Legal_Affairs == 0) #16
table(Relative_NA$Social_Policy == 0) #5
table(Relative_NA$Housing== 0) #6
table(Relative_NA$Business == 0) #10
table(Relative_NA$Defence == 0) #1
table(Relative_NA$Research_Technology == 0) #26
table(Relative_NA$Foreign_Trade == 0) #19
table(Relative_NA$Foreign_Policy == 0) #none
table(Relative_NA$Government_Operations == 0) #17
table(Relative_NA$Public_Land == 0) #30
table(Relative_NA$Culture == 0) #36
#Investigation zero-punctuation done manually in excel
#See "Data set - "Zero-punctuation" online appendix


