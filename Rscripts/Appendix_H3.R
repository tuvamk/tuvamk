#### Re-structure dataset ####

switchedrows <- as.data.frame(t(Relative_NA))
switchedrows <- switchedrows[-c(1,2,3,4,5,6), ] #Fjernet parti etc.

library(dplyr)
#Bytter slik at hver kolonne = 1 år
switchedrows <- switchedrows %>%
  rename("1946" = `V1`, "1947" = `V2`, "1948" = `V3`,	"1949" =`V4`,	"1950" =`V5`,	"1951" =`V6`,
         "1952" =`V7`,	"1953" =`V8`, "1954" =`V9`,	"1955" =`V10`, "1956" =`V11`,	"1957" =`V12`,	"1958" =`V13`,	"1959a" =`V14`,	"1959b" =`V15`,
         "1960" =`V16`,	"1961" =`V17`,	"1962" =`V18`,	"1963" =`V19`,	"1964" =`V20`,	"1965" =`V21`,	"1966" =`V22`,	
         "1967" =`V23`,	"1968" =`V24`,	"1969" =`V25`,	"1970" =`V26`,	"1971" =`V27`,	"1972" =`V28`,	"1973" =`V29`,	
         "1974" =`V30`,	"1975" =`V31`,	"1976" =`V32`,	"1977" =`V33`,	"1978" =`V34`,	"1979" =`V35`,	"1980"=`V36`,
         "1981" =`V37`,	"1982" =`V38`,	"1983" =`V39`,	"1984" =`V40`,	"1985" =`V41`, "1986" =`V42`,	"1987" =`V43`,	
         "1988" =`V44`,	"1989" =`V45`,	"1990" =`V46`,	"1991" =`V47`,	"1992" =`V48`,	"1993" =`V49`,	"1994" =`V50`,	
         "1995" =`V51`,	"1996" =`V52`,	"1997" =`V53`,	"1998" =`V54`,	"1999" =`V55`,	"2000" =`V56`,	"2001" =`V57`,	"2002" =`V58`,	
         "2003" =`V59`,	"2004" =`V60`,	"2005" =`V61`,	"2006" =`V62`,	"2007" =`V63`,	"2008" =`V64`,	"2009" = `V65`,
         "2010" =`V66`,	"2011" =`V67`,	"2012" =`V68`,	"2013" =`V69`,	"2014" =`V70`,	"2015" =`V71`,	"2016" =`V72`,
         "2017" =`V73`,	"2018" =`V74`,	"2019" =`V75`,	"2020" =`V76`,	"2021" =`V77`)



#Endrer til numeric
switchedrows$`1946` <- as.numeric(switchedrows$`1946`)
switchedrows$`1947` <- as.numeric(switchedrows$`1947`)
switchedrows$`1948` <- as.numeric(switchedrows$`1948`)
switchedrows$`1949` <- as.numeric(switchedrows$`1949`)

switchedrows$`1950` <- as.numeric(switchedrows$`1950`)
switchedrows$`1951` <- as.numeric(switchedrows$`1951`)
switchedrows$`1952` <- as.numeric(switchedrows$`1952`)
switchedrows$`1953` <- as.numeric(switchedrows$`1953`)
switchedrows$`1954` <- as.numeric(switchedrows$`1954`)
switchedrows$`1955` <- as.numeric(switchedrows$`1955`)
switchedrows$`1956` <- as.numeric(switchedrows$`1956`)
switchedrows$`1957` <- as.numeric(switchedrows$`1957`)
switchedrows$`1958` <- as.numeric(switchedrows$`1958`)
switchedrows$`1959a` <- as.numeric(switchedrows$`1959a`)
switchedrows$`1959b` <- as.numeric(switchedrows$`1959b`)

switchedrows$`1960` <- as.numeric(switchedrows$`1960`)
switchedrows$`1961` <- as.numeric(switchedrows$`1961`)
switchedrows$`1962` <- as.numeric(switchedrows$`1962`)
switchedrows$`1963` <- as.numeric(switchedrows$`1963`)
switchedrows$`1964` <- as.numeric(switchedrows$`1964`)
switchedrows$`1965` <- as.numeric(switchedrows$`1965`)
switchedrows$`1966` <- as.numeric(switchedrows$`1966`)
switchedrows$`1967` <- as.numeric(switchedrows$`1967`)
switchedrows$`1968` <- as.numeric(switchedrows$`1968`)
switchedrows$`1969` <- as.numeric(switchedrows$`1969`)

switchedrows$`1970` <- as.numeric(switchedrows$`1970`)
switchedrows$`1971` <- as.numeric(switchedrows$`1971`)
switchedrows$`1972` <- as.numeric(switchedrows$`1972`)
switchedrows$`1973` <- as.numeric(switchedrows$`1973`)
switchedrows$`1974` <- as.numeric(switchedrows$`1974`)
switchedrows$`1975` <- as.numeric(switchedrows$`1975`)
switchedrows$`1976` <- as.numeric(switchedrows$`1976`)
switchedrows$`1977` <- as.numeric(switchedrows$`1977`)
switchedrows$`1978` <- as.numeric(switchedrows$`1978`)
switchedrows$`1979` <- as.numeric(switchedrows$`1979`)

switchedrows$`1980` <- as.numeric(switchedrows$`1980`)
switchedrows$`1981` <- as.numeric(switchedrows$`1981`)
switchedrows$`1982` <- as.numeric(switchedrows$`1982`)
switchedrows$`1983` <- as.numeric(switchedrows$`1983`)
switchedrows$`1984` <- as.numeric(switchedrows$`1984`)
switchedrows$`1985` <- as.numeric(switchedrows$`1985`)
switchedrows$`1986` <- as.numeric(switchedrows$`1986`)
switchedrows$`1987` <- as.numeric(switchedrows$`1987`)
switchedrows$`1988` <- as.numeric(switchedrows$`1988`)
switchedrows$`1989` <- as.numeric(switchedrows$`1989`)

switchedrows$`1990` <- as.numeric(switchedrows$`1990`)
switchedrows$`1991` <- as.numeric(switchedrows$`1991`)
switchedrows$`1992` <- as.numeric(switchedrows$`1992`)
switchedrows$`1993` <- as.numeric(switchedrows$`1993`)
switchedrows$`1994` <- as.numeric(switchedrows$`1994`)
switchedrows$`1995` <- as.numeric(switchedrows$`1995`)
switchedrows$`1996` <- as.numeric(switchedrows$`1996`)
switchedrows$`1997` <- as.numeric(switchedrows$`1997`)
switchedrows$`1998` <- as.numeric(switchedrows$`1998`)
switchedrows$`1999` <- as.numeric(switchedrows$`1999`)

switchedrows$`2000` <- as.numeric(switchedrows$`2000`)
switchedrows$`2001` <- as.numeric(switchedrows$`2001`)
switchedrows$`2002` <- as.numeric(switchedrows$`2002`)
switchedrows$`2003` <- as.numeric(switchedrows$`2003`)
switchedrows$`2004` <- as.numeric(switchedrows$`2004`)
switchedrows$`2005` <- as.numeric(switchedrows$`2005`)
switchedrows$`2006` <- as.numeric(switchedrows$`2006`)
switchedrows$`2007` <- as.numeric(switchedrows$`2007`)
switchedrows$`2008` <- as.numeric(switchedrows$`2008`)
switchedrows$`2009` <- as.numeric(switchedrows$`2009`)

switchedrows$`2010` <- as.numeric(switchedrows$`2010`)
switchedrows$`2011` <- as.numeric(switchedrows$`2011`)
switchedrows$`2012` <- as.numeric(switchedrows$`2012`)
switchedrows$`2013` <- as.numeric(switchedrows$`2013`)
switchedrows$`2014` <- as.numeric(switchedrows$`2014`)
switchedrows$`2015` <- as.numeric(switchedrows$`2015`)
switchedrows$`2016` <- as.numeric(switchedrows$`2016`)
switchedrows$`2017` <- as.numeric(switchedrows$`2017`)
switchedrows$`2018` <- as.numeric(switchedrows$`2018`)
switchedrows$`2019` <- as.numeric(switchedrows$`2019`)
switchedrows$`2020` <- as.numeric(switchedrows$`2020`)
switchedrows$`2021` <- as.numeric(switchedrows$`2021`)


#### Entropy scores ####

#Calculate entropy score in data frame by year
#seperating from 1959 to include both speeches
library(entropy)
etest1 <- data.frame(
  Year = seq(1946, 1959, 1),
  Entropy = c(entropy(switchedrows$`1946`),
              entropy(switchedrows$`1947`),
              entropy(switchedrows$`1948`),
              entropy(switchedrows$`1949`),
              entropy(switchedrows$`1950`),
              entropy(switchedrows$`1951`),
              entropy(switchedrows$`1952`),
              entropy(switchedrows$`1953`),
              entropy(switchedrows$`1954`),
              entropy(switchedrows$`1955`),
              entropy(switchedrows$`1956`),
              entropy(switchedrows$`1957`),
              entropy(switchedrows$`1958`),
              entropy(switchedrows$`1959a`)))

etest2 <- data.frame(
  Year = seq(1959, 2021, 1),
  Entropy = c(entropy(switchedrows$`1959b`),
              entropy(switchedrows$`1960`),
              entropy(switchedrows$`1961`),
              entropy(switchedrows$`1962`),
              entropy(switchedrows$`1963`),
              entropy(switchedrows$`1964`),
              entropy(switchedrows$`1965`),
              entropy(switchedrows$`1966`),
              entropy(switchedrows$`1967`),
              entropy(switchedrows$`1968`),
              entropy(switchedrows$`1969`),
              entropy(switchedrows$`1970`),
              entropy(switchedrows$`1971`),
              entropy(switchedrows$`1972`),
              entropy(switchedrows$`1973`),
              entropy(switchedrows$`1974`),
              entropy(switchedrows$`1975`),
              entropy(switchedrows$`1976`),
              entropy(switchedrows$`1977`),
              entropy(switchedrows$`1978`),
              entropy(switchedrows$`1979`),
              entropy(switchedrows$`1980`),
              entropy(switchedrows$`1981`),
              entropy(switchedrows$`1982`),
              entropy(switchedrows$`1983`),
              entropy(switchedrows$`1984`),
              entropy(switchedrows$`1985`),
              entropy(switchedrows$`1986`),
              entropy(switchedrows$`1987`),
              entropy(switchedrows$`1988`),
              entropy(switchedrows$`1989`),
              entropy(switchedrows$`1990`),
              entropy(switchedrows$`1991`),
              entropy(switchedrows$`1992`),
              entropy(switchedrows$`1993`),
              entropy(switchedrows$`1994`),
              entropy(switchedrows$`1995`),
              entropy(switchedrows$`1996`),
              entropy(switchedrows$`1997`),
              entropy(switchedrows$`1998`),
              entropy(switchedrows$`1999`),
              entropy(switchedrows$`2000`),
              entropy(switchedrows$`2001`), 
              entropy(switchedrows$`2002`),
              entropy(switchedrows$`2003`),
              entropy(switchedrows$`2004`),
              entropy(switchedrows$`2005`),
              entropy(switchedrows$`2006`),
              entropy(switchedrows$`2007`),
              entropy(switchedrows$`2008`),
              entropy(switchedrows$`2009`),
              entropy(switchedrows$`2010`),
              entropy(switchedrows$`2011`),
              entropy(switchedrows$`2012`),
              entropy(switchedrows$`2013`),
              entropy(switchedrows$`2014`),
              entropy(switchedrows$`2015`),
              entropy(switchedrows$`2016`),
              entropy(switchedrows$`2017`),
              entropy(switchedrows$`2018`),
              entropy(switchedrows$`2019`),
              entropy(switchedrows$`2020`),
              entropy(switchedrows$`2021`)))
library(entropy)
#Combines into one entropy score dataset
EntropyScores_NA <- rbind(etest1, etest2)
#Attache the entropy into original dataset 
Relative_EntropyNA <- cbind(Relative_NA, EntropyScores_NA$Entropy)

H3 <- read.xlsx("H3.xlsx") #added statement and length variables manual in excel based on Relative_EntropyNA
H3 <- rename(H3, Entropy = `EntropyScores_NA$Entropy`)

#### Descriptive entropy ####
log(21) #3.044 is max entropy score

sd(H3$Entropy) #0.2189943
min(H3$Entropy) #1.893107: In 2020 - extreme outlier 
max(H3$Entropy) #2.908899: In 
mean(H3$Entropy) #2.604212

mean(H3$Statements) #71.5974
sd(H3$Statements) #25.4973
min(H3$Statements) #23
max(H3$Statements) #117

min(H3_Omit20$Entropy) #2.03 in 1953
mean(H3_Omit20$Entropy) #2.6
sd(H3_Omit20$Entropy) #0.2
mean(H3_Omit20$Statements) #71.61

#Entropy scores over time
ggplot(H3, aes(x = Year, y = Entropy))+
  geom_line()+
  geom_hline(yintercept = 2.604212, linetype = "dashed")+
  theme_classic()+
  ggtitle("Diversity of Trontaler 1946-2021")+
  ylim(1.89, 3)


#### Autoregressive model ####
adf.test(H3$Entropy) #non-Stationary
#As expected, the values are time-dependent, so the Entropy score needs
#to be controlled for a past value of itself 
H3 <- H3 %>%
  mutate(Lag_Entropy = lag(Entropy)) #lag entropy scores by 1 year

#Transform all variables to time series objects to use dynlm
H3_ts <- H3 %>%
  mutate(Entropy = as.ts(Entropy)) %>%
  mutate(Statements = as.ts(Statements)) %>%
  mutate(Partisan.control = as.ts(Partisan.control))%>%
  mutate(Macroeconomics = as.ts(Macroeconomics))%>%
  mutate(Civil_Liberties = as.ts(Civil_Liberties))%>%
  mutate(Health = as.ts(Health))%>%
  mutate(Agriculture = as.ts(Agriculture))%>%
  mutate(Labour = as.ts(Labour))%>%
  mutate(Education = as.ts(Education))%>%
  mutate(Environment = as.ts(Environment))%>%
  mutate(Energy = as.ts(Energy))%>%
  mutate(Immigration = as.ts(Immigration))%>%
  mutate(Traffic = as.ts(Traffic))%>%
  mutate(Legal_Affairs = as.ts(Legal_Affairs))%>%
  mutate(Social_Policy = as.ts(Social_Policy))%>%
  mutate(Housing = as.ts(Housing))%>%
  mutate(Business = as.ts(Business))%>%
  mutate(Defence = as.ts(Defence))%>%
  mutate(Research_Technology = as.ts(Research_Technology))%>%
  mutate(Foreign_Trade = as.ts(Foreign_Trade))%>%
  mutate(Foreign_Policy = as.ts(Foreign_Policy))%>%
  mutate(Government_Operations = as.ts(Government_Operations))%>%
  mutate(Public_Land = as.ts(Public_Land))%>%
  mutate(Culture = as.ts(Culture))

H3_ts$Partisan.control <- 
  factor(H3_ts$Partisan.control, levels = c("Left", "Center", "Right"))

Autoregressive <- dynlm(Entropy ~ L(Entropy) + Statements + Partisan.control, data = H3_ts)
stargazer(Autoregressive, covariate.labels = 
            c("Lagged Entropy", "Statements", "Center", "Right"),
          type = "html", out = "C:/Users/tuvamk/OneDrive - Universitetet i Oslo/Documents/Master/R-mapper/Analysis/AR.html")
#See that the diversity the previous year only accounts for about half of the diversity the next year.

#### Core Issues models ####
Macro1 <- dynlm(Entropy ~ L(Entropy) + Macroeconomics + 
                  Statements + Partisan.control, data = H3_ts)
Law12 <- dynlm(Entropy ~ L(Entropy) + Legal_Affairs + 
                 Statements + Partisan.control, data = H3_ts)
Defense16 <- dynlm(Entropy ~ L(Entropy) + Defence + 
                     Statements + Partisan.control, data = H3_ts)
Foreign19 <- dynlm(Entropy ~ L(Entropy) + Foreign_Policy + 
                     Statements + Partisan.control, data = H3_ts)
Government20 <- dynlm(Entropy ~ L(Entropy) + Government_Operations + 
                        Statements + Partisan.control, data = H3_ts)


#### Selective Issues models ####
Civil2 <- dynlm(Entropy ~ L(Entropy) + Civil_Liberties + 
                  Statements + Partisan.control, data = H3_ts)
Health3 <- dynlm(Entropy ~ L(Entropy) + Health + 
                   Statements + Partisan.control, data = H3_ts)
Agriculture4 <- dynlm(Entropy ~ L(Entropy) + Agriculture + 
                        Statements + Partisan.control, data = H3_ts)
Labour5 <- dynlm(Entropy ~ L(Entropy) + Labour + 
                   Statements + Partisan.control, data = H3_ts)
Environment7 <- dynlm(Entropy ~ L(Entropy) + Environment + 
                        Statements + Partisan.control, data = H3_ts)
Education6 <- dynlm(Entropy ~ L(Entropy) + Education + 
                      Statements + Partisan.control, data = H3_ts)
Energy8 <- dynlm(Entropy ~ L(Entropy) + Energy + 
                   Statements + Partisan.control, data = H3_ts)
Immigration9 <- dynlm(Entropy ~ L(Entropy) + Immigration + 
                        Statements + Partisan.control, data = H3_ts)
Traffic10 <- dynlm(Entropy ~ L(Entropy) + Traffic + 
                     Statements + Partisan.control, data = H3_ts)
Social13 <- dynlm(Entropy ~ L(Entropy) + Social_Policy + 
                    Statements + Partisan.control, data = H3_ts)
Housing14 <- dynlm(Entropy ~ L(Entropy) + Housing + 
                     Statements + Partisan.control, data = H3_ts)
Business15 <- dynlm(Entropy ~ L(Entropy) + Business + 
                      Statements + Partisan.control, data = H3_ts)
Technology17 <- dynlm(Entropy ~ L(Entropy) + Research_Technology + 
                        Statements + Partisan.control, data = H3_ts)
Trade18 <- dynlm(Entropy ~ L(Entropy) + Foreign_Trade + 
                   Statements + Partisan.control, data = H3_ts)
Land21 <- dynlm(Entropy ~ L(Entropy) + Public_Land + 
                  Statements + Partisan.control, data = H3_ts)
Culture23 <- dynlm(Entropy ~ L(Entropy) + Culture + 
                     Statements + Partisan.control, data = H3_ts)


#### Removing 2020 ####
library(dplyr)
H3_Omit20 <- H3 %>%
  filter(!Year == 2020)

#Transform all variables to time series objects to use dynlm
H3_Omit20 <- H3_Omit20 %>%
  mutate(Entropy = as.ts(Entropy)) %>%
  mutate(Statements = as.ts(Statements)) %>%
  mutate(Partisan.control = as.ts(Partisan.control))%>%
  mutate(Macroeconomics = as.ts(Macroeconomics))%>%
  mutate(Civil_Liberties = as.ts(Civil_Liberties))%>%
  mutate(Health = as.ts(Health))%>%
  mutate(Agriculture = as.ts(Agriculture))%>%
  mutate(Labour = as.ts(Labour))%>%
  mutate(Education = as.ts(Education))%>%
  mutate(Environment = as.ts(Environment))%>%
  mutate(Energy = as.ts(Energy))%>%
  mutate(Immigration = as.ts(Immigration))%>%
  mutate(Traffic = as.ts(Traffic))%>%
  mutate(Legal_Affairs = as.ts(Legal_Affairs))%>%
  mutate(Social_Policy = as.ts(Social_Policy))%>%
  mutate(Housing = as.ts(Housing))%>%
  mutate(Business = as.ts(Business))%>%
  mutate(Defence = as.ts(Defence))%>%
  mutate(Research_Technology = as.ts(Research_Technology))%>%
  mutate(Foreign_Trade = as.ts(Foreign_Trade))%>%
  mutate(Foreign_Policy = as.ts(Foreign_Policy))%>%
  mutate(Government_Operations = as.ts(Government_Operations))%>%
  mutate(Public_Land = as.ts(Public_Land))%>%
  mutate(Culture = as.ts(Culture))

library(dynlm)
H3_Omit20$Partisan.control <- factor(H3_Omit20$Partisan.control, levels = c("Left", "Center", "Right"))




#### AR model without 2020 ####
AR <- dynlm(Entropy ~ L(Entropy) + Statements + Partisan.control, data = H3_Omit20)
#See that the diversity the previous year only accounts for about half of the diversity the next year.

#### Core Issue models without 2020 ####
Macro <- dynlm(Entropy ~ L(Entropy) + Macroeconomics + 
                 Statements + Partisan.control, data = H3_Omit20)
H3_Omit20$Partisan.control <- factor(H3_Omit20$Partisan.control, levels = c("Left", "Center", "Right"))

Law <- dynlm(Entropy ~ L(Entropy) + Legal_Affairs + 
               Statements + Partisan.control, data = H3_Omit20)
H3_Omit20$Partisan.control <- factor(H3_Omit20$Partisan.control, levels = c("Left", "Center", "Right"))

Defense <- dynlm(Entropy ~ L(Entropy) + Defence + 
                   Statements + Partisan.control, data = H3_Omit20)
H3_Omit20$Partisan.control <- factor(H3_Omit20$Partisan.control, levels = c("Left", "Center", "Right"))

Foreign <- dynlm(Entropy ~ L(Entropy) + Foreign_Policy + 
                   Statements + Partisan.control, data = H3_Omit20)
H3_Omit20$Partisan.control <- factor(H3_Omit20$Partisan.control, levels = c("Left", "Center", "Right"))

Government <- dynlm(Entropy ~ L(Entropy) + Government_Operations + 
                      Statements + Partisan.control, data = H3_Omit20)
H3_Omit20$Partisan.control <- factor(H3_Omit20$Partisan.control, levels = c("Left", "Center", "Right"))


#### Selective Issue models without 2020####
Civil <- dynlm(Entropy ~ L(Entropy) + Civil_Liberties + 
                 Statements + Partisan.control, data = H3_Omit20)
Health <- dynlm(Entropy ~ L(Entropy) + Health + 
                  Statements + Partisan.control, data = H3_Omit20)
Agriculture <- dynlm(Entropy ~ L(Entropy) + Agriculture + 
                       Statements + Partisan.control, data = H3_Omit20)
Labor <- dynlm(Entropy ~ L(Entropy) + Labour + 
                 Statements + Partisan.control, data = H3_Omit20)
Environment <- dynlm(Entropy ~ L(Entropy) + Environment + 
                       Statements + Partisan.control, data = H3_Omit20)
Education <- dynlm(Entropy ~ L(Entropy) + Education + 
                     Statements + Partisan.control, data = H3_Omit20)
Energy <- dynlm(Entropy ~ L(Entropy) + Energy + 
                  Statements + Partisan.control, data = H3_Omit20)
Immigration <- dynlm(Entropy ~ L(Entropy) + Immigration + 
                       Statements + Partisan.control, data = H3_Omit20)
Traffic <- dynlm(Entropy ~ L(Entropy) + Traffic + 
                   Statements + Partisan.control, data = H3_Omit20)
Social <- dynlm(Entropy ~ L(Entropy) + Social_Policy + 
                  Statements + Partisan.control, data = H3_Omit20)
Housing <- dynlm(Entropy ~ L(Entropy) + Housing + 
                   Statements + Partisan.control, data = H3_Omit20)
Business <- dynlm(Entropy ~ L(Entropy) + Business + 
                    Statements + Partisan.control, data = H3_Omit20)
Technology <- dynlm(Entropy ~ L(Entropy) + Research_Technology + 
                      Statements + Partisan.control, data = H3_Omit20)
Trade <- dynlm(Entropy ~ L(Entropy) + Foreign_Trade + 
                 Statements + Partisan.control, data = H3_Omit20)
Land <- dynlm(Entropy ~ L(Entropy) + Public_Land + 
                Statements + Partisan.control, data = H3_Omit20)
Culture <- dynlm(Entropy ~ L(Entropy) + Culture + 
                   Statements + Partisan.control, data = H3_Omit20)

#### Analysis without 2011 and 2020 for Law ####
library(dplyr)
H3_Omit1120 <- H3_Omit20 %>%
  filter(!Year == 2011)

H3_Omit1120 <- H3_Omit1120 %>%
  mutate(Entropy = as.ts(Entropy)) %>%
  mutate(Statements = as.ts(Statements)) %>%
  mutate(Partisan.control = as.ts(Partisan.control))%>%
  mutate(Macroeconomics = as.ts(Macroeconomics))%>%
  mutate(Civil_Liberties = as.ts(Civil_Liberties))%>%
  mutate(Health = as.ts(Health))%>%
  mutate(Agriculture = as.ts(Agriculture))%>%
  mutate(Labour = as.ts(Labour))%>%
  mutate(Education = as.ts(Education))%>%
  mutate(Environment = as.ts(Environment))%>%
  mutate(Energy = as.ts(Energy))%>%
  mutate(Immigration = as.ts(Immigration))%>%
  mutate(Traffic = as.ts(Traffic))%>%
  mutate(Legal_Affairs = as.ts(Legal_Affairs))%>%
  mutate(Social_Policy = as.ts(Social_Policy))%>%
  mutate(Housing = as.ts(Housing))%>%
  mutate(Business = as.ts(Business))%>%
  mutate(Defence = as.ts(Defence))%>%
  mutate(Research_Technology = as.ts(Research_Technology))%>%
  mutate(Foreign_Trade = as.ts(Foreign_Trade))%>%
  mutate(Foreign_Policy = as.ts(Foreign_Policy))%>%
  mutate(Government_Operations = as.ts(Government_Operations))%>%
  mutate(Public_Land = as.ts(Public_Land))%>%
  mutate(Culture = as.ts(Culture))
library(dynlm)

H3_Omit1120$Partisan.control <- factor(H3_Omit1120$Partisan.control, levels = c("Left", "Center", "Right"))


Autoregg <- dynlm(Entropy ~ L(Entropy) + Statements + Partisan.control, data = H3_Omit1120)
Macro11 <- dynlm(Entropy ~ L(Entropy) + Macroeconomics + 
                   Statements + Partisan.control, data = H3_Omit1120)
Law2 <- dynlm(Entropy ~ L(Entropy) + Legal_Affairs + 
                Statements + Partisan.control, data = H3_Omit1120)
Defense166 <- dynlm(Entropy ~ L(Entropy) + Defence + 
                      Statements + Partisan.control, data = H3_Omit1120)
Foreign199 <- dynlm(Entropy ~ L(Entropy) + Foreign_Policy + 
                      Statements + Partisan.control, data = H3_Omit1120)
Government200 <- dynlm(Entropy ~ L(Entropy) + Government_Operations + 
                         Statements + Partisan.control, data = H3_Omit1120)


stargazer(Macro11, Law122, Defense166, Foreign199, Government200, type = "text")


Civil22 <- dynlm(Entropy ~ L(Entropy) + Civil_Liberties + 
                   Statements + Partisan.control, data = H3_Omit1120)
Health33 <- dynlm(Entropy ~ L(Entropy) + Health + 
                    Statements + Partisan.control, data = H3_Omit1120)
Agriculture44 <- dynlm(Entropy ~ L(Entropy) + Agriculture + 
                         Statements + Partisan.control, data = H3_Omit1120)
Labour55 <- dynlm(Entropy ~ L(Entropy) + Labour + 
                    Statements + Partisan.control, data = H3_Omit1120)
Environment77 <- dynlm(Entropy ~ L(Entropy) + Environment + 
                         Statements + Partisan.control, data = H3_Omit1120)
Education66 <- dynlm(Entropy ~ L(Entropy) + Education + 
                       Statements + Partisan.control, data = H3_Omit1120)
Energy88 <- dynlm(Entropy ~ L(Entropy) + Energy + 
                    Statements + Partisan.control, data = H3_Omit1120)
Immigration99 <- dynlm(Entropy ~ L(Entropy) + Immigration + 
                         Statements + Partisan.control, data = H3_Omit1120)
Traffic110 <- dynlm(Entropy ~ L(Entropy) + Traffic + 
                      Statements + Partisan.control, data = H3_Omit1120)
Social133 <- dynlm(Entropy ~ L(Entropy) + Social_Policy + 
                     Statements + Partisan.control, data = H3_Omit1120)
Housing144 <- dynlm(Entropy ~ L(Entropy) + Housing + 
                      Statements + Partisan.control, data = H3_Omit1120)
Business155 <- dynlm(Entropy ~ L(Entropy) + Business + 
                       Statements + Partisan.control, data = H3_Omit1120)
Technology177 <- dynlm(Entropy ~ L(Entropy) + Research_Technology + 
                         Statements + Partisan.control, data = H3_Omit1120)
Trade188 <- dynlm(Entropy ~ L(Entropy) + Foreign_Trade + 
                    Statements + Partisan.control, data = H3_Omit1120)
Land211 <- dynlm(Entropy ~ L(Entropy) + Public_Land + 
                   Statements + Partisan.control, data = H3_Omit1120)
Culture233 <- dynlm(Entropy ~ L(Entropy) + Culture + 
                      Statements + Partisan.control, data = H3_Omit1120)

#Omitting the 2011 does not change the overall conclusions for the rest of the issues
#so only Law-model will be included without 2011


#### Sensitivity tests : Cooks D ####
## LABOR and HEALTH
library(dynlm)
Health <- dynlm(Entropy ~ L(Entropy) + Health + 
                  Statements + Partisan.control, data = H3_ts)

Labour <- dynlm(Entropy ~ L(Entropy) + Labour + 
                  Statements + Partisan.control, data = H3_ts)


# Plot the Cook's Distance using the traditional 4/n criterion
Labor_cooksd <- cooks.distance(Labour)
sample_size <- nrow(H3_ts)
plot(Labor_cooksd, pch="*", cex=2, main="Influential Speeches by Cooks D")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(Labor_cooksd)+1, y=Labor_cooksd, labels=ifelse(Labor_cooksd>4/sample_size, names(Labor_cooksd),""), col="red")

Health_cooksd <- cooks.distance(Health)
sample_size <- nrow(H3_ts)
plot(Health_cooksd, pch="*", cex=2, main="Influential Speeches by Cooks D")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(Health_cooksd)+1, y=Health_cooksd, labels=ifelse(Health_cooksd>4/sample_size, names(Health_cooksd),""), col="red")

library(ggplot2)

ggplot(data = H3_ts, aes(x = Labour, y = Entropy)) +
  geom_point() + 
  geom_smooth(method = lm)+
  theme_classic()+
  ggtitle("Labor effect including 2020")
ggplot(data = H3_Omit20, aes(x = Labour, y = Entropy)) +
  geom_point() + 
  geom_smooth(method = lm)+
  ggtitle("Labor effect without 2020")+
  theme_classic()


ggplot(data = H3_ts, aes(x = Health, y = Entropy)) +
  geom_point() + 
  geom_smooth(method = lm)+
  theme_classic()+
  ggtitle("Health effect including 2020")
ggplot(data = H3_Omit20, aes(x = Health, y = Entropy)) +
  geom_point() + 
  geom_smooth(method = lm)+
  ggtitle("Health effect without 2020")+
  theme_classic()


### Cooks d core issues
Macro_cooksd <- cooks.distance(Macro)
sample_size <- nrow(H3_ts)
plot(Macro_cooksd, pch="*", cex=2, main="Influential Speeches by Cooks D") 
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(Macro_cooksd)+1, y=Macro_cooksd, labels=ifelse(Macro_cooksd>4/sample_size, names(Macro_cooksd),""), col="red")

Law_cooksd <- cooks.distance(Law)
sample_size <- nrow(H3_ts)
plot(Law_cooksd, pch="*", cex=2, main="Influential Speeches by Cooks D")  
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(Law_cooksd)+1, y=Law_cooksd, labels=ifelse(Law_cooksd>4/sample_size, names(Law_cooksd),""), col="red")

Defense_cooksd <- cooks.distance(Defense)
sample_size <- nrow(H3_ts)
plot(Defense_cooksd, pch="*", cex=2, main="Influential Speeches by Cooks D") 
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(Defense_cooksd)+1, y=Defense_cooksd, labels=ifelse(Defense_cooksd>4/sample_size, names(Defense_cooksd),""), col="red")

Foreign_cooksd <- cooks.distance(Foreign)
sample_size <- nrow(H3_ts)
plot(Foreign_cooksd, pch="*", cex=2, main="Influential Speeches by Cooks D") 
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(Foreign_cooksd)+1, y=Foreign_cooksd, labels=ifelse(Foreign_cooksd>4/sample_size, names(Foreign_cooksd),""), col="red")

Government_cooksd <- cooks.distance(Government)
sample_size <- nrow(H3_ts)
plot(Government_cooksd, pch="*", cex=2, main="Influential Speeches by Cooks D") 
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(Government_cooksd)+1, y=Government_cooksd, labels=ifelse(Government_cooksd>4/sample_size, names(Government_cooksd),""), col="red")

#Plotting core issue Cooks D scores over time
CooksD <- rbind(Macro_cooksd, Law_cooksd, Defense_cooksd, Foreign_cooksd, Government_cooksd,
                Labor_cooksd, Health_cooksd)
CooksD <- as.data.frame(CooksD)
CooksD <- t(CooksD)
CooksD <- as.data.frame(CooksD)


#### Cooks D selective ####
Ccooksd <- cooks.distance(Civil2)
sample_size <- nrow(H3_ts)
plot(Ccooksd, pch="*", cex=2, main="Influential Speeches by Cooks D")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(Ccooksd)+1, y=Ccooksd, labels=ifelse(Ccooksd>4/sample_size, names(Ccooksd),""), col="red")

Acooksd <- cooks.distance(Agriculture4)
sample_size <- nrow(H3_ts)
plot(Acooksd, pch="*", cex=2, main="Influential Speeches by Cooks D")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(Acooksd)+1, y=Acooksd, labels=ifelse(Acooksd>4/sample_size, names(Acooksd),""), col="red")

Ecooksd <- cooks.distance(Education6)
sample_size <- nrow(H3_ts)
plot(Ecooksd, pch="*", cex=2, main="Influential Speeches by Cooks D")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(Ecooksd)+1, y=Ecooksd, labels=ifelse(Ecooksd>4/sample_size, names(Ecooksd),""), col="red") 

Encooksd <- cooks.distance(Environment7)
sample_size <- nrow(H3_ts)
plot(Encooksd, pch="*", cex=2, main="Influential Speeches by Cooks D")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(Encooksd)+1, y=Encooksd, labels=ifelse(Encooksd>4/sample_size, names(Encooksd),""), col="red") 

Enecooksd <- cooks.distance(Energy8)
sample_size <- nrow(H3_ts)
plot(Enecooksd, pch="*", cex=2, main="Influential Speeches by Cooks D")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(Enecooksd)+1, y=Enecooksd, labels=ifelse(Enecooksd>4/sample_size, names(Enecooksd),""), col="red") 

Icooksd <- cooks.distance(Immigration9)
sample_size <- nrow(H3_ts)
plot(Icooksd, pch="*", cex=2, main="Influential Speeches by Cooks D")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(Icooksd)+1, y=Icooksd, labels=ifelse(Icooksd>4/sample_size, names(Icooksd),""), col="red") 

Tcooksd <- cooks.distance(Traffic10)
sample_size <- nrow(H3_ts)
plot(Tcooksd, pch="*", cex=2, main="Influential Speeches by Cooks D")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(Tcooksd)+1, y=Tcooksd, labels=ifelse(Tcooksd>4/sample_size, names(Tcooksd),""), col="red") 

Scooksd <- cooks.distance(Social13)
sample_size <- nrow(H3_ts)
plot(Scooksd, pch="*", cex=2, main="Influential Speeches by Cooks D")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(Scooksd)+1, y=Scooksd, labels=ifelse(Scooksd>4/sample_size, names(Scooksd),""), col="red") 

Hcooksd <- cooks.distance(Housing14)
sample_size <- nrow(H3_ts)
plot(Hcooksd, pch="*", cex=2, main="Influential Speeches by Cooks D")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(Hcooksd)+1, y=Hcooksd, labels=ifelse(Hcooksd>4/sample_size, names(Hcooksd),""), col="red") 

Bcooksd <- cooks.distance(Business15)
sample_size <- nrow(H3_ts)
plot(Bcooksd, pch="*", cex=2, main="Influential Speeches by Cooks D")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(Bcooksd)+1, y=Bcooksd, labels=ifelse(Bcooksd>4/sample_size, names(Bcooksd),""), col="red") 

Teccooksd <- cooks.distance(Technology17)
sample_size <- nrow(H3_ts)
plot(Teccooksd, pch="*", cex=2, main="Influential Speeches by Cooks D")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(Teccooksd)+1, y=Teccooksd, labels=ifelse(Teccooksd>4/sample_size, names(Teccooksd),""), col="red")

FTcooksd <- cooks.distance(Trade18)
sample_size <- nrow(H3_ts)
plot(FTcooksd, pch="*", cex=2, main="Influential Speeches by Cooks D")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(FTcooksd)+1, y=FTcooksd, labels=ifelse(FTcooksd>4/sample_size, names(FTcooksd),""), col="red")

Lcooksd <- cooks.distance(Land21)
sample_size <- nrow(H3_ts)
plot(Lcooksd, pch="*", cex=2, main="Influential Speeches by Cooks D")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(Lcooksd)+1, y=Lcooksd, labels=ifelse(Lcooksd>4/sample_size, names(Lcooksd),""), col="red")

Cucooksd <- cooks.distance(Culture23)
sample_size <- nrow(H3_ts)
plot(Cucooksd, pch="*", cex=2, main="Influential Speeches by Cooks D")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(Cucooksd)+1, y=Cucooksd, labels=ifelse(Cucooksd>4/sample_size, names(Cucooksd),""), col="red")

#### Cooks D plots for validity illustrations: years ####
#To get a year-timeline on the x-axcis, the first speech of 1959 is removed
library(dplyr)
library(tidyverse)
CooksD <- rbind(Macro_cooksd, Law_cooksd, Defense_cooksd, Foreign_cooksd, Government_cooksd,
                Labor_cooksd, Health_cooksd)
CooksD <- as.data.frame(CooksD)
CooksD <- t(CooksD)
CooksD$Health_cooksd <- as.numeric(CooksD$Health_cooksd)
CooksD$Labor_cooksd <- as.numeric(CooksD$Labor_cooksd)
CooksD$Macro_cooksd <- as.numeric(CooksD$Macro_cooksd)
CooksD$Law_cooksd <- as.numeric(CooksD$Law_cooksd)
CooksD$Defense_cooksd <- as.numeric(CooksD$Defense_cooksd)
CooksD$Foreign_cooksd <- as.numeric(CooksD$Foreign_cooksd)
CooksD$Government_cooksd <- as.numeric(CooksD$Government_cooksd)

max(CooksD$Health_cooksd)
library(tidyverse)

CooksD <- rename(CooksD, Macro = Macro_cooksd)
CooksD <- rename(CooksD, Law = Law_cooksd)
CooksD <- rename(CooksD, Defense = Defense_cooksd)
CooksD <- rename(CooksD, Foreign = Foreign_cooksd)
CooksD <- rename(CooksD, Goverment = Government_cooksd)
CooksD <- rename(CooksD, Health = Health_cooksd)
CooksD <- rename(CooksD, Labor = Labor_cooksd)

#### All issues
CooksD <- CooksD %>%
  mutate(SpeechNumber = 2:77)

CooksLong <- CooksD %>%
  pivot_longer(!SpeechNumber)

CooksLong <- rename(CooksLong, Topics = name)
CooksLong <- rename(CooksLong, CooksD = value)

ggplot(data = CooksLong, aes(x = SpeechNumber, y = CooksD, color = Topics))+
  geom_line()+
  theme_classic()

####Core 
CooksD2 <- subset(CooksD, select = -c(Labor, Health))
CooksD2 <- CooksD2[-14, ]
CooksD2 <- CooksD2 %>%
  mutate(Year = 1947:2021)
CooksLong2 <- CooksD2 %>%
  pivot_longer(!c(SpeechNumber, Year))

CooksLong2 <- rename(CooksLong2, Core.Issues = Topics)
CooksLong2 <- rename(CooksLong2, CooksD = value)

ggplot(data = CooksLong2, aes(x = Year, y = CooksD, color = Core.Issues))+
  geom_line()+
  theme_classic()+
  geom_hline(yintercept = 4/76, col = "darkblue")+
  ggtitle("Influential Speeches by Cooks D: Core Issues")+
  scale_x_continuous(breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2011, 2020))

####Labor
CooksLABOR <- subset(CooksD, select = c(Labor))
CooksLABOR <- CooksLABOR %>%
  mutate(SpeechNumber = 2:77)
CooksLABORlong <- CooksLABOR %>%
  pivot_longer(!SpeechNumber)

CooksLABORlong <- rename(CooksLABORlong, Labor = name)
CooksLABORlong <- rename(CooksLABORlong, CooksD = value)

ggplot(data = CooksLABORlong, aes(x = SpeechNumber, y = CooksD, color = Labor))+
  geom_line()+
  theme_classic()+
  geom_hline(yintercept = 4/76, col = "blue")+
  ggtitle("Influencial Speeches by Cooks D: Labor")

CooksLABOR2 <- CooksLABOR[-14, ]
CooksLABOR2 <- CooksLABOR2 %>%
  mutate(Year = 1947:2021)
CooksLABORlong2 <- CooksLABOR2 %>%
  pivot_longer(!c(Year, SpeechNumber))
CooksLABORlong2 <- rename(CooksLABORlong2, CooksD = value)
CooksLABORlong2 <- rename(CooksLABORlong2, Labor = name)

ggplot(data = CooksLABORlong2, aes(x = Year, y = CooksD, color = Labor))+
  geom_line()+
  theme_classic()+
  geom_hline(yintercept = 4/76, col = "blue")+
  ggtitle("Influencial Speeches by Cooks D: Labor")

####Health
CooksHEALTH <- subset(CooksD, select = c(Health))
CooksHEALTH2 <- CooksHEALTH[-14, ]

CooksHEALTH2 <- CooksHEALTH2 %>%
  mutate(Year = 1947:2021)

CooksHEALTHlong2 <- CooksHEALTH2 %>%
  pivot_longer(!c(Year, SpeechNumber))

CooksHEALTHlong2 <- rename(CooksHEALTHlong2, CooksD = value)


ggplot(data = CooksHEALTHlong2, aes(x = Year, y = CooksD, color = Health))+
  geom_line()+
  theme_classic()+
  geom_hline(yintercept = 4/76, col = "blue")+
  ggtitle("Influencial Speeches by Cooks D: Health")


