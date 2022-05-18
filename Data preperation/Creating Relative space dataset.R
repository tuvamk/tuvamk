library(openxlsx)
library(tidyverse)
library(textreadr)
library(haven)
library(tidycomm)
library(ggplot2)
library(MASS)
library(writexl)

#Estimating relative space devoted to Major topics, but omitting NA's (2999)
#This is more consistent with former literature 
#Better measurement of policy statements

#### 1946-51 ####
TT1946 <- read.xlsx("Trontale1946_90 (1).xlsx")
TT1947 <- read.xlsx("Trontale1947_91.xlsx")
TT1948 <- read.xlsx("Trontale1948 (1).xlsx")
TT1949 <- read.xlsx("Trontale1949_93.xlsx")
TT1950 <- read.xlsx("Trontale1950_94.xlsx")
TT1951 <- read.xlsx("Trontale1951_95.xlsx")

TT1946$Major_Topic <- ifelse(TT1946$Norway_Code <200,1, 
                             ifelse(TT1946$Norway_Code <300, 2, 
                                    ifelse(TT1946$Norway_Code < 400,3,
                                           ifelse(TT1946$Norway_Code < 500, 4,
                                                  ifelse(TT1946$Norway_Code < 600,5, 
                                                         ifelse(TT1946$Norway_Code < 700, 6,
                                                                ifelse(TT1946$Norway_Code < 800,7,
                                                                       ifelse(TT1946$Norway_Code < 900,8,
                                                                              ifelse(TT1946$Norway_Code < 1000,9,
                                                                                     ifelse(TT1946$Norway_Code < 1100,10,
                                                                                            ifelse(TT1946$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1946$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1946$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1946$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1946$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1946$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1946$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1946$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1946$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1946$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1946$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1946 <-  TT1946 %>%
  mutate(Major_Topic_Name = ifelse(TT1946$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1946$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1946$Norway_Code < 400,"Health",
                                                 ifelse(TT1946$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1946$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1946$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1946$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1946$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1946$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1946$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1946$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1946$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1946$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1946$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1946$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1946$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1946$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1946$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1946$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1946$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1946$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))






TT1947$Major_Topic <- ifelse(TT1947$Norway_Code <200,1, 
                             ifelse(TT1947$Norway_Code <300, 2, 
                                    ifelse(TT1947$Norway_Code < 400,3,
                                           ifelse(TT1947$Norway_Code < 500, 4,
                                                  ifelse(TT1947$Norway_Code < 600,5, 
                                                         ifelse(TT1947$Norway_Code < 700, 6,
                                                                ifelse(TT1947$Norway_Code < 800,7,
                                                                       ifelse(TT1947$Norway_Code < 900,8,
                                                                              ifelse(TT1947$Norway_Code < 1000,9,
                                                                                     ifelse(TT1947$Norway_Code < 1100,10,
                                                                                            ifelse(TT1947$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1947$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1947$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1947$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1947$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1947$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1947$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1947$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1947$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1947$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1947$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1947 <-  TT1947 %>%
  mutate(Major_Topic_Name = ifelse(TT1947$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1947$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1947$Norway_Code < 400,"Health",
                                                 ifelse(TT1947$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1947$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1947$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1947$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1947$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1947$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1947$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1947$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1947$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1947$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1947$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1947$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1947$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1947$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1947$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1947$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1947$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1947$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))



TT1948$Major_Topic <- ifelse(TT1948$Norway_Code <200,1, 
                             ifelse(TT1948$Norway_Code <300, 2, 
                                    ifelse(TT1948$Norway_Code < 400,3,
                                           ifelse(TT1948$Norway_Code < 500, 4,
                                                  ifelse(TT1948$Norway_Code < 600,5, 
                                                         ifelse(TT1948$Norway_Code < 700, 6,
                                                                ifelse(TT1948$Norway_Code < 800,7,
                                                                       ifelse(TT1948$Norway_Code < 900,8,
                                                                              ifelse(TT1948$Norway_Code < 1000,9,
                                                                                     ifelse(TT1948$Norway_Code < 1100,10,
                                                                                            ifelse(TT1948$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1948$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1948$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1948$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1948$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1948$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1948$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1948$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1948$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1948$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1948$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1948 <-  TT1948 %>%
  mutate(Major_Topic_Name = ifelse(TT1948$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1948$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1948$Norway_Code < 400,"Health",
                                                 ifelse(TT1948$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1948$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1948$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1948$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1948$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1948$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1948$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1948$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1948$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1948$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1948$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1948$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1948$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1948$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1948$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1948$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1948$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1948$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))



TT1949$Major_Topic <- ifelse(TT1949$Norway_Code <200,1, 
                             ifelse(TT1949$Norway_Code <300, 2, 
                                    ifelse(TT1949$Norway_Code < 400,3,
                                           ifelse(TT1949$Norway_Code < 500, 4,
                                                  ifelse(TT1949$Norway_Code < 600,5, 
                                                         ifelse(TT1949$Norway_Code < 700, 6,
                                                                ifelse(TT1949$Norway_Code < 800,7,
                                                                       ifelse(TT1949$Norway_Code < 900,8,
                                                                              ifelse(TT1949$Norway_Code < 1000,9,
                                                                                     ifelse(TT1949$Norway_Code < 1100,10,
                                                                                            ifelse(TT1949$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1949$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1949$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1949$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1949$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1949$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1949$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1949$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1949$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1949$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1949$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1949 <-  TT1949 %>%
  mutate(Major_Topic_Name = ifelse(TT1949$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1949$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1949$Norway_Code < 400,"Health",
                                                 ifelse(TT1949$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1949$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1949$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1949$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1949$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1949$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1949$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1949$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1949$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1949$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1949$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1949$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1949$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1949$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1949$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1949$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1949$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1949$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))




TT1950$Major_Topic <- ifelse(TT1950$Norway_Code <200,1, 
                             ifelse(TT1950$Norway_Code <300, 2, 
                                    ifelse(TT1950$Norway_Code < 400,3,
                                           ifelse(TT1950$Norway_Code < 500, 4,
                                                  ifelse(TT1950$Norway_Code < 600,5, 
                                                         ifelse(TT1950$Norway_Code < 700, 6,
                                                                ifelse(TT1950$Norway_Code < 800,7,
                                                                       ifelse(TT1950$Norway_Code < 900,8,
                                                                              ifelse(TT1950$Norway_Code < 1000,9,
                                                                                     ifelse(TT1950$Norway_Code < 1100,10,
                                                                                            ifelse(TT1950$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1950$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1950$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1950$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1950$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1950$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1950$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1950$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1950$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1950$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1950$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1950 <-  TT1950 %>%
  mutate(Major_Topic_Name = ifelse(TT1950$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1950$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1950$Norway_Code < 400,"Health",
                                                 ifelse(TT1950$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1950$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1950$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1950$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1950$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1950$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1950$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1950$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1950$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1950$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1950$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1950$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1950$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1950$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1950$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1950$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1950$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1950$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))





TT1951$Major_Topic <- ifelse(TT1951$Norway_Code <200,1, 
                             ifelse(TT1951$Norway_Code <300, 2, 
                                    ifelse(TT1951$Norway_Code < 400,3,
                                           ifelse(TT1951$Norway_Code < 500, 4,
                                                  ifelse(TT1951$Norway_Code < 600,5, 
                                                         ifelse(TT1951$Norway_Code < 700, 6,
                                                                ifelse(TT1951$Norway_Code < 800,7,
                                                                       ifelse(TT1951$Norway_Code < 900,8,
                                                                              ifelse(TT1951$Norway_Code < 1000,9,
                                                                                     ifelse(TT1951$Norway_Code < 1100,10,
                                                                                            ifelse(TT1951$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1951$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1951$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1951$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1951$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1951$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1951$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1951$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1951$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1951$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1951$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1951 <-  TT1951 %>%
  mutate(Major_Topic_Name = ifelse(TT1951$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1951$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1951$Norway_Code < 400,"Health",
                                                 ifelse(TT1951$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1951$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1951$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1951$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1951$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1951$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1951$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1951$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1951$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1951$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1951$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1951$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1951$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1951$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1951$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1951$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1951$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1951$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))

TT1946 <- na.omit(TT1946)
TT1947 <- na.omit(TT1947)
TT1948 <- na.omit(TT1948)
TT1949 <- na.omit(TT1949)
TT1950 <- na.omit(TT1950)
TT1951 <- na.omit(TT1951)

#### Relativt ####
Var1 <- as.factor(c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,23))

R_1946 <- as.data.frame(table(TT1946$Major_Topic)/(length(TT1946$Major_Topic)))
#Finner den relative fordelingen innenfor hvert Major topic

data <- as.data.frame(Var1)
#Gjør om kategoriene i Var1 til dataframe

R_1946 <- full_join(data, R_1946)
R_1946[is.na(R_1946)] <- 0
#Setter sammen data med relativ fordeling, og at missing på data-variabelen = 0 og ikke NA
R_1946 <- subset(R_1946, select = Freq)
R_1946 <- as.data.frame(t(R_1946))
#Lager et subset med kun de relative verdiene, og snur om rader og kolonner slik at hver major topic blir en variabel
R_1946 <- rename(R_1946, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1947 <- as.data.frame(table(TT1947$Major_Topic)/(length(TT1947$Major_Topic)))
data <- as.data.frame(Var1)
R_1947 <- full_join(data, R_1947)
R_1947[is.na(R_1947)] <- 0
R_1947 <- subset(R_1947, select = Freq)
R_1947 <- as.data.frame(t(R_1947))
R_1947 <- rename(R_1947, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1948 <- as.data.frame(table(TT1948$Major_Topic)/(length(TT1948$Major_Topic)))
data <- as.data.frame(Var1)
R_1948 <- full_join(data, R_1948)
R_1948[is.na(R_1948)] <- 0
R_1948 <- subset(R_1948, select = Freq)
R_1948 <- as.data.frame(t(R_1948))
R_1948 <- rename(R_1948, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1949 <- as.data.frame(table(TT1949$Major_Topic)/(length(TT1949$Major_Topic)))
data <- as.data.frame(Var1)
R_1949 <- full_join(data, R_1949)
R_1949[is.na(R_1949)] <- 0
R_1949 <- subset(R_1949, select = Freq)
R_1949 <- as.data.frame(t(R_1949))
R_1949 <- rename(R_1949, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1950 <- as.data.frame(table(TT1950$Major_Topic)/(length(TT1950$Major_Topic)))
data <- as.data.frame(Var1)
R_1950 <- full_join(data, R_1950)
R_1950[is.na(R_1950)] <- 0
R_1950 <- subset(R_1950, select = Freq)
R_1950 <- as.data.frame(t(R_1950))
R_1950 <- rename(R_1950, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)


R_1951 <- as.data.frame(table(TT1951$Major_Topic)/(length(TT1951$Major_Topic)))
data <- as.data.frame(Var1)
R_1951 <- full_join(data, R_1951)
R_1951[is.na(R_1951)] <- 0
R_1951 <- subset(R_1951, select = Freq)
R_1951 <- as.data.frame(t(R_1951))
R_1951 <- rename(R_1951, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

Gerhardsen2 <- rbind(R_1946, R_1947, R_1948, R_1949, R_1950, R_1951)

#### 1952 - 1955 ####

TT1952 <- read.xlsx("Trontale1952 (1).xlsx")
TT1953 <- read.xlsx("Trontale1953_97.xlsx")
TT1954 <- read.xlsx("Trontale1954_98.xlsx")
TT1955 <- read.xlsx("Trontale1955_99.xlsx")

#### Variabler ####

TT1952$Major_Topic <- ifelse(TT1952$Norway_Code <200,1, 
                             ifelse(TT1952$Norway_Code <300, 2, 
                                    ifelse(TT1952$Norway_Code < 400,3,
                                           ifelse(TT1952$Norway_Code < 500, 4,
                                                  ifelse(TT1952$Norway_Code < 600,5, 
                                                         ifelse(TT1952$Norway_Code < 700, 6,
                                                                ifelse(TT1952$Norway_Code < 800,7,
                                                                       ifelse(TT1952$Norway_Code < 900,8,
                                                                              ifelse(TT1952$Norway_Code < 1000,9,
                                                                                     ifelse(TT1952$Norway_Code < 1100,10,
                                                                                            ifelse(TT1952$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1952$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1952$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1952$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1952$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1952$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1952$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1952$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1952$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1952$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1952$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1952 <-  TT1952 %>%
  mutate(Major_Topic_Name = ifelse(TT1952$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1952$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1952$Norway_Code < 400,"Health",
                                                 ifelse(TT1952$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1952$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1952$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1952$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1952$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1952$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1952$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1952$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1952$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1952$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1952$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1952$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1952$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1952$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1952$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1952$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1952$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1952$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))




TT1953$Major_Topic <- ifelse(TT1953$Norway_Code <200,1, 
                             ifelse(TT1953$Norway_Code <300, 2, 
                                    ifelse(TT1953$Norway_Code < 400,3,
                                           ifelse(TT1953$Norway_Code < 500, 4,
                                                  ifelse(TT1953$Norway_Code < 600,5, 
                                                         ifelse(TT1953$Norway_Code < 700, 6,
                                                                ifelse(TT1953$Norway_Code < 800,7,
                                                                       ifelse(TT1953$Norway_Code < 900,8,
                                                                              ifelse(TT1953$Norway_Code < 1000,9,
                                                                                     ifelse(TT1953$Norway_Code < 1100,10,
                                                                                            ifelse(TT1953$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1953$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1953$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1953$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1953$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1953$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1953$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1953$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1953$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1953$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1953$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1953 <-  TT1953 %>%
  mutate(Major_Topic_Name = ifelse(TT1953$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1953$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1953$Norway_Code < 400,"Health",
                                                 ifelse(TT1953$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1953$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1953$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1953$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1953$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1953$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1953$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1953$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1953$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1953$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1953$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1953$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1953$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1953$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1953$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1953$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1953$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1953$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))




TT1954$Major_Topic <- ifelse(TT1954$Norway_Code <200,1, 
                             ifelse(TT1954$Norway_Code <300, 2, 
                                    ifelse(TT1954$Norway_Code < 400,3,
                                           ifelse(TT1954$Norway_Code < 500, 4,
                                                  ifelse(TT1954$Norway_Code < 600,5, 
                                                         ifelse(TT1954$Norway_Code < 700, 6,
                                                                ifelse(TT1954$Norway_Code < 800,7,
                                                                       ifelse(TT1954$Norway_Code < 900,8,
                                                                              ifelse(TT1954$Norway_Code < 1000,9,
                                                                                     ifelse(TT1954$Norway_Code < 1100,10,
                                                                                            ifelse(TT1954$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1954$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1954$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1954$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1954$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1954$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1954$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1954$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1954$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1954$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1954$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1954 <-  TT1954 %>%
  mutate(Major_Topic_Name = ifelse(TT1954$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1954$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1954$Norway_Code < 400,"Health",
                                                 ifelse(TT1954$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1954$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1954$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1954$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1954$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1954$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1954$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1954$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1954$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1954$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1954$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1954$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1954$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1954$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1954$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1954$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1954$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1954$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))


TT1955$Major_Topic <- ifelse(TT1955$Norway_Code <200,1, 
                             ifelse(TT1955$Norway_Code <300, 2, 
                                    ifelse(TT1955$Norway_Code < 400,3,
                                           ifelse(TT1955$Norway_Code < 500, 4,
                                                  ifelse(TT1955$Norway_Code < 600,5, 
                                                         ifelse(TT1955$Norway_Code < 700, 6,
                                                                ifelse(TT1955$Norway_Code < 800,7,
                                                                       ifelse(TT1955$Norway_Code < 900,8,
                                                                              ifelse(TT1955$Norway_Code < 1000,9,
                                                                                     ifelse(TT1955$Norway_Code < 1100,10,
                                                                                            ifelse(TT1955$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1955$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1955$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1955$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1955$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1955$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1955$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1955$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1955$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1955$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1955$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1955 <-  TT1955 %>%
  mutate(Major_Topic_Name = ifelse(TT1955$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1955$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1955$Norway_Code < 400,"Health",
                                                 ifelse(TT1955$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1955$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1955$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1955$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1955$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1955$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1955$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1955$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1955$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1955$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1955$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1955$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1955$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1955$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1955$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1955$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1955$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1955$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))


TT1952 <- na.omit(TT1952)
TT1953 <- na.omit(TT1953)
TT1954 <- na.omit(TT1954)
TT1955 <- na.omit(TT1955)


#### Relativt ####

Var1 <- as.factor(c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,23))

R_1952 <- as.data.frame(table(TT1952$Major_Topic)/(length(TT1952$Major_Topic)))
#Finner den relative fordelingen innenfor hvert Major topic

data <- as.data.frame(Var1)
#Gjør om kategoriene i Var1 til dataframe

R_1952 <- full_join(data, R_1952)
R_1952[is.na(R_1952)] <- 0
#Setter sammen data med relativ fordeling, og at missing på data-variabelen = 0 og ikke NA
R_1952 <- subset(R_1952, select = Freq)
R_1952 <- as.data.frame(t(R_1952))
#Lager et subset med kun de relative verdiene, og snur om rader og kolonner slik at hver major topic blir en variabel

R_1952 <- rename(R_1952, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1953 <- as.data.frame(table(TT1953$Major_Topic)/(length(TT1953$Major_Topic)))
#Finner den relative fordelingen innenfor hvert Major topic

data <- as.data.frame(Var1)
#Gjør om kategoriene i Var1 til dataframe

R_1953 <- full_join(data, R_1953)
R_1953[is.na(R_1953)] <- 0
#Setter sammen data med relativ fordeling, og at missing på data-variabelen = 0 og ikke NA
R_1953 <- subset(R_1953, select = Freq)
R_1953 <- as.data.frame(t(R_1953))
#Lager et subset med kun de relative verdiene, og snur om rader og kolonner slik at hver major topic blir en variabel

R_1953 <- rename(R_1953, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)




R_1954 <- as.data.frame(table(TT1954$Major_Topic)/(length(TT1954$Major_Topic)))
data <- as.data.frame(Var1)
#Gjør om kategoriene i Var1 til dataframe
R_1954 <- full_join(data, R_1954)
R_1954[is.na(R_1954)] <- 0
#Setter sammen data med relativ fordeling, og at missing på data-variabelen = 0 og ikke NA
R_1954 <- subset(R_1954, select = Freq)
R_1954 <- as.data.frame(t(R_1954))
#Lager et subset med kun de relative verdiene, og snur om rader og kolonner slik at hver major topic blir en variabel

R_1954 <- rename(R_1954, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)


R_1955 <- as.data.frame(table(TT1955$Major_Topic)/(length(TT1955$Major_Topic)))
#Finner den relative fordelingen innenfor hvert Major topic

data <- as.data.frame(Var1)
#Gjør om kategoriene i Var1 til dataframe

R_1955 <- full_join(data, R_1955)
R_1955[is.na(R_1955)] <- 0
#Setter sammen data med relativ fordeling, og at missing på data-variabelen = 0 og ikke NA
R_1955 <- subset(R_1955, select = Freq)
R_1955 <- as.data.frame(t(R_1955))
#Lager et subset med kun de relative verdiene, og snur om rader og kolonner slik at hver major topic blir en variabel

R_1955 <- rename(R_1955, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

#### Kobiner ####
Torp1952_1955 <- rbind(R_1952, R_1953, R_1954, R_1955)


#### 1956 - 62 ####
TT1956 <- read.xlsx("Trontale1956_100 (1).xlsx")
TT1957 <- read.xlsx("Trontale1957_101.xlsx")
TT1958 <- read.xlsx("Trontale1958_102.xlsx")
TT1959_1 <- read.xlsx("Trontale1959_103.xlsx")
TT1959_2 <- read.xlsx("Trontale1959_104.xlsx")
TT1960 <- read.xlsx("Trontale1960_105.xlsx")
TT1961 <- read.xlsx("Trontale1961_106.xlsx")
TT1962 <- read.xlsx("Trontale1962_107.xlsx")

TT1956$Major_Topic <- ifelse(TT1956$Norway_Code <200,1, 
                             ifelse(TT1956$Norway_Code <300, 2, 
                                    ifelse(TT1956$Norway_Code < 400,3,
                                           ifelse(TT1956$Norway_Code < 500, 4,
                                                  ifelse(TT1956$Norway_Code < 600,5, 
                                                         ifelse(TT1956$Norway_Code < 700, 6,
                                                                ifelse(TT1956$Norway_Code < 800,7,
                                                                       ifelse(TT1956$Norway_Code < 900,8,
                                                                              ifelse(TT1956$Norway_Code < 1000,9,
                                                                                     ifelse(TT1956$Norway_Code < 1100,10,
                                                                                            ifelse(TT1956$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1956$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1956$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1956$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1956$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1956$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1956$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1956$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1956$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1956$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1956$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1956 <-  TT1956 %>%
  mutate(Major_Topic_Name = ifelse(TT1956$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1956$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1956$Norway_Code < 400,"Health",
                                                 ifelse(TT1956$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1956$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1956$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1956$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1956$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1956$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1956$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1956$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1956$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1956$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1956$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1956$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1956$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1956$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1956$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1956$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1956$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1956$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))





TT1957$Major_Topic <- ifelse(TT1957$Norway_Code <200,1, 
                             ifelse(TT1957$Norway_Code <300, 2, 
                                    ifelse(TT1957$Norway_Code < 400,3,
                                           ifelse(TT1957$Norway_Code < 500, 4,
                                                  ifelse(TT1957$Norway_Code < 600,5, 
                                                         ifelse(TT1957$Norway_Code < 700, 6,
                                                                ifelse(TT1957$Norway_Code < 800,7,
                                                                       ifelse(TT1957$Norway_Code < 900,8,
                                                                              ifelse(TT1957$Norway_Code < 1000,9,
                                                                                     ifelse(TT1957$Norway_Code < 1100,10,
                                                                                            ifelse(TT1957$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1957$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1957$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1957$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1957$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1957$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1957$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1957$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1957$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1957$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1957$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1957 <-  TT1957 %>%
  mutate(Major_Topic_Name = ifelse(TT1957$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1957$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1957$Norway_Code < 400,"Health",
                                                 ifelse(TT1957$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1957$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1957$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1957$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1957$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1957$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1957$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1957$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1957$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1957$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1957$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1957$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1957$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1957$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1957$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1957$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1957$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1957$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))





TT1958$Major_Topic <- ifelse(TT1958$Norway_Code <200,1, 
                             ifelse(TT1958$Norway_Code <300, 2, 
                                    ifelse(TT1958$Norway_Code < 400,3,
                                           ifelse(TT1958$Norway_Code < 500, 4,
                                                  ifelse(TT1958$Norway_Code < 600,5, 
                                                         ifelse(TT1958$Norway_Code < 700, 6,
                                                                ifelse(TT1958$Norway_Code < 800,7,
                                                                       ifelse(TT1958$Norway_Code < 900,8,
                                                                              ifelse(TT1958$Norway_Code < 1000,9,
                                                                                     ifelse(TT1958$Norway_Code < 1100,10,
                                                                                            ifelse(TT1958$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1958$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1958$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1958$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1958$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1958$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1958$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1958$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1958$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1958$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1958$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1958 <-  TT1958 %>%
  mutate(Major_Topic_Name = ifelse(TT1958$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1958$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1958$Norway_Code < 400,"Health",
                                                 ifelse(TT1958$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1958$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1958$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1958$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1958$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1958$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1958$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1958$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1958$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1958$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1958$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1958$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1958$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1958$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1958$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1958$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1958$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1958$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))





TT1959_1$Major_Topic <- ifelse(TT1959_1$Norway_Code <200,1, 
                               ifelse(TT1959_1$Norway_Code <300, 2, 
                                      ifelse(TT1959_1$Norway_Code < 400,3,
                                             ifelse(TT1959_1$Norway_Code < 500, 4,
                                                    ifelse(TT1959_1$Norway_Code < 600,5, 
                                                           ifelse(TT1959_1$Norway_Code < 700, 6,
                                                                  ifelse(TT1959_1$Norway_Code < 800,7,
                                                                         ifelse(TT1959_1$Norway_Code < 900,8,
                                                                                ifelse(TT1959_1$Norway_Code < 1000,9,
                                                                                       ifelse(TT1959_1$Norway_Code < 1100,10,
                                                                                              ifelse(TT1959_1$Norway_Code < 1300,12,
                                                                                                     ifelse(TT1959_1$Norway_Code < 1400,13, 
                                                                                                            ifelse(TT1959_1$Norway_Code < 1500,14,
                                                                                                                   ifelse(TT1959_1$Norway_Code < 1600,15,
                                                                                                                          ifelse(TT1959_1$Norway_Code < 1700,16, 
                                                                                                                                 ifelse(TT1959_1$Norway_Code < 1800,17,
                                                                                                                                        ifelse(TT1959_1$Norway_Code < 1900,18,
                                                                                                                                               ifelse(TT1959_1$Norway_Code < 2000,19,
                                                                                                                                                      ifelse(TT1959_1$Norway_Code < 2100,20,
                                                                                                                                                             ifelse(TT1959_1$Norway_Code < 2200,21,
                                                                                                                                                                    ifelse(TT1959_1$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1959_1 <-  TT1959_1 %>%
  mutate(Major_Topic_Name = ifelse(TT1959_1$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1959_1$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1959_1$Norway_Code < 400,"Health",
                                                 ifelse(TT1959_1$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1959_1$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1959_1$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1959_1$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1959_1$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1959_1$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1959_1$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1959_1$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1959_1$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1959_1$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1959_1$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1959_1$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1959_1$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1959_1$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1959_1$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1959_1$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1959_1$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1959_1$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))






TT1959_2$Major_Topic <- ifelse(TT1959_2$Norway_Code <200,1, 
                               ifelse(TT1959_2$Norway_Code <300, 2, 
                                      ifelse(TT1959_2$Norway_Code < 400,3,
                                             ifelse(TT1959_2$Norway_Code < 500, 4,
                                                    ifelse(TT1959_2$Norway_Code < 600,5, 
                                                           ifelse(TT1959_2$Norway_Code < 700, 6,
                                                                  ifelse(TT1959_2$Norway_Code < 800,7,
                                                                         ifelse(TT1959_2$Norway_Code < 900,8,
                                                                                ifelse(TT1959_2$Norway_Code < 1000,9,
                                                                                       ifelse(TT1959_2$Norway_Code < 1100,10,
                                                                                              ifelse(TT1959_2$Norway_Code < 1300,12,
                                                                                                     ifelse(TT1959_2$Norway_Code < 1400,13, 
                                                                                                            ifelse(TT1959_2$Norway_Code < 1500,14,
                                                                                                                   ifelse(TT1959_2$Norway_Code < 1600,15,
                                                                                                                          ifelse(TT1959_2$Norway_Code < 1700,16, 
                                                                                                                                 ifelse(TT1959_2$Norway_Code < 1800,17,
                                                                                                                                        ifelse(TT1959_2$Norway_Code < 1900,18,
                                                                                                                                               ifelse(TT1959_2$Norway_Code < 2000,19,
                                                                                                                                                      ifelse(TT1959_2$Norway_Code < 2100,20,
                                                                                                                                                             ifelse(TT1959_2$Norway_Code < 2200,21,
                                                                                                                                                                    ifelse(TT1959_2$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1959_2 <-  TT1959_2 %>%
  mutate(Major_Topic_Name = ifelse(TT1959_2$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1959_2$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1959_2$Norway_Code < 400,"Health",
                                                 ifelse(TT1959_2$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1959_2$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1959_2$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1959_2$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1959_2$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1959_2$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1959_2$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1959_2$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1959_2$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1959_2$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1959_2$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1959_2$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1959_2$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1959_2$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1959_2$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1959_2$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1959_2$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1959_2$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))





TT1960$Major_Topic <- ifelse(TT1960$Norway_Code <200,1, 
                             ifelse(TT1960$Norway_Code <300, 2, 
                                    ifelse(TT1960$Norway_Code < 400,3,
                                           ifelse(TT1960$Norway_Code < 500, 4,
                                                  ifelse(TT1960$Norway_Code < 600,5, 
                                                         ifelse(TT1960$Norway_Code < 700, 6,
                                                                ifelse(TT1960$Norway_Code < 800,7,
                                                                       ifelse(TT1960$Norway_Code < 900,8,
                                                                              ifelse(TT1960$Norway_Code < 1000,9,
                                                                                     ifelse(TT1960$Norway_Code < 1100,10,
                                                                                            ifelse(TT1960$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1960$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1960$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1960$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1960$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1960$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1960$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1960$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1960$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1960$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1960$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1960 <-  TT1960 %>%
  mutate(Major_Topic_Name = ifelse(TT1960$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1960$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1960$Norway_Code < 400,"Health",
                                                 ifelse(TT1960$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1960$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1960$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1960$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1960$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1960$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1960$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1960$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1960$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1960$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1960$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1960$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1960$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1960$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1960$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1960$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1960$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1960$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))





TT1961$Major_Topic <- ifelse(TT1961$Norway_Code <200,1, 
                             ifelse(TT1961$Norway_Code <300, 2, 
                                    ifelse(TT1961$Norway_Code < 400,3,
                                           ifelse(TT1961$Norway_Code < 500, 4,
                                                  ifelse(TT1961$Norway_Code < 600,5, 
                                                         ifelse(TT1961$Norway_Code < 700, 6,
                                                                ifelse(TT1961$Norway_Code < 800,7,
                                                                       ifelse(TT1961$Norway_Code < 900,8,
                                                                              ifelse(TT1961$Norway_Code < 1000,9,
                                                                                     ifelse(TT1961$Norway_Code < 1100,10,
                                                                                            ifelse(TT1961$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1961$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1961$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1961$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1961$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1961$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1961$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1961$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1961$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1961$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1961$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1961 <-  TT1961 %>%
  mutate(Major_Topic_Name = ifelse(TT1961$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1961$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1961$Norway_Code < 400,"Health",
                                                 ifelse(TT1961$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1961$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1961$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1961$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1961$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1961$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1961$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1961$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1961$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1961$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1961$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1961$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1961$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1961$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1961$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1961$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1961$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1961$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))






TT1962$Major_Topic <- ifelse(TT1962$Norway_Code <200,1, 
                             ifelse(TT1962$Norway_Code <300, 2, 
                                    ifelse(TT1962$Norway_Code < 400,3,
                                           ifelse(TT1962$Norway_Code < 500, 4,
                                                  ifelse(TT1962$Norway_Code < 600,5, 
                                                         ifelse(TT1962$Norway_Code < 700, 6,
                                                                ifelse(TT1962$Norway_Code < 800,7,
                                                                       ifelse(TT1962$Norway_Code < 900,8,
                                                                              ifelse(TT1962$Norway_Code < 1000,9,
                                                                                     ifelse(TT1962$Norway_Code < 1100,10,
                                                                                            ifelse(TT1962$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1962$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1962$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1962$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1962$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1962$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1962$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1962$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1962$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1962$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1962$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1962 <-  TT1962 %>%
  mutate(Major_Topic_Name = ifelse(TT1962$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1962$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1962$Norway_Code < 400,"Health",
                                                 ifelse(TT1962$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1962$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1962$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1962$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1962$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1962$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1962$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1962$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1962$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1962$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1962$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1962$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1962$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1962$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1962$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1962$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1962$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1962$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))



TT1956 <- na.omit(TT1956)
TT1957 <- na.omit(TT1957)
TT1958 <- na.omit(TT1958)
TT1959_1 <- na.omit(TT1959_1)
TT1959_2 <- na.omit(TT1959_2)
TT1960 <- na.omit(TT1960)
TT1961 <- na.omit(TT1961)
TT1962 <- na.omit(TT1962)
TT1963 <- na.omit(TT1963)

#### Relative ####
Var1 <- as.factor(c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,23))
data <- as.data.frame(Var1)


R_1956 <- as.data.frame(table(TT1956$Major_Topic)/(length(TT1956$Major_Topic)))
data <- as.data.frame(Var1)
R_1956 <- full_join(data, R_1956)
R_1956[is.na(R_1956)] <- 0
R_1956 <- subset(R_1956, select = Freq)
R_1956 <- as.data.frame(t(R_1956))
R_1956 <- rename(R_1956, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1957 <- as.data.frame(table(TT1957$Major_Topic)/(length(TT1957$Major_Topic)))
data <- as.data.frame(Var1)
R_1957 <- full_join(data, R_1957)
R_1957[is.na(R_1957)] <- 0
R_1957 <- subset(R_1957, select = Freq)
R_1957 <- as.data.frame(t(R_1957))
R_1957 <- rename(R_1957, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1958 <- as.data.frame(table(TT1958$Major_Topic)/(length(TT1958$Major_Topic)))
data <- as.data.frame(Var1)
R_1958 <- full_join(data, R_1958)
R_1958[is.na(R_1958)] <- 0
R_1958 <- subset(R_1958, select = Freq)
R_1958 <- as.data.frame(t(R_1958))
R_1958 <- rename(R_1958, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1959_1 <- as.data.frame(table(TT1959_1$Major_Topic)/(length(TT1959_1$Major_Topic)))
data <- as.data.frame(Var1)
R_1959_1 <- full_join(data, R_1959_1)
R_1959_1[is.na(R_1959_1)] <- 0
R_1959_1 <- subset(R_1959_1, select = Freq)
R_1959_1 <- as.data.frame(t(R_1959_1))
R_1959_1 <- rename(R_1959_1, Macroeconomics = 1, Civil_Liberties = 2,
                   Health = 3, Agriculture = 4, Labour = 5,
                   Education = 6, Environment = 7, 
                   Energy = 8, Immigration = 9, Traffic = 10,
                   Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                   Business = 14, Defence = 15, Research_Technology = 16,
                   Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                   Public_Land = 20, Culture = 21)


R_1959_2 <- as.data.frame(table(TT1959_2$Major_Topic)/(length(TT1959_2$Major_Topic)))
data <- as.data.frame(Var1)
R_1959_2 <- full_join(data, R_1959_2)
R_1959_2[is.na(R_1959_2)] <- 0
R_1959_2 <- subset(R_1959_2, select = Freq)
R_1959_2 <- as.data.frame(t(R_1959_2))
R_1959_2 <- rename(R_1959_2, Macroeconomics = 1, Civil_Liberties = 2,
                   Health = 3, Agriculture = 4, Labour = 5,
                   Education = 6, Environment = 7, 
                   Energy = 8, Immigration = 9, Traffic = 10,
                   Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                   Business = 14, Defence = 15, Research_Technology = 16,
                   Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                   Public_Land = 20, Culture = 21)

R_1960 <- as.data.frame(table(TT1960$Major_Topic)/(length(TT1960$Major_Topic)))
data <- as.data.frame(Var1)
R_1960 <- full_join(data, R_1960)
R_1960[is.na(R_1960)] <- 0
R_1960 <- subset(R_1960, select = Freq)
R_1960 <- as.data.frame(t(R_1960))
R_1960 <- rename(R_1960, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1961 <- as.data.frame(table(TT1961$Major_Topic)/(length(TT1961$Major_Topic)))
data <- as.data.frame(Var1)
R_1961 <- full_join(data, R_1961)
R_1961[is.na(R_1961)] <- 0
R_1961 <- subset(R_1961, select = Freq)
R_1961 <- as.data.frame(t(R_1961))
R_1961 <- rename(R_1961, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1962 <- as.data.frame(table(TT1962$Major_Topic)/(length(TT1962$Major_Topic)))
data <- as.data.frame(Var1)
R_1962 <- full_join(data, R_1962)
R_1962[is.na(R_1962)] <- 0
R_1962 <- subset(R_1962, select = Freq)
R_1962 <- as.data.frame(t(R_1962))
R_1962 <- rename(R_1962, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

Gerharsen3_1956_1962 <- rbind(R_1956, R_1957, R_1958, R_1959_1, R_1959_2, R_1960, R_1961, R_1962)




#### 1963-70 ####
TT1963 <- read.xlsx("Trontale1963_108 (1).xlsx")
TT1964 <- read.xlsx("Trontale1964_109.xlsx")
TT1965 <- read.xlsx("Trontale1965_110.xlsx")

TT1963$Major_Topic <- ifelse(TT1963$Norway_Code <200,1, 
                             ifelse(TT1963$Norway_Code <300, 2, 
                                    ifelse(TT1963$Norway_Code < 400,3,
                                           ifelse(TT1963$Norway_Code < 500, 4,
                                                  ifelse(TT1963$Norway_Code < 600,5, 
                                                         ifelse(TT1963$Norway_Code < 700, 6,
                                                                ifelse(TT1963$Norway_Code < 800,7,
                                                                       ifelse(TT1963$Norway_Code < 900,8,
                                                                              ifelse(TT1963$Norway_Code < 1000,9,
                                                                                     ifelse(TT1963$Norway_Code < 1100,10,
                                                                                            ifelse(TT1963$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1963$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1963$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1963$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1963$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1963$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1963$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1963$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1963$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1963$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1963$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1963 <-  TT1963 %>%
  mutate(Major_Topic_Name = ifelse(TT1963$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1963$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1963$Norway_Code < 400,"Health",
                                                 ifelse(TT1963$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1963$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1963$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1963$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1963$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1963$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1963$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1963$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1963$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1963$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1963$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1963$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1963$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1963$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1963$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1963$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1963$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1963$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))




TT1964$Major_Topic <- ifelse(TT1964$Norway_Code <200,1, 
                             ifelse(TT1964$Norway_Code <300, 2, 
                                    ifelse(TT1964$Norway_Code < 400,3,
                                           ifelse(TT1964$Norway_Code < 500, 4,
                                                  ifelse(TT1964$Norway_Code < 600,5, 
                                                         ifelse(TT1964$Norway_Code < 700, 6,
                                                                ifelse(TT1964$Norway_Code < 800,7,
                                                                       ifelse(TT1964$Norway_Code < 900,8,
                                                                              ifelse(TT1964$Norway_Code < 1000,9,
                                                                                     ifelse(TT1964$Norway_Code < 1100,10,
                                                                                            ifelse(TT1964$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1964$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1964$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1964$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1964$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1964$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1964$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1964$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1964$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1964$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1964$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1964 <-  TT1964 %>%
  mutate(Major_Topic_Name = ifelse(TT1964$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1964$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1964$Norway_Code < 400,"Health",
                                                 ifelse(TT1964$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1964$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1964$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1964$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1964$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1964$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1964$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1964$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1964$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1964$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1964$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1964$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1964$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1964$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1964$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1964$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1964$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1964$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))




TT1965$Major_Topic <- ifelse(TT1965$Norway_Code <200,1, 
                             ifelse(TT1965$Norway_Code <300, 2, 
                                    ifelse(TT1965$Norway_Code < 400,3,
                                           ifelse(TT1965$Norway_Code < 500, 4,
                                                  ifelse(TT1965$Norway_Code < 600,5, 
                                                         ifelse(TT1965$Norway_Code < 700, 6,
                                                                ifelse(TT1965$Norway_Code < 800,7,
                                                                       ifelse(TT1965$Norway_Code < 900,8,
                                                                              ifelse(TT1965$Norway_Code < 1000,9,
                                                                                     ifelse(TT1965$Norway_Code < 1100,10,
                                                                                            ifelse(TT1965$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1965$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1965$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1965$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1965$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1965$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1965$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1965$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1965$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1965$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1965$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1965 <-  TT1965 %>%
  mutate(Major_Topic_Name = ifelse(TT1965$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1965$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1965$Norway_Code < 400,"Health",
                                                 ifelse(TT1965$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1965$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1965$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1965$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1965$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1965$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1965$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1965$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1965$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1965$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1965$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1965$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1965$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1965$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1965$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1965$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1965$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1965$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))


TT1963 <- na.omit(TT1963)
TT1964 <- na.omit(TT1964)
TT1965 <- na.omit(TT1965)



#### Relativ ####
Var1 <- as.factor(c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,23))
data <- as.data.frame(Var1)


R_1963 <- as.data.frame(table(TT1963$Major_Topic)/(length(TT1963$Major_Topic)))
data <- as.data.frame(Var1)
R_1963 <- full_join(data, R_1963)
R_1963[is.na(R_1963)] <- 0
R_1963 <- subset(R_1963, select = Freq)
R_1963 <- as.data.frame(t(R_1963))
R_1963 <- rename(R_1963, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1964 <- as.data.frame(table(TT1964$Major_Topic)/(length(TT1964$Major_Topic)))
data <- as.data.frame(Var1)
R_1964 <- full_join(data, R_1964)
R_1964[is.na(R_1964)] <- 0
R_1964 <- subset(R_1964, select = Freq)
R_1964 <- as.data.frame(t(R_1964))
R_1964 <- rename(R_1964, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1965 <- as.data.frame(table(TT1965$Major_Topic)/(length(TT1965$Major_Topic)))
data <- as.data.frame(Var1)
R_1965 <- full_join(data, R_1965)
R_1965[is.na(R_1965)] <- 0
R_1965 <- subset(R_1965, select = Freq)
R_1965 <- as.data.frame(t(R_1965))
R_1965 <- rename(R_1965, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

Gerhardsen4_1963_1965 <- rbind(R_1963, R_1964, R_1965)







TT1966 <- read.xlsx("Trontale1966_111 (1).xlsx")
TT1967 <- read.xlsx("Trontale1967_112.xlsx")
TT1968 <- read.xlsx("Trontale1968_113.xlsx")
TT1969 <- read.xlsx("Trontale1969_114.xlsx")
TT1970 <- read.xlsx("Trontale1970_115.xlsx")

TT1966$Major_Topic <- ifelse(TT1966$Norway_Code <200,1, 
                             ifelse(TT1966$Norway_Code <300, 2, 
                                    ifelse(TT1966$Norway_Code < 400,3,
                                           ifelse(TT1966$Norway_Code < 500, 4,
                                                  ifelse(TT1966$Norway_Code < 600,5, 
                                                         ifelse(TT1966$Norway_Code < 700, 6,
                                                                ifelse(TT1966$Norway_Code < 800,7,
                                                                       ifelse(TT1966$Norway_Code < 900,8,
                                                                              ifelse(TT1966$Norway_Code < 1000,9,
                                                                                     ifelse(TT1966$Norway_Code < 1100,10,
                                                                                            ifelse(TT1966$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1966$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1966$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1966$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1966$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1966$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1966$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1966$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1966$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1966$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1966$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1966 <-  TT1966 %>%
  mutate(Major_Topic_Name = ifelse(TT1966$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1966$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1966$Norway_Code < 400,"Health",
                                                 ifelse(TT1966$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1966$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1966$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1966$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1966$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1966$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1966$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1966$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1966$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1966$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1966$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1966$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1966$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1966$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1966$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1966$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1966$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1966$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))




TT1967$Major_Topic <- ifelse(TT1967$Norway_Code <200,1, 
                             ifelse(TT1967$Norway_Code <300, 2, 
                                    ifelse(TT1967$Norway_Code < 400,3,
                                           ifelse(TT1967$Norway_Code < 500, 4,
                                                  ifelse(TT1967$Norway_Code < 600,5, 
                                                         ifelse(TT1967$Norway_Code < 700, 6,
                                                                ifelse(TT1967$Norway_Code < 800,7,
                                                                       ifelse(TT1967$Norway_Code < 900,8,
                                                                              ifelse(TT1967$Norway_Code < 1000,9,
                                                                                     ifelse(TT1967$Norway_Code < 1100,10,
                                                                                            ifelse(TT1967$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1967$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1967$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1967$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1967$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1967$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1967$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1967$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1967$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1967$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1967$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1967 <-  TT1967 %>%
  mutate(Major_Topic_Name = ifelse(TT1967$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1967$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1967$Norway_Code < 400,"Health",
                                                 ifelse(TT1967$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1967$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1967$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1967$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1967$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1967$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1967$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1967$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1967$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1967$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1967$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1967$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1967$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1967$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1967$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1967$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1967$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1967$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))




TT1968$Major_Topic <- ifelse(TT1968$Norway_Code <200,1, 
                             ifelse(TT1968$Norway_Code <300, 2, 
                                    ifelse(TT1968$Norway_Code < 400,3,
                                           ifelse(TT1968$Norway_Code < 500, 4,
                                                  ifelse(TT1968$Norway_Code < 600,5, 
                                                         ifelse(TT1968$Norway_Code < 700, 6,
                                                                ifelse(TT1968$Norway_Code < 800,7,
                                                                       ifelse(TT1968$Norway_Code < 900,8,
                                                                              ifelse(TT1968$Norway_Code < 1000,9,
                                                                                     ifelse(TT1968$Norway_Code < 1100,10,
                                                                                            ifelse(TT1968$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1968$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1968$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1968$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1968$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1968$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1968$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1968$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1968$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1968$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1968$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1968 <-  TT1968 %>%
  mutate(Major_Topic_Name = ifelse(TT1968$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1968$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1968$Norway_Code < 400,"Health",
                                                 ifelse(TT1968$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1968$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1968$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1968$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1968$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1968$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1968$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1968$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1968$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1968$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1968$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1968$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1968$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1968$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1968$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1968$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1968$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1968$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))




TT1969$Major_Topic <- ifelse(TT1969$Norway_Code <200,1, 
                             ifelse(TT1969$Norway_Code <300, 2, 
                                    ifelse(TT1969$Norway_Code < 400,3,
                                           ifelse(TT1969$Norway_Code < 500, 4,
                                                  ifelse(TT1969$Norway_Code < 600,5, 
                                                         ifelse(TT1969$Norway_Code < 700, 6,
                                                                ifelse(TT1969$Norway_Code < 800,7,
                                                                       ifelse(TT1969$Norway_Code < 900,8,
                                                                              ifelse(TT1969$Norway_Code < 1000,9,
                                                                                     ifelse(TT1969$Norway_Code < 1100,10,
                                                                                            ifelse(TT1969$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1969$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1969$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1969$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1969$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1969$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1969$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1969$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1969$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1969$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1969$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1969 <-  TT1969 %>%
  mutate(Major_Topic_Name = ifelse(TT1969$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1969$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1969$Norway_Code < 400,"Health",
                                                 ifelse(TT1969$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1969$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1969$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1969$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1969$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1969$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1969$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1969$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1969$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1969$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1969$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1969$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1969$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1969$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1969$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1969$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1969$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1969$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))




TT1970$Major_Topic <- ifelse(TT1970$Norway_Code <200,1, 
                             ifelse(TT1970$Norway_Code <300, 2, 
                                    ifelse(TT1970$Norway_Code < 400,3,
                                           ifelse(TT1970$Norway_Code < 500, 4,
                                                  ifelse(TT1970$Norway_Code < 600,5, 
                                                         ifelse(TT1970$Norway_Code < 700, 6,
                                                                ifelse(TT1970$Norway_Code < 800,7,
                                                                       ifelse(TT1970$Norway_Code < 900,8,
                                                                              ifelse(TT1970$Norway_Code < 1000,9,
                                                                                     ifelse(TT1970$Norway_Code < 1100,10,
                                                                                            ifelse(TT1970$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1970$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1970$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1970$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1970$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1970$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1970$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1970$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1970$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1970$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1970$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1970 <-  TT1970 %>%
  mutate(Major_Topic_Name = ifelse(TT1970$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT1970$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT1970$Norway_Code < 400,"Health",
                                                 ifelse(TT1970$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT1970$Norway_Code < 600,"Labour", 
                                                               ifelse(TT1970$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT1970$Norway_Code < 800,"Environment",
                                                                             ifelse(TT1970$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT1970$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT1970$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT1970$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT1970$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT1970$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT1970$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT1970$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT1970$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT1970$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT1970$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT1970$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT1970$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT1970$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))



TT1966 <- na.omit(TT1966)
TT1967 <- na.omit(TT1967)
TT1968 <- na.omit(TT1968)
TT1969 <- na.omit(TT1969)
TT1970 <- na.omit(TT1970)

#### Relative ####
Var1 <- as.factor(c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,23))
data <- as.data.frame(Var1)


R_1966 <- as.data.frame(table(TT1966$Major_Topic)/(length(TT1966$Major_Topic)))
data <- as.data.frame(Var1)
R_1966 <- full_join(data, R_1966)
R_1966[is.na(R_1966)] <- 0
R_1966 <- subset(R_1966, select = Freq)
R_1966 <- as.data.frame(t(R_1966))
R_1966 <- rename(R_1966, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1967 <- as.data.frame(table(TT1967$Major_Topic)/(length(TT1967$Major_Topic)))
data <- as.data.frame(Var1)
R_1967 <- full_join(data, R_1967)
R_1967[is.na(R_1967)] <- 0
R_1967 <- subset(R_1967, select = Freq)
R_1967 <- as.data.frame(t(R_1967))
R_1967 <- rename(R_1967, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1968 <- as.data.frame(table(TT1968$Major_Topic)/(length(TT1968$Major_Topic)))
data <- as.data.frame(Var1)
R_1968 <- full_join(data, R_1968)
R_1968[is.na(R_1968)] <- 0
R_1968 <- subset(R_1968, select = Freq)
R_1968 <- as.data.frame(t(R_1968))
R_1968 <- rename(R_1968, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1969 <- as.data.frame(table(TT1969$Major_Topic)/(length(TT1969$Major_Topic)))
data <- as.data.frame(Var1)
R_1969 <- full_join(data, R_1969)
R_1969[is.na(R_1969)] <- 0
R_1969 <- subset(R_1969, select = Freq)
R_1969 <- as.data.frame(t(R_1969))
R_1969 <- rename(R_1969, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)


R_1970 <- as.data.frame(table(TT1970$Major_Topic)/(length(TT1970$Major_Topic)))
data <- as.data.frame(Var1)
R_1970 <- full_join(data, R_1970)
R_1970[is.na(R_1970)] <- 0
R_1970 <- subset(R_1970, select = Freq)
R_1970 <- as.data.frame(t(R_1970))
R_1970 <- rename(R_1970, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

Bo <- full_join(R_1966, R_1967)
Boo <- full_join(Bo, R_1968)
Booo <- full_join(Boo, R_1969)
Borten1966_1970 <- full_join(Booo, R_1970)

#### 1971 - 1981 ####
TT1971 <- read.xlsx("Trontale1971_116 (1).xlsx")
TT1972 <- read.xlsx("Trontale1972_117.xlsx")

TT1971$Major_Topic <- ifelse(TT1971$Norway_Code <200,1, 
                             ifelse(TT1971$Norway_Code <300, 2, 
                                    ifelse(TT1971$Norway_Code < 400,3,
                                           ifelse(TT1971$Norway_Code < 500, 4,
                                                  ifelse(TT1971$Norway_Code < 600,5, 
                                                         ifelse(TT1971$Norway_Code < 700, 6,
                                                                ifelse(TT1971$Norway_Code < 800,7,
                                                                       ifelse(TT1971$Norway_Code < 900,8,
                                                                              ifelse(TT1971$Norway_Code < 1000,9,
                                                                                     ifelse(TT1971$Norway_Code < 1100,10,
                                                                                            ifelse(TT1971$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1971$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1971$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1971$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1971$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1971$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1971$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1971$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1971$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1971$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1971$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1971 <-  TT1971 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))


TT1972$Major_Topic <- ifelse(TT1972$Norway_Code <200,1, 
                             ifelse(TT1972$Norway_Code <300, 2, 
                                    ifelse(TT1972$Norway_Code < 400,3,
                                           ifelse(TT1972$Norway_Code < 500, 4,
                                                  ifelse(TT1972$Norway_Code < 600,5, 
                                                         ifelse(TT1972$Norway_Code < 700, 6,
                                                                ifelse(TT1972$Norway_Code < 800,7,
                                                                       ifelse(TT1972$Norway_Code < 900,8,
                                                                              ifelse(TT1972$Norway_Code < 1000,9,
                                                                                     ifelse(TT1972$Norway_Code < 1100,10,
                                                                                            ifelse(TT1972$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1972$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1972$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1972$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1972$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1972$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1972$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1972$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1972$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1972$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1972$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1972 <-  TT1972 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))



TT1971 <- na.omit(TT1971)
TT1972 <- na.omit(TT1972)

#### Relative ####
Var1 <- as.factor(c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,23))
data <- as.data.frame(Var1)


R_1971 <- as.data.frame(table(TT1971$Major_Topic)/(length(TT1971$Major_Topic)))
data <- as.data.frame(Var1)
R_1971 <- full_join(data, R_1971)
R_1971[is.na(R_1971)] <- 0
R_1971 <- subset(R_1971, select = Freq)
R_1971 <- as.data.frame(t(R_1971))
R_1971 <- rename(R_1971, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1972 <- as.data.frame(table(TT1972$Major_Topic)/(length(TT1972$Major_Topic)))
data <- as.data.frame(Var1)
R_1972 <- full_join(data, R_1972)
R_1972[is.na(R_1972)] <- 0
R_1972 <- subset(R_1972, select = Freq)
R_1972 <- as.data.frame(t(R_1972))
R_1972 <- rename(R_1972, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

Bratteli1_1971_1972 <- rbind(R_1971, R_1972)




TT1973 <- read.xlsx("Trontale1973_118 (1).xlsx")

TT1973$Major_Topic <- ifelse(TT1973$Norway_Code <200,1, 
                             ifelse(TT1973$Norway_Code <300, 2, 
                                    ifelse(TT1973$Norway_Code < 400,3,
                                           ifelse(TT1973$Norway_Code < 500, 4,
                                                  ifelse(TT1973$Norway_Code < 600,5, 
                                                         ifelse(TT1973$Norway_Code < 700, 6,
                                                                ifelse(TT1973$Norway_Code < 800,7,
                                                                       ifelse(TT1973$Norway_Code < 900,8,
                                                                              ifelse(TT1973$Norway_Code < 1000,9,
                                                                                     ifelse(TT1973$Norway_Code < 1100,10,
                                                                                            ifelse(TT1973$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1973$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1973$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1973$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1973$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1973$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1973$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1973$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1973$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1973$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1973$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1973 <-  TT1973 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))
TT1973 <- na.omit(TT1973)

#### Relative ####
Var1 <- as.factor(c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,23))
data <- as.data.frame(Var1)


R_1973 <- as.data.frame(table(TT1973$Major_Topic)/(length(TT1973$Major_Topic)))
data <- as.data.frame(Var1)
R_1973 <- full_join(data, R_1973)
R_1973[is.na(R_1973)] <- 0
R_1973 <- subset(R_1973, select = Freq)
R_1973 <- as.data.frame(t(R_1973))
R_1973 <- rename(R_1973, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

Korvald1973 <- as.data.frame(table(TT1973$Major_Topic)/(length(TT1973$Major_Topic)))
data <- as.data.frame(Var1)
Korvald1973 <- full_join(data, Korvald1973)
Korvald1973[is.na(Korvald1973)] <- 0
Korvald1973 <- subset(Korvald1973, select = Freq)
Korvald1973 <- as.data.frame(t(Korvald1973))
Korvald1973 <- rename(Korvald1973, Macroeconomics = 1, Civil_Liberties = 2,
                      Health = 3, Agriculture = 4, Labour = 5,
                      Education = 6, Environment = 7, 
                      Energy = 8, Immigration = 9, Traffic = 10,
                      Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                      Business = 14, Defence = 15, Research_Technology = 16,
                      Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                      Public_Land = 20, Culture = 21)





TT1974 <- read.xlsx("Trontale1974_119 (1).xlsx")
TT1975 <- read.xlsx("Trontale1975_120 (1).xlsx")

TT1974$Major_Topic <- ifelse(TT1974$Norway_Code <200,1, 
                             ifelse(TT1974$Norway_Code <300, 2, 
                                    ifelse(TT1974$Norway_Code < 400,3,
                                           ifelse(TT1974$Norway_Code < 500, 4,
                                                  ifelse(TT1974$Norway_Code < 600,5, 
                                                         ifelse(TT1974$Norway_Code < 700, 6,
                                                                ifelse(TT1974$Norway_Code < 800,7,
                                                                       ifelse(TT1974$Norway_Code < 900,8,
                                                                              ifelse(TT1974$Norway_Code < 1000,9,
                                                                                     ifelse(TT1974$Norway_Code < 1100,10,
                                                                                            ifelse(TT1974$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1974$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1974$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1974$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1974$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1974$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1974$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1974$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1974$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1974$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1974$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1974 <-  TT1974 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))


TT1975$Major_Topic <- ifelse(TT1975$Norway_Code <200,1, 
                             ifelse(TT1975$Norway_Code <300, 2, 
                                    ifelse(TT1975$Norway_Code < 400,3,
                                           ifelse(TT1975$Norway_Code < 500, 4,
                                                  ifelse(TT1975$Norway_Code < 600,5, 
                                                         ifelse(TT1975$Norway_Code < 700, 6,
                                                                ifelse(TT1975$Norway_Code < 800,7,
                                                                       ifelse(TT1975$Norway_Code < 900,8,
                                                                              ifelse(TT1975$Norway_Code < 1000,9,
                                                                                     ifelse(TT1975$Norway_Code < 1100,10,
                                                                                            ifelse(TT1975$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1975$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1975$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1975$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1975$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1975$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1975$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1975$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1975$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1975$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1975$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1975 <-  TT1975 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))
TT1974 <- na.omit(TT1974)
TT1975 <- na.omit(TT1975)

#### Relative ####
Var1 <- as.factor(c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,23))
data <- as.data.frame(Var1)


R_1974 <- as.data.frame(table(TT1974$Major_Topic)/(length(TT1974$Major_Topic)))
data <- as.data.frame(Var1)
R_1974 <- full_join(data, R_1974)
R_1974[is.na(R_1974)] <- 0
R_1974 <- subset(R_1974, select = Freq)
R_1974 <- as.data.frame(t(R_1974))
R_1974 <- rename(R_1974, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1975 <- as.data.frame(table(TT1975$Major_Topic)/(length(TT1975$Major_Topic)))
data <- as.data.frame(Var1)
R_1975 <- full_join(data, R_1975)
R_1975[is.na(R_1975)] <- 0
R_1975 <- subset(R_1975, select = Freq)
R_1975 <- as.data.frame(t(R_1975))
R_1975 <- rename(R_1975, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

Bratteli2_1974_1975 <- rbind(R_1974, R_1975) 




TT1976 <- read.xlsx("Trontale1976_121 (1).xlsx")
TT1977 <- read.xlsx("Trontale1977_122.xlsx")
TT1978 <- read.xlsx("Trontale1978_123.xlsx")
TT1979 <- read.xlsx("Trontale1979_124.xlsx")
TT1980 <- read.xlsx("Trontale1980_125.xlsx")

TT1976$Major_Topic <- ifelse(TT1976$Norway_Code <200,1, 
                             ifelse(TT1976$Norway_Code <300, 2, 
                                    ifelse(TT1976$Norway_Code < 400,3,
                                           ifelse(TT1976$Norway_Code < 500, 4,
                                                  ifelse(TT1976$Norway_Code < 600,5, 
                                                         ifelse(TT1976$Norway_Code < 700, 6,
                                                                ifelse(TT1976$Norway_Code < 800,7,
                                                                       ifelse(TT1976$Norway_Code < 900,8,
                                                                              ifelse(TT1976$Norway_Code < 1000,9,
                                                                                     ifelse(TT1976$Norway_Code < 1100,10,
                                                                                            ifelse(TT1976$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1976$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1976$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1976$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1976$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1976$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1976$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1976$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1976$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1976$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1976$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1976 <-  TT1976 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))

TT1977$Major_Topic <- ifelse(TT1977$Norway_Code <200,1, 
                             ifelse(TT1977$Norway_Code <300, 2, 
                                    ifelse(TT1977$Norway_Code < 400,3,
                                           ifelse(TT1977$Norway_Code < 500, 4,
                                                  ifelse(TT1977$Norway_Code < 600,5, 
                                                         ifelse(TT1977$Norway_Code < 700, 6,
                                                                ifelse(TT1977$Norway_Code < 800,7,
                                                                       ifelse(TT1977$Norway_Code < 900,8,
                                                                              ifelse(TT1977$Norway_Code < 1000,9,
                                                                                     ifelse(TT1977$Norway_Code < 1100,10,
                                                                                            ifelse(TT1977$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1977$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1977$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1977$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1977$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1977$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1977$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1977$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1977$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1977$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1977$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1977 <-  TT1977 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))

TT1978$Major_Topic <- ifelse(TT1978$Norway_Code <200,1, 
                             ifelse(TT1978$Norway_Code <300, 2, 
                                    ifelse(TT1978$Norway_Code < 400,3,
                                           ifelse(TT1978$Norway_Code < 500, 4,
                                                  ifelse(TT1978$Norway_Code < 600,5, 
                                                         ifelse(TT1978$Norway_Code < 700, 6,
                                                                ifelse(TT1978$Norway_Code < 800,7,
                                                                       ifelse(TT1978$Norway_Code < 900,8,
                                                                              ifelse(TT1978$Norway_Code < 1000,9,
                                                                                     ifelse(TT1978$Norway_Code < 1100,10,
                                                                                            ifelse(TT1978$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1978$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1978$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1978$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1978$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1978$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1978$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1978$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1978$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1978$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1978$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1978 <-  TT1978 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))

TT1979$Major_Topic <- ifelse(TT1979$Norway_Code <200,1, 
                             ifelse(TT1979$Norway_Code <300, 2, 
                                    ifelse(TT1979$Norway_Code < 400,3,
                                           ifelse(TT1979$Norway_Code < 500, 4,
                                                  ifelse(TT1979$Norway_Code < 600,5, 
                                                         ifelse(TT1979$Norway_Code < 700, 6,
                                                                ifelse(TT1979$Norway_Code < 800,7,
                                                                       ifelse(TT1979$Norway_Code < 900,8,
                                                                              ifelse(TT1979$Norway_Code < 1000,9,
                                                                                     ifelse(TT1979$Norway_Code < 1100,10,
                                                                                            ifelse(TT1979$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1979$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1979$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1979$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1979$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1979$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1979$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1979$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1979$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1979$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1979$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1979 <-  TT1979 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))

TT1980$Major_Topic <- ifelse(TT1980$Norway_Code <200,1, 
                             ifelse(TT1980$Norway_Code <300, 2, 
                                    ifelse(TT1980$Norway_Code < 400,3,
                                           ifelse(TT1980$Norway_Code < 500, 4,
                                                  ifelse(TT1980$Norway_Code < 600,5, 
                                                         ifelse(TT1980$Norway_Code < 700, 6,
                                                                ifelse(TT1980$Norway_Code < 800,7,
                                                                       ifelse(TT1980$Norway_Code < 900,8,
                                                                              ifelse(TT1980$Norway_Code < 1000,9,
                                                                                     ifelse(TT1980$Norway_Code < 1100,10,
                                                                                            ifelse(TT1980$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1980$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1980$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1980$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1980$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1980$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1980$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1980$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1980$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1980$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1980$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1980 <-  TT1980 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))
TT1976 <- na.omit(TT1976)
TT1977 <- na.omit(TT1977)
TT1978 <- na.omit(TT1978)
TT1979 <- na.omit(TT1979)
TT1980 <- na.omit(TT1980)


####Relative ####
Var1 <- as.factor(c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,23))
data <- as.data.frame(Var1)


R_1976 <- as.data.frame(table(TT1976$Major_Topic)/(length(TT1976$Major_Topic)))
data <- as.data.frame(Var1)
R_1976 <- full_join(data, R_1976)
R_1976[is.na(R_1976)] <- 0
R_1976 <- subset(R_1976, select = Freq)
R_1976 <- as.data.frame(t(R_1976))
R_1976 <- rename(R_1976, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1977 <- as.data.frame(table(TT1977$Major_Topic)/(length(TT1977$Major_Topic)))
data <- as.data.frame(Var1)
R_1977 <- full_join(data, R_1977)
R_1977[is.na(R_1977)] <- 0
R_1977 <- subset(R_1977, select = Freq)
R_1977 <- as.data.frame(t(R_1977))
R_1977 <- rename(R_1977, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1978 <- as.data.frame(table(TT1978$Major_Topic)/(length(TT1978$Major_Topic)))
data <- as.data.frame(Var1)
R_1978 <- full_join(data, R_1978)
R_1978[is.na(R_1978)] <- 0
R_1978 <- subset(R_1978, select = Freq)
R_1978 <- as.data.frame(t(R_1978))
R_1978 <- rename(R_1978, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1979 <- as.data.frame(table(TT1979$Major_Topic)/(length(TT1979$Major_Topic)))
data <- as.data.frame(Var1)
R_1979 <- full_join(data, R_1979)
R_1979[is.na(R_1979)] <- 0
R_1979 <- subset(R_1979, select = Freq)
R_1979 <- as.data.frame(t(R_1979))
R_1979 <- rename(R_1979, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)


R_1980 <- as.data.frame(table(TT1980$Major_Topic)/(length(TT1980$Major_Topic)))
data <- as.data.frame(Var1)
R_1980 <- full_join(data, R_1980)
R_1980[is.na(R_1980)] <- 0
R_1980 <- subset(R_1980, select = Freq)
R_1980 <- as.data.frame(t(R_1980))
R_1980 <- rename(R_1980, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

Nordi1976_1980 <- rbind(R_1976, R_1977, R_1978, R_1979, R_1980)





TT1981 <- read.xlsx("Trontale 1981 126 (2).xlsx")

TT1981$Major_Topic <- ifelse(TT1981$Norway_Code <200,1, 
                             ifelse(TT1981$Norway_Code <300, 2, 
                                    ifelse(TT1981$Norway_Code < 400,3,
                                           ifelse(TT1981$Norway_Code < 500, 4,
                                                  ifelse(TT1981$Norway_Code < 600,5, 
                                                         ifelse(TT1981$Norway_Code < 700, 6,
                                                                ifelse(TT1981$Norway_Code < 800,7,
                                                                       ifelse(TT1981$Norway_Code < 900,8,
                                                                              ifelse(TT1981$Norway_Code < 1000,9,
                                                                                     ifelse(TT1981$Norway_Code < 1100,10,
                                                                                            ifelse(TT1981$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1981$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1981$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1981$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1981$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1981$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1981$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1981$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1981$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1981$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1981$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1981 <-  TT1981 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))


TT1981 <- na.omit(TT1981)
#### Relative ####
Var1 <- as.factor(c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,23))
data <- as.data.frame(Var1)

R_1981 <- as.data.frame(table(TT1981$Major_Topic)/(length(TT1981$Major_Topic)))
data <- as.data.frame(Var1)
R_1981 <- full_join(data, R_1981)
R_1981[is.na(R_1981)] <- 0
R_1981 <- subset(R_1981, select = Freq)
R_1981 <- as.data.frame(t(R_1981))
R_1981 <- rename(R_1981, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

Brundtland1_1981 <- as.data.frame(table(TT1981$Major_Topic)/(length(TT1981$Major_Topic)))
data <- as.data.frame(Var1)
Brundtland1_1981 <- full_join(data, Brundtland1_1981)
Brundtland1_1981[is.na(Brundtland1_1981)] <- 0
Brundtland1_1981 <- subset(Brundtland1_1981, select = Freq)
Brundtland1_1981 <- as.data.frame(t(Brundtland1_1981))
Brundtland1_1981 <- rename(Brundtland1_1981, Macroeconomics = 1, Civil_Liberties = 2,
                           Health = 3, Agriculture = 4, Labour = 5,
                           Education = 6, Environment = 7, 
                           Energy = 8, Immigration = 9, Traffic = 10,
                           Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                           Business = 14, Defence = 15, Research_Technology = 16,
                           Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                           Public_Land = 20, Culture = 21)

#### 1982 - 1996 ####
TT1982 <- read.xlsx("Trontale1982 (1).xlsx")
TT1983 <- read.xlsx("Trontale1983.xlsx")
TT1984 <- read.xlsx("Trontale1984.xlsx")
TT1985 <- read.xlsx("Trontale1985.xlsx")

TT1982$Major_Topic <- ifelse(TT1982$Norway_Code <200,1, 
                             ifelse(TT1982$Norway_Code <300, 2, 
                                    ifelse(TT1982$Norway_Code < 400,3,
                                           ifelse(TT1982$Norway_Code < 500, 4,
                                                  ifelse(TT1982$Norway_Code < 600,5, 
                                                         ifelse(TT1982$Norway_Code < 700, 6,
                                                                ifelse(TT1982$Norway_Code < 800,7,
                                                                       ifelse(TT1982$Norway_Code < 900,8,
                                                                              ifelse(TT1982$Norway_Code < 1000,9,
                                                                                     ifelse(TT1982$Norway_Code < 1100,10,
                                                                                            ifelse(TT1982$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1982$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1982$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1982$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1982$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1982$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1982$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1982$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1982$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1982$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1982$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT1982 <-  TT1982 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))

TT1983$Major_Topic <- ifelse(TT1983$Norway_Code <200,1, 
                             ifelse(TT1983$Norway_Code <300, 2, 
                                    ifelse(TT1983$Norway_Code < 400,3,
                                           ifelse(TT1983$Norway_Code < 500, 4,
                                                  ifelse(TT1983$Norway_Code < 600,5, 
                                                         ifelse(TT1983$Norway_Code < 700, 6,
                                                                ifelse(TT1983$Norway_Code < 800,7,
                                                                       ifelse(TT1983$Norway_Code < 900,8,
                                                                              ifelse(TT1983$Norway_Code < 1000,9,
                                                                                     ifelse(TT1983$Norway_Code < 1100,10,
                                                                                            ifelse(TT1983$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1983$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1983$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1983$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1983$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1983$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1983$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1983$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1983$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1983$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1983$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1983 <-  TT1983 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))
TT1984$Major_Topic <- ifelse(TT1984$Norway_Code <200,1, 
                             ifelse(TT1984$Norway_Code <300, 2, 
                                    ifelse(TT1984$Norway_Code < 400,3,
                                           ifelse(TT1984$Norway_Code < 500, 4,
                                                  ifelse(TT1984$Norway_Code < 600,5, 
                                                         ifelse(TT1984$Norway_Code < 700, 6,
                                                                ifelse(TT1984$Norway_Code < 800,7,
                                                                       ifelse(TT1984$Norway_Code < 900,8,
                                                                              ifelse(TT1984$Norway_Code < 1000,9,
                                                                                     ifelse(TT1984$Norway_Code < 1100,10,
                                                                                            ifelse(TT1984$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1984$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1984$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1984$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1984$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1984$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1984$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1984$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1984$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1984$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1984$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1984 <-  TT1984 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))
TT1985$Major_Topic <- ifelse(TT1985$Norway_Code <200,1, 
                             ifelse(TT1985$Norway_Code <300, 2, 
                                    ifelse(TT1985$Norway_Code < 400,3,
                                           ifelse(TT1985$Norway_Code < 500, 4,
                                                  ifelse(TT1985$Norway_Code < 600,5, 
                                                         ifelse(TT1985$Norway_Code < 700, 6,
                                                                ifelse(TT1985$Norway_Code < 800,7,
                                                                       ifelse(TT1985$Norway_Code < 900,8,
                                                                              ifelse(TT1985$Norway_Code < 1000,9,
                                                                                     ifelse(TT1985$Norway_Code < 1100,10,
                                                                                            ifelse(TT1985$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1985$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1985$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1985$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1985$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1985$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1985$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1985$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1985$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1985$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1985$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT1985 <-  TT1985 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))
TT1982 <- na.omit(TT1982)
TT1983 <- na.omit(TT1983)
TT1984 <- na.omit(TT1984)
TT1985 <- na.omit(TT1985)


#### Relative ####
Var1 <- as.factor(c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,23))
data <- as.data.frame(Var1)

R_1982 <- as.data.frame(table(TT1982$Major_Topic)/(length(TT1982$Major_Topic)))
data <- as.data.frame(Var1)
R_1982 <- full_join(data, R_1982)
R_1982[is.na(R_1982)] <- 0
R_1982 <- subset(R_1982, select = Freq)
R_1982 <- as.data.frame(t(R_1982))
R_1982 <- rename(R_1982, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1983 <- as.data.frame(table(TT1983$Major_Topic)/(length(TT1983$Major_Topic)))
data <- as.data.frame(Var1)
R_1983 <- full_join(data, R_1983)
R_1983[is.na(R_1983)] <- 0
R_1983 <- subset(R_1983, select = Freq)
R_1983 <- as.data.frame(t(R_1983))
R_1983 <- rename(R_1983, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1984 <- as.data.frame(table(TT1984$Major_Topic)/(length(TT1984$Major_Topic)))
data <- as.data.frame(Var1)
R_1984 <- full_join(data, R_1984)
R_1984[is.na(R_1984)] <- 0
R_1984 <- subset(R_1984, select = Freq)
R_1984 <- as.data.frame(t(R_1984))
R_1984 <- rename(R_1984, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1985 <- as.data.frame(table(TT1985$Major_Topic)/(length(TT1985$Major_Topic)))
data <- as.data.frame(Var1)
R_1985 <- full_join(data, R_1985)
R_1985[is.na(R_1985)] <- 0
R_1985 <- subset(R_1985, select = Freq)
R_1985 <- as.data.frame(t(R_1985))
R_1985 <- rename(R_1985, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)


Willoch1982_1985 <- rbind(R_1982, R_1983, R_1984, R_1985)



TT1986 <- read.xlsx("Trontale1986_131 (1).xlsx")
TT1987 <- read.xlsx("Trontale1987_132 (1).xlsx")
TT1988 <- read.xlsx("Trontale1988_133 (1).xlsx")
TT1989 <- read.xlsx("Trontale1989_134 (1).xlsx")

TT1986$Major_Topic <- ifelse(TT1986$Norway_Code <200,1, 
                             ifelse(TT1986$Norway_Code <300, 2, 
                                    ifelse(TT1986$Norway_Code < 400,3,
                                           ifelse(TT1986$Norway_Code < 500, 4,
                                                  ifelse(TT1986$Norway_Code < 600,5, 
                                                         ifelse(TT1986$Norway_Code < 700, 6,
                                                                ifelse(TT1986$Norway_Code < 800,7,
                                                                       ifelse(TT1986$Norway_Code < 900,8,
                                                                              ifelse(TT1986$Norway_Code < 1000,9,
                                                                                     ifelse(TT1986$Norway_Code < 1100,10,
                                                                                            ifelse(TT1986$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1986$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1986$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1986$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1986$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1986$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1986$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1986$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1986$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1986$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1986$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT1986 <-  TT1986 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))

TT1987$Major_Topic <- ifelse(TT1987$Norway_Code <200,1, 
                             ifelse(TT1987$Norway_Code <300, 2, 
                                    ifelse(TT1987$Norway_Code < 400,3,
                                           ifelse(TT1987$Norway_Code < 500, 4,
                                                  ifelse(TT1987$Norway_Code < 600,5, 
                                                         ifelse(TT1987$Norway_Code < 700, 6,
                                                                ifelse(TT1987$Norway_Code < 800,7,
                                                                       ifelse(TT1987$Norway_Code < 900,8,
                                                                              ifelse(TT1987$Norway_Code < 1000,9,
                                                                                     ifelse(TT1987$Norway_Code < 1100,10,
                                                                                            ifelse(TT1987$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1987$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1987$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1987$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1987$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1987$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1987$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1987$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1987$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1987$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1987$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT1987 <-  TT1987 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))

TT1988$Major_Topic <- ifelse(TT1988$Norway_Code <200,1, 
                             ifelse(TT1988$Norway_Code <300, 2, 
                                    ifelse(TT1988$Norway_Code < 400,3,
                                           ifelse(TT1988$Norway_Code < 500, 4,
                                                  ifelse(TT1988$Norway_Code < 600,5, 
                                                         ifelse(TT1988$Norway_Code < 700, 6,
                                                                ifelse(TT1988$Norway_Code < 800,7,
                                                                       ifelse(TT1988$Norway_Code < 900,8,
                                                                              ifelse(TT1988$Norway_Code < 1000,9,
                                                                                     ifelse(TT1988$Norway_Code < 1100,10,
                                                                                            ifelse(TT1988$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1988$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1988$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1988$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1988$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1988$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1988$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1988$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1988$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1988$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1988$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT1988 <-  TT1988 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))

TT1989$Major_Topic <- ifelse(TT1989$Norway_Code <200,1, 
                             ifelse(TT1989$Norway_Code <300, 2, 
                                    ifelse(TT1989$Norway_Code < 400,3,
                                           ifelse(TT1989$Norway_Code < 500, 4,
                                                  ifelse(TT1989$Norway_Code < 600,5, 
                                                         ifelse(TT1989$Norway_Code < 700, 6,
                                                                ifelse(TT1989$Norway_Code < 800,7,
                                                                       ifelse(TT1989$Norway_Code < 900,8,
                                                                              ifelse(TT1989$Norway_Code < 1000,9,
                                                                                     ifelse(TT1989$Norway_Code < 1100,10,
                                                                                            ifelse(TT1989$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1989$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1989$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1989$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1989$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1989$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1989$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1989$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1989$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1989$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1989$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT1989 <-  TT1989 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))

TT1986 <- na.omit(TT1986)
TT1987 <- na.omit(TT1987)
TT1988 <- na.omit(TT1988)
TT1989 <- na.omit(TT1989)

#### Relative ####
Var1 <- as.factor(c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,23))
data <- as.data.frame(Var1)

R_1986 <- as.data.frame(table(TT1986$Major_Topic)/(length(TT1986$Major_Topic)))
data <- as.data.frame(Var1)
R_1986 <- full_join(data, R_1986)
R_1986[is.na(R_1986)] <- 0
R_1986 <- subset(R_1986, select = Freq)
R_1986 <- as.data.frame(t(R_1986))
R_1986 <- rename(R_1986, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1987 <- as.data.frame(table(TT1987$Major_Topic)/(length(TT1987$Major_Topic)))
data <- as.data.frame(Var1)
R_1987 <- full_join(data, R_1987)
R_1987[is.na(R_1987)] <- 0
R_1987 <- subset(R_1987, select = Freq)
R_1987 <- as.data.frame(t(R_1987))
R_1987 <- rename(R_1987, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1988 <- as.data.frame(table(TT1988$Major_Topic)/(length(TT1988$Major_Topic)))
data <- as.data.frame(Var1)
R_1988 <- full_join(data, R_1988)
R_1988[is.na(R_1988)] <- 0
R_1988 <- subset(R_1988, select = Freq)
R_1988 <- as.data.frame(t(R_1988))
R_1988 <- rename(R_1988, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1989 <- as.data.frame(table(TT1989$Major_Topic)/(length(TT1989$Major_Topic)))
data <- as.data.frame(Var1)
R_1989 <- full_join(data, R_1989)
R_1989[is.na(R_1989)] <- 0
R_1989 <- subset(R_1989, select = Freq)
R_1989 <- as.data.frame(t(R_1989))
R_1989 <- rename(R_1989, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

Brundtland2_1986_1989 <- rbind(R_1986, R_1987, R_1988, R_1989)



TT1990 <- read.xlsx("Trontale 1990 135.xlsx")
TT1990$Major_Topic <- ifelse(TT1990$Norway_Code <200,1, 
                             ifelse(TT1990$Norway_Code <300, 2, 
                                    ifelse(TT1990$Norway_Code < 400,3,
                                           ifelse(TT1990$Norway_Code < 500, 4,
                                                  ifelse(TT1990$Norway_Code < 600,5, 
                                                         ifelse(TT1990$Norway_Code < 700, 6,
                                                                ifelse(TT1990$Norway_Code < 800,7,
                                                                       ifelse(TT1990$Norway_Code < 900,8,
                                                                              ifelse(TT1990$Norway_Code < 1000,9,
                                                                                     ifelse(TT1990$Norway_Code < 1100,10,
                                                                                            ifelse(TT1990$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1990$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1990$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1990$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1990$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1990$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1990$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1990$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1990$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1990$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1990$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT1990 <-  TT1990 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))



TT1990 <- na.omit(TT1990)

#### Realtive ####
Var1 <- as.factor(c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,23))
data <- as.data.frame(Var1)

R_1990 <- as.data.frame(table(TT1990$Major_Topic)/(length(TT1990$Major_Topic)))
data <- as.data.frame(Var1)
R_1990 <- full_join(data, R_1990)
R_1990[is.na(R_1990)] <- 0
R_1990 <- subset(R_1990, select = Freq)
R_1990 <- as.data.frame(t(R_1990))
R_1990 <- rename(R_1990, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

Syse1990 <- R_1990



TT1991 <- read.xlsx("Trontale1991_136.xlsx")
TT1992 <- read.xlsx("Trontale1992.xlsx")
TT1993 <- read.xlsx("Trontale1993_138.xlsx")
TT1994 <- read.xlsx("Trontale1994_139.xlsx")
TT1995 <- read.xlsx("Trontale 1995 140.xlsx")
TT1996 <- read.xlsx("Trontale 1996 141.xlsx")

TT1991$Major_Topic <- ifelse(TT1991$Norway_Code <200,1, 
                             ifelse(TT1991$Norway_Code <300, 2, 
                                    ifelse(TT1991$Norway_Code < 400,3,
                                           ifelse(TT1991$Norway_Code < 500, 4,
                                                  ifelse(TT1991$Norway_Code < 600,5, 
                                                         ifelse(TT1991$Norway_Code < 700, 6,
                                                                ifelse(TT1991$Norway_Code < 800,7,
                                                                       ifelse(TT1991$Norway_Code < 900,8,
                                                                              ifelse(TT1991$Norway_Code < 1000,9,
                                                                                     ifelse(TT1991$Norway_Code < 1100,10,
                                                                                            ifelse(TT1991$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1991$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1991$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1991$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1991$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1991$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1991$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1991$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1991$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1991$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1991$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT1991 <-  TT1991 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))





TT1992$Major_Topic <- ifelse(TT1992$Norway_Code <200,1, 
                             ifelse(TT1992$Norway_Code <300, 2, 
                                    ifelse(TT1992$Norway_Code < 400,3,
                                           ifelse(TT1992$Norway_Code < 500, 4,
                                                  ifelse(TT1992$Norway_Code < 600,5, 
                                                         ifelse(TT1992$Norway_Code < 700, 6,
                                                                ifelse(TT1992$Norway_Code < 800,7,
                                                                       ifelse(TT1992$Norway_Code < 900,8,
                                                                              ifelse(TT1992$Norway_Code < 1000,9,
                                                                                     ifelse(TT1992$Norway_Code < 1100,10,
                                                                                            ifelse(TT1992$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1992$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1992$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1992$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1992$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1992$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1992$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1992$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1992$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1992$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1992$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT1992 <-  TT1992 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))





TT1993$Major_Topic <- ifelse(TT1993$Norway_Code <200,1, 
                             ifelse(TT1993$Norway_Code <300, 2, 
                                    ifelse(TT1993$Norway_Code < 400,3,
                                           ifelse(TT1993$Norway_Code < 500, 4,
                                                  ifelse(TT1993$Norway_Code < 600,5, 
                                                         ifelse(TT1993$Norway_Code < 700, 6,
                                                                ifelse(TT1993$Norway_Code < 800,7,
                                                                       ifelse(TT1993$Norway_Code < 900,8,
                                                                              ifelse(TT1993$Norway_Code < 1000,9,
                                                                                     ifelse(TT1993$Norway_Code < 1100,10,
                                                                                            ifelse(TT1993$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1993$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1993$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1993$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1993$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1993$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1993$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1993$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1993$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1993$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1993$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT1993 <-  TT1993 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))





TT1994$Major_Topic <- ifelse(TT1994$Norway_Code <200,1, 
                             ifelse(TT1994$Norway_Code <300, 2, 
                                    ifelse(TT1994$Norway_Code < 400,3,
                                           ifelse(TT1994$Norway_Code < 500, 4,
                                                  ifelse(TT1994$Norway_Code < 600,5, 
                                                         ifelse(TT1994$Norway_Code < 700, 6,
                                                                ifelse(TT1994$Norway_Code < 800,7,
                                                                       ifelse(TT1994$Norway_Code < 900,8,
                                                                              ifelse(TT1994$Norway_Code < 1000,9,
                                                                                     ifelse(TT1994$Norway_Code < 1100,10,
                                                                                            ifelse(TT1994$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1994$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1994$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1994$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1994$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1994$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1994$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1994$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1994$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1994$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1994$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT1994 <-  TT1994 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))





TT1995$Major_Topic <- ifelse(TT1995$Norway_Code <200,1, 
                             ifelse(TT1995$Norway_Code <300, 2, 
                                    ifelse(TT1995$Norway_Code < 400,3,
                                           ifelse(TT1995$Norway_Code < 500, 4,
                                                  ifelse(TT1995$Norway_Code < 600,5, 
                                                         ifelse(TT1995$Norway_Code < 700, 6,
                                                                ifelse(TT1995$Norway_Code < 800,7,
                                                                       ifelse(TT1995$Norway_Code < 900,8,
                                                                              ifelse(TT1995$Norway_Code < 1000,9,
                                                                                     ifelse(TT1995$Norway_Code < 1100,10,
                                                                                            ifelse(TT1995$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1995$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1995$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1995$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1995$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1995$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1995$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1995$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1995$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1995$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1995$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT1995 <-  TT1995 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))





TT1996$Major_Topic <- ifelse(TT1996$Norway_Code <200,1, 
                             ifelse(TT1996$Norway_Code <300, 2, 
                                    ifelse(TT1996$Norway_Code < 400,3,
                                           ifelse(TT1996$Norway_Code < 500, 4,
                                                  ifelse(TT1996$Norway_Code < 600,5, 
                                                         ifelse(TT1996$Norway_Code < 700, 6,
                                                                ifelse(TT1996$Norway_Code < 800,7,
                                                                       ifelse(TT1996$Norway_Code < 900,8,
                                                                              ifelse(TT1996$Norway_Code < 1000,9,
                                                                                     ifelse(TT1996$Norway_Code < 1100,10,
                                                                                            ifelse(TT1996$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1996$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1996$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1996$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1996$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1996$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1996$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1996$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1996$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1996$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1996$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT1996 <-  TT1996 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))




TT1991 <- na.omit(TT1991)
TT1992 <- na.omit(TT1992)
TT1993 <- na.omit(TT1993)
TT1994 <- na.omit(TT1994)
TT1995 <- na.omit(TT1995)
TT1996 <- na.omit(TT1996)

#### Realtive ####
Var1 <- as.factor(c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,23))
data <- as.data.frame(Var1)

R_1991 <- as.data.frame(table(TT1991$Major_Topic)/(length(TT1991$Major_Topic)))
data <- as.data.frame(Var1)
R_1991 <- full_join(data, R_1991)
R_1991[is.na(R_1991)] <- 0
R_1991 <- subset(R_1991, select = Freq)
R_1991 <- as.data.frame(t(R_1991))
R_1991 <- rename(R_1991, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1992 <- as.data.frame(table(TT1992$Major_Topic)/(length(TT1992$Major_Topic)))
data <- as.data.frame(Var1)
R_1992 <- full_join(data, R_1992)
R_1992[is.na(R_1992)] <- 0
R_1992 <- subset(R_1992, select = Freq)
R_1992 <- as.data.frame(t(R_1992))
R_1992 <- rename(R_1992, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1993 <- as.data.frame(table(TT1993$Major_Topic)/(length(TT1993$Major_Topic)))
data <- as.data.frame(Var1)
R_1993 <- full_join(data, R_1993)
R_1993[is.na(R_1993)] <- 0
R_1993 <- subset(R_1993, select = Freq)
R_1993 <- as.data.frame(t(R_1993))
R_1993 <- rename(R_1993, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1994 <- as.data.frame(table(TT1994$Major_Topic)/(length(TT1994$Major_Topic)))
data <- as.data.frame(Var1)
R_1994 <- full_join(data, R_1994)
R_1994[is.na(R_1994)] <- 0
R_1994 <- subset(R_1994, select = Freq)
R_1994 <- as.data.frame(t(R_1994))
R_1994 <- rename(R_1994, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1995 <- as.data.frame(table(TT1995$Major_Topic)/(length(TT1995$Major_Topic)))
data <- as.data.frame(Var1)
R_1995 <- full_join(data, R_1995)
R_1995[is.na(R_1995)] <- 0
R_1995 <- subset(R_1995, select = Freq)
R_1995 <- as.data.frame(t(R_1995))
R_1995 <- rename(R_1995, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1996 <- as.data.frame(table(TT1996$Major_Topic)/(length(TT1996$Major_Topic)))
data <- as.data.frame(Var1)
R_1996 <- full_join(data, R_1996)
R_1996[is.na(R_1996)] <- 0
R_1996 <- subset(R_1996, select = Freq)
R_1996 <- as.data.frame(t(R_1996))
R_1996 <- rename(R_1996, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

Brundtland3_1991_1996 <- rbind(R_1991, R_1992, R_1993, R_1994, R_1995, R_1996)

#### 1997 - 2021 ####

TT1997 <- read.xlsx("Trontale 1997 142 (2).xlsx")

TT1997$Major_Topic <- ifelse(TT1997$Norway_Code <200,1, 
                             ifelse(TT1997$Norway_Code <300, 2, 
                                    ifelse(TT1997$Norway_Code < 400,3,
                                           ifelse(TT1997$Norway_Code < 500, 4,
                                                  ifelse(TT1997$Norway_Code < 600,5, 
                                                         ifelse(TT1997$Norway_Code < 700, 6,
                                                                ifelse(TT1997$Norway_Code < 800,7,
                                                                       ifelse(TT1997$Norway_Code < 900,8,
                                                                              ifelse(TT1997$Norway_Code < 1000,9,
                                                                                     ifelse(TT1997$Norway_Code < 1100,10,
                                                                                            ifelse(TT1997$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1997$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1997$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1997$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1997$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1997$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1997$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1997$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1997$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1997$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1997$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT1997 <-  TT1997 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))


TT1997 <- na.omit(TT1997)
#### Relative ####
Var1 <- as.factor(c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,23))
data <- as.data.frame(Var1)

R_1997 <- as.data.frame(table(TT1997$Major_Topic)/(length(TT1997$Major_Topic)))
data <- as.data.frame(Var1)
R_1997 <- full_join(data, R_1997)
R_1997[is.na(R_1997)] <- 0
R_1997 <- subset(R_1997, select = Freq)
R_1997 <- as.data.frame(t(R_1997))
R_1997 <- rename(R_1997, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

Jagland1997 <- R_1997



TT1998 <- read.xlsx("Trontale 1998 143.xlsx")
TT1999 <- read.xlsx("Trontale 1999 144.xlsx")

TT1998$Major_Topic <- ifelse(TT1998$Norway_Code <200,1, 
                             ifelse(TT1998$Norway_Code <300, 2, 
                                    ifelse(TT1998$Norway_Code < 400,3,
                                           ifelse(TT1998$Norway_Code < 500, 4,
                                                  ifelse(TT1998$Norway_Code < 600,5, 
                                                         ifelse(TT1998$Norway_Code < 700, 6,
                                                                ifelse(TT1998$Norway_Code < 800,7,
                                                                       ifelse(TT1998$Norway_Code < 900,8,
                                                                              ifelse(TT1998$Norway_Code < 1000,9,
                                                                                     ifelse(TT1998$Norway_Code < 1100,10,
                                                                                            ifelse(TT1998$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1998$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1998$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1998$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1998$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1998$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1998$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1998$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1998$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1998$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1998$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT1998 <-  TT1998 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))





TT1999$Major_Topic <- ifelse(TT1999$Norway_Code <200,1, 
                             ifelse(TT1999$Norway_Code <300, 2, 
                                    ifelse(TT1999$Norway_Code < 400,3,
                                           ifelse(TT1999$Norway_Code < 500, 4,
                                                  ifelse(TT1999$Norway_Code < 600,5, 
                                                         ifelse(TT1999$Norway_Code < 700, 6,
                                                                ifelse(TT1999$Norway_Code < 800,7,
                                                                       ifelse(TT1999$Norway_Code < 900,8,
                                                                              ifelse(TT1999$Norway_Code < 1000,9,
                                                                                     ifelse(TT1999$Norway_Code < 1100,10,
                                                                                            ifelse(TT1999$Norway_Code < 1300,12,
                                                                                                   ifelse(TT1999$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT1999$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT1999$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT1999$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT1999$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT1999$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT1999$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT1999$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT1999$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT1999$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT1999 <-  TT1999 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))




TT1998 <- na.omit(TT1998)
TT1999 <- na.omit(TT1999)

#### Realtive ####
Var1 <- as.factor(c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,23))
data <- as.data.frame(Var1)

R_1998 <- as.data.frame(table(TT1998$Major_Topic)/(length(TT1998$Major_Topic)))
data <- as.data.frame(Var1)
R_1998 <- full_join(data, R_1998)
R_1998[is.na(R_1998)] <- 0
R_1998 <- subset(R_1998, select = Freq)
R_1998 <- as.data.frame(t(R_1998))
R_1998 <- rename(R_1998, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_1999 <- as.data.frame(table(TT1999$Major_Topic)/(length(TT1999$Major_Topic)))
data <- as.data.frame(Var1)
R_1999 <- full_join(data, R_1999)
R_1999[is.na(R_1999)] <- 0
R_1999 <- subset(R_1999, select = Freq)
R_1999 <- as.data.frame(t(R_1999))
R_1999 <- rename(R_1999, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

Bondevik1_1998_1999 <- rbind(R_1998, R_1999)



TT2000 <- read.xlsx("Trontale 2000 145.xlsx")
TT2001 <- read.xlsx("Trontale 2001 146.xlsx")

TT2000$Major_Topic <- ifelse(TT2000$Norway_Code <200,1, 
                             ifelse(TT2000$Norway_Code <300, 2, 
                                    ifelse(TT2000$Norway_Code < 400,3,
                                           ifelse(TT2000$Norway_Code < 500, 4,
                                                  ifelse(TT2000$Norway_Code < 600,5, 
                                                         ifelse(TT2000$Norway_Code < 700, 6,
                                                                ifelse(TT2000$Norway_Code < 800,7,
                                                                       ifelse(TT2000$Norway_Code < 900,8,
                                                                              ifelse(TT2000$Norway_Code < 1000,9,
                                                                                     ifelse(TT2000$Norway_Code < 1100,10,
                                                                                            ifelse(TT2000$Norway_Code < 1300,12,
                                                                                                   ifelse(TT2000$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT2000$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT2000$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT2000$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT2000$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT2000$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT2000$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT2000$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT2000$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT2000$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT2000 <-  TT2000 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))





TT2001$Major_Topic <- ifelse(TT2001$Norway_Code <200,1, 
                             ifelse(TT2001$Norway_Code <300, 2, 
                                    ifelse(TT2001$Norway_Code < 400,3,
                                           ifelse(TT2001$Norway_Code < 500, 4,
                                                  ifelse(TT2001$Norway_Code < 600,5, 
                                                         ifelse(TT2001$Norway_Code < 700, 6,
                                                                ifelse(TT2001$Norway_Code < 800,7,
                                                                       ifelse(TT2001$Norway_Code < 900,8,
                                                                              ifelse(TT2001$Norway_Code < 1000,9,
                                                                                     ifelse(TT2001$Norway_Code < 1100,10,
                                                                                            ifelse(TT2001$Norway_Code < 1300,12,
                                                                                                   ifelse(TT2001$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT2001$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT2001$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT2001$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT2001$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT2001$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT2001$Norway_Code < 2000,19,
                                                                                                                                                    ifelse(TT2001$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT2001$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT2001$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT2001 <-  TT2001 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))





TT2000 <- na.omit(TT2000)
TT2001 <- na.omit(TT2001)

#### Realtive ####
Var1 <- as.factor(c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,23))
data <- as.data.frame(Var1)

R_2000 <- as.data.frame(table(TT2000$Major_Topic)/(length(TT2000$Major_Topic)))
data <- as.data.frame(Var1)
R_2000 <- full_join(data, R_2000)
R_2000[is.na(R_2000)] <- 0
R_2000 <- subset(R_2000, select = Freq)
R_2000 <- as.data.frame(t(R_2000))
R_2000 <- rename(R_2000, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_2001 <- as.data.frame(table(TT2001$Major_Topic)/(length(TT2001$Major_Topic)))
data <- as.data.frame(Var1)
R_2001 <- full_join(data, R_2001)
R_2001[is.na(R_2001)] <- 0
R_2001 <- subset(R_2001, select = Freq)
R_2001 <- as.data.frame(t(R_2001))
R_2001 <- rename(R_2001, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

Stoltenberg1_2000_2001 <- rbind(R_2000, R_2001)





TT2002 <- read.xlsx("Trontale 2002 147 (1).xlsx")
TT2003 <- read.xlsx("Trontale 2003 148.xlsx")
TT2004 <- read.xlsx("Trontale 2004 149.xlsx")
TT2005 <- read.xlsx("Trontale 2005 150.xlsx")


TT2002$Major_Topic <- ifelse(TT2002$Norway_Code <200,1, 
                             ifelse(TT2002$Norway_Code <300, 2, 
                                    ifelse(TT2002$Norway_Code < 400,3,
                                           ifelse(TT2002$Norway_Code < 500, 4,
                                                  ifelse(TT2002$Norway_Code < 600,5, 
                                                         ifelse(TT2002$Norway_Code < 700, 6,
                                                                ifelse(TT2002$Norway_Code < 800,7,
                                                                       ifelse(TT2002$Norway_Code < 900,8,
                                                                              ifelse(TT2002$Norway_Code < 1000,9,
                                                                                     ifelse(TT2002$Norway_Code < 1100,10,
                                                                                            ifelse(TT2002$Norway_Code < 1300,12,
                                                                                                   ifelse(TT2002$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT2002$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT2002$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT2002$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT2002$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT2002$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT2002$Norway_Code < 2002,19,
                                                                                                                                                    ifelse(TT2002$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT2002$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT2002$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT2002 <-  TT2002 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2002,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))





TT2003$Major_Topic <- ifelse(TT2003$Norway_Code <200,1, 
                             ifelse(TT2003$Norway_Code <300, 2, 
                                    ifelse(TT2003$Norway_Code < 400,3,
                                           ifelse(TT2003$Norway_Code < 500, 4,
                                                  ifelse(TT2003$Norway_Code < 600,5, 
                                                         ifelse(TT2003$Norway_Code < 700, 6,
                                                                ifelse(TT2003$Norway_Code < 800,7,
                                                                       ifelse(TT2003$Norway_Code < 900,8,
                                                                              ifelse(TT2003$Norway_Code < 1000,9,
                                                                                     ifelse(TT2003$Norway_Code < 1100,10,
                                                                                            ifelse(TT2003$Norway_Code < 1300,12,
                                                                                                   ifelse(TT2003$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT2003$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT2003$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT2003$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT2003$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT2003$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT2003$Norway_Code < 2002,19,
                                                                                                                                                    ifelse(TT2003$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT2003$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT2003$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT2003 <-  TT2003 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2002,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))




TT2004$Major_Topic <- ifelse(TT2004$Norway_Code <200,1, 
                             ifelse(TT2004$Norway_Code <300, 2, 
                                    ifelse(TT2004$Norway_Code < 400,3,
                                           ifelse(TT2004$Norway_Code < 500, 4,
                                                  ifelse(TT2004$Norway_Code < 600,5, 
                                                         ifelse(TT2004$Norway_Code < 700, 6,
                                                                ifelse(TT2004$Norway_Code < 800,7,
                                                                       ifelse(TT2004$Norway_Code < 900,8,
                                                                              ifelse(TT2004$Norway_Code < 1000,9,
                                                                                     ifelse(TT2004$Norway_Code < 1100,10,
                                                                                            ifelse(TT2004$Norway_Code < 1300,12,
                                                                                                   ifelse(TT2004$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT2004$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT2004$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT2004$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT2004$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT2004$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT2004$Norway_Code < 2002,19,
                                                                                                                                                    ifelse(TT2004$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT2004$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT2004$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT2004 <-  TT2004 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2002,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))





TT2005$Major_Topic <- ifelse(TT2005$Norway_Code <200,1, 
                             ifelse(TT2005$Norway_Code <300, 2, 
                                    ifelse(TT2005$Norway_Code < 400,3,
                                           ifelse(TT2005$Norway_Code < 500, 4,
                                                  ifelse(TT2005$Norway_Code < 600,5, 
                                                         ifelse(TT2005$Norway_Code < 700, 6,
                                                                ifelse(TT2005$Norway_Code < 800,7,
                                                                       ifelse(TT2005$Norway_Code < 900,8,
                                                                              ifelse(TT2005$Norway_Code < 1000,9,
                                                                                     ifelse(TT2005$Norway_Code < 1100,10,
                                                                                            ifelse(TT2005$Norway_Code < 1300,12,
                                                                                                   ifelse(TT2005$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT2005$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT2005$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT2005$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT2005$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT2005$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT2005$Norway_Code < 2002,19,
                                                                                                                                                    ifelse(TT2005$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT2005$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT2005$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT2005 <-  TT2005 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2002,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))





TT2002 <- na.omit(TT2002)
TT2003 <- na.omit(TT2003)
TT2004 <- na.omit(TT2004)
TT2005 <- na.omit(TT2005)
#### Realtive ####
Var1 <- as.factor(c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,23))
data <- as.data.frame(Var1)

R_2002 <- as.data.frame(table(TT2002$Major_Topic)/(length(TT2002$Major_Topic)))
data <- as.data.frame(Var1)
R_2002 <- full_join(data, R_2002)
R_2002[is.na(R_2002)] <- 0
R_2002 <- subset(R_2002, select = Freq)
R_2002 <- as.data.frame(t(R_2002))
R_2002 <- rename(R_2002, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_2003 <- as.data.frame(table(TT2003$Major_Topic)/(length(TT2003$Major_Topic)))
data <- as.data.frame(Var1)
R_2003 <- full_join(data, R_2003)
R_2003[is.na(R_2003)] <- 0
R_2003 <- subset(R_2003, select = Freq)
R_2003 <- as.data.frame(t(R_2003))
R_2003 <- rename(R_2003, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_2004 <- as.data.frame(table(TT2004$Major_Topic)/(length(TT2004$Major_Topic)))
data <- as.data.frame(Var1)
R_2004 <- full_join(data, R_2004)
R_2004[is.na(R_2004)] <- 0
R_2004 <- subset(R_2004, select = Freq)
R_2004 <- as.data.frame(t(R_2004))
R_2004 <- rename(R_2004, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_2005 <- as.data.frame(table(TT2005$Major_Topic)/(length(TT2005$Major_Topic)))
data <- as.data.frame(Var1)
R_2005 <- full_join(data, R_2005)
R_2005[is.na(R_2005)] <- 0
R_2005 <- subset(R_2005, select = Freq)
R_2005 <- as.data.frame(t(R_2005))
R_2005 <- rename(R_2005, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

Bondevik2_2002_2005 <- rbind(R_2002, R_2003, R_2004, R_2005)



TT2006 <- read.xlsx("Trontale 2006 151.xlsx")
TT2007 <- read.xlsx("Trontale 2007 152.xlsx")
TT2008 <- read.xlsx("Trontale 2008 153.xlsx")
TT2009 <- read.xlsx("Trontale 2009 154.xlsx")
TT2010 <- read.xlsx("Trontale 2010 155.xlsx")
TT2011 <- read.xlsx("Trontale 2011 156.xlsx")
TT2012 <- read.xlsx("Trontale 2012 157.xlsx")
TT2013 <- read.xlsx("Trontale 2013 158.xlsx")

TT2006$Major_Topic <- ifelse(TT2006$Norway_Code <200,1, 
                             ifelse(TT2006$Norway_Code <300, 2, 
                                    ifelse(TT2006$Norway_Code < 400,3,
                                           ifelse(TT2006$Norway_Code < 500, 4,
                                                  ifelse(TT2006$Norway_Code < 600,5, 
                                                         ifelse(TT2006$Norway_Code < 700, 6,
                                                                ifelse(TT2006$Norway_Code < 800,7,
                                                                       ifelse(TT2006$Norway_Code < 900,8,
                                                                              ifelse(TT2006$Norway_Code < 1000,9,
                                                                                     ifelse(TT2006$Norway_Code < 1100,10,
                                                                                            ifelse(TT2006$Norway_Code < 1300,12,
                                                                                                   ifelse(TT2006$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT2006$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT2006$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT2006$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT2006$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT2006$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT2006$Norway_Code < 2006,19,
                                                                                                                                                    ifelse(TT2006$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT2006$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT2006$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT2006 <-  TT2006 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2006,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))





TT2007$Major_Topic <- ifelse(TT2007$Norway_Code <200,1, 
                             ifelse(TT2007$Norway_Code <300, 2, 
                                    ifelse(TT2007$Norway_Code < 400,3,
                                           ifelse(TT2007$Norway_Code < 500, 4,
                                                  ifelse(TT2007$Norway_Code < 600,5, 
                                                         ifelse(TT2007$Norway_Code < 700, 6,
                                                                ifelse(TT2007$Norway_Code < 800,7,
                                                                       ifelse(TT2007$Norway_Code < 900,8,
                                                                              ifelse(TT2007$Norway_Code < 1000,9,
                                                                                     ifelse(TT2007$Norway_Code < 1100,10,
                                                                                            ifelse(TT2007$Norway_Code < 1300,12,
                                                                                                   ifelse(TT2007$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT2007$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT2007$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT2007$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT2007$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT2007$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT2007$Norway_Code < 2006,19,
                                                                                                                                                    ifelse(TT2007$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT2007$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT2007$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT2007 <-  TT2007 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2006,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))




TT2008$Major_Topic <- ifelse(TT2008$Norway_Code <200,1, 
                             ifelse(TT2008$Norway_Code <300, 2, 
                                    ifelse(TT2008$Norway_Code < 400,3,
                                           ifelse(TT2008$Norway_Code < 500, 4,
                                                  ifelse(TT2008$Norway_Code < 600,5, 
                                                         ifelse(TT2008$Norway_Code < 700, 6,
                                                                ifelse(TT2008$Norway_Code < 800,7,
                                                                       ifelse(TT2008$Norway_Code < 900,8,
                                                                              ifelse(TT2008$Norway_Code < 1000,9,
                                                                                     ifelse(TT2008$Norway_Code < 1100,10,
                                                                                            ifelse(TT2008$Norway_Code < 1300,12,
                                                                                                   ifelse(TT2008$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT2008$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT2008$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT2008$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT2008$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT2008$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT2008$Norway_Code < 2006,19,
                                                                                                                                                    ifelse(TT2008$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT2008$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT2008$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT2008 <-  TT2008 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2006,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))





TT2009$Major_Topic <- ifelse(TT2009$Norway_Code <200,1, 
                             ifelse(TT2009$Norway_Code <300, 2, 
                                    ifelse(TT2009$Norway_Code < 400,3,
                                           ifelse(TT2009$Norway_Code < 500, 4,
                                                  ifelse(TT2009$Norway_Code < 600,5, 
                                                         ifelse(TT2009$Norway_Code < 700, 6,
                                                                ifelse(TT2009$Norway_Code < 800,7,
                                                                       ifelse(TT2009$Norway_Code < 900,8,
                                                                              ifelse(TT2009$Norway_Code < 1000,9,
                                                                                     ifelse(TT2009$Norway_Code < 1100,10,
                                                                                            ifelse(TT2009$Norway_Code < 1300,12,
                                                                                                   ifelse(TT2009$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT2009$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT2009$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT2009$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT2009$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT2009$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT2009$Norway_Code < 2006,19,
                                                                                                                                                    ifelse(TT2009$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT2009$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT2009$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT2009 <-  TT2009 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2006,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))






TT2010$Major_Topic <- ifelse(TT2010$Norway_Code <200,1, 
                             ifelse(TT2010$Norway_Code <300, 2, 
                                    ifelse(TT2010$Norway_Code < 400,3,
                                           ifelse(TT2010$Norway_Code < 500, 4,
                                                  ifelse(TT2010$Norway_Code < 600,5, 
                                                         ifelse(TT2010$Norway_Code < 700, 6,
                                                                ifelse(TT2010$Norway_Code < 800,7,
                                                                       ifelse(TT2010$Norway_Code < 900,8,
                                                                              ifelse(TT2010$Norway_Code < 1000,9,
                                                                                     ifelse(TT2010$Norway_Code < 1100,10,
                                                                                            ifelse(TT2010$Norway_Code < 1300,12,
                                                                                                   ifelse(TT2010$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT2010$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT2010$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT2010$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT2010$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT2010$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT2010$Norway_Code < 2010,19,
                                                                                                                                                    ifelse(TT2010$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT2010$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT2010$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT2010 <-  TT2010 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2010,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))





TT2011$Major_Topic <- ifelse(TT2011$Norway_Code <200,1, 
                             ifelse(TT2011$Norway_Code <300, 2, 
                                    ifelse(TT2011$Norway_Code < 400,3,
                                           ifelse(TT2011$Norway_Code < 500, 4,
                                                  ifelse(TT2011$Norway_Code < 600,5, 
                                                         ifelse(TT2011$Norway_Code < 700, 6,
                                                                ifelse(TT2011$Norway_Code < 800,7,
                                                                       ifelse(TT2011$Norway_Code < 900,8,
                                                                              ifelse(TT2011$Norway_Code < 1000,9,
                                                                                     ifelse(TT2011$Norway_Code < 1100,10,
                                                                                            ifelse(TT2011$Norway_Code < 1300,12,
                                                                                                   ifelse(TT2011$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT2011$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT2011$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT2011$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT2011$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT2011$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT2011$Norway_Code < 2010,19,
                                                                                                                                                    ifelse(TT2011$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT2011$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT2011$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT2011 <-  TT2011 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2010,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))




TT2012$Major_Topic <- ifelse(TT2012$Norway_Code <200,1, 
                             ifelse(TT2012$Norway_Code <300, 2, 
                                    ifelse(TT2012$Norway_Code < 400,3,
                                           ifelse(TT2012$Norway_Code < 500, 4,
                                                  ifelse(TT2012$Norway_Code < 600,5, 
                                                         ifelse(TT2012$Norway_Code < 700, 6,
                                                                ifelse(TT2012$Norway_Code < 800,7,
                                                                       ifelse(TT2012$Norway_Code < 900,8,
                                                                              ifelse(TT2012$Norway_Code < 1000,9,
                                                                                     ifelse(TT2012$Norway_Code < 1100,10,
                                                                                            ifelse(TT2012$Norway_Code < 1300,12,
                                                                                                   ifelse(TT2012$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT2012$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT2012$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT2012$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT2012$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT2012$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT2012$Norway_Code < 2010,19,
                                                                                                                                                    ifelse(TT2012$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT2012$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT2012$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT2012 <-  TT2012 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2010,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))





TT2013$Major_Topic <- ifelse(TT2013$Norway_Code <200,1, 
                             ifelse(TT2013$Norway_Code <300, 2, 
                                    ifelse(TT2013$Norway_Code < 400,3,
                                           ifelse(TT2013$Norway_Code < 500, 4,
                                                  ifelse(TT2013$Norway_Code < 600,5, 
                                                         ifelse(TT2013$Norway_Code < 700, 6,
                                                                ifelse(TT2013$Norway_Code < 800,7,
                                                                       ifelse(TT2013$Norway_Code < 900,8,
                                                                              ifelse(TT2013$Norway_Code < 1000,9,
                                                                                     ifelse(TT2013$Norway_Code < 1100,10,
                                                                                            ifelse(TT2013$Norway_Code < 1300,12,
                                                                                                   ifelse(TT2013$Norway_Code < 1400,13, 
                                                                                                          ifelse(TT2013$Norway_Code < 1500,14,
                                                                                                                 ifelse(TT2013$Norway_Code < 1600,15,
                                                                                                                        ifelse(TT2013$Norway_Code < 1700,16, 
                                                                                                                               ifelse(TT2013$Norway_Code < 1800,17,
                                                                                                                                      ifelse(TT2013$Norway_Code < 1900,18,
                                                                                                                                             ifelse(TT2013$Norway_Code < 2010,19,
                                                                                                                                                    ifelse(TT2013$Norway_Code < 2100,20,
                                                                                                                                                           ifelse(TT2013$Norway_Code < 2200,21,
                                                                                                                                                                  ifelse(TT2013$Norway_Code < 2400,23,NA)))))))))))))))))))))

TT2013 <-  TT2013 %>%
  mutate(Major_Topic_Name = ifelse(Norway_Code <200,"Macroeconomics", 
                                   ifelse(Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(Norway_Code < 400,"Health",
                                                 ifelse(Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(Norway_Code < 600,"Labour", 
                                                               ifelse(Norway_Code < 700, "Education and culture",
                                                                      ifelse(Norway_Code < 800,"Environment",
                                                                             ifelse(Norway_Code < 900,"Energy",
                                                                                    ifelse(Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(Norway_Code < 2010,"Foreign policy",
                                                                                                                                                          ifelse(Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))






TT2006 <- na.omit(TT2006)
TT2007 <- na.omit(TT2007)
TT2008 <- na.omit(TT2008) 
TT2009 <- na.omit(TT2009)
TT2010 <- na.omit(TT2010)
TT2011 <- na.omit(TT2011)
TT2012 <- na.omit(TT2012)
TT2013 <- na.omit(TT2013)
#### Realtive ####
Var1 <- as.factor(c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,23))
data <- as.data.frame(Var1)

R_2006 <- as.data.frame(table(TT2006$Major_Topic)/(length(TT2006$Major_Topic)))
data <- as.data.frame(Var1)
R_2006 <- full_join(data, R_2006)
R_2006[is.na(R_2006)] <- 0
R_2006 <- subset(R_2006, select = Freq)
R_2006 <- as.data.frame(t(R_2006))
R_2006 <- rename(R_2006, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_2007 <- as.data.frame(table(TT2007$Major_Topic)/(length(TT2007$Major_Topic)))
data <- as.data.frame(Var1)
R_2007 <- full_join(data, R_2007)
R_2007[is.na(R_2007)] <- 0
R_2007 <- subset(R_2007, select = Freq)
R_2007 <- as.data.frame(t(R_2007))
R_2007 <- rename(R_2007, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_2008 <- as.data.frame(table(TT2008$Major_Topic)/(length(TT2008$Major_Topic)))
data <- as.data.frame(Var1)
R_2008 <- full_join(data, R_2008)
R_2008[is.na(R_2008)] <- 0
R_2008 <- subset(R_2008, select = Freq)
R_2008 <- as.data.frame(t(R_2008))
R_2008 <- rename(R_2008, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_2009 <- as.data.frame(table(TT2009$Major_Topic)/(length(TT2009$Major_Topic)))
data <- as.data.frame(Var1)
R_2009 <- full_join(data, R_2009)
R_2009[is.na(R_2009)] <- 0
R_2009 <- subset(R_2009, select = Freq)
R_2009 <- as.data.frame(t(R_2009))
R_2009 <- rename(R_2009, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_2010 <- as.data.frame(table(TT2010$Major_Topic)/(length(TT2010$Major_Topic)))
data <- as.data.frame(Var1)
R_2010 <- full_join(data, R_2010)
R_2010[is.na(R_2010)] <- 0
R_2010 <- subset(R_2010, select = Freq)
R_2010 <- as.data.frame(t(R_2010))
R_2010 <- rename(R_2010, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_2011 <- as.data.frame(table(TT2011$Major_Topic)/(length(TT2011$Major_Topic)))
data <- as.data.frame(Var1)
R_2011 <- full_join(data, R_2011)
R_2011[is.na(R_2011)] <- 0
R_2011 <- subset(R_2011, select = Freq)
R_2011 <- as.data.frame(t(R_2011))
R_2011 <- rename(R_2011, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_2012 <- as.data.frame(table(TT2012$Major_Topic)/(length(TT2012$Major_Topic)))
data <- as.data.frame(Var1)
R_2012 <- full_join(data, R_2012)
R_2012[is.na(R_2012)] <- 0
R_2012 <- subset(R_2012, select = Freq)
R_2012 <- as.data.frame(t(R_2012))
R_2012 <- rename(R_2012, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_2013 <- as.data.frame(table(TT2013$Major_Topic)/(length(TT2013$Major_Topic)))
data <- as.data.frame(Var1)
R_2013 <- full_join(data, R_2013)
R_2013[is.na(R_2013)] <- 0
R_2013 <- subset(R_2013, select = Freq)
R_2013 <- as.data.frame(t(R_2013))
R_2013 <- rename(R_2013, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

Stoltenberg2_2006_2013 <- rbind(R_2006, R_2007, R_2008, R_2009, R_2010, R_2011, R_2012, R_2013)






TT166_2021 <- read.xlsx("Trontale_2021_166.xlsx")
TT165_2020 <- read.xlsx("Trontale_2020_165.xlsx")
TT164_2019 <- read.xlsx("Trontale_2019_164.xlsx")
TT163_2018 <- read.xlsx("Trontale_2018_163.xlsx")
TT162_2017 <- read.xlsx("Trontale_2017_162.xlsx")
TT161_2016 <- read.xlsx("Trontale_2016_161.xlsx")
TT160_2015 <- read.xlsx("Trontale_2015_160.xlsx")
TT159_2014 <- read.xlsx("Trontale_2014_159.xlsx")

TT159_2014$Major_Topic <- ifelse(TT159_2014$Norway_Code <200,1, 
                                 ifelse(TT159_2014$Norway_Code <300, 2, 
                                        ifelse(TT159_2014$Norway_Code < 400,3,
                                               ifelse(TT159_2014$Norway_Code < 500, 4,
                                                      ifelse(TT159_2014$Norway_Code < 600,5, 
                                                             ifelse(TT159_2014$Norway_Code < 700, 6,
                                                                    ifelse(TT159_2014$Norway_Code < 800,7,
                                                                           ifelse(TT159_2014$Norway_Code < 900,8,
                                                                                  ifelse(TT159_2014$Norway_Code < 1000,9,
                                                                                         ifelse(TT159_2014$Norway_Code < 1100,10,
                                                                                                ifelse(TT159_2014$Norway_Code < 1300,12,
                                                                                                       ifelse(TT159_2014$Norway_Code < 1400,13, 
                                                                                                              ifelse(TT159_2014$Norway_Code < 1500,14,
                                                                                                                     ifelse(TT159_2014$Norway_Code < 1600,15,
                                                                                                                            ifelse(TT159_2014$Norway_Code < 1700,16, 
                                                                                                                                   ifelse(TT159_2014$Norway_Code < 1800,17,
                                                                                                                                          ifelse(TT159_2014$Norway_Code < 1900,18,
                                                                                                                                                 ifelse(TT159_2014$Norway_Code < 2000,19,
                                                                                                                                                        ifelse(TT159_2014$Norway_Code < 2100,20,
                                                                                                                                                               ifelse(TT159_2014$Norway_Code < 2200,21,
                                                                                                                                                                      ifelse(TT159_2014$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT159_2014 <- TT159_2014 %>%
  mutate(Major_Topic_Name = ifelse(TT159_2014$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT159_2014$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT159_2014$Norway_Code < 400,"Health",
                                                 ifelse(TT159_2014$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT159_2014$Norway_Code < 600,"Labour", 
                                                               ifelse(TT159_2014$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT159_2014$Norway_Code < 800,"Environment",
                                                                             ifelse(TT159_2014$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT159_2014$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT159_2014$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT159_2014$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT159_2014$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT159_2014$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT159_2014$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT159_2014$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT159_2014$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT159_2014$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT159_2014$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT159_2014$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT159_2014$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT159_2014$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))


TT160_2015$Major_Topic <- ifelse(TT160_2015$Norway_Code <200,1, 
                                 ifelse(TT160_2015$Norway_Code <300, 2, 
                                        ifelse(TT160_2015$Norway_Code < 400,3,
                                               ifelse(TT160_2015$Norway_Code < 500, 4,
                                                      ifelse(TT160_2015$Norway_Code < 600,5, 
                                                             ifelse(TT160_2015$Norway_Code < 700, 6,
                                                                    ifelse(TT160_2015$Norway_Code < 800,7,
                                                                           ifelse(TT160_2015$Norway_Code < 900,8,
                                                                                  ifelse(TT160_2015$Norway_Code < 1000,9,
                                                                                         ifelse(TT160_2015$Norway_Code < 1100,10,
                                                                                                ifelse(TT160_2015$Norway_Code < 1300,12,
                                                                                                       ifelse(TT160_2015$Norway_Code < 1400,13, 
                                                                                                              ifelse(TT160_2015$Norway_Code < 1500,14,
                                                                                                                     ifelse(TT160_2015$Norway_Code < 1600,15,
                                                                                                                            ifelse(TT160_2015$Norway_Code < 1700,16, 
                                                                                                                                   ifelse(TT160_2015$Norway_Code < 1800,17,
                                                                                                                                          ifelse(TT160_2015$Norway_Code < 1900,18,
                                                                                                                                                 ifelse(TT160_2015$Norway_Code < 2000,19,
                                                                                                                                                        ifelse(TT160_2015$Norway_Code < 2100,20,
                                                                                                                                                               ifelse(TT160_2015$Norway_Code < 2200,21,
                                                                                                                                                                      ifelse(TT160_2015$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT160_2015 <- TT160_2015 %>%
  mutate(Major_Topic_Name = ifelse(TT160_2015$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT160_2015$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT160_2015$Norway_Code < 400,"Health",
                                                 ifelse(TT160_2015$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT160_2015$Norway_Code < 600,"Labour", 
                                                               ifelse(TT160_2015$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT160_2015$Norway_Code < 800,"Environment",
                                                                             ifelse(TT160_2015$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT160_2015$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT160_2015$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT160_2015$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT160_2015$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT160_2015$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT160_2015$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT160_2015$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT160_2015$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT160_2015$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT160_2015$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT160_2015$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT160_2015$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT160_2015$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))






TT161_2016$Major_Topic <- ifelse(TT161_2016$Norway_Code <200,1, 
                                 ifelse(TT161_2016$Norway_Code <300, 2, 
                                        ifelse(TT161_2016$Norway_Code < 400,3,
                                               ifelse(TT161_2016$Norway_Code < 500, 4,
                                                      ifelse(TT161_2016$Norway_Code < 600,5, 
                                                             ifelse(TT161_2016$Norway_Code < 700, 6,
                                                                    ifelse(TT161_2016$Norway_Code < 800,7,
                                                                           ifelse(TT161_2016$Norway_Code < 900,8,
                                                                                  ifelse(TT161_2016$Norway_Code < 1000,9,
                                                                                         ifelse(TT161_2016$Norway_Code < 1100,10,
                                                                                                ifelse(TT161_2016$Norway_Code < 1300,12,
                                                                                                       ifelse(TT161_2016$Norway_Code < 1400,13, 
                                                                                                              ifelse(TT161_2016$Norway_Code < 1500,14,
                                                                                                                     ifelse(TT161_2016$Norway_Code < 1600,15,
                                                                                                                            ifelse(TT161_2016$Norway_Code < 1700,16, 
                                                                                                                                   ifelse(TT161_2016$Norway_Code < 1800,17,
                                                                                                                                          ifelse(TT161_2016$Norway_Code < 1900,18,
                                                                                                                                                 ifelse(TT161_2016$Norway_Code < 2000,19,
                                                                                                                                                        ifelse(TT161_2016$Norway_Code < 2100,20,
                                                                                                                                                               ifelse(TT161_2016$Norway_Code < 2200,21,
                                                                                                                                                                      ifelse(TT161_2016$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT161_2016 <- TT161_2016 %>%
  mutate(Major_Topic_Name = ifelse(TT161_2016$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT161_2016$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT161_2016$Norway_Code < 400,"Health",
                                                 ifelse(TT161_2016$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT161_2016$Norway_Code < 600,"Labour", 
                                                               ifelse(TT161_2016$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT161_2016$Norway_Code < 800,"Environment",
                                                                             ifelse(TT161_2016$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT161_2016$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT161_2016$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT161_2016$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT161_2016$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT161_2016$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT161_2016$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT161_2016$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT161_2016$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT161_2016$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT161_2016$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT161_2016$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT161_2016$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT161_2016$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))








TT162_2017$Major_Topic <- ifelse(TT162_2017$Norway_Code <200,1, 
                                 ifelse(TT162_2017$Norway_Code <300, 2, 
                                        ifelse(TT162_2017$Norway_Code < 400,3,
                                               ifelse(TT162_2017$Norway_Code < 500, 4,
                                                      ifelse(TT162_2017$Norway_Code < 600,5, 
                                                             ifelse(TT162_2017$Norway_Code < 700, 6,
                                                                    ifelse(TT162_2017$Norway_Code < 800,7,
                                                                           ifelse(TT162_2017$Norway_Code < 900,8,
                                                                                  ifelse(TT162_2017$Norway_Code < 1000,9,
                                                                                         ifelse(TT162_2017$Norway_Code < 1100,10,
                                                                                                ifelse(TT162_2017$Norway_Code < 1300,12,
                                                                                                       ifelse(TT162_2017$Norway_Code < 1400,13, 
                                                                                                              ifelse(TT162_2017$Norway_Code < 1500,14,
                                                                                                                     ifelse(TT162_2017$Norway_Code < 1600,15,
                                                                                                                            ifelse(TT162_2017$Norway_Code < 1700,16, 
                                                                                                                                   ifelse(TT162_2017$Norway_Code < 1800,17,
                                                                                                                                          ifelse(TT162_2017$Norway_Code < 1900,18,
                                                                                                                                                 ifelse(TT162_2017$Norway_Code < 2000,19,
                                                                                                                                                        ifelse(TT162_2017$Norway_Code < 2100,20,
                                                                                                                                                               ifelse(TT162_2017$Norway_Code < 2200,21,
                                                                                                                                                                      ifelse(TT162_2017$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT162_2017 <- TT162_2017 %>%
  mutate(Major_Topic_Name = ifelse(TT162_2017$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT162_2017$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT162_2017$Norway_Code < 400,"Health",
                                                 ifelse(TT162_2017$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT162_2017$Norway_Code < 600,"Labour", 
                                                               ifelse(TT162_2017$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT162_2017$Norway_Code < 800,"Environment",
                                                                             ifelse(TT162_2017$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT162_2017$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT162_2017$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT162_2017$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT162_2017$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT162_2017$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT162_2017$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT162_2017$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT162_2017$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT162_2017$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT162_2017$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT162_2017$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT162_2017$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT162_2017$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))







TT163_2018$Major_Topic <- ifelse(TT163_2018$Norway_Code <200,1, 
                                 ifelse(TT163_2018$Norway_Code <300, 2, 
                                        ifelse(TT163_2018$Norway_Code < 400,3,
                                               ifelse(TT163_2018$Norway_Code < 500, 4,
                                                      ifelse(TT163_2018$Norway_Code < 600,5, 
                                                             ifelse(TT163_2018$Norway_Code < 700, 6,
                                                                    ifelse(TT163_2018$Norway_Code < 800,7,
                                                                           ifelse(TT163_2018$Norway_Code < 900,8,
                                                                                  ifelse(TT163_2018$Norway_Code < 1000,9,
                                                                                         ifelse(TT163_2018$Norway_Code < 1100,10,
                                                                                                ifelse(TT163_2018$Norway_Code < 1300,12,
                                                                                                       ifelse(TT163_2018$Norway_Code < 1400,13, 
                                                                                                              ifelse(TT163_2018$Norway_Code < 1500,14,
                                                                                                                     ifelse(TT163_2018$Norway_Code < 1600,15,
                                                                                                                            ifelse(TT163_2018$Norway_Code < 1700,16, 
                                                                                                                                   ifelse(TT163_2018$Norway_Code < 1800,17,
                                                                                                                                          ifelse(TT163_2018$Norway_Code < 1900,18,
                                                                                                                                                 ifelse(TT163_2018$Norway_Code < 2000,19,
                                                                                                                                                        ifelse(TT163_2018$Norway_Code < 2100,20,
                                                                                                                                                               ifelse(TT163_2018$Norway_Code < 2200,21,
                                                                                                                                                                      ifelse(TT163_2018$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT163_2018 <- TT163_2018 %>%
  mutate(Major_Topic_Name = ifelse(TT163_2018$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT163_2018$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT163_2018$Norway_Code < 400,"Health",
                                                 ifelse(TT163_2018$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT163_2018$Norway_Code < 600,"Labour", 
                                                               ifelse(TT163_2018$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT163_2018$Norway_Code < 800,"Environment",
                                                                             ifelse(TT163_2018$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT163_2018$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT163_2018$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT163_2018$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT163_2018$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT163_2018$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT163_2018$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT163_2018$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT163_2018$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT163_2018$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT163_2018$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT163_2018$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT163_2018$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT163_2018$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))









TT164_2019$Major_Topic <- ifelse(TT164_2019$Norway_Code <200,1, 
                                 ifelse(TT164_2019$Norway_Code <300, 2, 
                                        ifelse(TT164_2019$Norway_Code < 400,3,
                                               ifelse(TT164_2019$Norway_Code < 500, 4,
                                                      ifelse(TT164_2019$Norway_Code < 600,5, 
                                                             ifelse(TT164_2019$Norway_Code < 700, 6,
                                                                    ifelse(TT164_2019$Norway_Code < 800,7,
                                                                           ifelse(TT164_2019$Norway_Code < 900,8,
                                                                                  ifelse(TT164_2019$Norway_Code < 1000,9,
                                                                                         ifelse(TT164_2019$Norway_Code < 1100,10,
                                                                                                ifelse(TT164_2019$Norway_Code < 1300,12,
                                                                                                       ifelse(TT164_2019$Norway_Code < 1400,13, 
                                                                                                              ifelse(TT164_2019$Norway_Code < 1500,14,
                                                                                                                     ifelse(TT164_2019$Norway_Code < 1600,15,
                                                                                                                            ifelse(TT164_2019$Norway_Code < 1700,16, 
                                                                                                                                   ifelse(TT164_2019$Norway_Code < 1800,17,
                                                                                                                                          ifelse(TT164_2019$Norway_Code < 1900,18,
                                                                                                                                                 ifelse(TT164_2019$Norway_Code < 2000,19,
                                                                                                                                                        ifelse(TT164_2019$Norway_Code < 2100,20,
                                                                                                                                                               ifelse(TT164_2019$Norway_Code < 2200,21,
                                                                                                                                                                      ifelse(TT164_2019$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT164_2019 <- TT164_2019 %>%
  mutate(Major_Topic_Name = ifelse(TT164_2019$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT164_2019$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT164_2019$Norway_Code < 400,"Health",
                                                 ifelse(TT164_2019$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT164_2019$Norway_Code < 600,"Labour", 
                                                               ifelse(TT164_2019$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT164_2019$Norway_Code < 800,"Environment",
                                                                             ifelse(TT164_2019$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT164_2019$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT164_2019$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT164_2019$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT164_2019$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT164_2019$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT164_2019$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT164_2019$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT164_2019$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT164_2019$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT164_2019$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT164_2019$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT164_2019$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT164_2019$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))







TT165_2020$Major_Topic <- ifelse(TT165_2020$Norway_Code <200,1, 
                                 ifelse(TT165_2020$Norway_Code <300, 2, 
                                        ifelse(TT165_2020$Norway_Code < 400,3,
                                               ifelse(TT165_2020$Norway_Code < 500, 4,
                                                      ifelse(TT165_2020$Norway_Code < 600,5, 
                                                             ifelse(TT165_2020$Norway_Code < 700, 6,
                                                                    ifelse(TT165_2020$Norway_Code < 800,7,
                                                                           ifelse(TT165_2020$Norway_Code < 900,8,
                                                                                  ifelse(TT165_2020$Norway_Code < 1000,9,
                                                                                         ifelse(TT165_2020$Norway_Code < 1100,10,
                                                                                                ifelse(TT165_2020$Norway_Code < 1300,12,
                                                                                                       ifelse(TT165_2020$Norway_Code < 1400,13, 
                                                                                                              ifelse(TT165_2020$Norway_Code < 1500,14,
                                                                                                                     ifelse(TT165_2020$Norway_Code < 1600,15,
                                                                                                                            ifelse(TT165_2020$Norway_Code < 1700,16, 
                                                                                                                                   ifelse(TT165_2020$Norway_Code < 1800,17,
                                                                                                                                          ifelse(TT165_2020$Norway_Code < 1900,18,
                                                                                                                                                 ifelse(TT165_2020$Norway_Code < 2000,19,
                                                                                                                                                        ifelse(TT165_2020$Norway_Code < 2100,20,
                                                                                                                                                               ifelse(TT165_2020$Norway_Code < 2200,21,
                                                                                                                                                                      ifelse(TT165_2020$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT165_2020 <- TT165_2020 %>%
  mutate(Major_Topic_Name = ifelse(TT165_2020$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT165_2020$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT165_2020$Norway_Code < 400,"Health",
                                                 ifelse(TT165_2020$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT165_2020$Norway_Code < 600,"Labour", 
                                                               ifelse(TT165_2020$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT165_2020$Norway_Code < 800,"Environment",
                                                                             ifelse(TT165_2020$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT165_2020$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT165_2020$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT165_2020$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT165_2020$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT165_2020$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT165_2020$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT165_2020$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT165_2020$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT165_2020$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT165_2020$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT165_2020$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT165_2020$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT165_2020$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))










TT166_2021$Major_Topic <- ifelse(TT166_2021$Norway_Code <200,1, 
                                 ifelse(TT166_2021$Norway_Code <300, 2, 
                                        ifelse(TT166_2021$Norway_Code < 400,3,
                                               ifelse(TT166_2021$Norway_Code < 500, 4,
                                                      ifelse(TT166_2021$Norway_Code < 600,5, 
                                                             ifelse(TT166_2021$Norway_Code < 700, 6,
                                                                    ifelse(TT166_2021$Norway_Code < 800,7,
                                                                           ifelse(TT166_2021$Norway_Code < 900,8,
                                                                                  ifelse(TT166_2021$Norway_Code < 1000,9,
                                                                                         ifelse(TT166_2021$Norway_Code < 1100,10,
                                                                                                ifelse(TT166_2021$Norway_Code < 1300,12,
                                                                                                       ifelse(TT166_2021$Norway_Code < 1400,13, 
                                                                                                              ifelse(TT166_2021$Norway_Code < 1500,14,
                                                                                                                     ifelse(TT166_2021$Norway_Code < 1600,15,
                                                                                                                            ifelse(TT166_2021$Norway_Code < 1700,16, 
                                                                                                                                   ifelse(TT166_2021$Norway_Code < 1800,17,
                                                                                                                                          ifelse(TT166_2021$Norway_Code < 1900,18,
                                                                                                                                                 ifelse(TT166_2021$Norway_Code < 2000,19,
                                                                                                                                                        ifelse(TT166_2021$Norway_Code < 2100,20,
                                                                                                                                                               ifelse(TT166_2021$Norway_Code < 2200,21,
                                                                                                                                                                      ifelse(TT166_2021$Norway_Code < 2400,23,NA)))))))))))))))))))))
TT166_2021 <- TT166_2021 %>%
  mutate(Major_Topic_Name = ifelse(TT166_2021$Norway_Code <200,"Macroeconomics", 
                                   ifelse(TT166_2021$Norway_Code <300, "Civil rights, minoriy issues, civil liberties", 
                                          ifelse(TT166_2021$Norway_Code < 400,"Health",
                                                 ifelse(TT166_2021$Norway_Code < 500, "Argriculture and fishing",
                                                        ifelse(TT166_2021$Norway_Code < 600,"Labour", 
                                                               ifelse(TT166_2021$Norway_Code < 700, "Education and culture",
                                                                      ifelse(TT166_2021$Norway_Code < 800,"Environment",
                                                                             ifelse(TT166_2021$Norway_Code < 900,"Energy",
                                                                                    ifelse(TT166_2021$Norway_Code < 1000,"Immigration and refugee issues",
                                                                                           ifelse(TT166_2021$Norway_Code < 1100,"Traffic",
                                                                                                  ifelse(TT166_2021$Norway_Code < 1300,"Legal affairs",
                                                                                                         ifelse(TT166_2021$Norway_Code < 1400,"Social policy", 
                                                                                                                ifelse(TT166_2021$Norway_Code < 1500,"Urban and housing issues",
                                                                                                                       ifelse(TT166_2021$Norway_Code < 1600,"Industrial and commercial policy",
                                                                                                                              ifelse(TT166_2021$Norway_Code < 1700,"Defence", 
                                                                                                                                     ifelse(TT166_2021$Norway_Code < 1800,"Research, technology and communications",
                                                                                                                                            ifelse(TT166_2021$Norway_Code < 1900,"Foreign trade",
                                                                                                                                                   ifelse(TT166_2021$Norway_Code < 2000,"Foreign policy",
                                                                                                                                                          ifelse(TT166_2021$Norway_Code < 2100,"Government operations and government issues",
                                                                                                                                                                 ifelse(TT166_2021$Norway_Code < 2200,"Public lands and water management",
                                                                                                                                                                        ifelse(TT166_2021$Norway_Code < 2400,"Cultural policy", "Irrelevant"))))))))))))))))))))))







TT159_2014 <- na.omit(TT159_2014)
TT160_2015 <- na.omit(TT160_2015)
TT161_2016 <- na.omit(TT161_2016)
TT162_2017 <- na.omit(TT162_2017)
TT163_2018 <- na.omit(TT163_2018)
TT164_2019 <- na.omit(TT164_2019)
TT165_2020 <- na.omit(TT165_2020)
TT166_2021 <- na.omit(TT166_2021)


#### Relative ####
Var1 <- as.factor(c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,23))
#Fester alle tallverdier for å kunne gi verdien 0 i kategoriene som ikke er nevnt
R_2014 <- as.data.frame(table(TT159_2014$Major_Topic)/(length(TT159_2014$Major_Topic)))
#Finner den relative fordelingen innenfor hvert Major topic
data <- as.data.frame(Var1)
#Gjør om kategoriene i Var1 til dataframe
R_2014 <- full_join(data, R_2014)
R_2014[is.na(R_2014)] <- 0
#Setter sammen data med relativ fordeling, og at missing på data-variabelen = 0 og ikke NA
R_2014 <- subset(R_2014, select = Freq)
R_2014 <- as.data.frame(t(R_2014))
#Lager et subset med kun de relative verdiene, og snur om rader og kolonner slik at hver major topic blir en variabel
R_2014 <- rename(R_2014, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_2015 <- as.data.frame(table(TT160_2015$Major_Topic)/(length(TT160_2015$Major_Topic)))
#Finner den relative fordelingen innenfor hvert Major topic
data <- as.data.frame(Var1)
#Gjør om kategoriene i Var1 til dataframe
R_2015 <- full_join(data, R_2015)
R_2015[is.na(R_2015)] <- 0
#Setter sammen data med relativ fordeling, og at missing på data-variabelen = 0 og ikke NA
R_2015 <- subset(R_2015, select = Freq)
R_2015 <- as.data.frame(t(R_2015))
#Lager et subset med kun de relative verdiene, og snur om rader og kolonner slik at hver major topic blir en variabel
R_2015 <- rename(R_2015, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_2016 <- as.data.frame(table(TT161_2016$Major_Topic)/(length(TT161_2016$Major_Topic)))
#Finner den relative fordelingen innenfor hvert Major topic
data <- as.data.frame(Var1)
#Gjør om kategoriene i Var1 til dataframe
R_2016 <- full_join(data, R_2016)
R_2016[is.na(R_2016)] <- 0
#Setter sammen data med relativ fordeling, og at missing på data-variabelen = 0 og ikke NA
R_2016 <- subset(R_2016, select = Freq)
R_2016 <- as.data.frame(t(R_2016))
#Lager et subset med kun de relative verdiene, og snur om rader og kolonner slik at hver major topic blir en variabel
R_2016 <- rename(R_2016, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_2017 <- as.data.frame(table(TT162_2017$Major_Topic)/(length(TT162_2017$Major_Topic)))
#Finner den relative fordelingen innenfor hvert Major topic
data <- as.data.frame(Var1)
#Gjør om kategoriene i Var1 til dataframe
R_2017 <- full_join(data, R_2017)
R_2017[is.na(R_2017)] <- 0
#Setter sammen data med relativ fordeling, og at missing på data-variabelen = 0 og ikke NA
R_2017 <- subset(R_2017, select = Freq)
R_2017 <- as.data.frame(t(R_2017))
#Lager et subset med kun de relative verdiene, og snur om rader og kolonner slik at hver major topic blir en variabel
R_2017 <- rename(R_2017, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_2018 <- as.data.frame(table(TT163_2018$Major_Topic)/(length(TT163_2018$Major_Topic)))
#Finner den relative fordelingen innenfor hvert Major topic
data <- as.data.frame(Var1)
#Gjør om kategoriene i Var1 til dataframe
R_2018 <- full_join(data, R_2018)
R_2018[is.na(R_2018)] <- 0
#Setter sammen data med relativ fordeling, og at missing på data-variabelen = 0 og ikke NA
R_2018 <- subset(R_2018, select = Freq)
R_2018 <- as.data.frame(t(R_2018))
#Lager et subset med kun de relative verdiene, og snur om rader og kolonner slik at hver major topic blir en variabel
R_2018 <- rename(R_2018, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_2019 <- as.data.frame(table(TT164_2019$Major_Topic)/(length(TT164_2019$Major_Topic)))
#Finner den relative fordelingen innenfor hvert Major topic
data <- as.data.frame(Var1)
#Gjør om kategoriene i Var1 til dataframe
R_2019 <- full_join(data, R_2019)
R_2019[is.na(R_2019)] <- 0
#Setter sammen data med relativ fordeling, og at missing på data-variabelen = 0 og ikke NA
R_2019 <- subset(R_2019, select = Freq)
R_2019 <- as.data.frame(t(R_2019))
#Lager et subset med kun de relative verdiene, og snur om rader og kolonner slik at hver major topic blir en variabel
R_2019 <- rename(R_2019, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

R_2020 <- as.data.frame(table(TT165_2020$Major_Topic)/(length(TT165_2020$Major_Topic)))
#Finner den relative fordelingen innenfor hvert Major topic
data <- as.data.frame(Var1)
#Gjør om kategoriene i Var1 til dataframe
R_2020 <- full_join(data, R_2020)
R_2020[is.na(R_2020)] <- 0
#Setter sammen data med relativ fordeling, og at missing på data-variabelen = 0 og ikke NA
R_2020 <- subset(R_2020, select = Freq)
R_2020 <- as.data.frame(t(R_2020))
#Lager et subset med kun de relative verdiene, og snur om rader og kolonner slik at hver major topic blir en variabel
R_2020 <- rename(R_2020, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)


R_2021 <- as.data.frame(table(TT166_2021$Major_Topic)/(length(TT166_2021$Major_Topic)))
#Finner den relative fordelingen innenfor hvert Major topic
data <- as.data.frame(Var1)
#Gjør om kategoriene i Var1 til dataframe
R_2021 <- full_join(data, R_2021)
R_2021[is.na(R_2021)] <- 0
#Setter sammen data med relativ fordeling, og at missing på data-variabelen = 0 og ikke NA
R_2021 <- subset(R_2021, select = Freq)
R_2021 <- as.data.frame(t(R_2021))
#Lager et subset med kun de relative verdiene, og snur om rader og kolonner slik at hver major topic blir en variabel
R_2021 <- rename(R_2021, Macroeconomics = 1, Civil_Liberties = 2,
                 Health = 3, Agriculture = 4, Labour = 5,
                 Education = 6, Environment = 7, 
                 Energy = 8, Immigration = 9, Traffic = 10,
                 Legal_Affairs = 11, Social_Policy = 12, Housing = 13,
                 Business = 14, Defence = 15, Research_Technology = 16,
                 Foreign_Trade = 17, Foreign_Policy = 18, Government_Operations = 19,
                 Public_Land = 20, Culture = 21)

Solberg2014_2021 <- rbind(R_2014, R_2015, R_2016, R_2017, R_2018, R_2019, R_2020, R_2021)

###Merging all governments to one dataset ####

#### Gerhardsen 2 1946 - 1951 ####
Ge <- full_join(R_1946, R_1947)
Gee <- full_join(Ge, R_1948)
Geee <- full_join(Gee, R_1949)
Geeee <- full_join(Geee, R_1950)
Gerhardsen2_1946_1951 <- full_join(Geeee, R_1951)

#### Torp 1952-1955 ####
to <- full_join(R_1952, R_1953)
to <- full_join(to, R_1954)
Torp1952_1955 <- full_join(to, R_1955)

#### Gerhardsen 3 1956- 1962 ####
g3 <- full_join(R_1956, R_1957)
g3 <- full_join(g3, R_1958)
g3 <- full_join(g3, R_1959_1)
g3 <- full_join(g3, R_1959_2)
g3 <- full_join(g3, R_1960)
g3 <- full_join(g3, R_1961)
Gerhardsen3_1956_1962 <- full_join(g3, R_1962)

#### Gerhardsen 4 1963-1965 ####
g4 <- full_join(R_1963, R_1964)
Gerhardsen4_1963_1965 <- full_join(g4, R_1965)

#### Borten 1966-1970 ####
Bo <- full_join(R_1966, R_1967)
Bo <- full_join(Bo, R_1968)
Bo <- full_join(Bo, R_1969)
Borten1966_1970 <- full_join(Bo, R_1970)

#### Bratteli 1 1971-72 ####
Bratteli1_1971_1972 <- full_join(R_1971, R_1972)

#### Korvald 1973 ####
Korvald1973

#### Bratteli 2 1974-75 ####
Bratteli2_1974_1975 <- full_join(R_1974, R_1975)

#### Nordli 1976-80 ####
no <- full_join(R_1976, R_1977)
no <- full_join(no, R_1978)
no <- full_join(no, R_1979)
Nordli1976_1980 <- full_join(no, R_1980)

#### Brundtland 1 1981 ####
Brundtland1_1981

#### Willoch 1982-85 ####
W <- full_join(R_1982, R_1983)
W <- full_join(W, R_1984)
Willoch1982_1985 <- full_join(W, R_1985)

#### Brundtland 86-89 ####
b2 <- full_join(R_1986, R_1987)
b2 <- full_join(b2, R_1988)
Brundtland2_1986_1989 <- full_join(b2, R_1989)

#### Syse 90 ####
Syse1990

#### Brundtland 91-96 ####
b3 <- full_join(R_1991, R_1992)
b3 <- full_join(b3, R_1993)
b3 <- full_join(b3, R_1994)
b3 <- full_join(b3, R_1995)
Brundtland3_1991_1996 <- full_join(b3, R_1996)

#### Jagland 1997 ####
Jagland1997

#### Bondevik 98-99 ####
Bondevik1_1998_1999 <- full_join(R_1998, R_1999)

#### Stoltenberg 2000-2001 ####
Stoltenberg1_2000_2001 <- full_join(R_2000, R_2001)

#### Bondevik 2002-2005 ####
bb <- full_join(R_2002, R_2003)
bb <- full_join(bb, R_2004)
Bondevik2_2002_2005 <- full_join(bb, R_2005)

#### Stoltenberg 2006-13 ####
st <- full_join(R_2006, R_2007)
st <- full_join(st, R_2008)
st <- full_join(st, R_2009)
st <- full_join(st, R_2010)
st <- full_join(st, R_2011)
st <- full_join(st, R_2012)
Stoltenberg2_2006_2013 <- full_join(st, R_2013)

#### Solberg 2014-21 ####
s <- full_join(R_2014, R_2015)
s <- full_join(s, R_2016)
s <- full_join(s, R_2017)
s <- full_join(s, R_2018)
s <- full_join(s, R_2019)
s <- full_join(s, R_2020)
Solberg2014_2021 <- full_join(s, R_2021)


#### ALL ####
Relative_NA <- full_join(Gerhardsen2_1946_1951, Torp1952_1955)
Relative_NA <- full_join(Relative_NA, Gerhardsen3_1956_1962)
Relative_NA <- full_join(Relative_NA, Gerhardsen4_1963_1965)
Relative_NA <- full_join(Relative_NA, Borten1966_1970)
Relative_NA <- full_join(Relative_NA, Bratteli1_1971_1972)
Relative_NA <- full_join(Relative_NA, Korvald1973)
Relative_NA <- full_join(Relative_NA, Bratteli2_1974_1975)
Relative_NA <- full_join(Relative_NA, Nordli1976_1980)
Relative_NA <- full_join(Relative_NA, Brundtland1_1981)
Relative_NA <- full_join(Relative_NA, Willoch1982_1985)
Relative_NA <- full_join(Relative_NA, Brundtland2_1986_1989)
Relative_NA <- full_join(Relative_NA, Syse1990)
Relative_NA <- full_join(Relative_NA, Brundtland3_1991_1996)
Relative_NA <- full_join(Relative_NA, Jagland1997)
Relative_NA <- full_join(Relative_NA, Bondevik1_1998_1999)
Relative_NA <- full_join(Relative_NA, Stoltenberg1_2000_2001)
Relative_NA <- full_join(Relative_NA, Bondevik2_2002_2005)
Relative_NA <- full_join(Relative_NA, Stoltenberg2_2006_2013)
Relative_NA <- full_join(Relative_NA, Solberg2014_2021)


