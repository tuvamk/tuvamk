####Creating dataset ####
H2 <- subset(Relative_NA2, select = c(1,3,4,5,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48))

#Setting nan and inf = NA
H2$Civil_Lib_change[is.infinite(H2$Civil_Lib_change)] <- NA
H2$Civil_Lib_change[is.nan(H2$Civil_Lib_change)] <- NA

H2$Health_change[is.infinite(H2$Health_change)] <- NA
H2$Health_change[is.nan(H2$Health_change)] <- NA

H2$Agriculture_change[is.infinite(H2$Agriculture_change)] <- NA
H2$Agriculture_change[is.nan(H2$Agriculture_change)] <- NA

H2$Labour_change[is.infinite(H2$Labour_change)] <- NA
H2$Labour_change[is.nan(H2$Labour_change)] <- NA

H2$Education_change[is.infinite(H2$Education_change)] <- NA
H2$Education_change[is.nan(H2$Education_change)] <- NA

H2$Environment_change[is.infinite(H2$Environment_change)] <- NA
H2$Environment_change[is.nan(H2$Environment_change)] <- NA

H2$Energy_change[is.infinite(H2$Energy_change)] <- NA
H2$Energy_change[is.nan(H2$Energy_change)] <- NA

H2$Immigration_change[is.infinite(H2$Immigration_change)] <- NA
H2$Immigration_change[is.nan(H2$Immigration_change)] <- NA

H2$Traffic_change[is.infinite(H2$Traffic_change)] <- NA
H2$Traffic_change[is.nan(H2$Traffic_change)] <- NA

H2$Legal_change[is.infinite(H2$Legal_change)] <- NA
H2$Legal_change[is.nan(H2$Legal_change)] <- NA

H2$Social_change[is.infinite(H2$Social_change)] <- NA
H2$Social_change[is.nan(H2$Social_change)] <- NA

H2$Housing_change[is.infinite(H2$Housing_change)] <- NA
H2$Housing_change[is.nan(H2$Housing_change)] <- NA

H2$Business_change[is.infinite(H2$Business_change)] <- NA
H2$Business_change[is.nan(H2$Business_change)] <- NA

H2$Defence_change[is.infinite(H2$Defence_change)] <- NA
H2$Defence_change[is.nan(H2$Defence_change)] <- NA

H2$ResearchTech_change[is.infinite(H2$ResearchTech_change)] <- NA
H2$ResearchTech_change[is.nan(H2$ResearchTech_change)] <- NA

H2$FTrade_change[is.infinite(H2$FTrade_change)] <- NA
H2$FTrade_change[is.nan(H2$FTrade_change)] <- NA

H2$Foreign_change[is.infinite(H2$Foreign_change)] <- NA
H2$Foreign_change[is.nan(H2$Foreign_change)] <- NA

H2$Government_change[is.infinite(H2$Government_change)] <- NA
H2$Government_change[is.nan(H2$Government_change)] <- NA

H2$PLand_change[is.infinite(H2$PLand_change)] <- NA
H2$PLand_change[is.nan(H2$PLand_change)] <- NA

H2$Culture_change[is.infinite(H2$Culture_change)] <- NA
H2$Culture_change[is.nan(H2$Culture_change)] <- NA


#Counting punctuations over 250 %
H2$Macro_P <- ifelse(H2$Macro_change > 2.5, 1,0)
H2$Civil_P <- ifelse(H2$Civil_Lib_change> 2.5, 1,0)
H2$Health_P <- ifelse(H2$Health_change> 2.5, 1,0)
H2$Agriculture_P <- ifelse(H2$Agriculture_change> 2.5, 1,0)
H2$Labor_P <- ifelse(H2$Labour_change> 2.5, 1,0)
H2$Education_P <- ifelse(H2$Education_change> 2.5, 1,0)
H2$Environment_P <- ifelse(H2$Environment_change> 2.5, 1,0)
H2$Energy_P <- ifelse(H2$Energy_change> 2.5, 1,0)
H2$Immigration_P <- ifelse(H2$Immigration_change> 2.5, 1,0)
H2$Traffic_P <- ifelse(H2$Traffic_change> 2.5, 1,0)
H2$Law_P <- ifelse(H2$Legal_change> 2.5, 1,0)
H2$Social_P <- ifelse(H2$Social_change> 2.5, 1,0)
H2$Housing_P <- ifelse(H2$Housing_change> 2.5, 1,0)
H2$Business_P <- ifelse(H2$Business_change> 2.5, 1,0)
H2$Defense_P <- ifelse(H2$Defence_change> 2.5, 1,0)
H2$Research_P <- ifelse(H2$ResearchTech_change> 2.5, 1,0)
H2$Trade_P <- ifelse(H2$FTrade_change> 2.5, 1,0)
H2$Foreign_P <- ifelse(H2$Foreign_change> 2.5, 1,0)
H2$Government_P <- ifelse(H2$Government_change> 2.5, 1,0)
H2$Land_P <- ifelse(H2$PLand_change> 2.5, 1,0)
H2$Culture_P <- ifelse(H2$Culture_change> 2.5, 1,0)

table(H2[, 26:46] == 1) #46 punctuations


#### subset punctuatiuons to investigate topics/details/paritsan change ####

p1 <- H2 %>%
  filter(Macro_change > 2.5)

p2 <- H2 %>%
  filter(Civil_Lib_change > 2.5)

p3 <- H2 %>%
  filter(Health_change > 2.5)

p4 <- H2 %>%
  filter(Agriculture_change > 2.5)

p5 <- H2 %>%
  filter(Labour_change > 2.5)

p6 <- H2 %>%
  filter(Education_change > 2.5)

p7 <- H2 %>%
  filter(Environment_change > 2.5)

p8 <- H2 %>%
  filter(Energy_change > 2.5)

p9 <- H2 %>%
  filter(Immigration_change > 2.5)

p10 <- H2 %>%
  filter(Traffic_change > 2.5)

p12 <- H2 %>%
  filter(Legal_change > 2.5)

p13 <- H2 %>%
  filter(Social_change > 2.5)

p15 <- H2 %>%
  filter(Business_change > 2.5)

p16 <- H2 %>%
  filter(Defence_change > 2.5)

p17 <- H2 %>%
  filter(ResearchTech_change > 2.5)

p18 <- H2 %>%
  filter(FTrade_change > 2.5)

p20 <- H2 %>%
  filter(Government_change > 2.5)

p21 <- H2 %>%
  filter(PLand_change > 2.5)


write.xlsx(p1, "C:\\Users\\tuvamk\\OneDrive - Universitetet i Oslo\\Documents\\Master\\p1.xlsx")
write.xlsx(p2, "C:\\Users\\tuvamk\\OneDrive - Universitetet i Oslo\\Documents\\Master\\p2.xlsx")
write.xlsx(p3, "C:\\Users\\tuvamk\\OneDrive - Universitetet i Oslo\\Documents\\Master\\p3.xlsx")
write.xlsx(p4, "C:\\Users\\tuvamk\\OneDrive - Universitetet i Oslo\\Documents\\Master\\p44.xlsx")
write.xlsx(p5, "C:\\Users\\tuvamk\\OneDrive - Universitetet i Oslo\\Documents\\Master\\p55.xlsx")
write.xlsx(p6, "C:\\Users\\tuvamk\\OneDrive - Universitetet i Oslo\\Documents\\Master\\p6.xlsx")
write.xlsx(p7, "C:\\Users\\tuvamk\\OneDrive - Universitetet i Oslo\\Documents\\Master\\p7.xlsx")
write.xlsx(p8, "C:\\Users\\tuvamk\\OneDrive - Universitetet i Oslo\\Documents\\Master\\p8.xlsx")
write.xlsx(p9, "C:\\Users\\tuvamk\\OneDrive - Universitetet i Oslo\\Documents\\Master\\p9.xlsx")
write.xlsx(p10, "C:\\Users\\tuvamk\\OneDrive - Universitetet i Oslo\\Documents\\Master\\p10.xlsx")
write.xlsx(p12, "C:\\Users\\tuvamk\\OneDrive - Universitetet i Oslo\\Documents\\Master\\p12.xlsx")
write.xlsx(p13, "C:\\Users\\tuvamk\\OneDrive - Universitetet i Oslo\\Documents\\Master\\p13.xlsx")
write.xlsx(p15, "C:\\Users\\tuvamk\\OneDrive - Universitetet i Oslo\\Documents\\Master\\p15.xlsx")
write.xlsx(p16, "C:\\Users\\tuvamk\\OneDrive - Universitetet i Oslo\\Documents\\Master\\p16.xlsx")
write.xlsx(p17, "C:\\Users\\tuvamk\\OneDrive - Universitetet i Oslo\\Documents\\Master\\p17.xlsx")
write.xlsx(p18, "C:\\Users\\tuvamk\\OneDrive - Universitetet i Oslo\\Documents\\Master\\p18.xlsx")
write.xlsx(p20, "C:\\Users\\tuvamk\\OneDrive - Universitetet i Oslo\\Documents\\Master\\p20.xlsx")
write.xlsx(p21, "C:\\Users\\tuvamk\\OneDrive - Universitetet i Oslo\\Documents\\Master\\p21.xlsx")
#Merge punctuations together in excel to create punctuation-table

PandP <- read.xlsx("PandP.xlsx")
write.xlsx(PandP, "C:\\Users\\tuvamk\\OneDrive - Universitetet i Oslo\\Documents\\Master\\PandP2.xlsx")

