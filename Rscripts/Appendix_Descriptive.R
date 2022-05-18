#### Statements and length over time ####
H3_sub <- H3_ts %>%
  select(Year, Length, Statements) %>%
  gather(key = "Agenda", value = "Count", -Year)
head(H3_sub)

ggplot(H3_sub, aes(x = Year, y = Count)) + 
  geom_line(aes(color = Agenda, linetype = Agenda))+
  scale_color_manual(values = c("darkred", "steelblue"))+
  theme_bw()+
  theme_classic()+
  ggtitle("Agenda Size 1946-2021")+
  xlim(1946,2021)+
  geom_hline(yintercept = 71.6, linetype = "dashed")

#### Topic count ####
Trontaler1946_21 <- read.xlsx("TT1946_21.xlsx")

Trontaler1946_21 %>%
  filter(!is.na(Major_Topic_Name)) %>%
  count(Major_Topic_Name) %>%
  ggplot(aes(reorder(Major_Topic_Name, n), n)) +
  geom_col() +
  coord_flip()+
  ggtitle("Number of statements by policy area 1946-2021") +
  ylab("")+
  xlab("Main topics")

#### Areaplot (relative issue attention) ####
One59 <- Relative_NA[-c(14),] #Removes speech from january 1959 to only have one speech per year
AreaPlot <- One59 %>%
  pivot_longer(!1:6, names_to = "Topics", values_to = "Relative attention")
AreaPlot$Topics <- as.factor(AreaPlot$Topics)


AreaPlot %>% filter (Year != 1959) %>%
  ggplot(aes(Year, `Relative attention`, fill = Topics)) +
  geom_area(alpha = .6, siye = 1, colour = "black") +
  scale_fill_manual(values=scales::hue_pal()(22)
                    ,breaks = c("Macroeconomics",
                                "Civil_Liberties",
                                "Health", "Agriculture",
                                "Labour", "Education",
                                "Environment", "Energy",
                                "Immigration", "Traffic",
                                "Legal_Affairs", "Social_Policy",
                                "Housing", "Business", "Defence",
                                "Research_Technology", "Foreign_Trade",
                                "Foreign_Policy", "Government_Operations",
                                "Public_Land", "Culture", "Irrelevant"))+
  ggtitle("Policy Content of Trontale, 1946 - 2021")+
  coord_cartesian(ylim = c(0,1), expand = FALSE) +
  theme_classic()