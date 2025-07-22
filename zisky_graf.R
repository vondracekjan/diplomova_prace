library(tidyverse)
library(ggplot2)
library(ggpubr)


#nacitani dat
party <- read.csv("data/view_party.csv")
election <- read.csv("data/view_election.csv")
#datovy soubor ParlGov pro zisky socialnedemokratickych stran lze stahnout zde: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/2VZ5ZC


#uprava dat
party <- party %>% select(family_name, party_id)

merge_data <- merge(party, election, by  = "party_id")

merge_data <- merge_data %>% filter(family_name == "Social democracy") %>% 
  filter(country_name %in% c("Poland", "Switzerland", "Italy", "Slovakia", "Denmark", "Czech Republic", "Norway", "Slovenia", "Ireland", 
                             "Belgium", "France", "Iceland", "Greece", "Romania", "Latvia", "United Kingdom", "Portugal", "Spain", "Malta", 
                             "Finland", "Estonia", "Lithuania", "Germany", "Luxembourg", "Bulgaria", "Netherlands", "Sweden", "Cyprus", 
                             "Austria", "Croatia", "Hungary")) %>%
  filter(election_type == "parliament") %>%
  filter(election_date >= 1945)
merge_data$election_date <- as.Date(merge_data$election_date)
merge_data <- merge_data %>% 
  group_by(country_name, election_date) %>% 
  summarize(sum_vote = sum(vote_share, na.rm = T))
merge_data$election_date <- format(merge_data$election_date, "%Y")
merge_data$election_date <- as.numeric(merge_data$election_date )

merge_data <- merge_data %>% 
  group_by(election_date) %>% 
  summarize(mean_vote = mean(sum_vote, na.rm = T))


#graf
ggplot(merge_data, aes(x = election_date, y = mean_vote)) + geom_point() + 
  geom_smooth(se = T, colour = "black", method = "loess") + 
  xlab("Rok voleb") + 
  ylab("Průměrný zisk sociálnědemokratickcýh stran (v %)") + 
  scale_x_continuous(breaks = seq(1945, 2023, by = 5)) +
  theme_pubr()

#export grafu
ggsave("export/zisky.jpeg", height = 6, width = 12)
