library(tidyverse)
library(scales)
library(readxl)


#nacitani datasetu vytvorenych v predeslych krocich, jejich spojeni a uprava
ess_all <- read.csv("data/ess_all.csv")
ches_all <- read.csv("data/ches_all.csv")

full_data <- merge(ess_all, ches_all, by = c("partyfacts_id", "year"), all = TRUE)
full_data[,c("party_partyfacts.x", "party_partyfacts.y", "party_ches", "party_ess", "year")]
full_data <- full_data %>% rename(eco_voters = mean_eco, cult_voters = mean_cult, eco_parties = redistribution, cult_parties = galtan)

#nacitani dat pro dalsi promenne


gdp_growth <- read.csv("data/gdp_growth.csv", header = F)
party_system_age <- read_xlsx("data/party_system_age.xlsx")
#datovy soubor World Bank pro rust HDP zeme lze stahnout zde: https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG
#datovy soubor od Casal Bértoa pro stari stranickeho systemu zeme lze stahnou zde: https://whogoverns.eu/party-systems/party-institutionalization/

#reskalovani

full_data$eco_voters_rescale <- rescale(full_data$eco_voters, to = c(0, 10), from = c(1,5))
full_data$cult_voters_rescale <- rescale(full_data$cult_voters, to = c(0, 10), from = c(0,10))

full_data$eco_parties_rescale <- rescale(full_data$eco_parties, to = c(0, 10), from = c(0,10))
full_data$cult_parties_rescale <- rescale(full_data$cult_parties, to = c(0, 10), from = c(0,10))

#vypocet kongruence

full_data$eco_cong <- full_data$eco_parties_rescale - full_data$eco_voters_rescale
full_data$cult_cong <- full_data$cult_parties_rescale - full_data$cult_voters_rescale

#vypocet zavisle promenne posun strany a pro pozici strany v case t + 1

full_data <- full_data %>%
  arrange(party_id, year) %>%
  group_by(party_id) %>%
  mutate(eco_parties_change = (lead(eco_parties_rescale, n = 1) - eco_parties_rescale))

full_data <- full_data %>%
  arrange(party_id, year) %>%
  group_by(party_id) %>%
  mutate(eco_parties_t2 = (lead(eco_parties_rescale, n = 1)))

full_data <- full_data %>%
  arrange(party_id, year) %>%
  group_by(party_id) %>%
  mutate(cult_parties_change = (lead(cult_parties_rescale, n = 1) - cult_parties_rescale))

full_data <- full_data %>%
  arrange(party_id, year) %>%
  group_by(party_id) %>%
  mutate(cult_parties_t2 = (lead(cult_parties_rescale, n = 1)))


#promenna pro vladni stranu

full_data <- full_data %>%
  mutate(govt_edit = if_else(govt > 0.5, 1, 0))

table(full_data$govt, full_data$govt_edit)

#promenna pro podil mandatu v case t - 1

full_data <- full_data %>%
  arrange(party_id, year) %>%
  group_by(party_id) %>%
  mutate(seat_lag = lag(seat, n = 1))

#promenna pro rust hdp

gdp_growth <- gdp_growth %>%
  slice(-c(1,2))
gdp_growth <- gdp_growth %>%
  select(-c(1,3,4:44))
colnames(gdp_growth) <- gdp_growth[1,]
gdp_growth <- gdp_growth %>%
  slice(-1)
gdp_growth <- gdp_growth %>%
  select(-c(26,27))
colnames(gdp_growth)[1] <- "country_abb"
gdp_growth <- pivot_longer(gdp_growth,
                      cols = !country_abb,
                      names_to = "year",
                      values_to = "gdp_growth")


unique(full_data$country_abb)
gdp_growth <- gdp_growth %>%
  filter(country_abb %in% c("IRL", "FRA", "EST", "BEL", "HUN", "NLD", "GRC", "ESP", "POL", "SVK", "LTU", "HRV", "SVN", "PRT", "ITA", "ROU", "LVA", "CZE", "LUX", "SWE", "DNK", "BGR", "DEU", "AUT", "FIN", "CYP", "GBR", "MLT", "TUR", "CHE", "ISL", "NOR")) #filtr zemi ktere jsou v hlavnim datovem souboru
gdp_growth <- gdp_growth %>%
  mutate(country_abb = case_when( 
    country_abb == "IRL" ~ "irl",
    country_abb == "FRA" ~ "fr", 
    country_abb == "EST" ~ "est", 
    country_abb == "BEL" ~ "be", 
    country_abb == "HUN" ~ "hun", 
    country_abb == "NLD" ~ "nl", 
    country_abb == "GRC" ~ "gr", 
    country_abb == "ESP" ~ "esp", 
    country_abb == "POL" ~ "pol", 
    country_abb == "SVK" ~ "slo", 
    country_abb == "LTU" ~ "lith", 
    country_abb == "HRV" ~ "cro", 
    country_abb == "SVN" ~ "sle", 
    country_abb == "PRT" ~ "por", 
    country_abb == "ITA" ~ "it", 
    country_abb == "ROU" ~ "rom", 
    country_abb == "LVA" ~ "lat", 
    country_abb == "CZE" ~ "cz", 
    country_abb == "LUX" ~ "lux", 
    country_abb == "SWE" ~ "sv", 
    country_abb == "DNK" ~ "dk",
    country_abb == "BGR" ~ "bul", 
    country_abb == "DEU" ~ "ge", 
    country_abb == "AUT" ~ "aus", 
    country_abb == "FIN" ~ "fin", 
    country_abb == "CYP" ~ "cyp", 
    country_abb == "GBR" ~ "uk", 
    country_abb == "MLT" ~ "mal", 
    country_abb == "TUR" ~ "tur", 
    country_abb == "CHE" ~ "swi", 
    country_abb == "ISL" ~ "ice", 
    country_abb == "NOR" ~ "nor",
    TRUE ~ NA
  ))

full_data <- merge(full_data, gdp_growth, by = c("country_abb", "year"), all.x = T, all.y = F)

#promenna pro stari stranickeho systemu

unique(full_data$country_abb)
party_system_age <- party_system_age %>%
  filter(country %in% c("Austria II", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Spain III", "Estonia II", "Finland II", "France IV", "Germany II", "Greece IV", "Hungary", "Iceland", "Ireland", "Italy", "Latvia II", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Norway", "Poland II", "Portugal II", "Romania", "Slovenia", "Slovakia", "Sweden", "Switzerland", "Turkey III", "United Kingdom")) #filtr zemi ktere jsou v hlavnim datovem souboru
party_system_age <- party_system_age %>%
  mutate(country_abb = case_when( 
    country == "Austria II" ~ "aus",
    country == "Belgium" ~ "be", 
    country == "Bulgaria" ~ "bul", 
    country == "Croatia" ~ "cro", 
    country == "Cyprus" ~ "cyp", 
    country == "Czechia" ~ "cz", 
    country == "Denmark" ~ "dk", 
    country == "Spain III" ~ "esp", 
    country == "Estonia II" ~ "est", 
    country == "Finland II" ~ "fin", 
    country == "France IV" ~ "fr", 
    country == "Germany II" ~ "ge", 
    country == "Greece IV" ~ "gr", 
    country == "Hungary" ~ "hun", 
    country == "Iceland" ~ "ice", 
    country == "Ireland" ~ "irl", 
    country == "Italy" ~ "it", 
    country == "Latvia II" ~ "lat", 
    country == "Lithuania" ~ "lith", 
    country == "Luxembourg" ~ "lux", 
    country == "Malta" ~ "mal",
    country == "Netherlands" ~ "nl", 
    country == "Norway" ~ "nor", 
    country == "Poland II" ~ "pol", 
    country == "Portugal II" ~ "por", 
    country == "Romania" ~ "rom", 
    country == "Slovenia" ~ "sle", 
    country == "Slovakia" ~ "slo", 
    country == "Sweden" ~ "sv", 
    country == "Switzerland" ~ "swi", 
    country == "Turkey III" ~ "tur",
    country == "United Kingdom" ~ "uk",
    TRUE ~ NA
  ))

party_system_age <- party_system_age %>%
  select(-c(1, 3))
colnames(party_system_age)[2] <- "party_system_age"
full_data <- merge(full_data, party_system_age, by = c("country_abb", "year"), all.x = T, all.y = F)



#export datoveho souboru
write.csv(full_data, file = "data/full_data.csv", row.names = FALSE)
