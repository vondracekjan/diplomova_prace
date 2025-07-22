library(tidyverse)
library(dplyr)

#nacitani dat
ches_1 <- read.csv("data/ches_1999-2019.csv")
ches_2 <- read.csv("data/ches_2024.csv")
#datovy soubor CHES pro roky 1999-2019 a prozatim oddeleny CHES datovy soubor pro rok 2024 lze stahnout zde: https://www.chesdata.eu/ches-europe

party_facts_ches <- read.csv("data/party_facts_ches.csv")
#datovy soubor Party Facts k CHES kodovani stran lze stahnout zde: https://github.com/hdigital/partyfactsdata/blob/main/import/ches/ches.csv

#uprava datasetu pro rok 2024

ches_2 <- ches_2 %>% mutate(year = 2024)
ches_2 <- ches_2 %>% mutate(year = 2024)

#sjednoceni kodu pro zeme
ches_1$country_abb[ches_1$country == 1] <- "be"
ches_1$country_abb[ches_1$country == 2] <- "dk"
ches_1$country_abb[ches_1$country == 3] <- "ge"
ches_1$country_abb[ches_1$country == 4] <- "gr"
ches_1$country_abb[ches_1$country == 5] <- "esp"
ches_1$country_abb[ches_1$country == 6] <- "fr"
ches_1$country_abb[ches_1$country == 7] <- "irl"
ches_1$country_abb[ches_1$country == 8] <- "it"
ches_1$country_abb[ches_1$country == 10] <- "nl"
ches_1$country_abb[ches_1$country == 11] <- "uk"
ches_1$country_abb[ches_1$country == 12] <- "por"
ches_1$country_abb[ches_1$country == 13] <- "aus"
ches_1$country_abb[ches_1$country == 14] <- "fin"
ches_1$country_abb[ches_1$country == 16] <- "sv"
ches_1$country_abb[ches_1$country == 20] <- "bul"
ches_1$country_abb[ches_1$country == 21] <- "cz"
ches_1$country_abb[ches_1$country == 22] <- "est"
ches_1$country_abb[ches_1$country == 23] <- "hun"
ches_1$country_abb[ches_1$country == 24] <- "lat"
ches_1$country_abb[ches_1$country == 25] <- "lith"
ches_1$country_abb[ches_1$country == 26] <- "pol"
ches_1$country_abb[ches_1$country == 27] <- "rom"
ches_1$country_abb[ches_1$country == 28] <- "slo"
ches_1$country_abb[ches_1$country == 29] <- "sle"
ches_1$country_abb[ches_1$country == 31] <- "cro"
ches_1$country_abb[ches_1$country == 37] <- "mal"
ches_1$country_abb[ches_1$country == 38] <- "lux"
ches_1$country_abb[ches_1$country == 40] <- "cyp"

unique(ches_1$country_abb)
table(ches_1$country, ches_1$country_abb)

#uprava nazvu promennych
names(ches_2)[names(ches_2) == "country"] <- "country_abb"
ches_2$seat <- NULL
names(ches_2)[names(ches_2) == "seatperc"] <- "seat"
unique(ches_2$country_abb)

#sjednoceni kodu pro stranicke rodiny a uprava nazvu promenne
ches_1$family_abb[ches_1$family == 1] <- "radrt"
ches_1$family_abb[ches_1$family == 2] <- "con"
ches_1$family_abb[ches_1$family == 3] <- "lib"
ches_1$family_abb[ches_1$family == 4] <- "cd"
ches_1$family_abb[ches_1$family == 5] <- "soc"
ches_1$family_abb[ches_1$family == 6] <- "radleft"
ches_1$family_abb[ches_1$family == 7] <- "green"
ches_1$family_abb[ches_1$family == 8] <- "reg"
ches_1$family_abb[ches_1$family == 9] <- "nofamily"
ches_1$family_abb[ches_1$family == 10] <- "confess"
ches_1$family_abb[ches_1$family == 11] <- "agrarian/center"

unique(ches_1$family_abb)
table(ches_1$family, ches_1$family_abb)

ches_2$family[ches_2$family == "confessional"] <- "confess"
names(ches_2)[names(ches_2) == "family"] <- "family_abb"
unique(ches_2$family_abb)

#odstraneni roku 1999 - nema pozadovane promenne
ches_1 <- ches_1 %>% filter(year != "1999")

#spojeni CHES dat s Party Facts
ches_1_party_facts <- merge(ches_1, party_facts_ches, by = "party_id", all.x = T)
ches_1_party_facts[,c("party.x", "party.y", "party_id", "year", "family_abb")]
ches_1_party_facts <- ches_1_party_facts %>% select(country_abb, family_abb, year, galtan, redistribution, partyfacts_id, party.x, party.y, party_id, govt, seat)
ches_1_party_facts <- ches_1_party_facts %>% rename(party = party.x, party_partyfacts = party.y)

#spojeni CHES datoveho souboru 1999-2019 trend file s nejnovejsim datovym souborem CHES za rok 2024
ches_all <- bind_rows(ches_1_party_facts, ches_2)
ches_all <- ches_all %>% rename(party_ches = party)
ches_all <- ches_all %>% select(country_abb, family_abb, year, galtan, redistribution, partyfacts_id, party_ches, party_partyfacts, party_id, govt, seat)

#export datoveho souboru
write.csv(ches_all, file = "data/ches_all.csv", row.names = FALSE)
