library(tidyverse)
library(dplyr)
library(scales)

#nacitani dat

ess_2006 <- read.csv("data/ess_2006.csv")
#datovy soubor ESS za rok 2006 lze stahnout zde: https://doi.org/10.21338/ess3e03_7

ess_2006 <- ess_2006 %>%
  mutate(anweight = pspwght * pweight) #nutny vypocet vahy pro tento dataset

ess_2010 <- read.csv("data/ess_2010.csv")
ess_2014 <- read.csv("data/ess_2014.csv")
ess_2018 <- read.csv("data/ess_2018.csv")
#datovy soubor ESS za rok 2010 lze stahnout zde: https://doi.org/10.21338/ess5e03_5
#datovy soubor ESS za rok 2014 lze stahnout zde: https://doi.org/10.21338/ess7e02_3 
#datovy soubor ESS za rok 2018 lze stahnout zde: https://doi.org/10.21338/ess9e03_2


#nacitani kodu party facts a uprava
party_facts <- read.csv("data/ess/essprt-all.csv")
#datovy soubor Party Facts k ESS kodovani stran lze stahnout zde: https://github.com/hdigital/partyfactsdata/blob/main/import/essprtv/essprt-all.csv

#spojeni promennych ESS ID strany a zeme v ramci datoveho souboru Party Facts
party_facts <- party_facts %>% unite("party_choice_country", c("ess_party_id", "ess_cntry"), remove = FALSE)


###ESS 2006###

#mazani dat o volebnich ziscich z dalsich hlasu v ramci jedne zeme a spojovani dat o volebnich ziscich
ess_2006 <- ess_2006 %>% unite("party_choice", c("prtvtaat", "prtvtabe", "prtvtbg", "prtvtach", "prtvtcy", "prtvbde2", "prtvtadk", "prtvtaee", "prtvtaes", "prtvtfi", "prtvtafr", "prtvtagb", "prtvtahu", "prtvtie", "prtvtlv", "prtvtbnl", "prtvtno", "prtvtapl", "prtvtapt", "prtvtro", "prtvtru", "prtvtse", "prtvtbsi", "prtvtask", "prtvtaua", "prtvbde2"),
                               na.rm = TRUE, remove = FALSE)

ess_2006$party_choice[ess_2006$party_choice %in% c(66, 77, 88, 99)] <- NA #cisteni dat
ess_2006 <- subset(ess_2006,!is.na(party_choice))
unique(ess_2006$party_choice)

#spojeni promennych volby strany a zeme
ess_2006 <- ess_2006 %>% unite("party_choice_country", c("party_choice", "cntry"), na.rm = TRUE, remove = FALSE)

#vyber relevantnich promennych o pozicich volicu a uprava

ess_2006_filter <- ess_2006 %>% select(party_choice_country, gincdif, imueclt, freehms, anweight)

ess_2006_filter$gincdif[ess_2006_filter$gincdif %in% c(7, 8, 9)] <- NA
ess_2006_filter$imueclt[ess_2006_filter$imueclt %in% c(77, 88, 99)] <- NA
ess_2006_filter$freehms[ess_2006_filter$freehms %in% c(7, 8, 9)] <- NA

ess_2006_filter$freehms <- rescale(ess_2006_filter$freehms, to = c(0, 10), from = c(1,5))

ess_2006_filter <- ess_2006_filter %>%
  mutate(imueclt_rev = case_when( 
    imueclt == 0 ~ 10,
    imueclt == 1 ~ 9,
    imueclt == 2 ~ 8,
    imueclt == 3 ~ 7,
    imueclt == 4 ~ 6,
    imueclt == 5 ~ 5,
    imueclt == 6 ~ 4,
    imueclt == 7 ~ 3,
    imueclt == 8 ~ 2,
    imueclt == 9 ~ 1,
    imueclt == 10 ~ 0,
    TRUE ~ NA
  ))


ess_2006_filter <- ess_2006_filter %>% 
  rowwise() %>%
  mutate(cult = mean(c(imueclt_rev, freehms), na.rm = F, )) #prumer otazek merici kulturni dimenzi

#prumerna hodnota pozicnich promennych pro volice kazde strany v datovem souboru
ess_2006_agg <- ess_2006_filter %>%
  group_by(party_choice_country) %>%
  summarise(mean_eco = weighted.mean(gincdif, anweight, na.rm = T), mean_cult = weighted.mean(cult, anweight, na.rm = T))

#spojeni dat ESS s kody z Party Facts
party_facts_2006 <- party_facts %>% filter(essround == 3)
party_facts_2006 <- party_facts_2006 %>% filter(ess_variable %in% c("prtvtaat", "prtvtabe", "prtvtbg", "prtvtach", "prtvtcy", "prtvbde2", "prtvtadk", "prtvtaee", "prtvtaes", "prtvtfi", "prtvtafr", "prtvtagb", "prtvtahu", "prtvtie", "prtvtlv", "prtvtbnl", "prtvtno", "prtvtapl", "prtvtapt", "prtvtro", "prtvtru", "prtvtse", "prtvtbsi", "prtvtask", "prtvtaua"))

ess_2006_agg <- merge(ess_2006_agg, party_facts_2006, by = "party_choice_country")

#vyber pozadovanych promennych a pridani roku
ess_2006_agg <- ess_2006_agg %>% select(mean_eco, mean_cult, ess_cntry, partyfacts_id, ess_party, name_short)
ess_2006_agg <- ess_2006_agg %>% mutate(year = 2006)




###ESS 2010###

#mazani dat o volebnich ziscich z dalsich hlasu v ramci jedne zeme a spojovani dat o volebnich ziscich
ess_2010 <- ess_2010 %>% unite("party_choice", c("prtvtcbe", "prtvtbbg", "prtvtcch", "prtvtcy", "prtvtbcz", "prtvcde2", "prtvtbdk", "prtvtcee", "prtvtbes", "prtvtbfi", "prtvtbfr", "prtvtgb", "prtvtcgr", "prtvthr", "prtvtchu", "prtvtaie", "prtvtbil", "prtvlt1", "prtvtdnl", "prtvtano", "prtvtbpl", "prtvtbpt", "prtvtbru", "prtvtase", "prtvtcsi", "prtvtbsk", "prtvtbua"),
                               na.rm = TRUE, remove = FALSE)

ess_2010$party_choice[ess_2010$party_choice %in% c(66, 77, 88, 99)] <- NA #cisteni dat
ess_2010 <- subset(ess_2010,!is.na(party_choice))
unique(ess_2010$party_choice)

#spojeni promennych volby strany a zeme
ess_2010 <- ess_2010 %>% unite("party_choice_country", c("party_choice", "cntry"), na.rm = TRUE, remove = FALSE)

#vyber relevantnich promennych o pozicich volicu a uprava

ess_2010_filter <- ess_2010 %>% select(party_choice_country, gincdif, imueclt, freehms, anweight)

ess_2010_filter$gincdif[ess_2010_filter$gincdif %in% c(7, 8, 9)] <- NA
ess_2010_filter$imueclt[ess_2010_filter$imueclt %in% c(77, 88, 99)] <- NA
ess_2010_filter$freehms[ess_2010_filter$freehms %in% c(7, 8, 9)] <- NA

ess_2010_filter$freehms <- rescale(ess_2010_filter$freehms, to = c(0, 10), from = c(1,5))

ess_2010_filter <- ess_2010_filter %>%
  mutate(imueclt_rev = case_when( 
    imueclt == 0 ~ 10,
    imueclt == 1 ~ 9,
    imueclt == 2 ~ 8,
    imueclt == 3 ~ 7,
    imueclt == 4 ~ 6,
    imueclt == 5 ~ 5,
    imueclt == 6 ~ 4,
    imueclt == 7 ~ 3,
    imueclt == 8 ~ 2,
    imueclt == 9 ~ 1,
    imueclt == 10 ~ 0,
    TRUE ~ NA
  ))

ess_2010_filter <- ess_2010_filter %>% 
  rowwise() %>%
  mutate(cult = mean(c(imueclt_rev, freehms), na.rm = F)) #prumer otazek merici kulturni dimenzi

#prumerna hodnota pozicnich promennych pro volice kazde strany v datovem souboru
ess_2010_agg <- ess_2010_filter %>%
  group_by(party_choice_country) %>%
  summarise(mean_eco = weighted.mean(gincdif, anweight, na.rm = T), mean_cult = weighted.mean(cult, anweight, na.rm = T))

#spojeni dat ESS s kody z Party Facts
party_facts_2010 <- party_facts %>% filter(essround == 5)
party_facts_2010 <- party_facts_2010 %>% filter(ess_variable %in% c("prtvtcbe", "prtvtbbg", "prtvtcch", "prtvtcy", "prtvtbcz", "prtvcde2", "prtvtbdk", "prtvtcee", "prtvtbes", "prtvtbfi", "prtvtbfr", "prtvtgb", "prtvtcgr", "prtvthr", "prtvtchu", "prtvtaie", "prtvtbil", "prtvlt1", "prtvtdnl", "prtvtano", "prtvtbpl", "prtvtbpt", "prtvtbru", "prtvtase", "prtvtcsi", "prtvtbsk", "prtvtbua"))

ess_2010_agg <- merge(ess_2010_agg, party_facts_2010, by = "party_choice_country")

#vyber pozadovanych promennych a pridani roku
ess_2010_agg <- ess_2010_agg %>% select(mean_eco, mean_cult, ess_cntry, partyfacts_id, ess_party, name_short)
ess_2010_agg <- ess_2010_agg %>% mutate(year = 2010)




###ESS 2014###

#mazani dat o volebnich ziscich z dalsich hlasu v ramci jedne zeme a spojovani dat o volebnich ziscich
ess_2014 <- ess_2014 %>% unite("party_choice", c("prtvtbat", "prtvtcbe", "prtvtech", "prtvtdcz", "prtvede2", "prtvtcdk", "prtvteee", "prtvtces", "prtvtcfi", "prtvtcfr", "prtvtbgb", "prtvtehu", "prtvtaie", "prtvtcil", "prtvalt1", "prtvtfnl", "prtvtbno", "prtvtcpl", "prtvtbpt", "prtvtbse", "prtvtesi"),
                               na.rm = TRUE, remove = FALSE)

ess_2014$party_choice[ess_2014$party_choice %in% c(66, 77, 88, 99)] <- NA
ess_2014 <- subset(ess_2014,!is.na(party_choice))
unique(ess_2014$party_choice)

#spojeni promennych volby strany a zeme
ess_2014 <- ess_2014 %>% unite("party_choice_country", c("party_choice", "cntry"), na.rm = TRUE, remove = FALSE)

#vyber relevantnich promennych o pozicich volicu a uprava

ess_2014_filter <- ess_2014 %>%  select(party_choice_country, gincdif, imueclt, freehms, anweight)

ess_2014_filter$gincdif[ess_2014_filter$gincdif %in% c(7, 8, 9)] <- NA
ess_2014_filter$imueclt[ess_2014_filter$imueclt %in% c(77, 88, 99)] <- NA
ess_2014_filter$freehms[ess_2014_filter$freehms %in% c(7, 8, 9)] <- NA

ess_2014_filter$freehms <- rescale(ess_2014_filter$freehms, to = c(0, 10), from = c(1,5))

ess_2014_filter <- ess_2014_filter %>%
  mutate(imueclt_rev = case_when( 
    imueclt == 0 ~ 10,
    imueclt == 1 ~ 9,
    imueclt == 2 ~ 8,
    imueclt == 3 ~ 7,
    imueclt == 4 ~ 6,
    imueclt == 5 ~ 5,
    imueclt == 6 ~ 4,
    imueclt == 7 ~ 3,
    imueclt == 8 ~ 2,
    imueclt == 9 ~ 1,
    imueclt == 10 ~ 0,
    TRUE ~ NA
  ))

ess_2014_filter <- ess_2014_filter %>% 
  rowwise() %>%
  mutate(cult = mean(c(imueclt_rev, freehms), na.rm = F)) #prumer otazek merici kulturni dimenzi

#prumerna hodnota pozicnich promennych pro volice kazde strany v datovem souboru
ess_2014_agg <- ess_2014_filter %>%
  group_by(party_choice_country) %>%
  summarise(mean_eco = weighted.mean(gincdif, anweight, na.rm = T), mean_cult = weighted.mean(cult, anweight, na.rm = T))

#spojeni dat ESS s kody z Party Facts
party_facts_2014 <- party_facts %>% filter(essround == 7)
party_facts_2014 <- party_facts_2014 %>% filter(ess_variable %in% c("prtvtbat", "prtvtcbe", "prtvtech", "prtvtdcz", "prtvede2", "prtvtcdk", "prtvteee", "prtvtces", "prtvtcfi", "prtvtcfr", "prtvtbgb", "prtvtehu", "prtvtaie", "prtvtcil", "prtvalt1", "prtvtfnl", "prtvtbno", "prtvtcpl", "prtvtbpt", "prtvtbse", "prtvtesi"))

ess_2014_agg <- merge(ess_2014_agg, party_facts_2014, by = "party_choice_country")

#vyber pozadovanych promennych a pridani roku
ess_2014_agg <- ess_2014_agg %>% select(mean_eco, mean_cult, ess_cntry, partyfacts_id, ess_party, name_short)
ess_2014_agg <- ess_2014_agg %>% mutate(year = 2014)




###ESS 2018###

#mazani dat o volebnich ziscich z dalsich hlasu v ramci jedne zeme a spojovani dat o volebnich ziscich
ess_2018 <- ess_2018 %>% unite("party_choice", c("prtvtcat", "prtvtdbe", "prtvtdbg", "prtvtgch", "prtvtbcy", "prtvtecz", "prtvede2", "prtvtddk", "prtvtgee", "prtvtees", "prtvtdfi", "prtvtdfr", "prtvtcgb", "prtvtahr", "prtvtfhu", "prtvtcie", "prtvtcis", "prtvtcit", "prtvblt1", "prtvtalv", "prtvtme", "prtvtgnl", "prtvtbno", "prtvtdpl", "prtvtcpt", "prtvtrs", "prtvtcse", "prtvtfsi", "prtvtdsk"),
                               na.rm = TRUE, remove = FALSE)

ess_2018$party_choice[ess_2018$party_choice %in% c(66, 77, 88, 99)] <- NA
ess_2018 <- subset(ess_2018,!is.na(party_choice))
unique(ess_2018$party_choice)

#spojeni promennych volby strany a zeme
ess_2018 <- ess_2018 %>% unite("party_choice_country", c("party_choice", "cntry"), na.rm = TRUE, remove = FALSE)

#vyber relevantnich promennych o pozicich volicu a uprava

ess_2018_filter <- ess_2018 %>% select(party_choice_country, gincdif, imueclt, freehms, anweight)

ess_2018_filter$gincdif[ess_2018_filter$gincdif %in% c(7, 8, 9)] <- NA
ess_2018_filter$imueclt[ess_2018_filter$imueclt %in% c(77, 88, 99)] <- NA
ess_2018_filter$freehms[ess_2018_filter$freehms %in% c(7, 8, 9)] <- NA

ess_2018_filter$freehms <- rescale(ess_2018_filter$freehms, to = c(0, 10), from = c(1,5))

ess_2018_filter <- ess_2018_filter %>%
  mutate(imueclt_rev = case_when( 
    imueclt == 0 ~ 10,
    imueclt == 1 ~ 9,
    imueclt == 2 ~ 8,
    imueclt == 3 ~ 7,
    imueclt == 4 ~ 6,
    imueclt == 5 ~ 5,
    imueclt == 6 ~ 4,
    imueclt == 7 ~ 3,
    imueclt == 8 ~ 2,
    imueclt == 9 ~ 1,
    imueclt == 10 ~ 0,
    TRUE ~ NA
  ))

ess_2018_filter <- ess_2018_filter %>% 
  rowwise() %>%
  mutate(cult = mean(c(imueclt_rev, freehms), na.rm = F)) #prumer otazek merici kulturni dimenzi

#prumerna hodnota pozicnich promennych pro volice kazde strany v datovem souboru
ess_2018_agg <- ess_2018_filter %>%
  group_by(party_choice_country) %>%
  summarise(mean_eco = weighted.mean(gincdif, anweight, na.rm = T), mean_cult = weighted.mean(cult, anweight, na.rm = T))

#spojeni dat ESS s kody z Party Facts
party_facts_2018 <- party_facts %>% filter(essround == 9)
party_facts_2018 <- party_facts_2018 %>% filter(ess_variable %in% c("prtvtcat", "prtvtdbe", "prtvtdbg", "prtvtgch", "prtvtbcy", "prtvtecz", "prtvede2", "prtvtddk", "prtvtgee", "prtvtees", "prtvtdfi", "prtvtdfr", "prtvtcgb", "prtvtahr", "prtvtfhu", "prtvtcie", "prtvtcis", "prtvtcit", "prtvblt1", "prtvtalv", "prtvtme", "prtvtgnl", "prtvtbno", "prtvtdpl", "prtvtcpt", "prtvtrs", "prtvtcse", "prtvtfsi", "prtvtdsk"))

ess_2018_agg <- merge(ess_2018_agg, party_facts_2018, by = "party_choice_country")

#vyber pozadovanych promennych a pridani roku
ess_2018_agg <- ess_2018_agg %>% select(mean_eco, mean_cult, ess_cntry, partyfacts_id, ess_party, name_short)
ess_2018_agg <- ess_2018_agg %>% mutate(year = 2019) #rok podle sberu CHES, ktery bude s timto ESS datasetem propoejn



###Spojeni upravenych ESS datasetu###

ess_all <- rbind(ess_2006_agg, ess_2010_agg, ess_2014_agg, ess_2018_agg)
ess_all <- ess_all %>% rename(party_ess = ess_party, party_partyfacts = name_short)

#smazani stran, jejichz Party Facts ID se objevuje vicekrat ve stejnem roce

del_ids <- ess_all %>% 
  group_by(year, partyfacts_id) %>%
  summarise(n = n()) %>%
  mutate(del_id = ifelse(n >= 2, 1, 0)) %>%
  select(partyfacts_id, del_id)
  
ess_all <- merge(ess_all, del_ids, by = c("partyfacts_id", "year"))

ess_all <- ess_all %>%
  subset(del_id == 0)

#export datoveho souboru
write.csv(ess_all, file = "data/ess_all.csv", row.names = FALSE)
