library(nlme)
library(tidyverse)
library(lme4)
library(sjPlot)
library(ggeffects)
library(fixest)
library(marginaleffects)
library(performance)
library(modelsummary)
library(ggpubr)

#nacteni, filtr a uprava dat
data <- read.csv("data/full_data.csv")

soc_data <- data %>%
  filter(family_abb == "soc")

soc_data$year <- as.factor(soc_data$year)
unique(soc_data$year)
soc_data$partyfacts_id <- as.factor(soc_data$partyfacts_id)
unique(soc_data$partyfacts_id)
soc_data$govt_edit <- as.factor(soc_data$govt_edit)
unique(soc_data$govt_edit)
soc_data$country_abb <- as.factor(soc_data$country_abb)
unique(soc_data$country_abb)


soc_data <- soc_data %>%
  mutate(eco_cong_abs = abs(eco_cong),
         cult_cong_abs = abs(cult_cong),
         eco_cong_abs_sqrt = sqrt(eco_cong_abs),
         cult_cong_abs_sqrt = sqrt(cult_cong_abs),
         eco_cong_sq = eco_cong^2,
         cult_cong_sq = cult_cong^2,
         eco_parties_change_sq = eco_parties_change^2,
         cult_parties_change_sq = cult_parties_change^2,
         parties_change = eco_parties_change_sq + cult_parties_change_sq,
         parties_change_abs = abs(eco_parties_change) + abs(cult_parties_change))

#pozorovani s hodnotami na zavisle i nezavisle promenne
soc_data %>%
  filter(!is.na(eco_cong) & !is.na(eco_parties_change))  %>%
  count(year)

soc_data %>%
  filter(!is.na(eco_cong) & !is.na(eco_parties_change))  %>%
  group_by(country_abb) %>%
  count(party_ches) %>%
  print(n = 35)

soc_data %>%
  filter(!is.na(eco_cong) & !is.na(eco_parties_change))  %>%
  count(partyfacts_id)


soc_data %>%
  filter(!is.na(cult_cong) & !is.na(cult_parties_change))  %>%
  count(year)

soc_data %>%
  filter(!is.na(cult_cong) & !is.na(cult_parties_change))  %>%
  group_by(country_abb) %>%
  count(party_ches) %>%
  print(n = 35)

soc_data %>%
  filter(!is.na(cult_cong) & !is.na(cult_parties_change))  %>%
  count(partyfacts_id)


####Deskriptivni grafy

#dumbbell grafy

soc_data_dumbbell_eco_max <- slice_max(soc_data, eco_cong_abs, n = 5)
soc_data_dumbbell_eco_min <- slice_min(soc_data, eco_cong_abs, n = 5)
soc_data_dumbbell_eco <- rbind(soc_data_dumbbell_eco_max, soc_data_dumbbell_eco_min)

soc_data_dumbbell_eco <- soc_data_dumbbell_eco %>%
  unite(party_year, party_ches, year, country_abb, sep = "_") %>%
  select(party_year, eco_voters_rescale, eco_parties_rescale, eco_cong_abs) %>%
  pivot_longer(cols = c(eco_voters_rescale, eco_parties_rescale))

dumb_eco <- ggplot(soc_data_dumbbell_eco, aes(x = value, y = reorder(party_year, eco_cong_abs))) +
  geom_line() +
  geom_point(aes(color = name), size = 3) +
  scale_x_continuous(n.breaks = 11, limits = c(0,10)) +
  scale_color_discrete(name = "", labels = c("Strana", "Podporovatelé")) +
  xlab("Pozice na ekonomické dimenzi") +
  ylab("Strana, rok a země") +
  scale_y_discrete(label = c("SAP 2010 (SV)", "SAP 2006 (SV)", "LAB 2006 (IRL)", "SPD 2010 (GE)", "SPA 2010 (BE)", "PS 2014 (SLE)", "DK 2019 (HUN)", "SLD 2006 (POL)", "EDEK 2019 (CYP)", "MSZP 2006 (HUN)")) +
  theme_pubr(legend = "right")


soc_data_dumbbell_cult_max <- slice_max(soc_data, cult_cong_abs, n = 5)
soc_data_dumbbell_cult_min <- slice_min(soc_data, cult_cong_abs, n = 5)
soc_data_dumbbell_cult <- rbind(soc_data_dumbbell_cult_max, soc_data_dumbbell_cult_min)

soc_data_dumbbell_cult <- soc_data_dumbbell_cult %>%
  unite(party_year, party_ches, year, country_abb, sep = "_") %>%
  select(party_year, cult_voters_rescale, cult_parties_rescale, cult_cong_abs) %>%
  pivot_longer(cols = c(cult_voters_rescale, cult_parties_rescale))

dumb_cult <- ggplot(soc_data_dumbbell_cult, aes(x = value, y = reorder(party_year, cult_cong_abs))) +
  geom_line() +
  geom_point(aes(color = name), size = 3) +
  scale_x_continuous(n.breaks = 11, limits = c(0,10)) +
  scale_color_discrete(name = "", labels = c("Strana", "Podporovatelé")) +
  xlab("Pozice na kulturní dimenzi") +
  ylab("Strana, rok a země") +
  scale_y_discrete(label = c("LAB 2006 (IRL)", "LAB 2019 (UK)", "PS 2014 (BE)", "SP/SPA 2019 (BE)", "SD 2014 (SLE)", "SD 2014 (DK)", "PASOK 2010 (GR)", "SDSS 2019 (CRO)", "BSP 2019 (BUL)", "DK 2019 (HUN)")) +
  theme_pubr(legend = "right")


#histogramy

dat_desc_iv <- soc_data %>%
  select(eco_cong, cult_cong)
dat_desc_iv <- pivot_longer(dat_desc_iv, 
                            cols = c(eco_cong, cult_cong),
                            names_to = "variable_type",
                            values_to = "value")
hist_cong <- ggplot(dat_desc_iv, aes(x = value, fill = variable_type)) +
  geom_histogram(alpha = 0.3, binwidth = 0.5, na.rm = T, position = "identity") +
  scale_fill_discrete(name = "", labels = c("Kulturní dimenze", "Ekonomická dimenze")) +
  xlab("Inkongruence") +
  ylab("Počet") +
  theme_pubr(legend = "right")

hist_change_all_sq <- soc_data %>%
  ggplot(aes(parties_change)) +
  geom_histogram(na.rm = TRUE, binwidth = 0.25) +
  xlab(bquote("Posun strany na ekonomické dimenzi"^2*" + Posun strany na kulturní dimenzi"^2)) +
  ylab("Počet") +
  theme_pubr()

hist_change_eco <- soc_data %>%
  ggplot(aes(eco_parties_change)) +
  geom_histogram(na.rm = TRUE, binwidth = 0.25) +
  xlab(bquote("Posun strany na ekonomické dimenzi")) +
  ylab("Počet") +
  theme_pubr()

hist_change_cult <- soc_data %>%
  ggplot(aes(cult_parties_change)) +
  geom_histogram(na.rm = TRUE, binwidth = 0.25) +
  xlab(bquote("Posun strany na kulturní dimenzi")) +
  ylab("") +
  theme_pubr()

dat_desc_parties <- soc_data %>%
  filter(year != 2002) %>%
  select(eco_parties_rescale, cult_parties_rescale)
dat_desc_parties <- pivot_longer(dat_desc_parties, 
                            cols = c(eco_parties_rescale, cult_parties_rescale),
                            names_to = "variable_type",
                            values_to = "value")
hist_desc_parties <- ggplot(dat_desc_parties, aes(x = value, fill = variable_type)) +
  geom_histogram(alpha = 0.2, binwidth = 0.5, na.rm = T, position = "identity") +
  scale_fill_discrete(name = "", labels = c("Kulturní dimenze", "Ekonomická dimenze")) +
  xlab("Pozice strany") +
  ylab("Počet") +
  scale_x_continuous(n.breaks = 11, limits = c(0,10)) +
  theme_pubr(legend = "right")

dat_desc_voters <- soc_data %>%
  select(eco_voters_rescale, cult_voters_rescale)
dat_desc_voters <- pivot_longer(dat_desc_voters, 
                                cols = c(eco_voters_rescale, cult_voters_rescale),
                                names_to = "variable_type",
                                values_to = "value")
hist_desc_voters <- ggplot(dat_desc_voters, aes(x = value, fill = variable_type)) +
  geom_histogram(alpha = 0.2, binwidth = 0.5, na.rm = T, position = "identity") +
  scale_fill_discrete(name = "", labels = c("Kulturní dimenze", "Ekonomická dimenze")) +
  xlab("Průměrná pozice podporovatelů") +
  ylab("Počet") +
  scale_x_continuous(n.breaks = 11, limits = c(0,10)) +
  theme_pubr(legend = "right")
  
####Modely - nasledovani politickych pozic

eco_cong_alt_t2 <- feols(eco_parties_t2~eco_voters_rescale*eco_cong_sq + govt_edit + seat + gdp_growth|
                           year+partyfacts_id,
                         cluster = c("year", "partyfacts_id"),
                         data=soc_data)

alt_eco <- plot_predictions(eco_cong_alt_t2, condition=list("eco_voters_rescale","eco_cong_sq"="minmax"), vcov=T, rug=T, conf_level = 0.95) + 
  xlab("Průměrná pozice podporovatelů na ekonomické dimenzi v čase t") + 
  ylab("Pozice strany na ekonomické dimenzi v čase t + 1") +  
  scale_fill_discrete(name = bquote("Inkongruence na ekonomické dimenzi"^2), labels = c("Minimální", "Maximální")) +  
  scale_color_discrete(name = bquote("Inkongruence na ekonomické dimenzi"^2), labels = c("Minimální", "Maximální")) +
  theme_pubr(legend = "bottom")

cult_cong_alt_t2 <- feols(cult_parties_t2~cult_voters_rescale*cult_cong_sq + govt_edit + seat + gdp_growth|
                            year+partyfacts_id, 
                          cluster = c("year", "partyfacts_id"),
                          data=soc_data)

alt_cult <- plot_predictions(cult_cong_alt_t2, condition=list("cult_voters_rescale","cult_cong_sq"="minmax"), vcov= T, rug=T, conf_level = 0.95) + 
  xlab("Průměrná pozice podporovatelů na kulturní dimenzi v čase t") + 
  ylab("Pozice strany na kulturní dimenzi v čase t + 1") +  
  scale_fill_discrete(name = bquote("Inkongruence na kutlurní dimenzi"^2), labels = c("Minimální", "Maximální")) +  
  scale_color_discrete(name = bquote("Inkongruence na kutlurní dimenzi"^2), labels = c("Minimální", "Maximální")) +
  theme_pubr(legend = "bottom")


subset(soc_data, year == "2019" & partyfacts_id == "469")
predict(cult_cong_alt_t2, newdata = subset(soc_data, year == "2019" & partyfacts_id == "469"))


####Modely - zmeny v politickych pozicich

#Poissonova regrese

pois_eco <- fepois(parties_change ~ eco_cong_sq + govt_edit + seat + gdp_growth|year+partyfacts_id, data = soc_data, cluster = c("year", "partyfacts_id"))
pois_cult <- fepois(parties_change ~ cult_cong_sq + govt_edit + seat + gdp_growth|year+partyfacts_id, data = soc_data, cluster = c("year", "partyfacts_id"))
pois_both <- fepois(parties_change ~ eco_cong_sq + cult_cong_sq + govt_edit + seat + gdp_growth|year+partyfacts_id, data = soc_data, cluster = c("year", "partyfacts_id"))

pois_time_eco_model <- fepois(parties_change ~  eco_cong_sq:year+cult_cong_sq + govt_edit + seat + gdp_growth|year+partyfacts_id, data = soc_data, cluster = c("year", "partyfacts_id"))
pois_time_cult_model <- fepois(parties_change ~ eco_cong_sq+cult_cong_sq:year + govt_edit + seat + gdp_growth|year+partyfacts_id, data = soc_data, cluster = c("year", "partyfacts_id"))

pois_time_eco <- plot_slopes(pois_time_eco_model, variables = "eco_cong_sq", by="year", conf_level = 0.95, slope = "eydx") + 
  xlab("Rok") +
  ylab(bquote("Efekt inkongruence na ekonomické dimenzi"^2)) +
  scale_y_continuous(breaks = seq(0, 0.8, by = 0.2)) +
  theme_pubr()
pois_time_cult <- plot_slopes(pois_time_cult_model, variables = "cult_cong_sq", by="year", conf_level = 0.95, slope = "eydx") + 
  xlab("Rok") +
  ylab(bquote("Efekt inkongruence na kulturní dimenzi"^2)) +
  scale_y_continuous(breaks = seq(-0.4, 0.8, by = 0.2)) +
  theme_pubr()

slopes(pois_time_eco_model, variables =  "eco_cong_sq", by = "year", conf_level = 0.95, slope = "eydx")
slopes(pois_time_cult_model, variables =  "cult_cong_sq", by = "year", conf_level = 0.95, slope = "eydx")

pois_age_eco_model <- fepois(parties_change ~ eco_cong_sq*party_system_age+cult_cong_sq + govt_edit + seat + gdp_growth|year+partyfacts_id, data = soc_data, cluster = c("year", "partyfacts_id"))
pois_age_cult_model <- fepois(parties_change ~ eco_cong_sq+cult_cong_sq*party_system_age + govt_edit + seat + gdp_growth|year+partyfacts_id, data = soc_data, cluster = c("year", "partyfacts_id"))


#linearni regrese

eco_fixed <- feols(eco_parties_change ~ eco_cong + govt_edit + seat + gdp_growth|year + partyfacts_id, data = soc_data, cluster = c("year", "partyfacts_id"))
eco_fixed_both <- feols(eco_parties_change ~ eco_cong + cult_cong + govt_edit + seat + gdp_growth|year + partyfacts_id, data = soc_data, cluster = c("year", "partyfacts_id"))

subset(soc_data, year == "2006" & partyfacts_id == "1408")
predict(eco_fixed, newdata = subset(soc_data, year == "2006" & partyfacts_id == "1408"))

cult_fixed <- feols(cult_parties_change ~ cult_cong + govt_edit + seat + gdp_growth|year + partyfacts_id, data = soc_data, cluster = c("year", "partyfacts_id"))
cult_fixed_both <- feols(cult_parties_change ~ cult_cong + eco_cong + govt_edit + seat + gdp_growth|year + partyfacts_id, data = soc_data, cluster = c("year", "partyfacts_id"))

subset(soc_data, year == "2019" & partyfacts_id == "469")
predict(cult_fixed, newdata = subset(soc_data, year == "2019" & partyfacts_id == "469"))

eco_fixed_time <- feols(eco_parties_change ~ eco_cong:year + govt_edit + seat + gdp_growth|year + partyfacts_id, data = soc_data, cluster = c("year", "partyfacts_id"))
cult_fixed_time <- feols(cult_parties_change ~ cult_cong:year + govt_edit + seat + gdp_growth|year + partyfacts_id, data = soc_data, cluster = c("year", "partyfacts_id"))

ols_time_eco <- plot_slopes(eco_fixed_time, variables = "eco_cong", by="year", conf_level = 0.95) + 
  xlab("Rok") +
  ylab("Efekt inkongruence na ekonomické dimenzi") +
  scale_y_continuous(breaks = seq(-1.4, -0.2, by = 0.2)) +
  theme_pubr()
slopes(eco_fixed_time, variables = "eco_cong", by="year", conf_level = 0.95)

ols_time_cult <- plot_slopes(cult_fixed_time, variables = "cult_cong", condition="year", conf_level = 0.95)  + 
  xlab("Rok") +
  ylab("Efekt inkongruence na kulturní dimenzi") +
  scale_y_continuous(breaks = seq(-0.8, 0, by = 0.2)) +
  theme_pubr()
slopes(cult_fixed_time, variables = "cult_cong", by="year", conf_level = 0.95)


eco_fixed_age<- feols(eco_parties_change ~ eco_cong*party_system_age + govt_edit + seat + gdp_growth|year + partyfacts_id, data = soc_data, cluster = c("year", "partyfacts_id"))
cult_fixed_age <- feols(cult_parties_change ~ cult_cong*party_system_age + govt_edit + seat + gdp_growth|year + partyfacts_id, data = soc_data, cluster = c("year", "partyfacts_id"))


#####Export modelu a grafu

##Deskripce

ggsave("export/deskripce/dumb_eco.jpg", dumb_eco, units = "cm", height = 15, width = 20)
ggsave("export/deskripce/dumb_cult.jpg", dumb_cult, units = "cm", height = 15, width = 20)

ggsave("export/deskripce/hist_cong.jpg", hist_cong, units = "cm", height = 15, width = 20)

ggsave("export/deskripce/hist_desc_parties.jpg", hist_desc_parties, units = "cm", height = 15, width = 20)

ggsave("export/deskripce/hist_desc_voters.jpg", hist_desc_voters, units = "cm", height = 15, width = 20)

ggsave("export/deskripce/hist_change_all_sq.jpg", hist_change_all_sq, units = "cm", height = 15, width = 20)

hist_change_all_orig <- ggarrange(hist_change_eco, hist_change_cult)
ggsave("export/deskripce/hist_change_all_orig.jpg", hist_change_all_orig, units = "cm", height = 15, width = 25)


####Modely

var_names <- c("eco_cong_sq" = "Inkong. na ekonomické dimenzi^2",
               "cult_cong_sq" = "Inkong. na kulturní dimenzi^2",
               "eco_cong" = "Inkong. na ekonomické dimenzi",
               "cult_cong" = "Inkong. na kulturní dimenzi",
               "eco_cong_sq:party_system_age" = "Inkong. na ekonomické dimenzi^2 * Stáří stranického systému",
               "cult_cong_sq:party_system_age" = "Inkong. na kulturní dimenzi^2 * Stáří stranického systému",
               "eco_cong:party_system_age" = "Inkong. na ekonomické dimenzi * Stáří stranického systému",
               "cult_cong:party_system_age" = "Inkong. na kulturní dimenzi * Stáří stranického systému",
               "govt_edit1" = "Vládní strana",
               "seat" = "Podíl držených mandátů",
               "seat_lag" = "Podíl držených mandátů v čase t - 1",
               "gdp_growth" = "Růst HDP",
               "party_system_age" = "Stáří stranického systému")


gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "r.squared", "R<sup>2</sup>", 2,
  "rmse",        "RMSE",        2,
  "vcov.type", "Standardní chyba", 0,
  "FE: year", "FE Rok", 0,
  "FE: partyfacts_id", "FE Strana", 0)

##Modely - nasledovani politickych pozic

ggsave("export/alt/alt_eco.jpg", alt_eco, units = "cm", height = 15, width = 20)
ggsave("export/alt/alt_cult.jpg", alt_cult, units = "cm", height = 15, width = 20)

##Modely - zmeny v politickych pozicich

#poisson regrese
pois_models_general <- list(pois_eco, pois_cult, pois_both, pois_age_eco_model, pois_age_cult_model)

modelsummary(pois_models_general,
             statistic = NULL,
             stars = T,
             coef_map = var_names,
             include_reference = F,
             gof_map = gm,
             output = "export/pois/pois_all.html")

ggsave("export/pois/pois_time_eco.jpg", pois_time_eco, units = "cm", height = 15, width = 20)
ggsave("export/pois/pois_time_cult.jpg", pois_time_cult, units = "cm", height = 15, width = 20)

#linearni regrese
ols_models<- list(eco_fixed, eco_fixed_both, eco_fixed_age, cult_fixed, cult_fixed_both , cult_fixed_age)

modelsummary(ols_models,
             statistic = NULL,
             stars = T,
             coef_map = var_names,
             include_reference = F,
             gof_map = gm,
             output = "export/ols/ols_all.html")


ggsave("export/ols/ols_time_eco.jpg", ols_time_eco, units = "cm", height = 15, width = 20)
ggsave("export/ols/ols_time_cult.jpg", ols_time_cult, units = "cm", height = 15, width = 20)