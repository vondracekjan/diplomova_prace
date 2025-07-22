Skripty, které upravují původní data, obsahují URL odkazy, odkud lze tato data stáhnout. Příslušná citace těchto dat v textu, odkaz ke stažení a název, pod kterým byla uložena, je uveden i zde pod názvem každého takového skriptu. Výstupem těchto skriptů jsou upravené datové soubory, které jsou načítány v dalších krocích. Všechny datové soubory byly uloženy do složky "data/".
 
„zisky_graf.R“ – kód pro vizualizaci volebních zisků sociálnědemokratických stran v rámci grafu 1
* Döring, Holger, a Philip Manow. 2024. „ParlGov 2024 Release". Harvard Dataverse, srpen 12. https://doi.org/10.7910/DVN/2VZ5ZC.
  - soubory „view_party.csv“ a „view_election.csv“
  - stáhnout lze zde: https://doi.org/10.7910/DVN/2VZ5ZC

„strany_data_priprava.R“ – kód pro přípravu dat z datových souborů CHES
* Jolly, Seth, Ryan Bakker, Liesbet Hooghe, et al. 2022. „Chapel Hill Expert Survey trend file, 1999–2019". Electoral Studies 75 (únor). https://doi.org/10.1016/j.electstud.2021.102420.
  - soubor „ches_1999-2019.csv“
  - stáhnout lze zde: https://www.chesdata.eu/ches-europe
* Rovny, Jan, Ryan Bakker, Liesbet Hooghe, et al. 2025. „25 Years of Political Party Positions in Europe: The Chapel Hill Expert Survey, 1999-2024". working paper.
  - soubor „ches_2024.csv.csv“
  - stáhnout lze zde: https://www.chesdata.eu/ches-europe
* PartyFacts. 2025b. „CHES". https://partyfacts.herokuapp.com/data/ches/.
  - soubor „party_facts_ches.csv“
  - stáhnout lze zde: https://github.com/hdigital/partyfactsdata/blob/main/import/ches/ches.csv
  
„volici_data_priprava.R“ – kód pro přípravu dat z datových souboru ESS
* European Social Survey European Research Infrastructure. 2018. „ESS Round 3 - 2006. Timing of Life, Personal Wellbeing". Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/NSD-ESS3-2006.
  - soubor „ess_2006.csv“
  - stáhnout lze zde: https://doi.org/10.21338/ess3e03_7
* European Social Survey European Research Infrastructure. 2023a. „ESS Round 5 - 2010. Family Work and Wellbeing, Justice". Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/NSD-ESS5-2010.
  - soubor „ess_2010.csv“
  - stáhnout lze zde: https://doi.org/10.21338/ess5e03_5
* European Social Survey European Research Infrastructure. 2023b. „ESS Round 7 - 2014. Immigration, Social Inequalities in Health". Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/NSD-ESS7-2014.
  - soubor „ess_2014.csv“
  - stáhnout lze zde: https://doi.org/10.21338/ess7e02_3 
* European Social Survey European Research Infrastructure. 2023c. „ESS Round 9 - 2018. Timing of Life, Justice and Fairness". Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/NSD-ESS9-2018.
  - soubor „ess_2018.csv“
  - stáhnout lze zde: https://doi.org/10.21338/ess9e03_2
* PartyFacts. 2025a. „ESS prtv*". https://partyfacts.herokuapp.com/data/essprtv/.
  - soubor „essprt-all.csv“
  - stáhnout lze zde: https://github.com/hdigital/partyfactsdata/blob/main/import/essprtv/essprt-all.csv


„spojeni_dat.R“ – kód pro spojení upravených dat vytvořených předešlými kroky; vytvoření závislých, nezávislých a kontrolních proměnných
* World Bank Group. 2025. „GDP Growth (Annual %)". https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG.
  - soubor „gdp_growth.csv“
  - stáhnout lze zde: https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG
* Casal Bértoa, Fernando. 2025. „Database on WHO GOVERNS in Europe and beyond, PSGo". whogoverns.eu.
  - soubor „party_system_age.xlsx“
  - stáhnout lze zde: https://whogoverns.eu/party-systems/party-institutionalization/
* dále jsou v tomto skriptu načítány datové soubory „ches_all.csv“ (exportován ze skritpu „strany_data_priprava.R“)  a „ess_all.csv“ (exportován ze skriptu „volici_data_priprava.R“)

„analyzy.R“ – kód pro vizualizaci dat a výpočet modelů
* v tomto skriptu je načítán datový soubor „full_data.csv“ (exportován ze skriptu „spojeni_dat.R“)



