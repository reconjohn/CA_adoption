library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(pander)
library(quantmod)
library(stringr)
library(SpatialEpi)
library(maps)
library(shapefiles)
# library(maptools)
library(RColorBrewer)
library(spdep)
# library(INLA)
# library(rgdal)
library(pander)
library(DCluster)
library(geoR)
library(sp)
library(splancs)
# library(GWmodel)
library(psych)
library (cluster)
library(reshape)
library(reshape2)
library(som)
library(GPArotation)
library(corrplot)
library(GGally)
library(faraway)
library(sjPlot)
library(lattice)
library(png)
library(grid)
library(gridExtra)
# library(rgeos)
library(leaflet)
library(tigris)
library(broom)
library("broom.mixed")
library(rmapshaper)
library(ggpubr)
library(lme4)
library(lmerTest)
library(stargazer)
library(arm)
library(pscl)
library(spatialreg)
library(ggstance)
library(viridis)
library(ggpubr)
library(ggrepel)
library(readxl)
library(fastDummies)
library(caret)
library(xgboost)
library(tidytext)
library(ggnewscale)
library(ggpattern)
library(scico)
library(shadowtext)
library(ggtext)
library(marginaleffects)
library(ggnewscale)
library(gridGraphics)
library(patchwork)
library(gtsummary)
options(tigris_use_cache = TRUE, tigris_class = "sf")

# uploading library 
lapply(c("spatstat","colorRamps","tmap","ggmap","mapview","geoR","knitr","kableExtra","data.table","gdata","tigris","sf","scales","tidycensus","plotly", "tidyverse"), require, character.only = TRUE)
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
sf::sf_use_s2(FALSE)

source("./syntax/Function.R")

ipt <- c("PV","EV","HP","IC","PS")
load("./data/dac_sf.RData") # CA_t, cz, dac_sf, sc_map (DAC, climate zone spatial data)
dac <- read_csv(file = "../DAC/data/DAC_CA_censustract.csv") %>%
  dplyr::select(GEOID, sample)
# names(mrp)
# crosswalk <- read_csv("./survey.csv")

## PSPS test from Qi
# CA_2010 <- get_acs("tract", state="CA", year = 2010, geometry = TRUE,
#                    variables= "B25026_001") # population
# save(CA_2010, file = "./data/raw/2010tr.Rdata")
# load("./data/raw/2010tr.Rdata")

# psps <- read_csv("./data/raw/PSPS_winter_updated.csv") %>%
#   rename(winter = PSPS_number) %>%
#   dplyr::select(GEOID,winter) %>%
#   left_join(read_csv("./data/raw/PSPS_fall_updated.csv") %>%
#               rename(fall = PSPS_number) %>%
#               dplyr::select(GEOID,fall), by = "GEOID") %>%
#   left_join(read_csv("./data/raw/PSPS_spring_updated.csv") %>%
#               rename(spring = PSPS_number) %>%
#               dplyr::select(GEOID,spring), by = "GEOID") %>%
#   left_join(read_csv("./data/raw/PSPS_summer_updated.csv") %>%
#               rename(summer = PSPS_number) %>%
#               dplyr::select(GEOID,summer), by = "GEOID") %>%
#   mutate(GEOID = paste0("0",GEOID))
# 
# psps[is.na(psps)] <- 0

# CA_2010 %>%
#   left_join(psps, by = "GEOID") %>%
#   gather(key, value, winter:summer) %>%
#   ggplot() +
#   geom_sf(fill = "white", color = "gray0") + # US border
#   geom_sf(aes(fill = value), color = NA, size = 0.3) +
#   facet_wrap(~key) +
#   
#   theme_minimal() +
#   # scale_fill_distiller(palette = "RdBu", direction = -1) +
#   scale_fill_viridis_c(option = "magma") +
#   
#   labs(title = "PSPS Frequency", fill = "") +
#   theme(legend.position = "right",
#         # legend.text=element_text(size=6),
#         # legend.key.size = unit(0.3, 'cm'),
#         strip.text = element_text(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         plot.title=element_text(family="Franklin Gothic Demi", size=15, hjust = 0))


# ## PSPS data (average by tract)
# psps <- read_csv("./data/raw/psps.csv") %>% 
#   dplyr::rename(dur = "Avg Dur St",
#                 freq = "out_freq_s",
#                 GEOID = "Fips") %>% 
#   mutate(GEOID = paste0("0",GEOID)) %>% 
#   mutate(
#     freq = na_if(freq, "Unknown"),
#     dur = na_if(dur, "Unknown"),
#     freq = as.numeric(freq),
#     dur = as.numeric(dur)
#   ) %>%
#   # mutate(
#   #   freq = if_else(is.na(freq), mean(freq, na.rm = TRUE), freq),
#   #   dur = if_else(is.na(dur), mean(dur, na.rm = TRUE), dur)
#   # ) %>%
#   dplyr::select(GEOID, freq, dur) %>% 
#   left_join(crosswalk %>% 
#               st_drop_geometry() %>% 
#               dplyr::select(GEOID,BZone), by = "GEOID") %>% 
#   mutate(puma = substr(GEOID, 0,7)) %>% 
#   group_by(puma) %>% 
#   mutate(freq = sum(freq, na.rm = T),
#             dur = mean(dur, na.rm = T)) %>% 
#   ungroup() %>% 
#   dplyr::select(-puma,-BZone)  # count/capita
# 
# 
# ## charging density data
# charge <- read_csv("./data/raw/charging_density_all_ca_tracts.csv") %>% 
#   left_join(CA_t %>% 
#               st_drop_geometry() %>% 
#               dplyr::select(estimate, GEOID), by = "GEOID") %>% 
#   left_join(crosswalk %>% 
#               st_drop_geometry() %>% 
#               dplyr::select(GEOID,BZone), by = "GEOID") %>% 
#   mutate(puma = substr(GEOID, 0,7)) %>% 
#   group_by(puma) %>% 
#   mutate(l1l2 = sum(`L1+L2`),
#             dc = sum(DCFC),
#             estimate = sum(estimate),
#             l1l2 = l1l2/estimate,
#             dc = dc/estimate) %>% 
#   ungroup() %>% 
#   dplyr::select(GEOID,l1l2,dc)  # count/capita

tb <- read.delim("./data/raw/survey_geoinfo.txt",
                 sep = "|", header = T, stringsAsFactors = FALSE) %>% 
  dplyr::select(tractid_2020,ExternalReference) %>% 
  dplyr::rename(tractid = tractid_2020) %>% 
  mutate(tractid = paste0("0",tractid))


mrp <- read_csv("./data/raw/mrp_scenariovars_tract_v5.csv") %>% 
  mutate(GEOID = str_sub(geoid_tract2020, 10)) %>%
  dplyr::select(-geoid_tract2020) 

# mrp %>% 
#   mutate(greaterps0 = ifelse(future_PS_0 > future_PS_low, 1, 0),
#          greaterpslow = ifelse(future_PS_low > future_PS_high, 1, 0),
#          greateric0 = ifelse(future_IC_0 > future_IC_low, 1, 0),
#          greatericlow = ifelse(future_IC_low > future_IC_high, 1, 0),
#          greaterhp0 = ifelse(future_HP_0 > future_HP_low, 1, 0),
#          greaterhplow = ifelse(future_HP_low > future_HP_high, 1, 0)) %>% View

mrp <- mrp %>% 
  mutate(future_PS_low = ifelse(future_PS_low > future_PS_high, future_PS_high, future_PS_low),
         future_PS_0 = ifelse(future_PS_0 > future_PS_low, future_PS_low, future_PS_0),
         
         future_EV_low = ifelse(future_EV_low > future_EV_high, future_EV_high, future_EV_low),
         future_EV_0 = ifelse(future_EV_0 > future_EV_low, future_EV_low, future_EV_0),
         
         future_HP_low = ifelse(future_HP_low > future_HP_high, future_HP_high, future_HP_low),
         future_HP_0 = ifelse(future_HP_0 > future_HP_low, future_HP_low, future_HP_0),
         
         future_IC_low = ifelse(future_IC_low > future_IC_high, future_IC_high, future_IC_low),
         future_IC_0 = ifelse(future_IC_0 > future_IC_low, future_IC_low, future_IC_0)) %>%
  left_join(read_csv("./data/raw/mrptractresults_currentadoption.csv") %>%
              mutate(GEOID = str_sub(ca.tract, 10)) %>%
              dplyr::rename(HP = current_HP,
                            PS = current_PS,
                            IC = current_IC,
                            EV = current_EV) %>%
              
              dplyr::select(-ca.tract), by = "GEOID")

# data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1) %>% 
#   dplyr::select(-c("cost_combo_winter_final","cost_combo_summer_final"),
#                 -c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv","solstor_wtp_dv"),
#                 -c("PS_int","EV_int","HP_int","IC_int"))


dat <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1) %>% 
  dplyr::select(-c("cost_combo_winter_final","cost_combo_summer_final"))


### modify peer effect variables to 4 categories
data_cl <- function(data, further = NULL){
  dat <- data %>% 
    # demo
    mutate(income = case_when(income == "Under $20,000" ~ 15000,
                              income == "$20,000 - $29,999" ~ 25000,
                              income == "$30,000 - $39,999" ~ 35000,
                              income == "$40,000 - $49,999" ~ 45000,
                              income == "$50,000 - $59,999" ~ 55000,
                              income == "$60,000 - $69,999" ~ 65000,
                              income == "$70,000 - $79,999" ~ 75000,
                              income == "$80,000 - $89,999" ~ 85000,
                              income == "$90,000 - $99,999" ~ 95000,
                              income == "$100,000 - $124,999" ~ 112500,
                              income == "$125,000 - $149,999" ~ 137500,
                              income == "$150,000 - $174,999" ~ 162500,
                              income == "$175,000 - $199,999" ~ 187500,
                              income == "More than $200,000" ~ 225000),
           # income = factor(income, levels = dat$income %>% unique() %>% .[c(7,14,15,11,8,6,1,5,10,9,4,3,12,2,13)]) %>% 
           #   as.numeric(),
           
           # education = factor(education, levels = data$education %>% unique() %>% .[c(8,6,3,1,5,7,2,4)]) %>% 
           #   as.numeric(),
           
           education = case_when(education == "Less than high school" ~ "less than HS",
                                 education == "Some high school, no diploma" ~ "less than HS",
                                 education == "High school diploma (or GED)" ~ "HS",
                                 education == "Associate's degree (2 yr)" ~ "some college",
                                 education == "Some college, but no degree" ~ "some college",
                                 education == "Trade school certificate (or equivalent)" ~ "some college",
                                 education == "Bachelor's degree (4 yr)" ~ "college",
                                 education == "Postgraduate or professional degree (e.g. MA, MD, MBA, PhD)" ~ "graduate"),
           
           born_us = ifelse(born_us == "Yes", 1, 0), 
           race = case_when(str_detect(race, "Hispanic") ~ "Hispanic",
                            race == "White" ~ "White",
                            race == "Black or African-American" ~ "Black",
                            race == "Asian" ~ "Asian",
                            race == "Other" ~ "Non-hispanic other",
                            race %in% c("American Indian or Alaska Native","Native Hawaiian or Pacific Islander") ~ "AIANNHPI",
                            # race %in% c("American Indian or Alaska Native","Native Hawaiian or Pacific Islander") ~ "Natives",
                            is.na(race) ~ NA_character_,
                            TRUE ~ "Multirace"),
           ideology = factor(ideology, levels = data$ideology %>% unique() %>% .[c(8,2,5,1,6,3,7)]) %>% 
             as.numeric(),
           employment = ifelse(str_detect(employment, "full"), 1,0)  %>% as.numeric(),
           # employment = ifelse(str_detect(employment, "full"), "Workfull",
           #                     ifelse(str_detect(employment, "part"), "Workpart", 
           #                            ifelse(employment == "Homemaker", "Homemaker",
           #                                   ifelse(employment == "Retired", "Retired", 
           #                                          ifelse(str_detect(employment, "Unemployed"), "Unemployed","Others"))))),
           
           # employment = ifelse(str_detect(employment, "full|part") & str_detect(workfromhome, "NOT"), "Work full",
           #                     ifelse(str_detect(employment, "full|part") & str_detect(workfromhome, "Always"), "Work home",
           #                            ifelse(str_detect(employment, "full|part"), "Work occasion",
           # 
           #                            ifelse(employment == "Homemaker", "Homemaker",
           #                                   ifelse(employment == "Retired", "Retired",
           #                                          ifelse(str_detect(employment, "Unemployed"), "Unemployed","Others")))))),
           # data$employment %>% unique()
           # data$born_us %>% table()
           
           # housing
           household_numpeople = as.numeric(household_numpeople),
           home_type = ifelse(str_detect(home_type, "Single family"), "SF",
                              ifelse(str_detect(home_type, "Apartment"), "MF", "Others")),
           # home_type = factor(home_type, levels = data$home_type %>% unique() %>% .[c(5,6,7,3,4,2,1)]) %>% 
           #   as.numeric(),
           home_area = case_when(home_area == "Less than 500 square feet" ~ 250,
                                 home_area == "500 - 999 square feet" ~ 750,
                                 home_area == "1,000 - 1,499 square feet" ~ 1250,
                                 home_area == "1,500 - 1,999 square feet" ~ 1750,
                                 home_area == "2,000 - 2,499 square feet" ~ 2250,
                                 home_area == "2,500 - 2,999 square feet" ~ 2750,
                                 home_area == "3,000 - 3,499 square feet" ~ 3250,
                                 home_area == "3,500 - 3,999 square feet" ~ 3750,
                                 home_area == "4,000 - 4,499 square feet" ~ 4250,
                                 home_area == "4,500 - 4,999 square feet" ~ 4750,
                                 home_area == "5,000 - 5,499 square feet" ~ 5250,
                                 home_area == "5,500 square feet or larger" ~ 6000),
           
           # home_area = factor(home_area, levels = data$home_area %>% unique() %>% .[c(5,8,4,2,3,1,7,10,11,9,12,6)]) %>% 
           #   as.numeric(),
           
           home_age = case_when(home_age == "Built before 1950"~ 1940,
                                home_age == "1950 - 1959"~ 1955,
                                home_age == "1960 - 1969"~ 1965,
                                home_age == "1970 - 1979"~ 1975,
                                home_age == "1980 - 1989"~ 1985,
                                home_age == "1990 - 1999"~ 1995,
                                home_age == "2000 - 2009"~ 2005,
                                home_age == "2010 - 2019"~ 2015, 
                                home_age == "Less than 5 years ago (built since 2020)"~ 2022),
           
           home_age = case_when(home_age < 1980 ~ "Older",
                                home_age < 2019 ~ "BrandNew",
                                TRUE ~ "Newer"),
           home_age = factor(home_age, levels = c("Older","BrandNew","Newer")),
           # home_age = factor(home_age, levels = data$home_age %>% unique() %>% .[c(7,6,1,2,5,3,4,9,10)]) %>% 
           #   as.numeric(),
           
           
           # PV
           solar_install = coalesce(solar_install_owner,solar_install_renter),
           solar_install = ifelse(str_detect(solar_install,"already"), "Already installed", "Installed after"),
           storage_own = ifelse(storage_own == "Yes", 1, 0),
           solar_date_owner = case_when(solar_date_owner == "2022 - 2023"~ "2022",
                                        solar_date_owner == "2018 - 2019"~ "2018",
                                        solar_date_owner == "2010 - 2011"~ "2010",
                                        solar_date_owner == "2020 - 2021"~ "2020",
                                        solar_date_owner == "2014 - 2015"~ "2014",
                                        solar_date_owner == "Before 2010"~ "2008",
                                        solar_date_owner == "2024 - 2025"~ "2024",
                                        solar_date_owner == "2016 - 2017"~ "2016",
                                        solar_date_owner == "2012 - 2013"~ "2012",
                                        solar_date_owner == "I don't know"~ "don't know"),
           
           # EV
           vehicle_1_miles = case_when(vehicle_1_miles == "Less than 2,500 miles per year"~ 1250,
                                       vehicle_1_miles == "2,500 - 4,999 miles per year"~ 3750,
                                       vehicle_1_miles == "5,000 - 7,499 miles per year"~ 6250,
                                       vehicle_1_miles == "7,500 - 9,999 miles per year"~ 8750,
                                       vehicle_1_miles == "10,000 - 12,499 miles per year"~ 11250,
                                       vehicle_1_miles == "12,500 - 14,999 miles per year"~ 13750,
                                       vehicle_1_miles == "15,000 - 17,499 miles per year"~ 16250,
                                       vehicle_1_miles == "17,500 - 20,000 miles per year"~ 18750,
                                       vehicle_1_miles == "More than 20,000 miles per year"~ 25000),
           # vehicle_1_miles = factor(vehicle_1_miles, levels = data$vehicle_1_miles %>% unique() %>% .[c(6,2,1,3,7,5,8,4,9)]) %>% 
           #   as.numeric(),
           
           charging_access = ifelse(vehicle_charging_own == "Yes", "own",
                                    ifelse(vehicle_charging_own == "No" & str_detect(parkingspot, "Yes") &
                                             str_detect(charging_install, "Yes"), "potential", "none")),
           
           charging_work = ifelse(charging_work == "Yes", 1, 0),
           # charging_work = factor(charging_work, levels = data$charging_work %>% unique() %>% .[c(1,2,4,3)]) %>% 
           #   as.numeric(),
           
           charging_5mile_r = ifelse(charging_5mile_non_1 == "Yes" | charging_5mile_own_1 == "Yes", 1, 0),
           charging_5mile_r = ifelse(is.na(charging_5mile_r), 0, charging_5mile_r),
           charging_5mile_f = ifelse(charging_5mile_non_2 == "Yes" | charging_5mile_own_2 == "Yes", 1, 0),
           charging_5mile_f = ifelse(is.na(charging_5mile_f), 0, charging_5mile_f),
           
           rangeanxiety = case_when(rangeanxiety == "More than 400 miles (exceeding the typical gas vehicle range)"~ 450,
                                    rangeanxiety == "301 - 400 miles (matching the typical mid-size or sedan gas vehicle range)"~ 350,
                                    rangeanxiety == "I can't decide until I know more about the location of chargers"~ NA_real_,
                                    rangeanxiety == "200 - 300 miles (comparable to a tank of gas for compact gas vehicles, with weekly charging for most users)"~ 250,
                                    rangeanxiety == "Less than 200 miles (sufficient for daily commuting, occasional charging needed)"~ 150,
                                    TRUE ~ NA_real_),        
           # rangeanxiety = ifelse(rangeanxiety == 300 & vehicle_1_class == "Compact", 150,
           #                       ifelse(rangeanxiety == 300 & vehicle_1_class == "Sedan", 250,
           #                              ifelse(rangeanxiety == 300 & vehicle_1_class == "Minivan", 350,
           #                                     ifelse(rangeanxiety == 300 & vehicle_1_class == "Truck", 450,rangeanxiety)))),
           
           # rangeanxiety = factor(rangeanxiety, levels = data$rangeanxiety %>% unique() %>% .[c(5,2,4,3,1)]) %>% 
           #   as.numeric(),
           vehicle_next_used = ifelse(vehicle_next_used == "New", 1, 0),
           
           
           vehicle_next_when = case_when(vehicle_next_when == "Within the next year"~ 1,
                                         vehicle_next_when == "In one or two years"~ 1,
                                         vehicle_next_when == "In three to five years"~ 1,
                                         vehicle_next_when == "More than five years from now"~ 0,
                                         vehicle_next_when == "I don’t plan to ever purchase/lease a vehicle"~ 0,
                                         TRUE ~ NA_real_),
           
           # vehicle_next_when = factor(vehicle_next_when, levels = data$vehicle_next_when %>% unique() %>% .[c(1,3,2,4,5)]) %>% 
           #   as.numeric(),
           
           vehicle_next_fuel = ifelse(str_detect(vehicle_next_fuel, "All-electric vehicle"), 1, 0),
           
           
           # HP
           hotwater_nextelec = case_when(hotwater_nextelec == "I don't know"~ 0,
                                         hotwater_nextelec == "No, definitely"~ 0,
                                         hotwater_nextelec == "Probably no"~ 0,
                                         hotwater_nextelec == "Probably yes"~ 1,
                                         hotwater_nextelec == "Yes, definitely"~ 1),
           
           hotwater_energy = case_when(hotwater_energy  == "I don't know"~ "Not know",
                                       hotwater_energy == "Electricity"~ "Elec",
                                       hotwater_energy == "Natural gas"~ "Gas",
                                       str_detect(hotwater_energy,"Propane|Wood|oil|thermal")~ "Other",
                                       hotwater_energy == "Other, please share:" & str_detect(hotwater_energy_6_TEXT, "gas|Gas") ~ "Gas",
                                       hotwater_energy == "Other, please share:" & str_detect(hotwater_energy_6_TEXT, "electric|Electric|split|Split|pump|Pump|PUMP|AC|ac|Air|air|A/C|A/c") ~ "Elec",
                                       hotwater_energy == "Other, please share:" ~ "Other"),
           
           primary_heating_type  = case_when(primary_heating_type  == "Central forced-air furnace"~"Central",
                                             primary_heating_type  == "Electric heaters"~ "Elec",
                                             primary_heating_type  == "A device called a \"heat pump\""~ "Elec",
                                             primary_heating_type  == "I do not have a home heating system" ~ "None",
                                             primary_heating_type  == "I don't know" ~ "Not know",
                                             primary_heating_type  == "Other, please share:" ~ "Further",
                                             primary_heating_type  == "Wood stove/fireplace" ~ "Other",
                                             primary_heating_type  == "Steam or hot water through radiators or pipes" ~ "Boiler",
                                             primary_heating_type  == "Geothermal or solar radiant heat" ~ "Other"),
           
           primary_heating_type = case_when(
             primary_heating_type == "Central" & str_detect(heatenergy_furnace, "heat pump|resistance") ~ "Elec",
             primary_heating_type == "Central" & str_detect(heatenergy_furnace, "fuel|Propane|Wood") ~ "Other",
             primary_heating_type == "Central" & str_detect(heatenergy_furnace, "Natural") ~ "Gas",
             primary_heating_type == "Central" & str_detect(heatenergy_furnace, "I don't know") ~ "Not know",
             
             primary_heating_type == "Boiler" & str_detect(heatenergy_boiler, "Natural") ~ "Gas",
             primary_heating_type == "Boiler" & str_detect(heatenergy_boiler, "fuel|Propane") ~ "Other",
             primary_heating_type == "Boiler" & str_detect(heatenergy_boiler, "pump") ~ "Elec",
             primary_heating_type == "Boiler" & str_detect(heatenergy_boiler, "I don't know") ~ "Not know",
             
             primary_heating_type == "Further" & str_detect(primary_heating_type_64_TEXT, "gas|Gas|GAS|Calentador") ~ "Gas",
             primary_heating_type == "Further" & str_detect(primary_heating_type_64_TEXT, "electric|Electric|split|Split|SPLIT|pump|Pump|AC|ac|Air|air|A/C|A/c|Ac|Coil|portable|Portable|Wall|wall|WALL") ~ "Elec",
             primary_heating_type == "Further" & str_detect(primary_heating_type_64_TEXT, "not|don’t|don't|dont|None|none|Don't|Don’t|never|Rarely|dress|cloth") ~ "None",
             primary_heating_type == "Further"  ~ "Other",
             
             TRUE ~ primary_heating_type
           ),
           
           previous_heating_type  = case_when(heatpump_adopter  == "Electric heater(s)" ~ "Elec",
                                              heatpump_adopter  == "Natural gas furnace"~ "Gas",
                                              heatpump_adopter  == "A device called a \"heat pump\""~ "Elec",
                                              heatpump_adopter  %in% c("Propane heater(s)","Steam or hot water through radiators or pipes") ~ "Other",
                                              heatpump_adopter  == "I don't know" ~ "Not know",
                                              heatpump_adopter  == "Other (please specify:)" ~ "Other",
                                              TRUE ~ primary_heating_type),
           
           primary_cooling_type  = case_when(primary_cooling_type  == "Air-conditioning unit(s) in specific room(s)"~"Room",
                                             primary_cooling_type  == "Central air-conditioning"~ "Central",
                                             primary_cooling_type  == "I do not have air-conditioning in my home"~ "None",
                                             primary_cooling_type  == "I don't know" ~ "Not know",
                                             primary_cooling_type  == "Other, please share:" & str_detect(primary_cooling_type_25_TEXT, "heat|Heat|central|Central|HEAT") ~ "Central", 
                                             primary_cooling_type  == "Other, please share:" & str_detect(primary_cooling_type_25_TEXT, "Portable|portable|air|Air|split|Split|AC|A/C|ac|Window units|window units|Window ac|window a/c") ~ "Room",
                                             primary_cooling_type  == "Other, please share:" ~ "Other"),
           
           
           # primary_heating_type  = ifelse(primary_heating_type  == "Central forced-air furnace", "Central", 
           #                                ifelse(primary_heating_type  == "Electric heaters", "Elec",
           #                                       ifelse(primary_heating_type  == "I do not have a home heating system", "None",
           #                                              ifelse(str_detect(primary_heating_type, "pump"), "HP","Others")))),
           # primary_cooling_type  = ifelse(primary_cooling_type  == "Central air-conditioning", "Central", 
           #                                ifelse(primary_cooling_type  == "Air-conditioning unit(s) in specific room(s)", "Room",
           #                                       ifelse(primary_cooling_type  == "I do not have air-conditioning in my home", "None", "Others"))),
           
           heatpump_direct = case_when(heatpump_direct == "I don't know"~ 0,
                                       heatpump_direct == "Not interested"~ 0,
                                       heatpump_direct == "Somewhat interested"~ 1,
                                       heatpump_direct == "Very interested"~ 1),
           
           heating_plan = coalesce(replan_heat_owner,replan_noheat_owner,replan_heat_renter,replan_noheat_renter),
           heating_plan = case_when(heating_plan == "In the next 1 to 2 years" ~ 1,
                                    heating_plan == "In the next 3 to 5 years" ~ 1,
                                    is.na(heating_plan) ~ NA_real_,
                                    TRUE ~ 0),
           
           cooling_plan = coalesce(replan_cool_owner,replan_nocool_owner,replan_cool_renter,replan_nocool_renter),
           cooling_plan = case_when(cooling_plan == "In the next 1 to 2 years" ~ 1,
                                    cooling_plan == "In the next 3 to 5 years" ~ 1,
                                    is.na(cooling_plan) ~ NA_real_,
                                    TRUE ~ 0),
           
           # heatpump_direct = ifelse(heatpump_direct == "I don't know", NA, heatpump_direct),
           # heatpump_direct = factor(heatpump_direct, levels = data$heatpump_direct %>% unique() %>% .[c(1,5,2)]) %>% 
           #   as.numeric(),
           # heating_plan = coalesce(replan_heat_owner,replan_noheat_owner,replan_heat_renter,replan_noheat_renter),
           # heating_plan = ifelse(heating_plan == "In the next 1 to 2 years", 4, 
           #                       ifelse(heating_plan == "In the next 3 to 5 years", 3, 
           #                              ifelse(heating_plan == "More than 5 years from now", 2, 
           #                                     ifelse(heating_plan == "Never", 1, NA)))),
           # cooling_plan = coalesce(replan_cool_owner,replan_nocool_owner,replan_cool_renter,replan_nocool_renter),
           # cooling_plan = ifelse(cooling_plan == "In the next 1 to 2 years", 4, 
           #                       ifelse(cooling_plan == "In the next 3 to 5 years", 3, 
           #                              ifelse(cooling_plan == "More than 5 years from now", 2, 
           #                                     ifelse(cooling_plan == "Never", 1, NA)))),
           # therm_summer = ifelse(therm_summer %in% c("I do not have a thermostat in my home",
           #                                           "I do not use my thermostat during the summer"), 0,1),
           # therm_winter = ifelse(therm_winter %in% c("I do not have a thermostat in my home",
           #                                           "I do not use my thermostat during the winter"), 0,1),
           
           # IC
           induction_direct = case_when(induction_direct == "I don't know"~ 0,
                                        induction_direct == "Not interested"~ 0,
                                        induction_direct == "Somewhat interested"~ 1,
                                        induction_direct == "Very interested"~ 1),
           # induction_direct = ifelse(induction_direct == "I don't know", NA, induction_direct),
           # induction_direct = factor(induction_direct, levels = data$induction_direct %>% unique() %>% .[c(1,4,2)]) %>% 
           #   as.numeric(),
           kitchen_range_type = case_when(kitchen_range_type == "Electric coil"~ "Elec",
                                          kitchen_range_type == "Natural gas"~ "Gas",
                                          kitchen_range_type == "Propane  or bottled gas"~ "Other",
                                          kitchen_range_type == "Induction"~ "Elec",
                                          kitchen_range_type == "I don't have a cooktop or range"~ "None",
                                          kitchen_range_type == "I don't know"~ "Not know"),
           
           # kitchen_range_type = ifelse(kitchen_range_type  == "Electric coil", "Elec_coil", 
           #                             ifelse(kitchen_range_type  == "Natural gas", "Natural_gas",
           #                                    ifelse(kitchen_range_type  == "Propane  or bottled gas", "Propane",
           #                                           ifelse(kitchen_range_type == "I don't have a cooktop or range", "None",
           #                                                  ifelse(kitchen_range_type == "I don't know", "not_known","Induction"))))),
           
           # perception
           heating_cost_burden = factor(heating_cost_burden, levels = data$heating_cost_burden %>% unique() %>% .[c(5,1,4,3,2)]) %>% 
             as.numeric(),
           elec_savemoney = coalesce(elec_savemoney,elec_savemoney_rent),
           elec_savemoney = factor(elec_savemoney, levels = data$elec_savemoney %>% unique() %>% .[c(1,2,5,6,3)]) %>%
             as.numeric(),
           elec_health = coalesce(elec_health,elec_health_renter),
           elec_health = factor(elec_health, levels = data$elec_health %>% unique() %>% .[c(6,5,1,4,2)]) %>%
             as.numeric(),
           elec_safety  = factor(elec_safety, levels = data$elec_safety %>% unique() %>% .[c(3,1,2)]) %>%
             as.numeric(),
           electrification = ifelse(natgas_ypccc %in% c("I don't know","No preference"), "No_prefer", 
                                    ifelse(str_detect(natgas_ypccc, "but"), "Half_elec",
                                           ifelse(str_detect(natgas_ypccc, "electricity"), "Full_elec", "Full_fossil"))),
           
           # electrification = factor(natgas_ypccc, levels = data$natgas_ypccc %>% unique() %>% .[c(4,2,3)]) %>%
           #   as.numeric(),
           # elec_home_cost_1 = ifelse(elec_home_cost_1 == "I don't know", NA, elec_home_cost_1),
           elec_home_cost_1 = ifelse(elec_home_cost_1 == "I don't know", "No change in cost", elec_home_cost_1),
           elec_home_cost_EV = factor(elec_home_cost_1, levels = data$elec_home_cost_1 %>% unique() %>% .[c(5,3,7,6,2)]) %>%
             as.numeric(),
           # elec_home_cost_2 = ifelse(elec_home_cost_2 == "I don't know", NA, elec_home_cost_2),
           elec_home_cost_2 = ifelse(elec_home_cost_2 == "I don't know", "No change in cost", elec_home_cost_2),
           elec_home_cost_IC = factor(elec_home_cost_2, levels = data$elec_home_cost_1 %>% unique() %>% .[c(5,3,7,6,2)]) %>%
             as.numeric(),
           # elec_home_cost_3 = ifelse(elec_home_cost_3 == "I don't know", NA, elec_home_cost_3),
           elec_home_cost_3 = ifelse(elec_home_cost_3 == "I don't know", "No change in cost", elec_home_cost_3),
           elec_home_cost_HP = factor(elec_home_cost_3, levels = data$elec_home_cost_1 %>% unique() %>% .[c(5,3,7,6,2)]) %>%
             as.numeric(),
           # elec_home_cost_4 = ifelse(elec_home_cost_4 == "I don't know", NA, elec_home_cost_4),
           elec_home_cost_4 = ifelse(elec_home_cost_4 == "I don't know", "No change in cost", elec_home_cost_4),
           elec_home_cost_PV = factor(elec_home_cost_4, levels = data$elec_home_cost_1 %>% unique() %>% .[c(5,3,7,6,2)]) %>%
             as.numeric(),
           upfrontpayback = ifelse(upfrontpayback == "I would prefer an upfront subsidy", 1, 0),
           
           # resilience
           outage_impact = factor(outage_impact, levels = data$outage_impact %>% unique() %>% .[c(2,1,3,4)]) %>%
             as.numeric(),
           outage_generatorown = ifelse(outage_generatorown == "Yes", 1, 0),
           outage_generatorplan = factor(outage_generatorplan, levels = data$outage_generatorplan %>% unique() %>% .[c(2,1,3,4)]) %>%
             as.numeric(),
           
           peer_EV = ifelse(str_detect(elec_home_who_1, "neighbor"), "neighbor_peer",
                            ifelse(str_detect(elec_home_who_1, "coworker"), "coworker", "acquaintance")), 
           peer_EV = ifelse(is.na(peer_EV), "none", peer_EV),
           peer_EV = ifelse(peer_EV == "none" & elec_home_others_1 == "Yes", "not-specific", peer_EV),
           peer_EV = ifelse(peer_EV %in% c("not-specific","coworker", "acquaintance"), "peer", peer_EV),
           peer_EV = ifelse(peer_EV == "neighbor_peer" & elec_home_who_1 == "A neighbor within 1 mile", "neighbor", peer_EV),
           peer_EV = factor(peer_EV, levels = c("neighbor_peer","neighbor","peer","none")),
           
           
           peer_IC = ifelse(str_detect(elec_home_who_2, "neighbor"), "neighbor_peer",
                            ifelse(str_detect(elec_home_who_2, "coworker"), "coworker", "acquaintance")), 
           peer_IC = ifelse(is.na(peer_IC), "none", peer_IC),
           peer_IC = ifelse(peer_IC == "none" & elec_home_others_2 == "Yes", "not-specific", peer_IC),
           peer_IC = ifelse(peer_IC %in% c("not-specific","coworker", "acquaintance"), "peer", peer_IC),
           peer_IC = ifelse(peer_IC == "neighbor_peer" & elec_home_who_2 == "A neighbor within 1 mile", "neighbor", peer_IC),
           peer_IC = factor(peer_IC, levels = c("neighbor_peer","neighbor","peer","none")),
           
           peer_HP = ifelse(str_detect(elec_home_who_3, "neighbor"), "neighbor_peer",
                            ifelse(str_detect(elec_home_who_3, "coworker"), "coworker", "acquaintance")), 
           peer_HP = ifelse(is.na(peer_HP), "none", peer_HP),
           peer_HP = ifelse(peer_HP == "none" & elec_home_others_3 == "Yes", "not-specific", peer_HP),
           peer_HP = ifelse(peer_HP %in% c("not-specific","coworker", "acquaintance"), "peer", peer_HP),
           peer_HP = ifelse(peer_HP == "neighbor_peer" & elec_home_who_3 == "A neighbor within 1 mile", "neighbor", peer_HP),
           peer_HP = factor(peer_HP, levels = c("neighbor_peer","neighbor","peer","none")),
           
           peer_PV = ifelse(str_detect(elec_home_who_4, "neighbor"), "neighbor_peer",
                            ifelse(str_detect(elec_home_who_4, "coworker"), "coworker", "acquaintance")), 
           peer_PV = ifelse(is.na(peer_PV), "none", peer_PV),
           peer_PV = ifelse(peer_PV == "none" & elec_home_others_4 == "Yes", "not-specific", peer_PV),
           peer_PV = ifelse(peer_PV %in% c("not-specific","coworker", "acquaintance"), "peer", peer_PV),
           peer_PV = ifelse(peer_PV == "neighbor_peer" & elec_home_who_4 == "A neighbor within 1 mile", "neighbor", peer_PV),
           peer_PV = factor(peer_PV, levels = c("neighbor_peer","neighbor","peer","none")),
           
           
           # WTP
           evupfront_num = case_when(vehicle_1_class == "Compact" ~ 27804,
                                     vehicle_1_class == "Sedan " ~ 40380,
                                     vehicle_1_class %in% c("SUV / Crossover SUV","Minivan") ~ 43990,
                                     vehicle_1_class == "Truck" ~ 55177) %>% as.numeric(),
           ev_wtp_pc = case_when(ev_wtp_pc == "always"~ 0,
                                 ev_wtp_pc == "$2,000 in financial assistance"~ 2000,
                                 ev_wtp_pc == "$4,000 in financial assistance"~ 4000,
                                 ev_wtp_pc == "$6,000 in financial assistance"~ 6000, 
                                 ev_wtp_pc == "$8,000 in financial assistance"~ 8000,
                                 ev_wtp_pc == "$10,000 in financial assistance"~ 10000,
                                 ev_wtp_pc == "$12,500 in financial assistance"~ 12500,
                                 ev_wtp_pc == "$15,000 in financial assistance"~ 15000,
                                 ev_wtp_pc == "never"~ 20000),
           # ev_wtp_pc = factor(ev_wtp_pc, levels = data$ev_wtp_pc %>% unique() %>% .[c(8,4,10,7,1,2,6,5,9)]) %>%
           #   as.numeric(),
           
           ssupfront_num = str_remove_all(ss_upfr_text, "[^0-9]") %>% as.numeric(),
           solstor_wtp_dv = case_when(solstor_wtp_dv == "always"~ 0,
                                      solstor_wtp_dv == "$1,000 in financial assistance"~ 1000,
                                      solstor_wtp_dv == "$2,000 in financial assistance"~ 2000,
                                      solstor_wtp_dv == "$3,000 in financial assistance"~ 3000, 
                                      solstor_wtp_dv == "$5,000 in financial assistance"~ 5000,
                                      solstor_wtp_dv == "$7,500 in financial assistance"~ 7500,
                                      solstor_wtp_dv == "$10,000 in financial assistance"~ 10000,
                                      solstor_wtp_dv == "$12,500 in financial assistance"~ 12500,
                                      solstor_wtp_dv == "$15,000 in financial assistance"~ 15000,
                                      solstor_wtp_dv == "$17,500 in financial assistance"~ 17500,
                                      solstor_wtp_dv == "$20,000 in financial assistance"~ 20000,
                                      solstor_wtp_dv == "$25,000 in financial assistance"~ 25000,
                                      solstor_wtp_dv == "$30,000 in financial assistance"~ 30000,
                                      solstor_wtp_dv == "never"~ 35000),
           # 
           # solstor_wtp_dv = case_when(solstor_wtp_dv == "always"~ 0,
           #                            solstor_wtp_dv == "$1,000 in financial assistance"~ 1000,
           #                            solstor_wtp_dv == "$2,000 in financial assistance"~ 2000,
           #                            solstor_wtp_dv == "$3,000 in financial assistance"~ 3000, 
           #                            solstor_wtp_dv == "$5,000 in financial assistance"~ 5000,
           #                            solstor_wtp_dv == "$7,500 in financial assistance"~ 7500,
           #                            solstor_wtp_dv == "$10,000 in financial assistance"~ 10000,
           #                            solstor_wtp_dv == "$12,500 in financial assistance"~ 12500,
           #                            solstor_wtp_dv == "$15,000 in financial assistance"~ 15000,
           #                            solstor_wtp_dv == "$17,500 in financial assistance"~ 17500,
           #                            solstor_wtp_dv == "$20,000 in financial assistance"~ 20000,
           #                            solstor_wtp_dv == "$25,000 in financial assistance"~ 25000,
           #                            solstor_wtp_dv == "$30,000 in financial assistance"~ 30000,
           #                            solstor_wtp_dv == "never"~ 35000),
           # solstor_wtp_dv = factor(solstor_wtp_dv, levels = data$solstor_wtp_dv %>% unique() %>% .[c(5,9,8,10,13,12,14,3,2,6,7,15,4,11)]) %>%
           #   as.numeric(),
           
           solstor_wtp_payback = case_when(solstor_wtp_payback == "always"~ 0,
                                           solstor_wtp_payback == "2 years to break even"~ 2,
                                           solstor_wtp_payback == "4 years to break even"~ 4,
                                           solstor_wtp_payback == "6 years to break even"~ 6, 
                                           solstor_wtp_payback == "8 years to break even"~ 8,
                                           solstor_wtp_payback == "10 years to break even"~ 10,
                                           solstor_wtp_payback == "15 years to break even"~ 15,
                                           solstor_wtp_payback == "20 years to break even"~ 20,
                                           solstor_wtp_payback == "never"~ 25),
           # solstor_wtp_payback  = factor(solstor_wtp_payback , levels = data$solstor_wtp_payback  %>% unique() %>% .[c(2,10,9,4,5,3,6,8,7)]) %>%
           #   as.numeric(),
           hpupfront_num = case_when(home_area > 0 ~ 13000,
                                     home_area > 1500 ~ 15000,
                                     home_area > 2500 ~ 20000),
           heatpump_wtp_pc = case_when(heatpump_wtp_pc == "always"~ 0,
                                       heatpump_wtp_pc == "$2,000 in financial assistance"~ 2000,
                                       heatpump_wtp_pc == "$4,000 in financial assistance"~ 4000,
                                       heatpump_wtp_pc == "$6,000 in financial assistance"~ 6000, 
                                       heatpump_wtp_pc == "$8,000 in financial assistance"~ 8000,
                                       heatpump_wtp_pc == "$10,000 in financial assistance"~ 10000,
                                       heatpump_wtp_pc == "$12,500 in financial assistance"~ 12500,
                                       heatpump_wtp_pc == "$15,000 in financial assistance"~ 15000,
                                       heatpump_wtp_pc == "never"~ 20000),
           # heatpump_wtp_pc = factor(heatpump_wtp_pc, levels = data$heatpump_wtp_pc %>% unique() %>% .[c(1,6,10,7,5,2,3,8,9)]) %>%
           #   as.numeric(),
           
           heatpump_wtp_payback = case_when(heatpump_wtp_payback == "always"~ 0,
                                            heatpump_wtp_payback == "3 years to break even"~ 3,
                                            heatpump_wtp_payback == "5 years to break even"~ 5,
                                            heatpump_wtp_payback == "7 years to break even"~ 7, 
                                            heatpump_wtp_payback == "9 years to break even"~ 9,
                                            heatpump_wtp_payback == "11 years to break even"~ 11,
                                            heatpump_wtp_payback == "13 years to break even"~ 13,
                                            heatpump_wtp_payback == "15 years to break even"~ 15,
                                            heatpump_wtp_payback == "never"~ 18),
           # heatpump_wtp_payback = factor(heatpump_wtp_payback, levels = data$heatpump_wtp_payback %>% unique() %>% .[c(5,8,10,7,6,9,3,2,1)]) %>%
           #   as.numeric(),
           
           induction_dv = case_when(induction_dv == "always"~ 0,
                                    induction_dv == "$100"~ 100,
                                    induction_dv == "$200"~ 200,
                                    induction_dv == "$300" ~ 300,
                                    induction_dv == "$400"~ 400,
                                    induction_dv == "$500" ~ 500,
                                    induction_dv == "$600"~ 600,
                                    induction_dv == "$700" ~ 700,
                                    induction_dv == "$800" ~ 800,
                                    induction_dv == "$900" ~ 900,
                                    induction_dv == "$1000" ~ 1000,
                                    induction_dv == "never" ~ 1200),
           # induction_dv = factor(induction_dv, levels = data$induction_dv %>% unique() %>% .[c(5,7,12,11,9,10,3,2,13,6,8)]) %>%
           #   as.numeric(),
           
           # climate change 
           yale_worried = factor(yale_worried, levels = data$yale_worried %>% unique() %>% .[c(4,2,3,1)]) %>%
             as.numeric(),
           cclive = factor(cclive, levels = data$cclive %>% unique() %>% .[c(2,3,4,1)]) %>%
             as.numeric(),
           ccpastmove = factor(ccpastmove, levels = data$ccpastmove %>% unique() %>% .[c(1,3,2)]) %>%
             as.numeric(),
           ccfuturemove = factor(ccfuturemove, levels = data$ccfuturemove %>% unique() %>% .[c(1,2,4,3)]) %>%
             as.numeric(),
           homevac = factor(homevac, levels = data$homevac %>% unique() %>% .[c(2,1,3)]) %>%
             as.numeric(),
           ccmove_where = ifelse(ccmove_where == "Move to a different state", 1, 0),
           
           ### additional variables
           cost_fuel_winter = case_when(cost_fuel_winter == "$0-25 per month" ~ 12.5,
                                        cost_fuel_winter == "$25-50 per month" ~ 37.5,
                                        cost_fuel_winter == "$50-100 per month" ~ 75,
                                        cost_fuel_winter == "$100-199 per month" ~ 150,
                                        cost_fuel_winter == "$200-299 per month" ~ 250,
                                        cost_fuel_winter == "$300-399 per month" ~ 350,
                                        cost_fuel_winter == "$400-499 per month" ~ 450,
                                        cost_fuel_winter == "$500-599 per month" ~ 550,
                                        cost_fuel_winter == "$600-699 per month" ~ 650,
                                        cost_fuel_winter == "$700-799 per month" ~ 750,
                                        cost_fuel_winter == "$800 or more per month" ~ 850,
                                        cost_fuel_winter == "I do not know or remember" ~ NA),
           cost_electric_winter = case_when(cost_electric_winter == "$0-25 per month" ~ 12.5,
                                            cost_electric_winter == "$25-50 per month" ~ 37.5,
                                            cost_electric_winter == "$50-100 per month" ~ 75,
                                            cost_electric_winter == "$100-199 per month" ~ 150,
                                            cost_electric_winter == "$200-299 per month" ~ 250,
                                            cost_electric_winter == "$300-399 per month" ~ 350,
                                            cost_electric_winter == "$400-499 per month" ~ 450,
                                            cost_electric_winter == "$500-599 per month" ~ 550,
                                            cost_electric_winter == "$600-699 per month" ~ 650,
                                            cost_electric_winter == "$700-799 per month" ~ 750,
                                            cost_electric_winter == "$800 or more per month" ~ 850,
                                            cost_electric_winter == "I do not know or remember" ~ NA),
           
           cost_combo_winter_summed = cost_fuel_winter + cost_electric_winter,
           
           cost_combo_winter = case_when(cost_combo_winter == "$0-25 per month" ~ 12.5,
                                         cost_combo_winter == "$25-50 per month" ~ 37.5,
                                         cost_combo_winter == "$50-100 per month" ~ 75,
                                         cost_combo_winter == "$100-199 per month" ~ 150,
                                         cost_combo_winter == "$200-299 per month" ~ 250,
                                         cost_combo_winter == "$300-399 per month" ~ 350,
                                         cost_combo_winter == "$400-499 per month" ~ 450,
                                         cost_combo_winter == "$500-599 per month" ~ 550,
                                         cost_combo_winter == "$600-699 per month" ~ 650,
                                         cost_combo_winter == "$700-799 per month" ~ 750,
                                         cost_combo_winter == "$800 or more per month" ~ 850,
                                         cost_combo_winter == "I do not know or remember" ~ NA),
           cost_combo_winter_final = coalesce(cost_combo_winter,cost_combo_winter_summed),
           
           ## summer energy costs
           cost_combo_summer_final = coalesce(cost_electric_summer, cost_combo_summer),
           cost_combo_summer_final = case_when(cost_combo_summer_final == "$0-25 per month" ~ 12.5,
                                               cost_combo_summer_final == "$25-50 per month" ~ 37.5,
                                               cost_combo_summer_final == "$50-100 per month" ~ 75,
                                               cost_combo_summer_final == "$100-199 per month" ~ 150,
                                               cost_combo_summer_final == "$200-299 per month" ~ 250,
                                               cost_combo_summer_final == "$300-399 per month" ~ 350,
                                               cost_combo_summer_final == "$400-499 per month" ~ 450,
                                               cost_combo_summer_final == "$500-599 per month" ~ 550,
                                               cost_combo_summer_final == "$600-699 per month" ~ 650,
                                               cost_combo_summer_final == "$700-799 per month" ~ 750,
                                               cost_combo_summer_final == "$800 or more per month" ~ 850,
                                               cost_combo_summer_final == "I do not know or remember" ~ NA),
           
           ## age
           age = case_when(age == "18 - 24" ~ 21,
                           age == "25 - 34" ~ 30,
                           age == "35 - 44" ~ 40,
                           age == "45 - 54" ~ 50,
                           age == "55 - 64" ~ 60,
                           age == "65 - 74" ~ 70,
                           age == "75 - 84" ~ 80,
                           age == "85 or older" ~ 90),
           
           ## gender
           gender = case_when(gender == "I want to self-identify" ~ "Other",
                              .default = gender),
           
           ## home_own
           home_own = ifelse(home_own == "Yes", 1, 0), 
           
           ## ac_unit_number
           ac_unit_number = case_when(ac_unit_number == "1" ~ 1,
                                      ac_unit_number == "2" ~ 2,
                                      ac_unit_number == "3" ~ 3,
                                      ac_unit_number == "4" ~ 4,
                                      ac_unit_number == "5 or more" ~ 5,
                                      .default = NA),
           
           # vehicle_num
           vehicle_num = case_when(vehicle_num == "1" ~ 1,
                                   vehicle_num == "2" ~ 2,
                                   vehicle_num == "3" ~ 3,
                                   vehicle_num == "4 or more vehicles" ~ 4,
                                   .default = NA),
           
           # vehicle_2_miles
           vehicle_2_miles = case_when(vehicle_2_miles == "Less than 2,500 miles per year"~ 1250,
                                       vehicle_2_miles == "2,500 - 4,999 miles per year"~ 3750,
                                       vehicle_2_miles == "5,000 - 7,499 miles per year"~ 6250,
                                       vehicle_2_miles == "7,500 - 9,999 miles per year"~ 8750,
                                       vehicle_2_miles == "10,000 - 12,499 miles per year"~ 11250,
                                       vehicle_2_miles == "12,500 - 14,999 miles per year"~ 13750,
                                       vehicle_2_miles == "15,000 - 17,499 miles per year"~ 16250,
                                       vehicle_2_miles == "17,500 - 20,000 miles per year"~ 18750,
                                       vehicle_2_miles == "More than 20,000 miles per year"~ 25000,
                                       .default = 0),
           
           ## vehicle_comb_miles
           vehicle_comb_miles = vehicle_1_miles + vehicle_2_miles,
           
           ## fastcharge_likely
           fastcharge_likely = case_when(fastcharge_likely == "No, not more likely at all"~ 1,
                                         fastcharge_likely == "Only a little more likely" ~ 2,
                                         fastcharge_likely == "Somewhat more likely" ~ 3,
                                         fastcharge_likely == "Much more likely" ~ 4,
                                         .default = NA),
           
           # lowincome_sup
           lowincome_sup = case_when(lowincome_sup == "Yes"~ 1,
                                     lowincome_sup == "No" ~ 0,
                                     lowincome_sup == "I don't know" ~ 0,
                                     .default = NA),
           
           # elec_avail 
           elec_avail = case_when(elec_avail == "Very confident"~ 4,
                                  elec_avail == "Somewhat confident" ~ 3,
                                  elec_avail == "Not too confident" ~ 2,
                                  elec_avail == "Not confident at all" ~ 1,
                                  .default = NA),
           
           # hvac_avail
           hvac_avail = case_when(hvac_avail == "Very confident"~ 4,
                                  hvac_avail == "Somewhat confident" ~ 3,
                                  hvac_avail == "Not too confident" ~ 2,
                                  hvac_avail == "Not confident at all" ~ 1,
                                  .default = NA),
           
           # ccinsure_owner
           ccinsure_owner = ifelse(ccinsure_owner == "Yes", 1, 0), 
           
           # ccinsure_renter
           ccinsure_renter = ifelse(ccinsure_renter == "Yes", 1, 0), 
           
           ## ccinsure_both
           ccinsure_both = coalesce(ccinsure_owner, ccinsure_renter),
           
           # ccinsure_lost
           ccinsure_lost = ifelse(ccinsure_lost == "Yes", 1, 0), 
           
           # pid
           pid_composite = case_when(pid == "Democrat" & pid_dem == "Strong Democrat" ~ 1,
                                     pid == "Democrat" & pid_dem == "Not a very strong Democrat" ~ 2,
                                     pid == "Independent" & pid_ind == "Closer to the Democratic Party" ~ 3,
                                     pid == "Something else" & pid_ind == "Closer to the Democratic Party" ~ 3,
                                     pid == "Independent" & pid_ind == "No, neither party" ~ 4,
                                     pid == "Something else" & pid_ind == "No, neither party" ~ 4,
                                     pid == "Something else" & pid_ind == "Closer to the Republican Party" ~ 5,
                                     pid == "Independent" & pid_ind == "Closer to the Republican Party" ~ 5,
                                     pid == "Republican" & pid_rep == "Not a very strong Republican" ~ 6,
                                     pid == "Republican" & pid_rep == "Strong Republican" ~ 7,
                                     pid == "I am not eligible to vote" ~ NA, 
                                     .default = NA),
           
           ## thermostat use:
           therm_summer = case_when(str_detect(therm_summer, "60|61|62") ~ 1,
                                    str_detect(therm_summer, "63|64|65") ~ 2,
                                    str_detect(therm_summer, "66|67|68") ~ 3,
                                    str_detect(therm_summer, "69|70|71") ~ 4,
                                    str_detect(therm_summer, "72|73|74") ~ 5,
                                    str_detect(therm_summer, "75|76|77") ~ 6,
                                    str_detect(therm_summer, "78|79|80|I do not") ~ 7,
                                    .default = NA),
           
           therm_winter = case_when(str_detect(therm_winter, "60|61|62|I do not") ~ 1,
                                    str_detect(therm_winter, "63|64|65") ~ 2,
                                    str_detect(therm_winter, "66|67|68") ~ 3,
                                    str_detect(therm_winter, "69|70|71") ~ 4,
                                    str_detect(therm_winter, "72|73|74") ~ 5,
                                    str_detect(therm_winter, "75|76|77") ~ 6,
                                    str_detect(therm_winter, "78|79|80") ~ 7,
                                    .default = NA)
    )
  
}


### add urban variable
dat1 <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
  data_process(ev = c("Fully electric")) %>% 
  left_join(CA_t %>% 
              st_drop_geometry() %>% 
              dplyr::select(GEOID, estimate), by = c("tractid" = "GEOID")) %>% 
  mutate(urban = ifelse(estimate > 4000, "Urban", "Rural")) %>% 
  mutate(dac = ifelse(dac == 1 & urban == "Urban", "Urban_DAC",
                      ifelse(dac == 1 & urban == "Rural", "Rural_DAC",
                             ifelse(dac == 0 & urban == "Urban", "Urban_Non_DAC", "Rural_Non_DAC")))) %>% 
  dplyr::select(-estimate, -urban) %>% 
  data_clean(1) %>% 
  dplyr::select(-c("cost_combo_winter_final","cost_combo_summer_final"),
                -c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv","solstor_wtp_dv"),
                -c("PS_int","EV_int","HP_int","IC_int"))

# save(CA_t, dac_sf, cz, mrp, dat, dat1, file = "./data/data.RData")
