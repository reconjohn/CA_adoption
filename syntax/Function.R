
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
options(tigris_use_cache = TRUE, tigris_class = "sf")

# uploading library 
lapply(c("spatstat","colorRamps","tmap","ggmap","mapview","geoR","knitr","kableExtra","data.table","gdata","tigris","sf","scales","tidycensus","plotly", "tidyverse"), require, character.only = TRUE)

sf::sf_use_s2(FALSE)
load("./data/dac_sf.RData") # CA_t, cz, dac_sf, sc_map (DAC, climate zone spatial data)
# names(mrp)
# crosswalk <- read_csv("./survey.csv")

## PSPS test from Qi
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
# 
# CA_t %>% 
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


### data
data_process <- function(data, ev_type){
  data %>% 
    filter(age != "Under 18") %>% 
    # filter(Progress == 100) %>% 
    mutate(PV = ifelse(adopter_solar == "adopter_solar", 1, 
                       ifelse(is.na(adopter_solar),0, 0)),
           PV = ifelse(is.na(PV), 0, PV),
           
           SS = ifelse(storage_own == "Yes", 1, 0),
           
           PS = ifelse(PV == 1 & SS == 1, 1, 0),
           
           # EV = ifelse(adopter_ev == "adopter_ev", 1, # missing EV for the 2nd car
           #             ifelse(is.na(adopter_ev),0, 0)),
           # EV = ifelse(is.na(EV), 0, EV),
           
           
           # EV = ifelse(vehicle_1_energy %in% c("A plug-in hybrid","Fully electric") | vehicle_2_energy %in% c("A plug-in hybrid","Fully electric"),
           #             1, 0),
           # EV = ifelse(vehicle_1_energy %in% c("Fully electric") | vehicle_2_energy %in% c("Fully electric"),
           #             1, 0),
           
           EV = ifelse(vehicle_1_energy %in% ev_type | vehicle_2_energy %in% ev_type,
                       1, 0),
           
           HP = ifelse(adopter_heatpump == 'adopter_heatpump', 1, 
                       ifelse(is.na(adopter_heatpump),0, 0)),
           HP = ifelse(is.na(HP), 0, HP),
           IC = ifelse(adopter_induction == 'adopter_induction', 1,
                       ifelse(is.na(adopter_induction),0, 0)),
           IC = ifelse(is.na(IC), 0, IC)) %>% 
    
    dplyr::select(ExternalReference,climatezone,dac,age,
                  gender,income,education,race,ideology,pid,born_us,employment,workfromhome, # demo
                  home_type,household_numpeople,home_own,home_area,home_age, # housing
                  
                  # PV
                  solar_install_owner,solar_install_renter,
                  solar_date_owner,
                  solar_pv_plans,
                  storage_plans, 
                  storage_own,
                  
                  # EV
                  vehicle_1_miles, 
                  vehicle_charging_own,parkingspot,charging_install,charging_work,
                  charging_5mile_non_1,charging_5mile_non_2,
                  charging_5mile_own_1,charging_5mile_own_2,
                  fastcharge_likely,
                  vehicle_wherecharge_1, vehicle_wherecharge_2, vehicle_wherecharge_3, vehicle_whencharge,
                  rangeanxiety,
                  vehicle_next_used,vehicle_next_when,vehicle_next_fuel,
                  vehicle_1_class,
                  
                  # HP
                  primary_heating_type,
                  primary_cooling_type, # heating and cooling
                  heatpump_direct, # previous heating before hp
                  replan_heat_owner,replan_noheat_owner,replan_heat_renter,replan_noheat_renter,
                  replan_cool_owner,replan_cool_renter,replan_nocool_owner,replan_nocool_renter,
                  therm_summer,therm_winter,
                  
                  # IC
                  kitchen_range_type,induction_direct,
                  
                  # perception
                  heating_cost_burden,elec_savemoney,elec_savemoney_rent,elec_health,elec_health_renter,
                  elec_safety,natgas_ypccc,
                  elec_home_cost_1,elec_home_cost_2,elec_home_cost_3,elec_home_cost_4, # EV, IC and HP, PV
                  upfrontpayback,
                  
                  # resilience
                  outage_impact, 
                  outage_generatorown,
                  outage_generatorplan,
                  
                  # peer effect
                  elec_home_others_1,elec_home_others_2,elec_home_others_3, elec_home_others_4, # EV, IC and HP, PV
                  elec_home_who_1,elec_home_who_2,elec_home_who_3,elec_home_who_4, 
                  
                  # WTP
                  solstor_wtp_dv, solstor_wtp_payback, 
                  ev_wtp_pc,
                  heatpump_wtp_pc,heatpump_wtp_payback,heatpump_motiv,
                  induction_dv,induction_appeal,induction_downsides,
                  
                  # tech
                  PV, PS, EV, HP, IC, 
                  
                  # climate change
                  yale_worried,cclive,ccpastmove,ccfuturemove,homevac,ccmove_where,
                  
                  # weight
                  wt_ca,
                  
                  # GEOID
                  tractid
    ) %>% 
    mutate(solstor_wtp_dv = ifelse(str_detect(solstor_wtp_dv,"without"), "always",
                                   ifelse(str_detect(solstor_wtp_dv,"never"), "never",solstor_wtp_dv)),
           solstor_wtp_payback = ifelse(str_detect(solstor_wtp_payback,"if"), "always",
                                        ifelse(str_detect(solstor_wtp_payback,"NOT"), "never",solstor_wtp_payback)),
           ev_wtp_pc = ifelse(str_detect(ev_wtp_pc,"without"), "always",
                              ifelse(str_detect(ev_wtp_pc,"NOT"), "never",ev_wtp_pc)),
           heatpump_wtp_pc = ifelse(str_detect(heatpump_wtp_pc,"without"), "always",
                                    ifelse(str_detect(heatpump_wtp_pc,"never"), "never",heatpump_wtp_pc)),
           heatpump_wtp_payback = ifelse(str_detect(heatpump_wtp_payback,"if"), "always",
                                         ifelse(str_detect(heatpump_wtp_payback,"NOT"), "never",heatpump_wtp_payback)),
           induction_dv = ifelse(str_detect(induction_dv,"without"), "always",
                                 ifelse(str_detect(induction_dv,"NOT"), "never",induction_dv))
    ) %>% 
    dplyr::select(-tractid) %>% 
    left_join(tb, by = "ExternalReference")

}

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
           
           education = factor(education, levels = data$education %>% unique() %>% .[c(8,6,3,1,5,7,2,4)]) %>% 
             as.numeric(),
           born_us = ifelse(born_us == "Yes", 1, 0), 
           race = case_when(str_detect(race, "Hispanic") ~ "Hispanic",
                            race == "White" ~ "White",
                            race == "Black or African-American" ~ "Black",
                            race == "Asian" ~ "Asian",
                            race %in% c("American Indian or Alaska Native","Native Hawaiian or Pacific Islander") ~ "Natives",
                            is.na(race) ~ NA_character_,
                            TRUE ~ "Multirace"),
           ideology = factor(ideology, levels = data$ideology %>% unique() %>% .[c(8,2,5,1,6,3,7)]) %>% 
             as.numeric(),
           employment = ifelse(str_detect(employment, "full"), "Workfull",
                               ifelse(str_detect(employment, "part"), "Workpart", 
                                      ifelse(employment == "Homemaker", "Homemaker",
                                             ifelse(employment == "Retired", "Retired", 
                                                    ifelse(str_detect(employment, "Unemployed"), "Unemployed","Others"))))),
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
                                home_age < 2019 ~ "New",
                                TRUE ~ "Newer"),
           home_age = factor(home_age, levels = c("Older","New","Newer")),
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
                                    rangeanxiety == "I can't decide until I know more about the location of chargers"~ 300,
                                    rangeanxiety == "200 - 300 miles (comparable to a tank of gas for compact gas vehicles, with weekly charging for most users)"~ 250,
                                    rangeanxiety == "Less than 200 miles (sufficient for daily commuting, occasional charging needed)"~ 150,
                                    TRUE ~ NA_real_),        
           rangeanxiety = ifelse(rangeanxiety == 300 & vehicle_1_class == "Compact", 150,
                                 ifelse(rangeanxiety == 300 & vehicle_1_class == "Sedan", 250,
                                        ifelse(rangeanxiety == 300 & vehicle_1_class == "Minivan", 350,
                                               ifelse(rangeanxiety == 300 & vehicle_1_class == "Truck", 450,rangeanxiety)))),
           
           # rangeanxiety = factor(rangeanxiety, levels = data$rangeanxiety %>% unique() %>% .[c(5,2,4,3,1)]) %>% 
           #   as.numeric(),
           vehicle_next_used = ifelse(vehicle_next_used == "New", 1, 0),
           
           vehicle_next_when = factor(vehicle_next_when, levels = data$vehicle_next_when %>% unique() %>% .[c(1,3,2,4,5)]) %>% 
             as.numeric(),
           vehicle_next_fuel = ifelse(str_detect(vehicle_next_fuel, "All-electric vehicle"), 1, 0),
           
           
           # HP
           primary_heating_type  = ifelse(primary_heating_type  == "Central forced-air furnace", "Central", 
                                          ifelse(primary_heating_type  == "Electric heaters", "Elec",
                                                 ifelse(primary_heating_type  == "I do not have a home heating system", "None",
                                                        ifelse(str_detect(primary_heating_type, "pump"), "HP","Others")))),
           primary_cooling_type  = ifelse(primary_cooling_type  == "Central air-conditioning", "Central", 
                                          ifelse(primary_cooling_type  == "Air-conditioning unit(s) in specific room(s)", "Room",
                                                 ifelse(primary_cooling_type  == "I do not have air-conditioning in my home", "None", "Others"))),
           # heatpump_direct = ifelse(heatpump_direct == "I don't know", NA, heatpump_direct),
           heatpump_direct = factor(heatpump_direct, levels = data$heatpump_direct %>% unique() %>% .[c(1,4,5,2)]) %>% 
             as.numeric(),
           heating_plan = coalesce(replan_heat_owner,replan_noheat_owner,replan_heat_renter,replan_noheat_renter),
           heating_plan = ifelse(heating_plan == "In the next 1 to 2 years", 5, 
                                 ifelse(heating_plan == "In the next 3 to 5 years", 4, 
                                        ifelse(heating_plan == "More than 5 years from now", 3, 
                                               ifelse(heating_plan == "Never", 1, 2)))),
           cooling_plan = coalesce(replan_cool_owner,replan_nocool_owner,replan_cool_renter,replan_nocool_renter),
           cooling_plan = ifelse(cooling_plan == "In the next 1 to 2 years", 5, 
                                 ifelse(cooling_plan == "In the next 3 to 5 years", 4, 
                                        ifelse(cooling_plan == "More than 5 years from now", 3, 
                                               ifelse(cooling_plan == "Never", 1, 2)))),
           therm_summer = ifelse(therm_summer %in% c("I do not have a thermostat in my home",
                                                     "I do not use my thermostat during the summer"), 0,1),
           therm_winter = ifelse(therm_winter %in% c("I do not have a thermostat in my home",
                                                     "I do not use my thermostat during the winter"), 0,1),
           
           # IC
           # induction_direct = ifelse(induction_direct == "I don't know", NA, induction_direct),
           induction_direct = factor(induction_direct, levels = data$induction_direct %>% unique() %>% .[c(1,5,4,2)]) %>% 
             as.numeric(),
           kitchen_range_type = ifelse(kitchen_range_type  == "Electric coil", "Elec_coil", 
                                       ifelse(kitchen_range_type  == "Natural gas", "Natural_gas",
                                              ifelse(kitchen_range_type  == "Propane  or bottled gas", "Propane",
                                                     ifelse(kitchen_range_type == "I don't have a cooktop or range", "None",
                                                            ifelse(kitchen_range_type == "I don't know", "not_known","Induction"))))),
           
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
           elec_home_cost_1 = ifelse(elec_home_cost_1 == "I don't know", "No change in cost", elec_home_cost_1),
           elec_home_cost_EV = factor(elec_home_cost_1, levels = data$elec_home_cost_1 %>% unique() %>% .[c(5,3,7,6,2)]) %>%
             as.numeric(),
           elec_home_cost_2 = ifelse(elec_home_cost_2 == "I don't know", "No change in cost", elec_home_cost_2),
           elec_home_cost_IC = factor(elec_home_cost_2, levels = data$elec_home_cost_1 %>% unique() %>% .[c(5,3,7,6,2)]) %>%
             as.numeric(),
           elec_home_cost_3 = ifelse(elec_home_cost_3 == "I don't know", "No change in cost", elec_home_cost_3),
           elec_home_cost_HP = factor(elec_home_cost_3, levels = data$elec_home_cost_1 %>% unique() %>% .[c(5,3,7,6,2)]) %>%
             as.numeric(),
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
           
           # peer 
           # peer_EV = ifelse(str_detect(elec_home_who_1, "neighbor"), "neighbor",
           #                  ifelse(str_detect(elec_home_who_1, "coworker"), "coworker", "acquaintance")), 
           # peer_EV = ifelse(is.na(peer_EV), "none", peer_EV),
           # peer_EV = ifelse(peer_EV == "none" & elec_home_others_1 == "Yes", "not-specific", peer_EV),
           
           peer_EV = ifelse(str_detect(elec_home_who_1, "neighbor"), "neighbor",
                            ifelse(str_detect(elec_home_who_1, "coworker"), "coworker", "acquaintance")), 
           peer_EV = ifelse(is.na(peer_EV), "none", peer_EV),
           peer_EV = ifelse(peer_EV == "none" & elec_home_others_1 == "Yes", "not-specific", peer_EV),
           peer_EV = ifelse(peer_EV %in% c("not-specific","coworker", "acquaintance"), "peer", peer_EV),
           peer_EV = factor(peer_EV, levels = c("neighbor","peer","none")),
           
           peer_IC = ifelse(str_detect(elec_home_who_2, "neighbor"), "neighbor",
                            ifelse(str_detect(elec_home_who_2, "coworker"), "coworker", "acquaintance")), 
           peer_IC = ifelse(is.na(peer_IC), "none", peer_IC),
           peer_IC = ifelse(peer_IC == "none" & elec_home_others_2 == "Yes", "not-specific", peer_IC),
           peer_IC = ifelse(peer_IC %in% c("not-specific","coworker", "acquaintance"), "peer", peer_IC),
           peer_IC = factor(peer_IC, levels = c("neighbor","peer","none")),
           
           peer_HP = ifelse(str_detect(elec_home_who_3, "neighbor"), "neighbor",
                            ifelse(str_detect(elec_home_who_3, "coworker"), "coworker", "acquaintance")), 
           peer_HP = ifelse(is.na(peer_HP), "none", peer_HP),
           peer_HP = ifelse(peer_HP == "none" & elec_home_others_3 == "Yes", "not-specific", peer_HP),
           peer_HP = ifelse(peer_HP %in% c("not-specific","coworker", "acquaintance"), "peer", peer_HP),
           peer_HP = factor(peer_HP, levels = c("neighbor","peer","none")),
           
           peer_PV = ifelse(str_detect(elec_home_who_4, "neighbor"), "neighbor",
                            ifelse(str_detect(elec_home_who_4, "coworker"), "coworker", "acquaintance")), 
           peer_PV = ifelse(is.na(peer_PV), "none", peer_PV),
           peer_PV = ifelse(peer_PV == "none" & elec_home_others_4 == "Yes", "not-specific", peer_PV),
           peer_PV = ifelse(peer_PV %in% c("not-specific","coworker", "acquaintance"), "peer", peer_PV),
           peer_PV = factor(peer_PV, levels = c("neighbor","peer","none")),
           
           
           # WTP
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
           
           solstor_wtp_dv = case_when(solstor_wtp_dv == "always"~ 0,
                                      solstor_wtp_dv == "$1,000 in financial assistance"~ 1000,
                                      solstor_wtp_dv == "$2,000 in financial assistance"~ 2000,
                                      solstor_wtp_dv == "$3,000 in financial assistance"~ 3000, 
                                      solstor_wtp_dv == "$5,000 in financial assistance"~ 4000,
                                      solstor_wtp_dv == "$7,500 in financial assistance"~ 75000,
                                      solstor_wtp_dv == "$10,000 in financial assistance"~ 10000,
                                      solstor_wtp_dv == "$12,500 in financial assistance"~ 12500,
                                      solstor_wtp_dv == "$15,000 in financial assistance"~ 15000,
                                      solstor_wtp_dv == "$17,500 in financial assistance"~ 17500,
                                      solstor_wtp_dv == "$20,000 in financial assistance"~ 20000,
                                      solstor_wtp_dv == "$25,000 in financial assistance"~ 25000,
                                      solstor_wtp_dv == "$30,000 in financial assistance"~ 30000,
                                      solstor_wtp_dv == "never"~ 35000),
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
           yale_worried = factor(yale_worried, levels = data$yale_worried %>% unique() %>% .[c(4,3,2,1)]) %>%
             as.numeric(),
           cclive = factor(cclive, levels = data$cclive %>% unique() %>% .[c(2,3,4,1)]) %>%
             as.numeric(),
           ccpastmove = factor(ccpastmove, levels = data$ccpastmove %>% unique() %>% .[c(1,3,2)]) %>%
             as.numeric(),
           ccfuturemove = factor(ccfuturemove, levels = data$ccfuturemove %>% unique() %>% .[c(1,2,4,3)]) %>%
             as.numeric(),
           homevac = factor(homevac, levels = data$homevac %>% unique() %>% .[c(2,1,3)]) %>%
             as.numeric(),
           ccmove_where = ifelse(ccmove_where == "Move to a different state", 1, 0))
}


data_clean <- function(data, further = NULL){
  
  dat <- data %>% 
    data_cl()
  
  # ### Imputation
  # imp <- lm(solstor_wtp_dv ~ income+race+pid+employment+home_type+home_age+kitchen_range_type+primary_cooling_type+
  #             primary_heating_type+PV+peer_PV, dat %>% 
  #             filter(!is.na(solstor_wtp_dv)))
  # # summary(imp)
  # missing_rows <- which(is.na(dat$solstor_wtp_dv))
  # estimated_value <- predict(imp, newdata = dat[missing_rows, ])
  # dat$solstor_wtp_dv[missing_rows] <- estimated_value
  # 
  # imp <- lm(heatpump_wtp_pc ~ income+race+pid+employment+home_type+home_age+kitchen_range_type+primary_cooling_type+
  #             HP+peer_HP, dat %>% 
  #             filter(!is.na(heatpump_wtp_pc)))
  # # summary(imp)
  # missing_rows <- which(is.na(dat$heatpump_wtp_pc))
  # estimated_value <- predict(imp, newdata = dat[missing_rows, ])
  # dat$heatpump_wtp_pc[missing_rows] <- estimated_value
  # rows <- which(dat$heatpump_wtp_pc<0)
  # dat$heatpump_wtp_pc[rows] <- 0
  # 
  # imp <- lm(induction_dv ~ income+race+pid+employment+home_type+home_age+primary_cooling_type+
  #             primary_heating_type+IC+peer_IC, dat %>% 
  #             filter(!is.na(induction_dv)))
  # # summary(imp)
  # missing_rows <- which(is.na(dat$induction_dv))
  # estimated_value <- predict(imp, newdata = dat[missing_rows, ])
  # dat$induction_dv[missing_rows] <- estimated_value
  
           
  dat <- dat %>% 
    mutate(
      # future adoption
      future_EV10 = ifelse((vehicle_next_when > 1 & vehicle_next_fuel == 1)| EV == 1, 1, 0),
      future_EV_0 = ifelse((vehicle_next_when > 2 & vehicle_next_fuel == 1 &
                              ev_wtp_pc == 0)| EV == 1, 1, 0),
      future_EV_75 = ifelse((vehicle_next_when > 2 & vehicle_next_fuel == 1 &
                               ev_wtp_pc < 7500)| EV == 1, 1, 0),
      future_EV_120 = ifelse((vehicle_next_when > 2 & vehicle_next_fuel == 1 &
                                ev_wtp_pc < 12000)| EV == 1, 1, 0),
      
      future_PV = ifelse(solar_pv_plans %in% c("Yes", "Maybe") | PV == 1, 1, 0),
      
      future_PS_0 = ifelse(storage_plans %in% c("Yes", "Maybe") & solar_pv_plans %in% c("Yes", "Maybe") &
                             solstor_wtp_dv == 0| PS == 1, 1, 0),
      future_PS_72 = ifelse(storage_plans %in% c("Yes", "Maybe") & solar_pv_plans %in% c("Yes", "Maybe") &
                              solstor_wtp_dv < 7200| PS == 1, 1, 0),
      future_PS_127 = ifelse(storage_plans %in% c("Yes", "Maybe") & solar_pv_plans %in% c("Yes", "Maybe") &
                               solstor_wtp_dv < 12700| PS == 1, 1, 0),
      
      future_HP_0 = ifelse(heatpump_direct > 2 & 
                             heatpump_wtp_pc == 0| HP == 1, 1, 0),
      future_HP_30 = ifelse(heatpump_direct > 2 & 
                              heatpump_wtp_pc < 3000| HP == 1, 1, 0),
      future_HP_110 = ifelse(heatpump_direct > 2 & 
                               heatpump_wtp_pc < 11000| HP == 1, 1, 0),
      
      future_IC_0 = ifelse(induction_direct > 2 &
                             induction_dv == 0| IC == 1, 1, 0),
      future_IC_35 = ifelse(induction_direct > 2 &
                              induction_dv < 360| IC == 1, 1, 0),
      future_IC_159 = ifelse(induction_direct > 2 &
                               induction_dv < 1590| IC == 1, 1, 0)
    ) %>% 
    # mutate(across(matches("future"), ~ ifelse(is.na(.), 0, .))) %>% 
  

    # remove variables 
    dplyr::select(-solar_install_owner,-solar_install_renter,
                  -workfromhome,-vehicle_charging_own:-charging_install,-charging_5mile_non_1:-charging_5mile_own_2,
                  -replan_heat_owner:-replan_nocool_renter,-elec_savemoney_rent,-elec_health_renter,
                  -natgas_ypccc,
                  -elec_home_cost_1:-elec_home_cost_4,-elec_home_others_1:-elec_home_who_4,vehicle_next_fuel,
                  -heatpump_motiv,-induction_appeal,-induction_downsides,
                  # -solstor_wtp_dv,-heatpump_wtp_pc,
                  -vehicle_next_fuel) %>%
    relocate(wt_ca, .after = last_col()) %>%
    relocate(tractid, .after = last_col())
  
  if(!is.null(further)){
    dat <- dat %>% 
      # left_join(psps, by = c("tractid" = "GEOID")) %>% 
      # left_join(charge, by = c("tractid" = "GEOID")) %>% 
      dplyr::select(-ExternalReference, -home_own, -age, -gender,
                    -solar_install,-solar_date_owner,-solar_pv_plans,-storage_plans,-elec_home_cost_PV,
                    -fastcharge_likely,-vehicle_wherecharge_1,-vehicle_wherecharge_2,-vehicle_wherecharge_3,-vehicle_whencharge,
                    -elec_home_cost_EV,-vehicle_next_when,-vehicle_1_class,
                    -elec_home_cost_HP,-elec_home_cost_IC,-tractid,
                    
                    -storage_own,
                    -charging_access,-charging_5mile_r,
                    -cooling_plan,-therm_summer,
                    -yale_worried,-pid,-ccfuturemove,
                    
                    -solstor_wtp_payback,-heatpump_wtp_payback, # remove payback
                    -future_EV10) %>% 
      relocate(wt_ca, .after = last_col())
  }
  
  return(dat)

}


get_estimate <- function(var_name, sum_fit) {
  est <- sum_fit %>%
    filter(var == var_name) %>%
    pull(Estimate)
  if (length(est) == 0) return(0) else return(est)
}

ipt <- c("PV","EV","HP","IC","PS")


### modeling
# logit model 
# give not standardized data, tech, scenario, and future
# output: predictor impact, random effects, coefficient estimates 
mreg_logit <- function(data, remove = NULL, i, scenario = NULL, future = NULL){
  
  # data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1)
  # remove <- c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv", "education","employment")
  # scenario <- c("peer_PV")
  # i <- 5
  # future <- 127
  
  da_r <- data %>% 
    dplyr::select(# remove multicollinear variables

      # remove predictors with substantial NAs
      -heatpump_direct,-induction_direct,
      
      # remove future adoption
      -starts_with("future"),
    ) %>% 
    dplyr::select(-remove)
  
  future_var <- sym(paste0("future_", ipt[i],"_",future))
  
  if(!is.null(future)){
    da_r <- data %>% 
      mutate(!!ipt[i] := .data[[future_var]]) %>% 

      dplyr::select(# remove multicollinear variables
        
        # remove predictors with substantial NAs
        -heatpump_direct,-induction_direct,
        
        # remove future adoption
        -starts_with("future")
      ) %>% 
      dplyr::select(-remove)
  }
  
  
  # binary
  bina <- c(
    "born_us",
    
    "charging_5mile_f",
    "vehicle_next_used",
    # "vehicle_next_fuel",
    "charging_work",
    
    "therm_winter",
    
    "upfrontpayback",
    "outage_generatorown",
    "ccmove_where"
  ) %>% 
    setdiff(remove)
  
  # categorical variables: 
  cat <- c("race",
           # "born_us",
           "home_type",
           "employment",
           
           "peer_EV",
           "peer_PV",
           "peer_HP",
           "peer_IC",
           
           # "charging_5mile_f",
           # "vehicle_next_used",
           # "vehicle_next_fuel",
           # "charging_work",
           
           "primary_heating_type",
           "primary_cooling_type",
           # "therm_winter",
           
           "kitchen_range_type",
           "electrification"
           # "upfrontpayback",
           # "outage_generatorown",
           # "ccmove_where"
  ) %>% 
    setdiff(remove)
  
  exclusions <- list(
    c("peer_EV", "peer_HP", "peer_IC"),
    c("peer_PV", "peer_HP", "peer_IC"),
    c("peer_EV", "peer_PV", "peer_IC"),
    c("peer_EV", "peer_HP", "peer_PV")
  )
  
  if(i == 5){
    cat <- cat %>% setdiff(exclusions[[1]])
  }else{
    cat <- cat %>% setdiff(exclusions[[i]])
  }
  
  
  
  ftr <- c("climatezone",
           "dac",
           "race",
           "born_us",
           "home_type",
           "employment",
           
           "peer_EV",
           "peer_PV",
           "peer_HP",
           "peer_IC",
           
           "charging_5mile_f",
           "vehicle_next_used",
           # "vehicle_next_fuel",
           "charging_work",
           
           "primary_heating_type",
           "primary_cooling_type",
           "therm_winter",
           
           "kitchen_range_type",
           "electrification",
           "upfrontpayback",
           "outage_generatorown",
           "ccmove_where"
  ) %>% 
    setdiff(remove)
  
  # da$race %>% unique
  da_r[,ftr] <- data.frame(lapply(da_r[ftr],as.factor)) 
  
  # apply sum contrast for mean effects 
  for (var in cat) {
    k <- length(levels(da_r[[var]]))
    contrasts(da_r[[var]]) <- contr.sum(k)
    colnames(contrasts(da_r[[var]])) <- levels(da_r[[var]])[1:(k-1)]
  }
  
  for (var in bina) {
    da_r[[var]] <- factor(da_r[[var]], levels = c(1,0))
    contrasts(da_r[[var]]) <- contr.sum(2)
    colnames(contrasts(da_r[[var]])) <- levels(da_r[[var]])[1]
  }
  
  da_r <- da_r %>% 
    mutate(across(where(is.numeric) & !c("PV","PS","EV","HP","IC","wt_ca"), ~ scale(.) %>% as.numeric())) %>% 
    mutate(across(any_of(c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv")), ~ .x * -1))
  
  # lapply(da_r, unique)
  # summary(da_r)
  
  if(i %in% c(1,5)){
    # for PV, remove zone effect, tech, peer
    tract <- c("climatezone","dac","ev_wtp_pc","heatpump_wtp_pc","induction_dv","wt_ca",
               "peer_EV","peer_HP","peer_IC",
               "PV","PS","EV","HP","IC")
    
    model1vars <- setdiff(names(da_r), tract)
    
  }else if(i == 2){
    # for EV, remove zone effect, tech
    tract <- c("climatezone","dac","solstor_wtp_dv","heatpump_wtp_pc","induction_dv","wt_ca",
               "peer_PV","peer_HP","peer_IC",
               "PV","PS","EV","HP","IC")
    model1vars <- setdiff(names(da_r), tract)
    
  }else if(i == 3){
    # for HP, remove zone effect, tech, heating/cooling type
    tract <- c("climatezone","dac","ev_wtp_pc","solstor_wtp_dv","induction_dv","wt_ca",
               "peer_EV","peer_PV","peer_IC",
               "PV","PS","EV","HP","IC","primary_heating_type","primary_cooling_type")
    model1vars <- setdiff(names(da_r), tract)
    
  }else{
    # for IC, remove zone effect, tech, cooking type
    tract <- c("climatezone","dac","ev_wtp_pc","solstor_wtp_dv","heatpump_wtp_pc","wt_ca",
               "peer_EV","peer_HP","peer_PV",
               "PV","PS","EV","HP","IC","kitchen_range_type")
    model1vars <- setdiff(names(da_r), tract)
  }
  
  model1vars <- setdiff(model1vars, scenario)
  
  if (is.null(scenario)) {
    fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), 
                             "+ (1|climatezone) + (1|dac)"))
  } else {
    fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), 
                             "+ (1+",paste(scenario, collapse = " + "),"|climatezone) + (1+",paste(scenario, collapse = " + "),
                             "|dac)"))
  }
  
  fit <- glmer(fvar, weights = wt_ca, family = binomial(link = "logit"), data = da_r,
               nAGQ = 0, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000),calc.derivs = FALSE))
  # summary(fit)
  sum_fit <- summary(fit)$coef %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("var") %>% 
    dplyr::select(var, Estimate)
  
  m <- dim(fit %>% tidy() %>% filter(is.na(group)))[[1]]
  
  sims <- 1000
  pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
  pe <- pe[1:m,]
  
  vc <- vcov(fit)
  simbetas <- mvrnorm(sims, pe, vc)
  colnames(simbetas)[1] <- "Cont"
  
  # Create an empty data frame with NA values and the specified row and column names
  df <- as.data.frame(matrix(NA, nrow = length(colnames(simbetas)), ncol = length(colnames(simbetas))))
  rownames(df) <- colnames(simbetas)
  colnames(df) <- colnames(simbetas)
  
  reference_levels <- c()
  for (var in cat) {
    k <- length(levels(da_r[[var]]))
    reference_levels[var] <- levels(da_r[[var]])[k]
  }
  
  cat_ref <- paste0(names(reference_levels),reference_levels)
  
  df_r <- as.data.frame(matrix(NA, nrow = length(cat_ref), ncol = length(colnames(simbetas))))
  rownames(df_r) <- cat_ref
  colnames(df_r) <- colnames(simbetas)
  
  df_r1 <- df_r2 <- df_r
  df_r2[is.na(df_r2)] <- 0

  for(v in names(reference_levels)){
    # Identify target row(s) starting with "race"
    target_rows <- grep(paste0("^",v), rownames(df_r))
    
    # Identify target columns starting with "race"
    target_cols <- grep(paste0("^",v), colnames(df_r))
    
    # Replace NAs with -1 for matching row-column combinations
    for (r in target_rows) {
      
      df_r1[r, target_cols] <- ifelse(is.na(df_r[r, target_cols]), -1, 0)
    }
  }
  df_r1[is.na(df_r1)] <- 0
  
  
  df1 <- df2 <- df 
  # Replace values in each row
  for (j in seq_along(colnames(simbetas))) {
    df1[j, ] <- 0              # Set all elements in the row to 0
    df1[j, colnames(simbetas)[j]] <- 1  # Set the element in the column matching the row name to 1
  }
  
  df1 <- df1[-1,] %>% rbind(df_r1)
  
  for (j in seq_along(colnames(simbetas))) {
    df2[j, ] <- 0              # Set all elements in the row to 0
    df2[j, colnames(simbetas)[j]] <- 0  # Set the element in the column matching the row name to 0
  }
  
  df2 <- df2[-1,] %>% rbind(df_r2)
  
  x <- df1 %>% rbind(df2) %>% 
    mutate(Cont = 1) %>% as.matrix()
  
  m <- m + length(cat_ref)
  xbeta <- x %*% t(simbetas) 
  
  pi <- inv.logit(xbeta)
  
  pi <- pi[1:(m-1),] - pi[m:(2*(m-1)),]
  
  pe <- apply(pi, 1, mean) 
  lwr <- apply(pi, 1, quantile, probs= 0.975) 
  upr <- apply(pi, 1, quantile, probs= 0.025)
  
  dd <- cbind(pe,upr,lwr) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("var") %>%
    mutate(color = ifelse(upr*lwr >0, "Y", "N")) %>%
    mutate(IV = ipt[i])

  # ### reference level for categorical variables 
  # reference_levels <- c()
  # for (var in cat) {
  #   k <- length(levels(da_r[[var]]))
  #   reference_levels[var] <- levels(da_r[[var]])[k]
  # }
  # 
  # add_reference_level <- function(var_prefix, ref_level, df) {
  #   matches <- df[grep(paste0("^", var_prefix), df$var), ]
  #   if (nrow(matches) == 0) return(NULL)
  #   
  #   implied_estimate <- -sum(matches$Estimate)
  #   
  #   new_row <- data.frame(
  #     var = paste0(var_prefix, ref_level),
  #     Estimate = implied_estimate
  #   )
  #   return(new_row)
  # }
  # 
  # # Apply the function to all listed categorical variables
  # implied_rows <- do.call(rbind, Map(
  #   f = add_reference_level,
  #   var_prefix = names(reference_levels),
  #   ref_level = reference_levels,
  #   MoreArgs = list(df = sum_fit)
  # ))
  # 
  # se_ref <- c()
  # for(v in rownames(implied_rows)){
  #   levels_v_dummies <- grep(paste0("^", v), sum_fit$var, value = TRUE)
  #   levels_v_dummies <- intersect(levels_v_dummies, rownames(vc))
  #   
  #   # Get the sub-matrix of the variance-covariance matrix for these dummy variables
  #   vc_sub <- vc[levels_v_dummies, levels_v_dummies, drop = FALSE]
  #   se_ref[v] <- sum(vc_sub) %>% sqrt()
  # }
  # 
  # 
  # dd <- cbind(pe,upr,lwr) %>%
  #   as.data.frame() %>% 
  #   tibble::rownames_to_column("var") %>% 
  #   rbind(
  #     implied_rows %>% 
  #       cbind(se_ref) %>% 
  #       dplyr::rename(pe = Estimate) %>% 
  #       mutate(upr = pe + 1.96*se_ref,
  #              lwr = pe - 1.96*se_ref) %>% 
  #       dplyr::select(-se_ref)
  #   ) %>% 
  #   mutate(color = ifelse(upr*lwr >0, "Y", "N")) %>%
  #   mutate(IV = ipt[i])
  # 
  


  if (is.null(scenario)) {
    
    ds <- c()
    dc <- c()
    
  } else {
    names <- ranef(fit)[[1]] %>% colnames()
    ds <- ranef(fit)[[1]] %>% 
      rownames_to_column("zone") 
    
    dc <- ranef(fit)[[2]] %>% 
      rownames_to_column("zone") 
    
    zone_r <- cz %>% 
      left_join(ds %>% 
                  gather(scene, R_effect, names), by = c("BZone" = "zone")) %>% 
      dplyr::select(BZone,scene,R_effect)
    
    dac_r <- dac_sf %>% 
      mutate(sample = as.character(sample)) %>% 
      left_join(dc %>% 
                  gather(scene, R_effect, names), by = c("sample" = "zone")) %>% 
      dplyr::select(sample,scene,R_effect)
    
    re_slope <- zone_r %>% 
      st_drop_geometry() %>% 
      inner_join(dac_r %>% 
                   st_drop_geometry(), by = c("scene")) %>% 
      unite(col = "group", BZone, sample, sep = "") %>% 
      mutate(R_effect = R_effect.x + R_effect.y) %>% 
      dplyr::select(-R_effect.x, -R_effect.y) %>% 
      pivot_wider(names_from = scene, values_from = R_effect) 
    
    # add random effects to the average effects
    for(k in seq_along(names)){
      re_slope <- re_slope %>% 
        mutate(across(all_of(names[k]), ~ .x + get_estimate(names[k], sum_fit)))
    }
    
    re <- re_slope %>% 
      mutate(across(all_of(names[-1]), ~ .x + get(names[1]))) %>% # to have total effects
      mutate(across(all_of(names), ~ inv.logit(.x))) %>%  # convert to probability
      mutate(across(all_of(names[-1]), ~ .x - get(names[1]))) %>% # get marginal effects
      dplyr::select(-names[1]) %>% 
      gather(scene, R_effect, names[-1]) %>% 
      mutate(IV = ipt[i])
    
    re <- sc_map%>% 
      left_join(re, by = "group")

  }
  
  
  # dd$var %>% unique() 
  # da$primary_heating_type %>% unique() 
  name_mapping <- c(
    "PV" = "PV", 
    "PS" = "PS",
    "EV" = "EV", 
    "HP" = "HP",
    "IC" = "IC",
    
    "born_us1" = "Born US",
    "upfrontpayback1" = "Upfront prefer",
    "therm_winter1" = "Thermostat use",
    "outage_generatorown1" = "Own generator",
    
    "charging_5mile_f1" = "Fast charger",
    # "vehicle_next_fuel1" = "EV to buy",
    "vehicle_next_used1" = "New car",
    "vehicle_next_when" = "Later to buy car",
    "vehicle_1_miles" = "Milage",
    "charging_work1" = "Charger at work",
    
    "home_age" = "Newer home",
    "rangeanxiety" = "more range for EV",
    "ccmove_where1" = "Leave CA",
    "homevac" = "Evacuation"
    
  )
  
  reg <- dd %>% 
    mutate(var = recode(var, !!!name_mapping),
           domain = ifelse(str_detect(var,"income|education|race|ideology|pid|US|employment"), "Sociodemo",
                           
                           ifelse(str_detect(var, "home|household"), "Housing",
                                  
                                  ifelse(str_detect(var,"primary|Thermostat"), "Heat/Cool",
                                         
                                         ifelse(var %in% dd$var[grep(pattern="kitchen",
                                                                     dd$var)], "Cook",
                                                
                                                ifelse(str_detect(var,"therm|burden|savemoney|health|safety|Upfront|direct|plan|electrification"), "Behavior",
                                                       
                                                       ifelse(str_detect(var, "outage_impact|freq|dur|generator"), "Resilience", 
                                                              
                                                              ifelse(str_detect(var, "peer"), "Peer Effect", 
                                                                     ifelse(str_detect(var, "Milage|charger|Charger|car|EV|l1l2|dc"), "Vehicle", 
                                                                            ifelse(str_detect(var, "payback|dv|pc"), "WTP",
                                                                                   ifelse(str_detect(var, "cc|CA|Evacuation"), "Climate", var)))))))))),
           domain = factor(domain, levels = c("Housing","Heat/Cool","Cook","Vehicle","Sociodemo","Behavior","Resilience","Climate","Peer Effect","WTP"))) %>% 
    
    mutate(IV = factor(IV, levels = ipt)) %>% 
    
    mutate(color = factor(color, levels = c("Y","N")))
  
  return(list(reg, re, sum_fit))
}

### LPM model
# give not standardized data, tech, scenario, and future
# output: predictor impact, random effects, coefficient estimates 
mreg <- function(data, remove = NULL, i, scenario = NULL, future = NULL){
  
  # data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1)
  # remove <- c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv", "education","employment")
  # scenario <- c("peer_PV")
  # i <- 5
  # future <- 127
  
  da_r <- data %>% 
    dplyr::select(# remove multicollinear variables
      
      # remove predictors with substantial NAs
      -heatpump_direct,-induction_direct,
      
      # remove future adoption
      -starts_with("future")
    ) %>% 
    dplyr::select(-remove)
  
  if(i == 1){
    future_var <- sym(paste0("future_", ipt[i]))
  }else{
    future_var <- sym(paste0("future_", ipt[i],"_",future))
  }
  
  
  if(!is.null(future)){
    da_r <- data %>% 
      mutate(!!ipt[i] := .data[[future_var]]) %>% 
      dplyr::select(# remove multicollinear variables
        
        # remove predictors with substantial NAs
        -heatpump_direct,-induction_direct,
        
        # remove future adoption
        -starts_with("future")
      ) %>% 
      dplyr::select(-remove)
  }
  
  
  # binary
  bina <- c(
    "born_us",
    
    "charging_5mile_f",
    "vehicle_next_used",
    # "vehicle_next_fuel",
    "charging_work",
    
    "therm_winter",
    
    "upfrontpayback",
    "outage_generatorown",
    "ccmove_where"
  ) %>% 
    setdiff(remove)
  
  # categorical variables: 
  cat <- c("race",
           # "born_us",
           "home_type",
           "employment",
           
           "peer_EV",
           "peer_PV",
           "peer_HP",
           "peer_IC",
           
           # "charging_5mile_f",
           # "vehicle_next_used",
           # "vehicle_next_fuel",
           # "charging_work",
           
           "primary_heating_type",
           "primary_cooling_type",
           # "therm_winter",
           
           "kitchen_range_type",
           "electrification"
           # "upfrontpayback",
           # "outage_generatorown",
           # "ccmove_where"
  ) %>% 
    setdiff(remove)
  
  exclusions <- list(
    c("peer_EV", "peer_HP", "peer_IC"),
    c("peer_PV", "peer_HP", "peer_IC"),
    c("peer_EV", "peer_PV", "peer_IC"),
    c("peer_EV", "peer_HP", "peer_PV")
  )
  
  if(i %in% c(1,5)){
    cat <- cat %>% setdiff(exclusions[[1]])
  }else{
    cat <- cat %>% setdiff(exclusions[[i]])
  }
  
  
  ftr <- c("climatezone",
           "dac",
           "race",
           "born_us",
           "home_type",
           "employment",
           
           "peer_EV",
           "peer_PV",
           "peer_HP",
           "peer_IC",
           
           "charging_5mile_f",
           "vehicle_next_used",
           # "vehicle_next_fuel",
           "charging_work",
           
           "primary_heating_type",
           "primary_cooling_type",
           "therm_winter",
           
           "kitchen_range_type",
           "electrification",
           "upfrontpayback",
           "outage_generatorown",
           "ccmove_where"
  ) %>% 
    setdiff(remove)
  
  # da$race %>% unique
  da_r[,ftr] <- data.frame(lapply(da_r[ftr],as.factor)) 
  
  # dummy sum contrast based on reference category (-1)
  for (var in cat) {
    k <- length(levels(da_r[[var]]))
    contrasts(da_r[[var]]) <- contr.sum(k)
    colnames(contrasts(da_r[[var]])) <- levels(da_r[[var]])[1:(k-1)]
  }
  
  # for binary
  for (var in bina) {
    da_r[[var]] <- factor(da_r[[var]], levels = c(1,0))
    contrasts(da_r[[var]]) <- contr.sum(2)
    colnames(contrasts(da_r[[var]])) <- levels(da_r[[var]])[1]
  }
  
  da_r <- da_r %>% 
    mutate(across(starts_with("peer"), ~factor(.x, levels = c("neighbor","peer","none")))) %>% # avoid the sum contrast
    # mutate(race = relevel(race, ref = "White"),
    #        peer_PV = relevel(peer_PV, ref = "none"),
    #        peer_EV = relevel(peer_EV, ref = "none"),
    #        peer_HP = relevel(peer_HP, ref = "none"),
    #        peer_IC = relevel(peer_IC, ref = "none"),
    #        primary_heating_type = relevel(primary_heating_type, ref = "Central"),
    #        primary_cooling_type = relevel(primary_cooling_type, ref = "None"),
    #        kitchen_range_type = relevel(kitchen_range_type, ref = "Natural_gas"),
    #        electrification = relevel(electrification, ref = "No_prefer")
    # ) %>%  # relevel for categorical variables 
    # 
  # fastDummies::dummy_cols(
  #   select_columns = cat[!cat %in% c("climatezone","dac")],  # categorical variables
  #   remove_first_dummy = TRUE,    # Avoid multicollinearity
  #   remove_selected_columns = TRUE  # Drop original factor columns
  # ) %>% 
  
  mutate(across(where(is.numeric) & !c("PV","PS","EV","HP","IC","wt_ca"), ~ scale(.) %>% as.numeric())) %>% 
    mutate(across(any_of(c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv")), ~ .x * -1))
  
  # lapply(da_r, unique)
  # summary(da_r)
  
  if(i %in% c(1,5)){
    # for PV, remove zone effect, tech
    tract <- c("climatezone","dac","ev_wtp_pc","heatpump_wtp_pc","induction_dv","wt_ca",
               "peer_EV","peer_HP","peer_IC",
               "PV","PS","EV","HP","IC")
    
    model1vars <- setdiff(names(da_r), tract)
    
  }else if(i == 2){
    # for EV, remove zone effect, tech
    tract <- c("climatezone","dac","solstor_wtp_dv","heatpump_wtp_pc","induction_dv","wt_ca",
               "peer_IC","peer_HP","peer_PV",
               "PV","PS","EV","HP","IC")
    model1vars <- setdiff(names(da_r), tract)
    
  }else if(i == 3){
    # for HP, remove zone effect, tech, heating/cooling type
    tract <- c("climatezone","dac","ev_wtp_pc","solstor_wtp_dv","induction_dv","wt_ca",
               "peer_EV","peer_IC","peer_PV",
               "PV","PS","EV","HP","IC","primary_heating_type","primary_cooling_type")
    model1vars <- setdiff(names(da_r), tract)
    
  }else{
    # for IC, remove zone effect, tech, cooking type
    tract <- c("climatezone","dac","ev_wtp_pc","solstor_wtp_dv","heatpump_wtp_pc","wt_ca",
               "peer_EV","peer_HP","peer_PV",
               "PV","PS","EV","HP","IC","kitchen_range_type")
    model1vars <- setdiff(names(da_r), tract)
  }
  
  model1vars <- setdiff(model1vars, scenario)
  
  if (is.null(scenario)) {
    fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), 
                             "+ (1|climatezone) + (1|dac)"))
  } else {
    fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), 
                             "+ (1+",paste(scenario, collapse = " + "),"|climatezone) + (1+",paste(scenario, collapse = " + "),
                             "|dac)"))
  }
  
  fit <- lmer(fvar, weights = wt_ca, data = da_r)
  # summary(fit)
  sum_fit <- summary(fit)$coef %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("var") %>% 
    dplyr::select(var, Estimate)
  
  m <- dim(fit %>% tidy() %>% filter(is.na(group)))[[1]]
  
  sims <- 1000
  pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
  pe <- pe[1:m,]
  
  vc <- vcov(fit)
  simbetas <- mvrnorm(sims, pe, vc)
  colnames(simbetas)[1] <- "Cont"
  
  # Create an empty data frame with NA values and the specified row and column names
  df <- as.data.frame(matrix(NA, nrow = length(colnames(simbetas)), ncol = length(colnames(simbetas))))
  rownames(df) <- colnames(simbetas)
  colnames(df) <- colnames(simbetas)
  
  ### adding reference categories
  reference_levels <- c()
  for (var in cat) {
    k <- length(levels(da_r[[var]]))
    reference_levels[var] <- levels(da_r[[var]])[k]
  }
  
  cat_ref <- paste0(names(reference_levels),reference_levels)
  
  df_r <- as.data.frame(matrix(NA, nrow = length(cat_ref), ncol = length(colnames(simbetas))))
  rownames(df_r) <- cat_ref
  colnames(df_r) <- colnames(simbetas)
  
  df_r1 <- df_r2 <- df_r
  df_r2[is.na(df_r2)] <- 0
  
  for(v in names(reference_levels)){
    # Identify target row(s) starting with "race"
    target_rows <- grep(paste0("^",v), rownames(df_r))
    
    # Identify target columns starting with "race"
    target_cols <- grep(paste0("^",v), colnames(df_r))
    
    # Replace NAs with -1 for matching row-column combinations
    for (r in target_rows) {
      
      df_r1[r, target_cols] <- ifelse(is.na(df_r[r, target_cols]), -1, 0)
    }
  }
  df_r1[is.na(df_r1)] <- 0
  
  
  df1 <- df2 <- df 
  # Replace values in each row
  for (j in seq_along(colnames(simbetas))) {
    df1[j, ] <- 0              # Set all elements in the row to 0
    df1[j, colnames(simbetas)[j]] <- 1  # Set the element in the column matching the row name to 1
  }
  
  df1 <- df1[-1,] %>% rbind(df_r1)
  
  for (j in seq_along(colnames(simbetas))) {
    df2[j, ] <- 0              # Set all elements in the row to 0
    df2[j, colnames(simbetas)[j]] <- 0  # Set the element in the column matching the row name to 0
  }
  
  df2 <- df2[-1,] %>% rbind(df_r2)
  
  x <- df1 %>% rbind(df2) %>% 
    mutate(Cont = 1) %>% as.matrix()
  
  m <- m + length(cat_ref)
  xbeta <- x %*% t(simbetas) 
  
  xbeta <- xbeta[1:(m-1),] - xbeta[m:(2*(m-1)),]
  
  pe <- apply(xbeta, 1, mean) 
  lwr <- apply(xbeta, 1, quantile, probs= 0.975) 
  upr <- apply(xbeta, 1, quantile, probs= 0.025)
  
  dd <- cbind(pe,upr,lwr) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("var") %>%
    mutate(color = ifelse(upr*lwr >0, "Y", "N")) %>%
    mutate(IV = ipt[i])
  
  # ### reference level for categorical variables 
  # reference_levels <- c()
  # for (var in cat) {
  #   k <- length(levels(da_r[[var]]))
  #   reference_levels[var] <- levels(da_r[[var]])[k]
  # }
  # 
  # add_reference_level <- function(var_prefix, ref_level, df) {
  #   matches <- df[grep(paste0("^", var_prefix), df$var), ]
  #   if (nrow(matches) == 0) return(NULL)
  #   
  #   implied_estimate <- -sum(matches$Estimate)
  #   
  #   new_row <- data.frame(
  #     var = paste0(var_prefix, ref_level),
  #     Estimate = implied_estimate
  #   )
  #   return(new_row)
  # }
  # 
  # # Apply the function to all listed categorical variables
  # implied_rows <- do.call(rbind, Map(
  #   f = add_reference_level,
  #   var_prefix = names(reference_levels),
  #   ref_level = reference_levels,
  #   MoreArgs = list(df = sum_fit)
  # ))
  # 
  # se_ref <- c()
  # for(v in rownames(implied_rows)){
  #   levels_v_dummies <- grep(paste0("^", v), sum_fit$var, value = TRUE)
  #   levels_v_dummies <- intersect(levels_v_dummies, rownames(vc))
  #   
  #   # Get the sub-matrix of the variance-covariance matrix for these dummy variables
  #   vc_sub <- vc[levels_v_dummies, levels_v_dummies, drop = FALSE]
  #   se_ref[v] <- sum(vc_sub) %>% sqrt()
  # }
  # 
  # 
  # dd <- cbind(pe,upr,lwr) %>%
  #   as.data.frame() %>% 
  #   tibble::rownames_to_column("var") %>% 
  #   rbind(
  #     implied_rows %>% 
  #       cbind(se_ref) %>% 
  #       dplyr::rename(pe = Estimate) %>% 
  #       mutate(upr = pe + 1.96*se_ref,
  #              lwr = pe - 1.96*se_ref) %>% 
  #       dplyr::select(-se_ref)
  #   ) %>% 
  #   mutate(color = ifelse(upr*lwr >0, "Y", "N")) %>%
  #   mutate(IV = ipt[i])
  # 
  
  if (is.null(scenario)) {
    tp <- sqrt(attr(ranef(fit, condVar=T)[[1]], "postVar"))*1.96 
    
    ds <- as.data.frame(ranef(fit)$climatezone) %>% 
      dplyr::rename(R_effect = "(Intercept)") %>% 
      tibble::rownames_to_column("zone") %>% 
      mutate(SE = tp[1,1,],
             lower = R_effect - SE,
             upper = R_effect + SE) %>% 
      mutate(IV = ipt[i])
    
    
    tp <- sqrt(attr(ranef(fit, condVar=T)[[2]], "postVar"))*1.96 
    
    dc <- as.data.frame(ranef(fit)$dac) %>% 
      dplyr::rename(R_effect = "(Intercept)") %>% 
      tibble::rownames_to_column("zone") %>% 
      mutate(SE = tp[1,1,],
             lower = R_effect - SE,
             upper = R_effect + SE) %>% 
      mutate(IV = ipt[i])
    
    
  } else {
    tp <- sqrt(attr(ranef(fit, condVar=T)[[1]], "postVar"))*1.96 
    
    d <- as.data.frame(ranef(fit)$climatezone) %>%
      tibble::rownames_to_column("zone") 
    
    colnames(d)[1:2] <- c("zone","RE")
    
    n <- length(d)-1
    
    # extract relevant SE values dynamically
    se_vec <- unlist(lapply(1:n, function(i) tp[i, i, ]))
    
    ds <- d %>% 
      gather(scene, R_effect, -zone) %>% 
      mutate(SE = se_vec,
             lower = R_effect - SE,
             upper = R_effect + SE) %>%
      mutate(IV = ipt[i])
    
    
    tp <- sqrt(attr(ranef(fit, condVar=T)[[2]], "postVar"))*1.96 
    
    d <- as.data.frame(ranef(fit)$dac) %>%
      tibble::rownames_to_column("zone") 
    
    colnames(d)[1:2] <- c("zone","RE")
    
    n <- length(d)-1
    
    # extract relevant SE values dynamically
    se_vec <- unlist(lapply(1:n, function(i) tp[i, i, ]))
    
    dc <- d %>% 
      gather(scene, R_effect, -zone) %>% 
      mutate(SE = se_vec,
             lower = R_effect - SE,
             upper = R_effect + SE) %>%
      mutate(IV = ipt[i])
    
  }
  
  
  # dd$var %>% unique() 
  # da$primary_heating_type %>% unique() 
  name_mapping <- c(
    "PV" = "PV", 
    "PS" = "PS",
    "EV" = "EV", 
    "HP" = "HP",
    "IC" = "IC",
    
    "born_us1" = "Born US",
    "upfrontpayback1" = "Upfront prefer",
    "therm_winter1" = "Thermostat use",
    "outage_generatorown1" = "Own generator",
    
    "charging_5mile_f1" = "Fast charger",
    # "vehicle_next_fuel1" = "EV to buy",
    "vehicle_next_used1" = "New car",
    # "vehicle_next_when" = "Later to buy car",
    "vehicle_1_miles" = "Milage",
    "charging_work1" = "Charger at work",
    
    "home_age" = "Newer home",
    "rangeanxiety" = "more range for EV",
    "ccmove_where1" = "Leave CA",
    "homevac" = "Evacuation"
    
  )
  
  reg <- dd %>% 
    mutate(var = recode(var, !!!name_mapping),
           domain = ifelse(str_detect(var,"income|education|race|ideology|pid|US|employment"), "Sociodemo",
                           
                           ifelse(str_detect(var, "home|household"), "Housing",
                                  
                                  ifelse(str_detect(var,"primary|Thermostat"), "Heat/Cool",
                                         
                                         ifelse(var %in% dd$var[grep(pattern="kitchen",
                                                                     dd$var)], "Cook",
                                                
                                                ifelse(str_detect(var,"therm|burden|savemoney|health|safety|Upfront|direct|plan|electrification"), "Behavior",
                                                       
                                                       ifelse(str_detect(var, "outage_impact|freq|dur|generator"), "Resilience", 
                                                              
                                                              ifelse(str_detect(var, "peer"), "Peer Effect", 
                                                                     ifelse(str_detect(var, "Milage|charger|Charger|car|EV|l1l2|dc"), "Vehicle", 
                                                                            ifelse(str_detect(var, "payback|dv|pc"), "WTP",
                                                                                   ifelse(str_detect(var, "cc|CA|Evacuation"), "Climate", var)))))))))),
           domain = factor(domain, levels = c("Housing","Heat/Cool","Cook","Vehicle","Sociodemo","Behavior","Resilience","Climate","Peer Effect","WTP"))) %>% 
    
    mutate(IV = factor(IV, levels = ipt)) %>% 
    
    mutate(color = factor(color, levels = c("Y","N")))
  
  return(list(reg, ds, dc, sum_fit))
}


### regression result plot
reg_plot <- function(data, tech){
  data %>% 
    
    ggplot(aes(x = pe, y = reorder(var, pe), xmin=lwr, xmax=upr)) +
    geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
    
    geom_pointrangeh(aes(fill = color), 
                     position = position_dodge2v(height = 0.4), fatten = 4, size = 0.5,
                     pch=21) +
    
    facet_wrap(~ domain, scale = "free", nrow = 2) + 
    theme_bw() +
    
    coord_flip() +
    
    labs(x = "", y ="", fill = "Significant",
         title = paste0("Impact of adoption or 1sd increase on ",tech)) +
    
    scale_fill_manual(values=c("red", "gray")) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.background =element_rect(fill="gray22",color="gray22"),
          strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=17),
          legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1, color = "black",family="Franklin Gothic Book",size=9),
          axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
          axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
          plot.title=element_text(family="Franklin Gothic Demi", size=20))
}

### reg compare plot
# to compare different datasets with comparing variable: class
com_reg_plot <- function(data, tech){
  
  data %>% 
    
    ggplot(aes(x = pe, y = reorder(var, pe), xmin=lwr, xmax=upr)) +
    geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
    
    geom_pointrangeh(aes(fill = color, color = class), 
                     position = position_dodge2v(height = 0.4), fatten = 4, size = 0.5,
                     pch=21) +
    
    facet_wrap(~ domain, scale = "free", nrow = 2) + 
    theme_bw() +
    
    coord_flip() +
    
    labs(x = "", y ="", fill = "Significant", color = "",
         title = paste0("Impact of adoption or 1sd increase on ",tech)) +
    
    scale_fill_manual(values=c("black", "white")) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.background =element_rect(fill="gray22",color="gray22"),
          strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=17),
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1, color = "black",family="Franklin Gothic Book",size=9),
          axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
          axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
          plot.title=element_text(family="Franklin Gothic Demi", size=20))
}

### random effect string plots 
# zone effect, and name
zone_plot <- function(data, spatial){
  
  colnames(data)[[1]] <- "zone"
  
  if ("scene" %in% names(data)) {
    data <- data %>%
      mutate(scene = factor(scene, levels = unique(scene)))
  }
  
  data %>% 
    ggplot(aes(x = R_effect, y = reorder(zone, R_effect), xmin=lower, xmax=upper,
               color = if (length(data) == 7) scene else NULL)) +
    geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
    
    geom_pointrangeh(position = position_dodge2v(height = 0.4), fatten = 4, size = 0.5) +
    facet_wrap(~IV, nrow = 1, scale = "free") + 

    theme_bw() +
    
    labs(x = "", y ="", fill = "Significant",color = "",
         title = paste0(spatial," random effects")) +
    
    scale_fill_manual(values=c("red", "gray")) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.background =element_rect(fill="gray22",color="gray22"),
          strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
          legend.position = "bottom",
          axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=6),
          axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
          axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
          plot.title=element_text(family="Franklin Gothic Demi", size=20)) 
  
}

### combining random effects for mapping
dat_pr <- function(ds,dc){
  zone_r <- cz %>% 
    left_join(ds, by = c("BZone" = "zone")) %>% 
    dplyr::select(BZone,IV,R_effect)
  
  
  dac_r <- dac_sf %>% 
    mutate(sample = as.character(sample)) %>% 
    left_join(dc, by = c("sample" = "zone")) %>% 
    dplyr::select(sample,IV,R_effect)
  
  
  re_slope <- zone_r %>% 
    st_drop_geometry() %>% 
    inner_join(dac_r %>% 
                 st_drop_geometry(), by = c("IV")) %>% 
    unite(col = "group", BZone, sample, sep = "") %>% 
    mutate(R_effect = R_effect.x + R_effect.y)
  
  
  tp_map <- sc_map%>% 
    left_join(re_slope, by = "group")
  
  return(tp_map)
}


dat_prep <- function(ds,dc){
  zone_r <- cz %>% 
    left_join(ds, by = c("BZone" = "zone")) %>% 
    dplyr::select(BZone,IV,scene,R_effect)
  
  
  dac_r <- dac_sf %>% 
    mutate(sample = as.character(sample)) %>% 
    left_join(dc, by = c("sample" = "zone")) %>% 
    dplyr::select(sample,IV,scene,R_effect)
  
  
  re_slope <- zone_r %>% 
    st_drop_geometry() %>% 
    inner_join(dac_r %>% 
                 st_drop_geometry(), by = c("IV","scene")) %>% 
    unite(col = "group", BZone, sample, sep = "") %>% 
    mutate(R_effect = R_effect.x + R_effect.y)
  
  
  tp_map <- sc_map%>% 
    left_join(re_slope, by = "group")
  
  return(tp_map)
}

### logit data preparation
logit_prep <- function(tp_map, sum_fit, scenario){
  
  sum_fit <- sum_fit %>% 
    mutate(var = ifelse(var == "(Intercept)", "RE", var))
  
  random_effects_wide <- tp_map %>% 
    pivot_wider(names_from = scene, values_from = R_effect) %>% 
    group_by(group) %>% 
    summarise(across(all_of(scenario), ~ sum(.x, na.rm = TRUE), .names = "{.col}")) 
  
  for (i in seq_len(nrow(sum_fit))) {
    var_name <- sum_fit$var[i]
    if (var_name %in% colnames(random_effects_wide)) {
      random_effects_wide[[var_name]] <- random_effects_wide[[var_name]] + sum_fit$Estimate[i]
    }
  }
  
  random_effects1 <- random_effects_wide %>% 
    mutate(across(
      all_of(scenario[-1]),
      ~ .x + get(scenario[1])  # add RE effect row-wise
    )) 

  for (i in seq_len(nrow(sum_fit))) {
    var_name <- sum_fit$var[i]
    if (var_name %in% colnames(random_effects1)) {
      if (var_name == "RE"){
        random_effects1[[var_name]] <- inv.logit(random_effects1[[var_name]]) - inv.logit(sum_fit$Estimate[i])
      }else{
        random_effects1[[var_name]] <- inv.logit(random_effects1[[var_name]]) - inv.logit(sum_fit$Estimate[i]+sum_fit$Estimate[1])
      }
      
    }
  }
  
  tp <- random_effects1 %>% 
    gather(scene, R_effect, scenario)
  
  
  return(tp)
}


### mapping
# input: zone level RE if LPM, the effect is based on mean
# input: zone level RE if logit, the effect is based on sum constrast mean
map_pl <- function(tp_map, der){ # sceanario - variables to map

  map <- tp_map %>%
                ggplot() +
                geom_sf(fill = "white", color = "gray0") + # US border
                geom_sf(aes(fill = R_effect), color = NA, size = 0.3) +
                
                theme_minimal() +
                scale_fill_viridis_c(option = "magma") +
                # scale_fill_distiller(palette = "RdBu", direction = -1) +
                
                
                labs(title = paste0(der, " random effect"), fill = "") +
                theme(legend.position = "right",
                      # legend.text=element_text(size=6),
                      # legend.key.size = unit(0.3, 'cm'),
                      axis.text.x = element_blank(),
                      axis.text.y = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.grid.major = element_blank(),
                      plot.title=element_text(family="Franklin Gothic Demi", size=15, hjust = 0.2))
  return(map)
}


map_plot <- function(tp_map, der, scenario){ # sceanario - variables to map

  tp <- list()
  maps <- map(.x = scenario,
              .f = function(x) tp_map %>%
                filter(scene == x) %>% 
                
                ggplot() +
                geom_sf(fill = "white", color = "gray0") + # US border
                geom_sf(aes(fill = R_effect), color = NA, size = 0.3) +
                
                theme_minimal() +
                scale_fill_viridis_c(option = "magma") +
                # scale_fill_distiller(palette = "RdBu", direction = -1) +
                
                
                labs(title = x, fill = "") +
                theme(legend.position = "right",
                      # legend.text=element_text(size=6),
                      # legend.key.size = unit(0.3, 'cm'),
                      axis.text.x = element_blank(),
                      axis.text.y = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.grid.major = element_blank(),
                      plot.title=element_text(family="Franklin Gothic Demi", size=15, hjust = 0.2))
  ) 
  
  cowplot::plot_grid(plotlist = maps, labels = der, label_size = 20, nrow = 1,
                  label_x = 0, hjust = 0)
}


### separate map
# show those random effects and slopes separately into zone and DAC
sep_map <- function(data,spatial, scenario){
  
  if(spatial == "DAC"){
    da <- dac_sf %>% 
      mutate(sample = as.character(sample)) %>% 
      st_make_valid() %>% 
      left_join(data, by = c("sample" = "zone")) %>% 
      filter(scene %in% scenario)
    
  }else{
    da <- cz%>% 
      st_make_valid() %>% 
      left_join(data, by = c("BZone" = "zone")) %>% 
      filter(scene %in% scenario)
  }

  da %>% 
    mutate(scene = factor(scene, levels = unique(scene))) %>% 
    ggplot() +
    geom_sf(fill = "white", color = "gray0") + # US border
    geom_sf(aes(fill = R_effect), color = NA, size = 0.3) +
    theme_minimal() +
    scale_fill_distiller(palette = "RdBu") +
    # scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    labs(title = spatial, fill = "") +
    facet_wrap(~scene) +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 10),
      plot.title=element_text(family="Franklin Gothic Demi", size=20),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 14, hjust = 0), 
      panel.grid.major = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    )
}
   

#### adoption effect map
# combine all effects to the final adoption
plot_adoption_effects <- function(tp_map, der, scenario, sum_fit){
  
  # # tech adoption mean random effect
  tp <- da %>%
    dplyr::select(!!sym(ipt[der]), wt_ca) %>%
    summarise(mean = weighted.mean((!!sym(ipt[der])), w = wt_ca)) %>%
    pull(mean)
  
  # automate based on different number of scenarios
  tp_map_effects <- tp_map %>% 
    rowwise() %>% # by row calculate
    mutate(Effect = case_when(
      scene == "RE" ~ R_effect + tp,
      scene %in% scenario[-1] ~ R_effect + get_estimate(scene, sum_fit)
    )) %>%
    ungroup()
  
  # Reshape and summarize
  effects_wide <- tp_map_effects %>%
    pivot_wider(names_from = scene, values_from = Effect) %>%
    group_by(group) %>%
    summarise(across(all_of(scenario), ~ sum(.x, na.rm = TRUE), .names = "{.col}")) %>%
    mutate(across(
      all_of(scenario[-1]),
      ~ .x + get(scenario[1])  # add RE effect row-wise
    )) %>%
    pivot_longer(cols = all_of(scenario), names_to = "key", values_to = "R_effect") %>%
    mutate(key = factor(key, levels = scenario))

  
  effects_wide %>% 
    ggplot() +
    geom_sf(fill = "white", color = "gray0") + # US border
    geom_sf(aes(fill = R_effect), color = NA, size = 0.3) +
    facet_wrap(~key) +
    
    theme_minimal() +
    # scale_fill_distiller(palette = "RdBu", direction = -1) +
    scale_fill_viridis_c() +
    
    labs(title = "Final adoption results", fill = "") +
    theme(legend.position = "right",
          # legend.text=element_text(size=6),
          # legend.key.size = unit(0.3, 'cm'),
          strip.text = element_text(size = 14, hjust = 0), 
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.title=element_text(family="Franklin Gothic Demi", size=20))
}


### for LPM, based on MRP baseline, show the final adoption
mrp_effects <- function(tp_map, scenario, sum_fit, mrp){
  
  # automate based on different number of scenarios
  tp_map_effects <- tp_map %>% 
    rowwise() %>% # by row calculate
    mutate(Effect = case_when(
      scene %in% scenario ~ R_effect + get_estimate(scene, sum_fit)
    )) %>%
    ungroup() %>% 
    pivot_wider(names_from = scene, values_from = Effect) %>% 
    group_by(group) %>%
    summarise(across(all_of(scenario), ~ sum(.x, na.rm = TRUE), .names = "{.col}")) 
  
  # Reshape and summarize
  effects_wide <- tp_map_effects %>% 
    left_join(mrp, by = "group") %>% 
    mutate(across(all_of(scenario), ~ .x + Base)) %>% 
    pivot_longer(cols = c("Base",all_of(scenario)), names_to = "key", values_to = "R_effect") %>%
    mutate(key = factor(key, levels = c("Base",scenario)))
  
  return(effects_wide)

}


### for logit, based on MRP baseline, show the final adoption
mrp_logit <- function(tp_map, scenario, mrp){
  
  # Reshape and summarize
  effects_wide <- tp_map %>% 
    pivot_wider(names_from = scene, values_from = R_effect) %>% 
    left_join(mrp, by = "group") %>% 
    mutate(across(all_of(scenario), ~ .x + Base)) %>% 
    pivot_longer(cols = c("Base",all_of(scenario)), names_to = "key", values_to = "R_effect") %>%
    mutate(key = factor(key, levels = c("Base",scenario)))
  
  return(effects_wide)

}


# # glm logit but not random effects cos model not working due to complexity 
# # either glm or lmer
# ### bootstrapping methods
# mreg_prob <- function(data, data_ref, data_scen, remove = NULL, tech, scenario = NULL, future = NULL){
#   
#   # data <- da
#   # data_ref <- ref
#   # data_scen <- dat1
#   # remove = c("education","employment")
#   # tech = 1
#   # future = 1
#   
#   da_r <- data %>% 
#     dplyr::select(-group) %>% 
#     dplyr::select(# remove multicollinear variables
#       
#       # remove predictors with substantial NAs
#       -heatpump_direct,-induction_direct,
#       
#       # remove future adoption
#       -future_PV,-future_PS,-future_EV,-future_HP,-future_IC
#     ) %>% 
#     dplyr::select(-remove)
#   
#   if(!is.null(future)){
#     da_r <- data %>% 
#       dplyr::select(-group) %>% 
#       mutate(PV = future_PV,
#              PS = future_PS,
#              EV = future_EV,
#              HP = future_HP,
#              IC = future_IC) %>% 
#       dplyr::select(# remove multicollinear variables
#         
#         # remove predictors with substantial NAs
#         -heatpump_direct,-induction_direct,
#         
#         # remove future adoption
#         -future_PV,-future_PS,-future_EV,-future_HP,-future_IC
#       ) %>% 
#       dplyr::select(-remove)
#   }
#   
# 
#   if(tech == 1){
#     # for PV, remove zone effect, tech
#     tract <- c("climatezone","dac","ev_wtp_pc","heatpump_wtp_pc","induction_dv","wt_ca",
#                "peer_EV","peer_HP","peer_IC",
#                "PV","PS","EV","HP","IC")
#     
#     model1vars <- setdiff(names(da_r), tract)
#     
#   }else if(tech == 2){
#     # for EV, remove zone effect, tech
#     tract <- c("climatezone","dac","solstor_wtp_dv","heatpump_wtp_pc","induction_dv","wt_ca",
#                "peer_PV","peer_HP","peer_IC",
#                "PV","PS","EV","HP","IC")
#     model1vars <- setdiff(names(da_r), tract)
#     
#   }else if(tech == 3){
#     # for HP, remove zone effect, tech, heating/cooling type
#     tract <- c("climatezone","dac","ev_wtp_pc","solstor_wtp_dv","induction_dv","wt_ca",
#                "peer_EV","peer_PV","peer_IC",
#                "PV","PS","EV","HP","IC","primary_heating_type","primary_cooling_type")
#     model1vars <- setdiff(names(da_r), tract)
#     
#   }else{
#     # for IC, remove zone effect, tech, cooking type
#     tract <- c("climatezone","dac","ev_wtp_pc","solstor_wtp_dv","heatpump_wtp_pc","wt_ca",
#                "peer_EV","peer_HP","peer_PV",
#                "PV","PS","EV","HP","IC","kitchen_range_type")
#     model1vars <- setdiff(names(da_r), tract)
#   }
#   
#   if (is.null(scenario)) {
#     fvar <- as.formula(paste(ipt[tech], " ~", paste(model1vars, collapse = " + "), 
#                              "+ climatezone + dac"))
#   } else {
#     fvar <- as.formula(paste(ipt[tech], " ~", paste(model1vars, collapse = " + "), 
#                              "+ (1+",paste(scenario, collapse = " + "),"|climatezone) + (1+",paste(scenario, collapse = " + "),
#                              "|dac)"))
#   }
#   
#   # fit <- glmer(fvar, family = binomial(link = "logit"), data = da_r,
#   #              nAGQ = 0, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000),calc.derivs = FALSE))
#   
#   fit <- glm(fvar, family = binomial(link = "logit"), data = da_r)
# 
#   # fit <- lmer(fvar, data = data)
#   # summary(fit)
#   
#   data_ref$pred <- predict(fit, type = "response", newdata = data_ref)
#   data_scen$pred <- predict(fit, type = "response", newdata = data_scen)
#   
#   d <- data_ref %>% 
#     group_by(group) %>% 
#     summarise(Baseline = mean(pred, na.rm = T)) %>% 
#     left_join(  data_scen %>% 
#                   group_by(group) %>% 
#                   summarise(Scenario = mean(pred, na.rm = T)), by = "group") %>% 
#     mutate(Effects = Scenario - Baseline) %>% 
#     dplyr::select(-Baseline:-Scenario)
#   
# 
#   return(d)
# }
# 
# ### under built for propensity weight methods
# propensity_prob <- function(data, data_ref, data_scen, remove = NULL, tech, scenario = NULL, future = NULL){
#   
#   # data <- da
#   # data_ref <- ref
#   # data_scen <- dat1
#   # remove = c("education","employment")
#   # tech = 1
#   # future = 1
#   
#   da_r <- data %>% 
#     dplyr::select(-group) %>% 
#     dplyr::select(# remove multicollinear variables
#       
#       # remove predictors with substantial NAs
#       -heatpump_direct,-induction_direct,
#       
#       # remove future adoption
#       -future_PV,-future_PS,-future_EV,-future_HP,-future_IC
#     ) %>% 
#     dplyr::select(-remove)
#   
#   if(!is.null(future)){
#     da_r <- data %>% 
#       dplyr::select(-group) %>% 
#       mutate(PV = future_PV,
#              PS = future_PS,
#              EV = future_EV,
#              HP = future_HP,
#              IC = future_IC) %>% 
#       dplyr::select(# remove multicollinear variables
#         
#         # remove predictors with substantial NAs
#         -heatpump_direct,-induction_direct,
#         
#         # remove future adoption
#         -future_PV,-future_PS,-future_EV,-future_HP,-future_IC
#       ) %>% 
#       dplyr::select(-remove)
#   }
#   
#   
#   if(tech == 1){
#     # for PV, remove zone effect, tech
#     tract <- c("climatezone","dac","ev_wtp_pc","heatpump_wtp_pc","induction_dv","wt_ca",
#                "peer_EV","peer_HP","peer_IC",
#                "PV","PS","EV","HP","IC")
#     
#     model1vars <- setdiff(names(da_r), tract)
#     
#   }else if(tech == 2){
#     # for EV, remove zone effect, tech
#     tract <- c("climatezone","dac","solstor_wtp_dv","heatpump_wtp_pc","induction_dv","wt_ca",
#                "peer_PV","peer_HP","peer_IC",
#                "PV","PS","EV","HP","IC")
#     model1vars <- setdiff(names(da_r), tract)
#     
#   }else if(tech == 3){
#     # for HP, remove zone effect, tech, heating/cooling type
#     tract <- c("climatezone","dac","ev_wtp_pc","solstor_wtp_dv","induction_dv","wt_ca",
#                "peer_EV","peer_PV","peer_IC",
#                "PV","PS","EV","HP","IC","primary_heating_type","primary_cooling_type")
#     model1vars <- setdiff(names(da_r), tract)
#     
#   }else{
#     # for IC, remove zone effect, tech, cooking type
#     tract <- c("climatezone","dac","ev_wtp_pc","solstor_wtp_dv","heatpump_wtp_pc","wt_ca",
#                "peer_EV","peer_HP","peer_PV",
#                "PV","PS","EV","HP","IC","kitchen_range_type")
#     model1vars <- setdiff(names(da_r), tract)
#   }
#   
#   if (is.null(scenario)) {
#     fvar <- as.formula(paste(ipt[tech], " ~", paste(model1vars, collapse = " + "), 
#                              "+ climatezone + dac"))
#   } else {
#     fvar <- as.formula(paste(ipt[tech], " ~", paste(model1vars, collapse = " + "), 
#                              "+ (1+",paste(scenario, collapse = " + "),"|climatezone) + (1+",paste(scenario, collapse = " + "),
#                              "|dac)"))
#   }
#   
#   # fit <- glmer(fvar, family = binomial(link = "logit"), data = da_r,
#   #              nAGQ = 0, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000),calc.derivs = FALSE))
#   
#   fit <- glm(fvar, family = binomial(link = "logit"), data = da_r)
#   
#   # fit <- lmer(fvar, data = data)
#   # summary(fit)
#   
#   data_ref$pred <- predict(fit, type = "response", newdata = data_ref)
#   data_scen$pred <- predict(fit, type = "response", newdata = data_scen)
#   
#   d <- data_ref %>% 
#     group_by(group) %>% 
#     summarise(Baseline = mean(pred, na.rm = T)) %>% 
#     left_join(  data_scen %>% 
#                   group_by(group) %>% 
#                   summarise(Scenario = mean(pred, na.rm = T)), by = "group") %>% 
#     mutate(Effects = Scenario - Baseline) %>% 
#     dplyr::select(-Baseline:-Scenario)
#   
#   
#   return(d)
# }

freg_logit <- function(data, remove = NULL, i, scenario = NULL, future = NULL){
  
  # data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1)
  # remove <- c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv", "education","employment")
  # scenario <- c("peer_EV","charging_5mile_f")
  # i <- 2
  # future <- 120
  
  da_r <- data %>% 
    dplyr::select(# remove multicollinear variables
      
      # remove predictors with substantial NAs
      -heatpump_direct,-induction_direct,
      
      # remove future adoption
      -starts_with("future"),
    ) %>% 
    dplyr::select(-remove)
  
  if(i == 1){
    future_var <- sym(paste0("future_", ipt[i]))
  }else{
    future_var <- sym(paste0("future_", ipt[i],"_",future))
  }
  future_var <- sym(paste0("future_", ipt[i],"_",future))
  
  if(!is.null(future)){
    da_r <- data %>% 
      mutate(!!ipt[i] := .data[[future_var]]) %>% 
      
      dplyr::select(# remove multicollinear variables
        
        # remove predictors with substantial NAs
        -heatpump_direct,-induction_direct,
        
        # remove future adoption
        -starts_with("future")
      ) %>% 
      dplyr::select(-remove)
  }
  
  
  # binary
  bina <- c(
    "born_us",
    
    # "charging_5mile_f",
    "vehicle_next_used",
    # "vehicle_next_fuel",
    "charging_work",
    
    "therm_winter",
    
    "upfrontpayback",
    "outage_generatorown",
    "ccmove_where"
  ) %>% 
    setdiff(remove)
  
  # categorical variables: 
  cat <- c("race",
           # "born_us",
           "home_type",
           "employment",
           "home_age",
           
           "peer_EV",
           "peer_PV",
           "peer_HP",
           "peer_IC",
           
           # "charging_5mile_f",
           # "vehicle_next_used",
           # "vehicle_next_fuel",
           # "charging_work",
           
           "primary_heating_type",
           "primary_cooling_type",
           # "therm_winter",
           
           "kitchen_range_type",
           "electrification"
           # "upfrontpayback",
           # "outage_generatorown",
           # "ccmove_where"
  ) %>% 
    setdiff(remove)
  
  exclusions <- list(
    c("peer_EV", "peer_HP", "peer_IC"),
    c("peer_PV", "peer_HP", "peer_IC"),
    c("peer_EV", "peer_PV", "peer_IC"),
    c("peer_EV", "peer_HP", "peer_PV")
  )
  
  if(i %in% c(1,5)){
    cat <- cat %>% setdiff(exclusions[[1]])
  }else{
    cat <- cat %>% setdiff(exclusions[[i]])
  }
  
  ftr <- c("climatezone",
           "dac",
           "race",
           "born_us",
           "home_type",
           "employment",
           "home_age",
           
           "peer_EV",
           "peer_PV",
           "peer_HP",
           "peer_IC",
           
           "charging_5mile_f",
           "vehicle_next_used",
           # "vehicle_next_fuel",
           "charging_work",
           
           "primary_heating_type",
           "primary_cooling_type",
           "therm_winter",
           
           "kitchen_range_type",
           "electrification",
           "upfrontpayback",
           "outage_generatorown",
           "ccmove_where"
  ) %>% 
    setdiff(remove)
  
  # da$race %>% unique
  da_r[,ftr] <- data.frame(lapply(da_r[ftr],as.factor)) 
  
  
  # dummy sum contrast based on reference category (-1)
  for (var in cat) {
    k <- length(levels(da_r[[var]]))
    contrasts(da_r[[var]]) <- contr.sum(k)
    colnames(contrasts(da_r[[var]])) <- levels(da_r[[var]])[1:(k-1)]
  }
  
  # for binary
  for (var in bina) {
    da_r[[var]] <- factor(da_r[[var]], levels = c(1,0))
    contrasts(da_r[[var]]) <- contr.sum(2)
    colnames(contrasts(da_r[[var]])) <- levels(da_r[[var]])[1]
  }
  
  
  da_r <- da_r %>% 
    mutate(across(where(is.numeric) & !c("PV","PS","EV","HP","IC","wt_ca"), ~ scale(.) %>% as.numeric())) %>% 
    mutate(across(any_of(c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv")), ~ .x * -1)) %>% 
    mutate(across(starts_with("peer"), ~factor(.x, levels = c("neighbor","peer","none"))))

  
  # lapply(da_r, unique)
  # summary(da_r)
  
  if(i %in% c(1,5)){
    # for PV, remove zone effect, tech, peer
    tract <- c("climatezone","dac","ev_wtp_pc","heatpump_wtp_pc","induction_dv","wt_ca",
               "peer_EV","peer_HP","peer_IC",
               "PV","PS","EV","HP","IC",
               "charging_5mile_f","rangeanxiety","vehicle_1_miles","charging_work","vehicle_next_used")
    
    model1vars <- setdiff(names(da_r), tract)
    
  }else if(i == 2){
    # for EV, remove zone effect, tech
    tract <- c("climatezone","dac","solstor_wtp_dv","heatpump_wtp_pc","induction_dv","wt_ca",
               "peer_PV","peer_HP","peer_IC",
               "PV","PS","EV","HP","IC")
    model1vars <- setdiff(names(da_r), tract)
    
  }else if(i == 3){
    # for HP, remove zone effect, tech, heating/cooling type
    tract <- c("climatezone","dac","ev_wtp_pc","solstor_wtp_dv","induction_dv","wt_ca",
               "peer_EV","peer_PV","peer_IC",
               "PV","PS","EV","HP","IC","primary_heating_type","primary_cooling_type",
               "charging_5mile_f","rangeanxiety","vehicle_1_miles","charging_work","vehicle_next_used")
    model1vars <- setdiff(names(da_r), tract)
    
  }else{
    # for IC, remove zone effect, tech, cooking type
    tract <- c("climatezone","dac","ev_wtp_pc","solstor_wtp_dv","heatpump_wtp_pc","wt_ca",
               "peer_EV","peer_HP","peer_PV",
               "PV","PS","EV","HP","IC","kitchen_range_type",
               "charging_5mile_f","rangeanxiety","vehicle_1_miles","charging_work","vehicle_next_used")
    model1vars <- setdiff(names(da_r), tract)
  }
  
  model1vars <- setdiff(model1vars, scenario)
  
  if (is.null(scenario)) {
    fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), 
                             "+ (1|climatezone) + (1|dac)"))
  } else {
    fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), 
                             "+ (1+",paste(scenario, collapse = " + "),"|climatezone) + (1+",paste(scenario, collapse = " + "),
                             "|dac)"))
  }
  
  fit <- glmer(fvar, weights = wt_ca, family = binomial(link = "logit"), data = da_r,
               nAGQ = 0, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000),calc.derivs = FALSE))
  # summary(fit)
  sum_fit <- summary(fit)$coef %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("var") %>% 
    dplyr::select(var, Estimate)
  

    names <- ranef(fit)[[1]] %>% colnames()
    ds <- ranef(fit)[[1]] %>% 
      rownames_to_column("zone") 
    
    dc <- ranef(fit)[[2]] %>% 
      rownames_to_column("zone") 
    
    zone_r <- cz %>% 
      left_join(ds %>% 
                  gather(scene, R_effect, names), by = c("BZone" = "zone")) %>% 
      dplyr::select(BZone,scene,R_effect)
    
    dac_r <- dac_sf %>% 
      mutate(sample = as.character(sample)) %>% 
      left_join(dc %>% 
                  gather(scene, R_effect, names), by = c("sample" = "zone")) %>% 
      dplyr::select(sample,scene,R_effect)
    
    re_slope <- zone_r %>% 
      st_drop_geometry() %>% 
      inner_join(dac_r %>% 
                   st_drop_geometry(), by = c("scene")) %>% 
      unite(col = "group", BZone, sample, sep = "") %>% 
      mutate(R_effect = R_effect.x + R_effect.y) %>% 
      dplyr::select(-R_effect.x, -R_effect.y) %>% 
      pivot_wider(names_from = scene, values_from = R_effect) 
    
    # add random effects to the average effects
    for(k in seq_along(names)){
      re_slope <- re_slope %>% 
        mutate(across(all_of(names[k]), ~ .x + get_estimate(names[k], sum_fit)))
    }
    
    re <- re_slope %>% 
      mutate(across(all_of(names[-1]), ~ .x + get(names[1]))) %>% # to have total effects
      mutate(across(all_of(names), ~ inv.logit(.x))) %>%  # convert to probability
      mutate(across(all_of(names[-1]), ~ .x - get(names[1]))) %>% # get marginal effects
      dplyr::select(-names[1]) %>% 
      gather(scene, R_effect, names[-1]) %>% 
      mutate(IV = ipt[i])
    
    re <- sc_map%>% 
      left_join(re, by = "group")
    
    return(re)

}


freg_lpm <- function(data, remove = NULL, i, scenario = NULL, future = NULL){
  
  # data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1)
  # remove <- c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv", "education","employment")
  # scenario <- c("peer_EV","charging_5mile_f","rangeanxiety")
  # 
  # future <- 120
  
  da_r <- data %>% 
    dplyr::select(# remove multicollinear variables
      
      # remove predictors with substantial NAs
      -heatpump_direct,-induction_direct,
      
      # remove future adoption
      -starts_with("future"),
    ) %>% 
    dplyr::select(-remove)
  
  if(i == 1){
    future_var <- sym(paste0("future_", ipt[i]))
  }else{
    future_var <- sym(paste0("future_", ipt[i],"_",future))
  }
  
  
  if(!is.null(future)){
    da_r <- data %>% 
      mutate(!!ipt[i] := .data[[future_var]]) %>% 
      
      dplyr::select(# remove multicollinear variables
        
        # remove predictors with substantial NAs
        -heatpump_direct,-induction_direct,
        
        # remove future adoption
        -starts_with("future")
      ) %>% 
      dplyr::select(-remove)
  }
  
  
  # binary
  bina <- c(
    "born_us",
    
    # "charging_5mile_f",
    "vehicle_next_used",
    # "vehicle_next_fuel",
    "charging_work",
    
    "therm_winter",
    
    "upfrontpayback",
    "outage_generatorown",
    "ccmove_where"
  ) %>% 
    setdiff(remove)
  
  # categorical variables: 
  cat <- c("race",
           # "born_us",
           "home_type",
           "employment",
           "home_age",
           
           "peer_EV",
           "peer_PV",
           "peer_HP",
           "peer_IC",
           
           # "charging_5mile_f",
           # "vehicle_next_used",
           # "vehicle_next_fuel",
           # "charging_work",
           
           "primary_heating_type",
           "primary_cooling_type",
           # "therm_winter",
           
           "kitchen_range_type",
           "electrification"
           # "upfrontpayback",
           # "outage_generatorown",
           # "ccmove_where"
  ) %>% 
    setdiff(remove)
  
  exclusions <- list(
    c("peer_EV", "peer_HP", "peer_IC"),
    c("peer_PV", "peer_HP", "peer_IC"),
    c("peer_EV", "peer_PV", "peer_IC"),
    c("peer_EV", "peer_HP", "peer_PV")
  )
  
  if(i %in% c(1,5)){
    cat <- cat %>% setdiff(exclusions[[1]])
  }else{
    cat <- cat %>% setdiff(exclusions[[i]])
  }
  
  ftr <- c("climatezone",
           "dac",
           "race",
           "born_us",
           "home_type",
           "employment",
           "home_age",
           
           "peer_EV",
           "peer_PV",
           "peer_HP",
           "peer_IC",
           
           "charging_5mile_f",
           "vehicle_next_used",
           # "vehicle_next_fuel",
           "charging_work",
           
           "primary_heating_type",
           "primary_cooling_type",
           "therm_winter",
           
           "kitchen_range_type",
           "electrification",
           "upfrontpayback",
           "outage_generatorown",
           "ccmove_where"
  ) %>% 
    setdiff(remove)
  
  # da$race %>% unique
  da_r[,ftr] <- data.frame(lapply(da_r[ftr],as.factor)) 
  
  # dummy sum contrast based on reference category (-1)
  for (var in cat) {
    k <- length(levels(da_r[[var]]))
    contrasts(da_r[[var]]) <- contr.sum(k)
    colnames(contrasts(da_r[[var]])) <- levels(da_r[[var]])[1:(k-1)]
  }
  
  # for binary
  for (var in bina) {
    da_r[[var]] <- factor(da_r[[var]], levels = c(1,0))
    contrasts(da_r[[var]]) <- contr.sum(2)
    colnames(contrasts(da_r[[var]])) <- levels(da_r[[var]])[1]
  }
  
  
  da_r <- da_r %>% 
    mutate(across(where(is.numeric) & !c("PV","PS","EV","HP","IC","wt_ca"), ~ scale(.) %>% as.numeric())) %>% 
    mutate(across(any_of(c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv")), ~ .x * -1)) %>% 
    mutate(across(starts_with("peer"), ~factor(.x, levels = c("neighbor","peer","none")))) # avoid the sum contrast
  
  # lapply(da_r, unique)
  # summary(da_r)
  
  if(i %in% c(1,5)){
    # for PV, remove zone effect, tech, peer
    tract <- c("climatezone","dac","ev_wtp_pc","heatpump_wtp_pc","induction_dv","wt_ca",
               "peer_EV","peer_HP","peer_IC",
               "PV","PS","EV","HP","IC",
               "charging_5mile_f","rangeanxiety","vehicle_1_miles","charging_work","vehicle_next_used")
    
    model1vars <- setdiff(names(da_r), tract)
    
  }else if(i == 2){
    # for EV, remove zone effect, tech
    tract <- c("climatezone","dac","solstor_wtp_dv","heatpump_wtp_pc","induction_dv","wt_ca",
               "peer_PV","peer_HP","peer_IC",
               "PV","PS","EV","HP","IC")
    model1vars <- setdiff(names(da_r), tract)
    
  }else if(i == 3){
    # for HP, remove zone effect, tech, heating/cooling type
    tract <- c("climatezone","dac","ev_wtp_pc","solstor_wtp_dv","induction_dv","wt_ca",
               "peer_EV","peer_PV","peer_IC",
               "PV","PS","EV","HP","IC","primary_heating_type","primary_cooling_type",
               "charging_5mile_f","rangeanxiety","vehicle_1_miles","charging_work","vehicle_next_used")
    model1vars <- setdiff(names(da_r), tract)
    
  }else{
    # for IC, remove zone effect, tech, cooking type
    tract <- c("climatezone","dac","ev_wtp_pc","solstor_wtp_dv","heatpump_wtp_pc","wt_ca",
               "peer_EV","peer_HP","peer_PV",
               "PV","PS","EV","HP","IC","kitchen_range_type",
               "charging_5mile_f","rangeanxiety","vehicle_1_miles","charging_work","vehicle_next_used")
    model1vars <- setdiff(names(da_r), tract)
  }
  
  model1vars <- setdiff(model1vars, scenario)
  
  if (is.null(scenario)) {
    fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), 
                             "+ (1|climatezone) + (1|dac)"))
  } else {
    fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), 
                             "+ (1+",paste(scenario, collapse = " + "),"|climatezone) + (1+",paste(scenario, collapse = " + "),
                             "|dac)"))
  }
  
  fit <- lmer(fvar, weights = wt_ca, data = da_r)
  # summary(fit)
  sum_fit <- summary(fit)$coef %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("var") %>% 
    dplyr::select(var, Estimate)
  
  
  names <- ranef(fit)[[1]] %>% colnames()
  ds <- ranef(fit)[[1]] %>% 
    rownames_to_column("zone") 
  
  dc <- ranef(fit)[[2]] %>% 
    rownames_to_column("zone") 
  
  zone_r <- cz %>% 
    left_join(ds %>% 
                gather(scene, R_effect, names), by = c("BZone" = "zone")) %>% 
    dplyr::select(BZone,scene,R_effect)
  
  dac_r <- dac_sf %>% 
    mutate(sample = as.character(sample)) %>% 
    left_join(dc %>% 
                gather(scene, R_effect, names), by = c("sample" = "zone")) %>% 
    dplyr::select(sample,scene,R_effect)
  
  re_slope <- zone_r %>% 
    st_drop_geometry() %>% 
    inner_join(dac_r %>% 
                 st_drop_geometry(), by = c("scene")) %>% 
    unite(col = "group", BZone, sample, sep = "") %>% 
    mutate(R_effect = R_effect.x + R_effect.y) %>% 
    dplyr::select(-R_effect.x, -R_effect.y) %>% 
    pivot_wider(names_from = scene, values_from = R_effect) 
  
  
  re <- re_slope %>% 
    dplyr::select(-names[1]) %>% 
    gather(scene, R_effect, names[-1]) %>% 
    mutate(IV = ipt[i])
  
  return(re)
}


### including peer effects
final_ef_peer <- function(data, i, future){
  
  scenario <- if (i %in% c(1, 5)) {
    c("peer_PV", "home_age")
  } else if (i == 2) {
    c("peer_EV", "charging_5mile_f", "rangeanxiety", "home_age")
  } else if (i == 3) {
    "peer_HP"
  } else if (i == 4) {
    c("peer_IC", "home_age")
  } else {
    NA_character_
  }
  
  remove <- c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv", "education","employment")
  
  # lpm
  re <- freg_lpm(data, remove, i, scenario, future)
  
  # logit
  # re <- freg_logit(data, remove, i, scenario, future)
  if(future > 100){ # if optimistic
    if(i %in% c(1,5)){
      re <- re %>%
        mutate(R_effect = case_when(
          str_detect(scene, "none") ~ -0.23*R_effect,
          str_detect(scene, "peer") ~ -0.14*R_effect,
          str_detect(scene, "Older") ~ -0.2*R_effect
        ))
    }else if(i == 3){
      re <- re %>%
        mutate(R_effect = case_when(
          str_detect(scene, "none") ~ -0.25*R_effect
        ))
    }else if(i == 4){
      re <- re %>%
        mutate(R_effect = case_when(
          str_detect(scene, "none") ~ -0.17*R_effect,
          str_detect(scene, "Older") ~ -0.2*R_effect
        ))
    }else{
      re <- re %>%
        mutate(R_effect = case_when(
          str_detect(scene, "none") ~ -0.26*R_effect,
          str_detect(scene, "peer") ~ -0.04*R_effect,
          str_detect(scene, "charging") ~ 0.32*R_effect,
          str_detect(scene, "Older") ~ -0.2*R_effect,
          str_detect(scene, "range") ~ -2*R_effect # 100 mile different is 1 sd
        ))
    }
    
  }else{ # if pessimistic
    if(i %in% c(1,5)){
      re <- re %>%
        mutate(R_effect = case_when(
          str_detect(scene, "none") ~ -0.18*R_effect,
          str_detect(scene, "Older") ~ -0.09*R_effect
        ))
    }else if(i == 3){
      re <- re %>%
        mutate(R_effect = case_when(
          str_detect(scene, "none") ~ -0.11*R_effect
        ))
    }else if(i == 4){
      re <- re %>%
        mutate(R_effect = case_when(
          str_detect(scene, "none") ~ -0.07*R_effect,
          str_detect(scene, "Older") ~ -0.09*R_effect
        ))
    }else{
      re <- re %>%
        mutate(R_effect = case_when(
          str_detect(scene, "none") ~ -0.15*R_effect,
          str_detect(scene, "charging") ~ 0.16*R_effect,
          str_detect(scene, "range") ~ -1*R_effect,
          str_detect(scene, "Older") ~ -0.09*R_effect
        ))
    }
  }
  
  ### scenario
  # which WTP?, which peer?, which charger?
  crss <- re %>%
    pivot_wider(names_from = scene, values_from = R_effect) %>% 
    mutate(Effect = rowSums(dplyr::select(., where(is.numeric)), na.rm = TRUE)) 
  
  crss <- sc_map%>% 
    left_join(crss, by = "group") %>% 
    
    st_make_valid() %>%
    st_intersection(CA_t %>% dplyr::select(GEOID) %>%
                      st_make_valid())
  
  # calculate intersected areas
  crss$area <- st_area(crss) %>% as.numeric()
  
  # aggregate at puma level with area weights
  f_crss <- crss %>%
    st_drop_geometry() %>% 
    group_by(GEOID) %>%
    summarise(across(where(is.numeric), ~ weighted.mean(.x, area, na.rm = TRUE))) %>% 
    dplyr::select(-area)
  
  # should be modified after real MRP data received
  if(i == 1){
    Mrp <- mrp %>%
      dplyr::select(GEOID, which(str_detect(names(.), paste0("future_",ipt[i]))))
  }else{
    Mrp <- mrp %>%
      dplyr::select(GEOID, which(str_detect(names(.), paste0("future_",ipt[i],"_",future))))
  }
  
  colnames(Mrp)[2] <- "MRP"
  
  ### final adoption
  f_d <- f_crss %>%
    left_join(Mrp, by = "GEOID") %>%
    mutate(Final = MRP + Effect)
  
  return(f_d)
  
}


# ### excluding peer effects
# final_ef <- function(data, i, future){
#   
#   scenario <- if (i %in% c(1,5)) {
#     c("home_age")
#   } else if (i == 2) {
#     c("charging_5mile_f", "rangeanxiety")
#   } else {
#     NA_character_
#   }
#   
#   remove <- c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv", "education","employment")
#   
#   # lpm
#   re <- freg_lpm(data, remove, i, scenario, future) 
#   
#   # logit
#   # re <- freg_logit(data, remove, i, scenario, future)
#   if(future > 100){ # if optimistic
#     if(i %in% c(1,5)){
#       re <- re %>% 
#         mutate(R_effect = case_when(
#           str_detect(scene, "Older") ~ -0.22*R_effect
#         ))
#     }else{
#       re <- re %>% 
#         mutate(R_effect = case_when(
#           str_detect(scene, "charging") ~ 0.32*R_effect,
#           str_detect(scene, "range") ~ -2*R_effect
#         ))
#     }
#     
#   }else{ # if pessimistic
#     if(i %in% c(1,5)){
#       re <- re %>% 
#         mutate(R_effect = case_when(
#           str_detect(scene, "Older") ~ -0.11*R_effect
#         ))
#     }else{
#       re <- re %>% 
#         mutate(R_effect = case_when(
#           str_detect(scene, "charging") ~ 0.16*R_effect,
#           str_detect(scene, "range") ~ -1*R_effect
#         ))
#     }
#   }
#   
#   
#   
#   
#   ### scenario
#   # which WTP?, which peer?, which charger?
#   crss <- re %>% 
#     group_by(group) %>% 
#     summarise(Effect = sum(R_effect, na.rm = T)) %>% 
#     
#     st_make_valid() %>% 
#     st_intersection(CA_t %>% dplyr::select(GEOID) %>% 
#                       st_make_valid())
#   
#   # calculate intersected areas
#   crss$area <- st_area(crss) %>% as.numeric()
#   
#   # aggregate at puma level with area weights
#   f_crss <- crss %>% 
#     st_make_valid() %>% 
#     group_by(GEOID) %>% 
#     summarise(across(Effect, ~ weighted.mean(.x, area))) %>% 
#     st_drop_geometry()
#   
#   
#   # should be modified after real MRP data received
#   Mrp <- mrp %>% 
#     dplyr::select(GEOID, which(str_detect(names(.), paste0("future_",ipt[i],"_",future))))
#   
#   colnames(Mrp)[2] <- "MRP"
#   
#   ### final adoption
#   f_d <- f_crss %>% 
#     left_join(Mrp, by = "GEOID") %>% 
#     mutate(Final = MRP + Effect)
#   
#   return(f_d)
#   
# }


data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1)
mrp <- read_csv("./data/raw/mrp_scenariovars_tract.csv") %>% 
  mutate(GEOID = str_sub(geoid_tract2020, 10)) %>%
  dplyr::select(-geoid_tract2020,-future_EV10) %>% 
  mutate(future_PS_0 = ifelse(future_PS_0 > future_PS_72, future_PS_72, future_PS_0))