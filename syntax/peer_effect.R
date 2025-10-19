source("./syntax/Function.R")
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

# data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
#   data_process(ev = c("Fully electric"))

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

load("../adoption/data/dac_sf.RData") # CA_t, cz, dac_sf, sc_map (DAC, climate zone spatial data)

### add urban variable
data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
  data_process(ev = c("Fully electric")) %>% 
  left_join(CA_t %>% 
              st_drop_geometry() %>% 
              dplyr::select(GEOID, estimate), by = c("tractid" = "GEOID")) %>% 
  mutate(urban = ifelse(estimate > 7000, "Urban", "Rural")) %>% 
  mutate(dac = ifelse(dac == 1 & urban == "Urban", "Urban_DAC",
                      ifelse(dac == 1 & urban == "Rural", "Rural_DAC",
                             ifelse(dac == 0 & urban == "Urban", "Urban_Non_DAC", "Rural_Non_DAC")))) %>% 
  dplyr::select(-estimate, -urban) %>% 
  data_clean(1)


### peer effects 
select_scene <- list(c("home_age","peer_PV"),
                     c("home_age","peer_EV","charging_5mile_f","rangeanxiety"),
                     c("peer_HP"),
                     c("home_age","peer_IC"),
                     c("home_age","peer_PV"))

peer <- data.frame()
for(k in 1:5){
  
  b_ev <- mreg_dac(data, 
                   remove = c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv",
                              "education","employment"),
                   scenario = select_scene[[k]],
                   i = k,
                   future = fut[[k]][1],
                   peer = 1)
  
  num <- length(b_ev[[1]])
  
  df <- b_ev[[1]] %>% 
    dplyr::select(matches("peer")) %>% 
    dplyr::rename(Peer = names(.)[[1]],
                  Neighbor = names(.)[[2]],
                  Both = names(.)[[3]]) %>% 
  
    mutate(dac = row.names(.),
           tech = ipt[k]) %>% 
    gather(key, value, Peer:Both) %>% 
    mutate(SE = c(b_ev[[2]][num-2,num-2,],
                  b_ev[[2]][num-1,num-1,],
                  b_ev[[2]][num,num,]))
  
  peer <- rbind(df, peer)
}

f4c <- peer %>%
  mutate(urban = ifelse(dac %in% c("Urban_DAC","Urban_Non_DAC"), "Urban", "Rural"),
         dac = ifelse(dac %in% c("Urban_DAC","Rural_DAC"), "DAC", "Non-DAC")) %>% 
  mutate(
    tech = recode(tech,
                  "PS" = "PV + Storage",
                  "IC" = "Induction stoves",
                  "HP" = "Heat pumps",
                  "EV" = "Electric vehicles",
                  "PV" = "Photovoltaics"),
    tech = factor(tech, levels = c("Photovoltaics","PV + Storage", "Electric vehicles", "Heat pumps", "Induction stoves")),
    # dac = factor(dac, levels = c("Urban_DAC", "Urban_Non_DAC", "Rural_DAC","Rural_Non_DAC")),
    key = factor(key, levels = c("Both","Neighbor", "Peer"))
  ) %>% 
  ggplot(aes(x = key, y = value, fill = dac)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(
    aes(ymin = value - SE, ymax = value + SE),
    position = position_dodge(width = 0.7),
    width = 0.2
  ) +
  
  facet_grid(urban ~ tech, switch = "y") +
  # scale_fill_manual(values = c(
  #   "Urban_DAC"      = "#d94801",  # rich burnt orange
  #   "Rural_DAC"      = "#fdae6b",  # soft apricot
  #   "Urban_Non_DAC"  = "#238b45",  # deep forest green
  #   "Rural_Non_DAC"  = "#a1d99b"   # light sage green
  # ),
  # 
  # labels = c("Urban DAC", "Urban Non-DAC", "Rural DAC", "Rural Non-DAC")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "",
    y = "Effect magnitude",
    fill = "",
    title = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 16),
    legend.text = element_text(size = 10),
    legend.position = "bottom",
    
    strip.placement = "outside", # Keep labels on the outside
    strip.background =element_rect(fill="gray22",color="gray22"),
    strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=14, face = "bold"),
    strip.text.y.left = element_text(angle = 0), # Ensure domain labels are horizontal
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) 


f4 <- ggarrange(
  ggarrange(f4a, f4b, nrow = 2), f4c, nrow = 2,
                heights = c(1,1),
                labels = c("A", "B"),  # Adds labels to plots
                label.x = 0,        # Adjust horizontal position of labels
                label.y = 1,        # Adjust vertical position of labels
                font.label = list(size = 14, face = "bold")
)


ggsave("./fig/f4.png",
       f4,
       width = 12, height = 12)


