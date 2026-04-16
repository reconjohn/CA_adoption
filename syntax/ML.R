source("./syntax/Function.R")
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)


### including WTP
data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1) %>% 
  dplyr::select(-c("cost_combo_winter_final","cost_combo_summer_final"),
                # -c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv","solstor_wtp_dv"),
                -c("PS_int","EV_int","HP_int","IC_int"))

names(data)

library(gtsummary)
dt_summary <- data %>%
  mutate(across(everything(), ~na_if(as.character(.), "NA"))) %>%
  mutate(across(c(age, income, household_numpeople, home_area, rangeanxiety, 
                  vehicle_num, ev_wtp_pc, heatpump_wtp_pc, vehicle_comb_miles,
                  starts_with("future_")), as.numeric))

# make_sub_tbl <- function(data, vars, header_text) {
#   data %>%
#     select(dac, all_of(vars)) %>%
#     tbl_summary(
#       by = dac,
#       missing_text = "Missing",
#       statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)")
#     ) %>%
#     add_overall() %>%   # Add Overall column HERE
#     add_p() %>%         # Add P-value column HERE
#     modify_header(label = paste0("**", header_text, "**"))
# }

make_sub_tbl <- function(data, vars) {
  data %>%
    select(dac, all_of(vars)) %>%
    tbl_summary(
      by = dac,
      missing_text = "Missing",
      statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)")
    ) %>%
    add_overall() %>%
    add_p(test = list(all_categorical() ~ "chisq.test"))
}

# 2. Re-create the individual sections
t1 <- make_sub_tbl(dt_summary, c("age", "gender", "income", "education", "race", "born_us", "employment"))
t2 <- make_sub_tbl(dt_summary, c("home_type", "home_own", "home_area", "home_age", "household_numpeople", "climatezone"))
t3 <- make_sub_tbl(dt_summary, c("primary_heating_type", "primary_cooling_type", "hotwater_energy", "therm_summer", "therm_winter", "kitchen_range_type", "heating_cost_burden", "electrification", "previous_heating_type"))
t4 <- make_sub_tbl(dt_summary, c("charging_work", "rangeanxiety", "vehicle_next_used", "vehicle_num", "charging_5mile_f", "vehicle_comb_miles"))
t5 <- make_sub_tbl(dt_summary, c("pid_composite", "elec_savemoney", "elec_health", "elec_safety", "upfrontpayback", "lowincome_sup", "elec_avail", "solstor_wtp_dv", "ev_wtp_pc", "heatpump_wtp_pc", "induction_dv"))
t6 <- make_sub_tbl(dt_summary, c("PV", "PS", "EV", "HP", "IC"))
t7 <- make_sub_tbl(dt_summary, c("outage_impact", "outage_generatorown", "outage_generatorplan", "ccinsure_lost", "cclive", "ccpastmove", "ccfuturemove", "homevac", "ccmove_where"))
t8 <- make_sub_tbl(dt_summary, c("peer_EV", "peer_IC", "peer_HP", "peer_PV"))

full_stacked_table <- tbl_stack(
  list(t1, t2, t3, t4, t5, t6, t7, t8),
  group_header = c("DEMOGRAPHICS", "HOUSING", "ENERGY & COOKING", "VEHICLE LOGISTICS", 
                   "WTP & BELIEFS", "CURRENT ADOPTION", "CLIMATE RESILIENCE", "SOCIAL INFLUENCE")
) %>%
  modify_header(label = "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**DAC Status**")


library(flextable)
full_stacked_table %>%
  as_flex_table() %>%
  save_as_docx(path = "./docs/Summary_Table_v2.docx")



var_description_data <- tribble(
  ~Variable, ~Description, ~Values_Units, ~Section,
  
  # --- DEMOGRAPHICS ---
  "age", "Categorical variable of age", "18 to 84", "Demographics",
  "gender", "Self-reported gender identity", "Categorical", "Demographics",
  "race", "Categorical variable representing racial/ethnic identity", "Asian, Hispanic, White, Black, Natives, Multirace", "Demographics",
  "income", "Annual household gross income", "<$20k to >$200k", "Demographics",
  "education", "Educational attainment level", "Less than HS to Postgraduate", "Demographics",
  "born_us", "Binary indicator for U.S.-born status", "1 = Yes, 0 = No", "Demographics",
  "employment", "Current employment status", "Full-time, Part-time, Retired, etc.", "Demographics",
  "ideology", "Self-identified political ideology", "Very Conservative to Very Liberal", "Demographics",
  "pid", "Political party affiliation", "Democrat, Republican, Independent, etc.", "Demographics",
  
  # --- HOUSING & LOCATION ---
  "dac", "Lives in a disadvantaged community (CalEnviroScreen 4.0)", "1 = Yes, 0 = No", "Housing",
  "climatezone", "California Building Climate Zone", "1 to 16", "Housing",
  "home_type", "Type of residential structure", "Mobile, Multi-family, Single-family", "Housing",
  "home_own", "Homeownership status", "1 = Owner, 0 = Renter", "Housing",
  "home_area", "Total livable square footage", "<500 to >5,500 sq. ft.", "Housing",
  "home_age", "Home vintage by year built", "Before 1950 to <5 years old", "Housing",
  "household_numpeople", "Total number of household members", "Numeric", "Housing",
  
  # --- SOLAR & STORAGE (PV/PS) ---
  "PV", "Current household solar PV installation", "1 = Yes, 0 = No", "Solar & Storage",
  "solar_install", "Timing of solar installation relative to move-in", "Already installed, Installed after", "Solar & Storage",
  "solar_date_owner", "Year of solar installation", "Before 2010 to 2025", "Solar & Storage",
  "solar_pv_plans", "Plans to adopt solar PV within 5 years", "Yes, Maybe, No", "Solar & Storage",
  "storage_own", "Ownership of battery storage system", "1 = Yes, 0 = No", "Solar & Storage",
  "storage_plans", "Plans to adopt battery storage within 5 years", "Yes, Maybe, No", "Solar & Storage",
  "solstor_wtp_dv", "Willingness to adopt solar/battery based on subsidy", "$0 to $35,000", "Solar & Storage",
  "solstor_wtp_payback", "Minimum acceptable payback period for solar/battery", "0 to 25 years", "Solar & Storage",
  "peer_PV", "Peer influence on PV adoption", "Neighbor, Acquaintance, None", "Solar & Storage",
  "elec_home_cost_PV", "Perceived cost impact of PV adoption", "More expensive to Much cheaper", "Solar & Storage",
  
  # --- ELECTRIC VEHICLES (EV) ---
  "EV", "Current ownership of a full electric vehicle", "1 = Yes, 0 = No", "Electric Vehicles",
  "vehicle_1_miles", "Annual mileage for primary vehicle", "<2,500 to >20,000 miles", "Electric Vehicles",
  "charging_access", "Access to EV charging at home", "Charger, Possible, No access", "Electric Vehicles",
  "charging_work", "Charger availability at workplace", "1 = Yes, 0 = No", "Electric Vehicles",
  "charging_5mile_r", "Regular public charger within 5 miles", "1 = Yes, 0 = No", "Electric Vehicles",
  "charging_5mile_f", "Fast public charger within 5 miles", "1 = Yes, 0 = No", "Electric Vehicles",
  "fastcharge_likely", "Likelihood of EV adoption if fast charging available", "Never to Much more likely", "Electric Vehicles",
  "vehicle_wherecharge_1", "Frequency of home charging", "Never to Daily", "Electric Vehicles",
  "vehicle_wherecharge_2", "Frequency of workplace charging", "Never to Daily", "Electric Vehicles",
  "vehicle_wherecharge_3", "Frequency of public charging", "Never to Daily", "Electric Vehicles",
  "vehicle_whencharge", "Typical time of charging", "9am-5pm, 5pm-8pm, etc.", "Electric Vehicles",
  "rangeanxiety", "Minimum range needed for EV confidence", "<200 to >400 miles", "Electric Vehicles",
  "vehicle_next_used", "Preference for next vehicle to be new", "1 = Yes, 0 = No", "Electric Vehicles",
  "vehicle_next_when", "Planned timing for next vehicle acquisition", "Within 1 year to No plan", "Electric Vehicles",
  "vehicle_next_fuel", "Plan to purchase EV as next vehicle", "1 = Yes, 0 = No", "Electric Vehicles",
  "ev_wtp_pc", "Willingness to adopt EV based on subsidy", "$0 to $20,000", "Electric Vehicles",
  "peer_EV", "Peer influence on EV adoption", "Neighbor, Acquaintance, None", "Electric Vehicles",
  "elec_home_cost_1", "Perceived cost impact of EV adoption", "More expensive to Much cheaper", "Electric Vehicles",
  
  # --- HEAT PUMP & COOKING (HP/IC) ---
  "HP", "Current heat pump usage", "1 = Yes, 0 = No", "Heating & Cooling",
  "primary_heating_type", "Primary heating system type", "Central, Electric, HP, Other", "Heating & Cooling",
  "primary_cooling_type", "Primary cooling system type", "Central, Room, Other", "Heating & Cooling",
  "heatpump_direct", "Interest in heat pump adoption", "Not interested to Very interested", "Heating & Cooling",
  "heating_plan", "Plan to replace heating system", "Never to Next year", "Heating & Cooling",
  "cooling_plan", "Plan to replace cooling system", "Never to Next year", "Heating & Cooling",
  "therm_summer", "Thermostat use in summer", "1 = Yes, 0 = No", "Heating & Cooling",
  "therm_winter", "Thermostat use in winter", "1 = Yes, 0 = No", "Heating & Cooling",
  "peer_HP", "Peer influence on HP adoption", "Neighbor, Acquaintance, None", "Heating & Cooling",
  "elec_home_cost_HP", "Perceived cost impact of HP adoption", "More expensive to Much cheaper", "Heating & Cooling",
  "heatpump_wtp_pc", "Willingness to adopt HP based on subsidy", "$0 to $20,000", "Heating & Cooling",
  "heatpump_wtp_payback", "Minimum acceptable payback period for HP", "0 to 18 years", "Heating & Cooling",
  "heatpump_motiv", "Primary motivation for adopting heat pump", "Categorical", "Heating & Cooling",
  "IC", "Current induction cooktop use", "1 = Yes, 0 = No", "Kitchen & Cooking",
  "kitchen_range_type", "Kitchen range fuel type", "Electric, Induction, Gas, Propane", "Kitchen & Cooking",
  "induction_direct", "Interest in induction cooktop adoption", "Not interested to Very interested", "Kitchen & Cooking",
  "induction_dv", "Willingness to adopt IC based on subsidy", "$0 to $1,200", "Kitchen & Cooking",
  "peer_IC", "Peer influence on IC adoption", "Neighbor, Acquaintance, None", "Kitchen & Cooking",
  "elec_home_cost_IC", "Perceived cost impact of IC adoption", "More expensive to Much cheaper", "Kitchen & Cooking",
  
  # --- ATTITUDES & ELECTRIFICATION ---
  "heating_cost_burden", "Perceived burden of heating costs", "No burden to Large burden", "Attitudes",
  "elec_savemoney", "Perceived cost change from switching to electricity", "Increase to Decrease", "Attitudes",
  "elec_health", "Perceived health impact of electrification", "Harm to Improve", "Attitudes",
  "elec_safety", "Safety perception: Electricity vs. Gas", "1=Gas safer, 3=Elec safer", "Attitudes",
  "electrification", "Preferences for fossil fuel vs. electricity", "Full fossil to Full elec", "Attitudes",
  "upfrontpayback", "Preference for upfront vs. long-term savings", "1 = Upfront", "Attitudes",
  
  # --- CLIMATE & OUTAGES ---
  "outage_impact", "Household impact of power outages", "None to Extreme", "Climate & Resilience",
  "outage_generatorown", "Ownership of backup generator", "1 = Yes, 0 = No", "Climate & Resilience",
  "outage_generatorplan", "Plan to acquire generator within 3 years", "No to Yes", "Climate & Resilience",
  "yale_worried", "Level of concern about climate change", "Not worried to Very worried", "Climate & Resilience",
  "cclive", "Climate change impact on residential location choice", "No impact to Very much", "Climate & Resilience",
  "ccpastmove", "Frequency of past moves due to climate change", "Never to >Once", "Climate & Resilience",
  "ccfuturemove", "Likelihood of moving in next 10 years due to climate", "Unlikely to Likely", "Climate & Resilience",
  "homevac", "Frequency of climate-related evacuation orders", "Never to >Once", "Climate & Resilience",
  "ccmove_where", "Likelihood of remaining in California", "1 = Leave, 0 = Stay", "Climate & Resilience"
)

# 2. Format the table for Word
final_desc_table <- var_description_data %>%
  arrange(factor(Section, levels = c("Demographics", "Housing", "Solar & Storage", "Electric Vehicles", 
                                     "Heating & Cooling", "Kitchen & Cooking", "Attitudes", "Climate & Resilience"))) %>%
  as_grouped_data(groups = "Section") %>%
  flextable() %>%
  set_header_labels(
    Variable = "Variable Name",
    Description = "Description",
    Values_Units = "Values / Coding"
  ) %>%
  theme_booktabs() %>%
  bold(i = ~ !is.na(Section), part = "body") %>% # Bold Section headers
  bold(j = 1, part = "body") %>%                 # Bold Variable names
  fontsize(size = 9, part = "all") %>%
  width(j = 1, width = 1.5) %>%
  width(j = 2, width = 3.2) %>%
  width(j = 3, width = 1.8) %>%
  align(i = ~ !is.na(Section), align = "left", part = "body")

save_as_docx(final_desc_table, path = "./docs/Appendix_Variable_Descriptions.docx")


# ML feature importance
tc_kfold <- trainControl(method = "cv", number = 10, returnResamp="all",
                         savePredictions = TRUE, classProbs = TRUE,
                         summaryFunction = twoClassSummary)

result <- data.frame()
for(k in 2:5){
  dt <- mreg_var(data, i = k)
  fvar <- as.formula(paste(ipt[k], "~", paste(dt[[2]], collapse = " + ")))
  
  outcome_var <- as.character(fvar[[2]])
  # Sanitize factor levels
  levels(dt[[1]][[outcome_var]]) <- make.names(levels(dt[[1]][[outcome_var]]))
  
  ### GLM
  glm <- train(form = fvar, data= dt[[1]], method = "glm",
               trControl = tc_kfold,
               na.action=na.omit,
               family = 'binomial',
               weights = dt[[1]]$wt_ca)

  
  varimp_glm <- varImp(glm)
  
  ### lasso
  lambda <- 10^seq(-3, 3, length = 100)
  lasso <- train(form = fvar, data=dt[[1]], method = "glmnet",
                 trControl = tc_kfold,
                 tuneGrid = expand.grid(alpha = 1, lambda = lambda),
                 na.action=na.omit,
                 weights = dt[[1]]$wt_ca)
  
  varimp_lasso <- varImp(lasso)
  
  ### xgbTree
  tune_grid <- expand.grid(
    nrounds = c(50),       # Number of boosting iterations
    max_depth = c(3, 6),           # Maximum depth of trees
    eta = c(0.01, 0.1),         # Learning rate
    gamma = c(1),               # Minimum loss reduction required
    colsample_bytree = c(0.6),# Fraction of features used per tree
    min_child_weight = c(1, 5),
    subsample = c(0.6)        # Fraction of samples used per tree
  )
  xg = train(form = fvar, data=dt[[1]],
             method="xgbTree", trControl = tc_kfold,
             tuneGrid = tune_grid,
             na.action = na.omit,
             weights = dt[[1]]$wt_ca)
  
  varimp_xg <- varImp(xg)
  
  
  imp <- rbind(
    varimp_xg[[1]] %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      mutate(model = "XGBoost"),
    varimp_lasso[[1]] %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      mutate(model = "Lasso"),
    varimp_glm[[1]] %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      mutate(model = "GLM")) %>% 
    mutate(tech = ipt[k])
  
  result <- rbind(imp, result)
}


result$rowname %>% unique()
results <- results <- result %>% 
  # 1. Expand the recode list to catch the raw names appearing in your NA list
  mutate(rowname = recode(
    rowname,
    # --- Demographics ---
    "age" = "Age", "genderMale" = "Male", "genderOther" = "Gender: Other", "income" = "Income",
    "educationgraduate" = "Graduate Degree", "educationHS" = "High School",
    "educationless than HS" = "Less than HS", "`educationless than HS`" = "Less than HS",
    "educationsome college" = "Some College", "`educationsome college`" = "Some College",
    "raceAsian" = "Asian", "raceBlack" = "Black", "raceHispanic" = "Hispanic",
    "raceAIANNHPI" = "Native/Pacific Islander", "raceMultirace" = "Multiracial",
    "raceNon-hispanic other" = "Non-Hispanic Other", "`raceNon-hispanic other`" = "Non-Hispanic Other",
    "born_us1" = "US Born", "employment0" = "Unemployed", "pid_composite" = "Political Ideology",
    "household_numpeople" = "Household Size",
    
    # --- Home & Tech Status ---
    "home_own" = "Homeowner", "home_area" = "Home Area", "home_typeMF" = "Multi-family",
    "home_typeOthers" = "Other Home Types", "home_ageBrandNew" = "Home Built 1980-2020",
    "home_ageNewer" = "Home Built after 2020", "primary_heating_typeGas" = "Heating: Gas",
    "primary_heating_typeElec" = "Heating: Electric", "primary_heating_typeNone" = "Heating: None",
    "primary_heating_typeNot know" = "Heating: Unknown", "`primary_heating_typeNot know`" = "Heating: Unknown",
    "primary_cooling_typeCentral" = "Central AC", "primary_cooling_typeNone" = "No Cooling",
    "primary_cooling_typeNot know" = "Cooling: Unknown", "`primary_cooling_typeNot know`" = "Cooling: Unknown",
    "primary_cooling_typeOther" = "Cooling: Other",
    "hotwater_energyGas" = "Water Heating: Gas", "hotwater_energyNot know" = "Water Heating: Unknown",
    "`hotwater_energyNot know`" = "Water Heating: Unknown","hotwater_energyOther" = "Water Heating: Other",
    "kitchen_range_typeElec" = "Electric Cooktop", "kitchen_range_typeGas" = "Gas Cooktop",
    "kitchen_range_typeNone" = "Cooktop: None", "kitchen_range_typeNot know" = "Cooktop: Unknown",
    "`kitchen_range_typeNot know`" = "Cooktop: Unknown",
    
    # --- Previous Tech (for HP/EV) ---
    "previous_heating_typeGas" = "Heating: Gas (previous)", "previous_heating_typeElec" = "Heating: Electric (previous)",
    "previous_heating_typeNone" = "Heating: None (previous)", "previous_heating_typeNot know" = "Heating: Unknown (previous)",
    "`previous_heating_typeNot know`" = "Heating: Unknown (previous)",  "previous_heating_typeOther" = "Heating: Other (previous)",
    "previous_cooling_typeCentral" = "Central AC (previous)", "previous_cooling_typeNone" = "No Cooling (previous)",
    
    # --- Vehicles ---
    "vehicle_num" = "Number of Vehicles", "vehicle_comb_miles" = "Mileage",
    "charging_work1" = "Workplace Charger", "charging_5mile_f1" = "Fast Charger (<5mi)",
    "vehicle_next_used1" = "Planning Used Car", "rangeanxiety" = "EV Range",
    
    # --- Behavioral / Preferences ---
    "solstor_wtp_dv" = "WTP: Solar + Storage", "ev_wtp_pc" = "WTP: Electric Vehicle",
    "heatpump_wtp_pc" = "WTP: Heat Pump", "induction_dv" = "WTP: Induction Stove",
    "upfrontpayback1" = "Prefer Upfront Subsidy", "elec_savemoney" = "Perceived Cost Savings (Elec)",
    "elec_health" = "Perceived Health Benefits (Elec)", "elec_safety" = "Perceived Safety (Elec)",
    "elec_avail" = "Contractor Availability Confidence", "heating_cost_burden" = "Heating Financial Burden",
    "electrificationHalf_elec" = "Prefer Partial Electrification", "electrificationFull_elec" = "Prefer Full Electrification",
    "electrificationFull_fossil" = "Prefer Fossil Fuels", "lowincome_sup" = "Receives Bill Assistance",
    
    # --- Climate Resilience ---
    "ccinsure_lost" = "Lost Home Insurance", "ccmove_where1" = "Leave CA (Climate Move)",
    "homevac" = "Evacuation History", "cclive" = "Climate Change Concern",
    "ccpastmove" = "Past Climate Move", "ccfuturemove" = "Future Climate Move",
    "outage_impact" = "Outage Frequency Impact", "outage_generatorown1" = "Owns Backup Generator",
    "outage_generatorplan" = "Plans for Backup Generator", "therm_winter1" = "Thermostat (Winter)",
    "therm_summer" = "Thermostat (Summer)"
  )) %>% 
  # 2. Peer/Neighbor logic
  mutate(rowname = case_when(
    str_detect(rowname, "peer_.*none") ~ "Neighbor/Peer",
    str_detect(rowname, "peer_.*peer") ~ "Peer only",
    TRUE ~ rowname
  )) %>% 
  # 3. Categorize into Domains using updated keywords
  mutate(domain = case_when(
    str_detect(rowname, "Insurance|Leave CA|Evacuation|Concern|Outage|Generator|Move") ~ "Climate resilience",
    str_detect(rowname, "WTP|Subsidy|Savings|Health|Safety|Confidence|Burden|Electrification|Assistance|Fossil|Thermostat|Ideology") ~ "Behavioral",
    str_detect(rowname, "Heating|Cooling|AC|Water Heating") ~ "Heating/Cooling",
    str_detect(rowname, "Home|Multi-family") ~ "Housing",
    
    str_detect(rowname, "Income|Degree|College|School|HS|Asian|Black|Hispanic|Native|Multi|Other|US Born|Unemployed|Age|Male|Gender|Size") ~ "Socio-demographic",
    
    str_detect(rowname, "Cooktop|Induction|Stove|Kitchen") ~ "Cooking",

    str_detect(rowname, "Vehicle|Mileage|Charger|Car|EV Range") ~ "Vehicle",
    str_detect(rowname, "Effect") ~ "Social Influence",
    TRUE ~ "Socio-demographic" # Default for leftovers like "High School"
  )) %>% 
  # 4. Clean factors and tech names
  mutate(
    domain = factor(domain, levels = c("Housing", "Heating/Cooling", "Cooking", "Vehicle", "Socio-demographic", "Behavioral", "Climate resilience", "Social Influence")),
    tech = recode(tech, "PS" = "PV + Storage", "EV" = "Electric vehicles", "HP" = "Heat pumps", "IC" = "Induction stoves"),
    tech = factor(tech, levels = c("PV + Storage", "Electric vehicles", "Heat pumps", "Induction stoves"))
  )


s1 <- results %>% 
  filter(rowname != "Homeowner") %>% 
  group_by(rowname, tech) %>% 
  mutate(mean = mean(Overall)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_col(aes(y = reorder(rowname, mean), x = Overall, fill = model), width = 0.7, position = position_dodge(0.8)) +
  geom_point(aes(y = reorder(rowname, mean), x = mean), color = "red", width = 0.7, position = position_dodge(0.8))+
  labs(y = "", x = "Importance (%)", fill = "", title = "") +
  facet_grid(domain ~ tech, space = "free", scale = "free", switch = "y") +
  theme_bw() +
  scale_fill_brewer(palette = "Paired") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        
        strip.placement = "outside", # Keep labels on the outside
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
        strip.text.y.left = element_text(angle = 0), # Ensure domain labels are horizontal
        
        legend.position = "bottom",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=10),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=10),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.03)) 


ggsave("./fig/s1.png",
       s1,
       width = 12, height = 12)


### not including WTP
data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1) %>% 
  dplyr::select(-c("cost_combo_winter_final","cost_combo_summer_final"),
                -c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv","solstor_wtp_dv"),
                -c("PS_int","EV_int","HP_int","IC_int"))

# ML feature importance
tc_kfold <- trainControl(method = "cv", number = 10, returnResamp="all",
                         savePredictions = TRUE, classProbs = TRUE,
                         summaryFunction = twoClassSummary)

result <- data.frame()
for(k in 2:5){
  dt <- mreg_var(data, i = k)
  fvar <- as.formula(paste(ipt[k], "~", paste(dt[[2]], collapse = " + ")))
  
  outcome_var <- as.character(fvar[[2]])
  # Sanitize factor levels
  levels(dt[[1]][[outcome_var]]) <- make.names(levels(dt[[1]][[outcome_var]]))
  
  ### GLM
  glm <- train(form = fvar, data= dt[[1]], method = "glm",
               trControl = tc_kfold,
               na.action=na.omit,
               family = 'binomial',
               weights = dt[[1]]$wt_ca)
  
  varimp_glm <- varImp(glm)
  
  ### lasso
  lambda <- 10^seq(-3, 3, length = 100)
  lasso <- train(form = fvar, data=dt[[1]], method = "glmnet",
                 trControl = tc_kfold,
                 tuneGrid = expand.grid(alpha = 1, lambda = lambda),
                 na.action=na.omit,
                 weights = dt[[1]]$wt_ca)
  
  varimp_lasso <- varImp(lasso)
  
  ### xgbTree
  tune_grid <- expand.grid(
    nrounds = c(50),       # Number of boosting iterations
    max_depth = c(3, 6),           # Maximum depth of trees
    eta = c(0.01, 0.1),         # Learning rate
    gamma = c(1),               # Minimum loss reduction required
    colsample_bytree = c(0.6),# Fraction of features used per tree
    min_child_weight = c(1, 5),
    subsample = c(0.6)        # Fraction of samples used per tree
  )
  xg = train(form = fvar, data=dt[[1]],
             method="xgbTree", trControl = tc_kfold,
             tuneGrid = tune_grid,
             na.action = na.omit,
             weights = dt[[1]]$wt_ca)
  
  varimp_xg <- varImp(xg)
  
  
  imp <- rbind(
    varimp_xg[[1]] %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      mutate(model = "XGBoost"),
    varimp_lasso[[1]] %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      mutate(model = "Lasso"),
    varimp_glm[[1]] %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      mutate(model = "GLM")) %>% 
    mutate(tech = ipt[k])
  
  result <- rbind(imp, result)
}


result$rowname %>% unique()
results <- results <- result %>% 
  # 1. Expand the recode list to catch the raw names appearing in your NA list
  mutate(rowname = recode(
    rowname,
    # --- Demographics ---
    "age" = "Age", "genderMale" = "Male", "genderOther" = "Gender: Other", "income" = "Income",
    "educationgraduate" = "Graduate Degree", "educationHS" = "High School",
    "educationless than HS" = "Less than HS", "`educationless than HS`" = "Less than HS",
    "educationsome college" = "Some College", "`educationsome college`" = "Some College",
    "raceAsian" = "Asian", "raceBlack" = "Black", "raceHispanic" = "Hispanic",
    "raceAIANNHPI" = "Native/Pacific Islander", "raceMultirace" = "Multiracial",
    "raceNon-hispanic other" = "Non-Hispanic Other", "`raceNon-hispanic other`" = "Non-Hispanic Other",
    "born_us1" = "US Born", "employment0" = "Unemployed", "pid_composite" = "Political Ideology",
    "household_numpeople" = "Household Size",
    
    # --- Home & Tech Status ---
    "home_own" = "Homeowner", "home_area" = "Home Area", "home_typeMF" = "Multi-family",
    "home_typeOthers" = "Other Home Types", "home_ageBrandNew" = "Home Built 1980-2020",
    "home_ageNewer" = "Home Built after 2020", "primary_heating_typeGas" = "Heating: Gas",
    "primary_heating_typeElec" = "Heating: Electric", "primary_heating_typeNone" = "Heating: None",
    "primary_heating_typeNot know" = "Heating: Unknown", "`primary_heating_typeNot know`" = "Heating: Unknown",
    "primary_cooling_typeCentral" = "Central AC", "primary_cooling_typeNone" = "No Cooling",
    "primary_cooling_typeNot know" = "Cooling: Unknown", "`primary_cooling_typeNot know`" = "Cooling: Unknown",
    "primary_cooling_typeOther" = "Cooling: Other",
    "hotwater_energyGas" = "Water Heating: Gas", "hotwater_energyNot know" = "Water Heating: Unknown",
    "`hotwater_energyNot know`" = "Water Heating: Unknown","hotwater_energyOther" = "Water Heating: Other",
    "kitchen_range_typeElec" = "Electric Cooktop", "kitchen_range_typeGas" = "Gas Cooktop",
    "kitchen_range_typeNone" = "Cooktop: None", "kitchen_range_typeNot know" = "Cooktop: Unknown",
    "`kitchen_range_typeNot know`" = "Cooktop: Unknown",
    
    # --- Previous Tech (for HP/EV) ---
    "previous_heating_typeGas" = "Heating: Gas (previous)", "previous_heating_typeElec" = "Heating: Electric (previous)",
    "previous_heating_typeNone" = "Heating: None (previous)", "previous_heating_typeNot know" = "Heating: Unknown (previous)",
    "`previous_heating_typeNot know`" = "Heating: Unknown (previous)",  "previous_heating_typeOther" = "Heating: Other (previous)",
    "previous_cooling_typeCentral" = "Central AC (previous)", "previous_cooling_typeNone" = "No Cooling (previous)",
    
    # --- Vehicles ---
    "vehicle_num" = "Number of Vehicles", "vehicle_comb_miles" = "Mileage",
    "charging_work1" = "Workplace Charger", "charging_5mile_f1" = "Fast Charger (<5mi)",
    "vehicle_next_used1" = "Planning Used Car", "rangeanxiety" = "EV Range",
    
    # --- Behavioral / Preferences ---
    "solstor_wtp_dv" = "WTP: Solar + Storage", "ev_wtp_pc" = "WTP: Electric Vehicle",
    "heatpump_wtp_pc" = "WTP: Heat Pump", "induction_dv" = "WTP: Induction Stove",
    "upfrontpayback1" = "Prefer Upfront Subsidy", "elec_savemoney" = "Perceived Cost Savings (Elec)",
    "elec_health" = "Perceived Health Benefits (Elec)", "elec_safety" = "Perceived Safety (Elec)",
    "elec_avail" = "Contractor Availability Confidence", "heating_cost_burden" = "Heating Financial Burden",
    "electrificationHalf_elec" = "Prefer Partial Electrification", "electrificationFull_elec" = "Prefer Full Electrification",
    "electrificationFull_fossil" = "Prefer Fossil Fuels", "lowincome_sup" = "Receives Bill Assistance",
    
    # --- Climate Resilience ---
    "ccinsure_lost" = "Lost Home Insurance", "ccmove_where1" = "Leave CA (Climate Move)",
    "homevac" = "Evacuation History", "cclive" = "Climate Change Concern",
    "ccpastmove" = "Past Climate Move", "ccfuturemove" = "Future Climate Move",
    "outage_impact" = "Outage Frequency Impact", "outage_generatorown1" = "Owns Backup Generator",
    "outage_generatorplan" = "Plans for Backup Generator", "therm_winter1" = "Thermostat (Winter)",
    "therm_summer" = "Thermostat (Summer)"
  )) %>% 
  # 2. Peer/Neighbor logic
  mutate(rowname = case_when(
    str_detect(rowname, "peer_.*none") ~ "Neighbor/Peer",
    str_detect(rowname, "peer_.*peer") ~ "Peer only",
    TRUE ~ rowname
  )) %>% 
  # 3. Categorize into Domains using updated keywords
  mutate(domain = case_when(
    str_detect(rowname, "Insurance|Leave CA|Evacuation|Concern|Outage|Generator|Move") ~ "Climate\nresilience",
    str_detect(rowname, "WTP|Subsidy|Savings|Health|Safety|Confidence|Burden|Electrification|Assistance|Fossil|Thermostat|Ideology") ~ "Behavioral",
    str_detect(rowname, "Heating|Cooling|AC|Water Heating") ~ "Heating/\nCooling",
    str_detect(rowname, "Home|Multi-family") ~ "Housing",
    
    str_detect(rowname, "Income|Degree|College|School|HS|Asian|Black|Hispanic|Native|Multi|Other|US Born|Unemployed|Age|Male|Gender|Size") ~ "Socio-\ndemographic",
    
    str_detect(rowname, "Cooktop|Induction|Stove|Kitchen") ~ "Cooking",
    
    str_detect(rowname, "Vehicle|Mileage|Charger|Car|EV Range") ~ "Vehicle",
    str_detect(rowname, "Peer") ~ "Social\nInfluence",
    TRUE ~ "Socio-\ndemographic" # Default for leftovers like "High School"
  )) %>% 
  # 4. Clean factors and tech names
  mutate(
    domain = factor(domain, levels = c("Housing", "Heating/\nCooling", "Cooking", "Vehicle", "Socio-\ndemographic","Social\nInfluence", "Behavioral", "Climate\nresilience")),
    tech = recode(tech, "PS" = "PV + Storage", "EV" = "Electric vehicles", "HP" = "Heat pumps", "IC" = "Induction stoves"),
    tech = factor(tech, levels = c("PV + Storage", "Electric vehicles", "Heat pumps", "Induction stoves"))
  )


f1 <- results %>% 
  group_by(rowname, tech) %>% 
  mutate(mean = mean(Overall)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_col(aes(y = reorder(rowname, mean), x = Overall, fill = model), width = 0.7, position = position_dodge(0.8)) +
  geom_point(aes(y = reorder(rowname, mean), x = mean), color = "red", width = 0.7, position = position_dodge(0.8))+
  labs(y = "", x = "Importance (%)", fill = "", title = "") +
  facet_grid(domain ~ tech, space = "free", scale = "free", switch = "y") +
  theme_bw() +
  scale_fill_brewer(palette = "Paired") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        
        strip.placement = "outside", # Keep labels on the outside
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
        strip.text.y.left = element_text(angle = 0), # Ensure domain labels are horizontal
        
        legend.position = "bottom",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=10),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=10),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.03)) 


ggsave("./fig/f1.png",
       f1,
       width = 12, height = 15)


