source("./syntax/Function.R")
source("./syntax/Data.R")

### including WTP
data <- dat %>% 
  dplyr::select(-c("PS_int","EV_int","HP_int","IC_int"))


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
results <- result %>% 
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
data <- dat %>% 
  dplyr::select(-c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv","solstor_wtp_dv"),
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


