################################################################################
### variable selection
### comparison for WTP 
wtp_var <- c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv","solstor_wtp_dv")
### future modiviation version 
# Total, WTP subset + WTP, WTP subset
data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
  data_process(ev = c("Fully electric")) %>% 
  data_cl() %>% 
  mutate(
    # future adoption
    # future_EV10 = ifelse((vehicle_next_when > 1 & vehicle_next_fuel == 1)| EV == 1, 1, 0),
    future_EV_0 = ifelse((vehicle_next_when == 1 & vehicle_next_fuel == 1)| EV == 1, 1, 0),
    future_EV_low = ifelse((vehicle_next_when == 1 & vehicle_next_fuel == 1 &
                              ev_wtp_pc <= 8000)| EV == 1, 1, 0),
    future_EV_high = ifelse((vehicle_next_when == 1 & vehicle_next_fuel == 1 &
                               ev_wtp_pc <= 12500)| EV == 1, 1, 0),
    
    future_PV = ifelse(solar_pv_plans %in% c("Yes", "Maybe") | PV == 1, 1, 0),
    future_PS_0 = ifelse((storage_plans %in% c("Yes", "Maybe")) | 
                           (solar_pv_plans %in% c("Yes", "Maybe")) |
                           PS == 1, 1, 0),
    future_PS_low = ifelse((storage_plans %in% c("Yes", "Maybe") & solstor_wtp_dv <= 5000) |
                             (solar_pv_plans %in% c("Yes", "Maybe") & solstor_wtp_dv <= 5000) |
                             PS == 1, 1, 0),
    future_PS_high = ifelse((storage_plans %in% c("Yes", "Maybe") & solstor_wtp_dv <= 12500) |
                              (solar_pv_plans %in% c("Yes", "Maybe") & solstor_wtp_dv <= 12500) |
                              PS == 1, 1, 0),
    
    future_HP_0 = ifelse(heatpump_direct == 1 | HP == 1, 1, 0),
    future_HP_low = ifelse(heatpump_direct == 1 & 
                             heatpump_wtp_pc <= 4000| HP == 1, 1, 0),
    future_HP_high = ifelse(heatpump_direct == 1 & 
                              heatpump_wtp_pc <= 8000| HP == 1, 1, 0),
    
    future_IC_0 = ifelse(induction_direct == 1| IC == 1, 1, 0),
    future_IC_low = ifelse(induction_direct == 1 &
                             induction_dv <= 300| IC == 1, 1, 0),
    future_IC_high = ifelse(induction_direct == 1 &
                              induction_dv <= 800| IC == 1, 1, 0)
  ) %>% 
  # mutate(across(matches("future"), ~ ifelse(is.na(.), 0, .))) %>% 
  
  
  # remove variables
  dplyr::select(-pid,-pid_dem,-pid_ind,-pid_rep,
                -solar_install_owner,-solar_install_renter,-solar_install,-solar_date_owner,-solar_pv_plans,-storage_plans,-storage_own,
                -vehicle_1_miles,-vehicle_2_miles,-workfromhome,-vehicle_charging_own:-charging_install,-fastcharge_likely,
                -charging_5mile_non_1:-charging_5mile_own_2,
                -vehicle_wherecharge_1,-vehicle_wherecharge_2,-vehicle_wherecharge_3,-vehicle_whencharge,
                -vehicle_next_fuel,
                
                -replan_heat_owner:-replan_nocool_renter,-elec_savemoney_rent,-elec_health_renter,
                -natgas_ypccc,
                
                -elec_home_cost_1:-elec_home_cost_4,-elec_home_others_1:-elec_home_who_4,
                -heatpump_motiv,-induction_appeal,-induction_downsides,
                
                -cost_fuel_winter,-cost_electric_winter,-cost_combo_winter_summed,-cost_combo_winter,
                -cost_combo_summer,-cost_electric_summer,
                -ccinsure_owner,-ccinsure_renter,
                
                -elec_home_cost_PV,
                -elec_home_cost_EV,-vehicle_next_when,-vehicle_1_class,
                -elec_home_cost_HP,-elec_home_cost_IC,
                -cooling_plan,-heating_plan,
                
                -solstor_wtp_payback,-heatpump_wtp_payback,-ss_upfr_text,-ssupfront_num,-hpupfront_num,-evupfront_num,
                -primary_cooling_type_25_TEXT,-primary_heating_type_64_TEXT,-heatenergy_furnace,-heatenergy_boiler,-hotwater_energy_6_TEXT,
                -heatpump_adopter
                # -future_EV10
  ) %>%
  relocate(wt_ca, .after = last_col()) %>%
  relocate(tractid, .after = last_col()) %>% 
  dplyr::select(-ExternalReference, -tractid,
                # remove NAs
                -induction_direct, -heatpump_direct, -ac_unit_number,-hotwater_nextelec,
                # remove not significant or collinear variables
                -charging_access,-charging_5mile_r, -ccinsure_both,-yale_worried,-ideology,-hvac_avail
                
                # -charging_work, -vehicle_num,
                # -born_us, -employment, -education, -race, -gender, -lowincome_sup,
                # -elec_health, -upfrontpayback, -elec_safety, -ccinsure_lost,
                # -ccpastmove, -elec_avail, -ccmove_where, -homevac
  ) %>% 
  relocate(wt_ca, .after = last_col())


### have to change the value for scenario
  ffu <- "high"

### without Home own
  data <- data %>% 
    dplyr::select(-matches("future"), matches(paste0("_",ffu)),-home_own,
                  -matches("combo")) # collinear with wtp

categorical_vars <- c(
  "climatezone","dac","gender","race","education","born_us","employment","home_type","home_age","home_own","charging_work",
  "vehicle_next_used","primary_heating_type","hotwater_energy","previous_heating_type",
  "primary_cooling_type","kitchen_range_type",
  "upfrontpayback","lowincome_sup","outage_generatorown",
  "ccinsure_lost","ccmove_where","charging_5mile_f",
  "electrification","peer_EV","peer_PV","peer_HP","peer_IC"
)

for (var in categorical_vars) {
  # Check if var is in the dataframe colnames first
  if (var %in% colnames(data)) { # Check in 'x' now, not 'data'
    data[[var]] <- as.factor(data[[var]])
  }
}


tech <- c("PV","EV","HP","IC","PS")
plot <- list()
tp <- data.frame()
for(k in 2:5){
  
  # without WTP
  tot <- mreg(data, 
              remove = wtp_var,
              i = k,
              future = ffu)
  
  # with WTP with subset 
  wtp_w <- mreg(data, 
                i = k,
                future = ffu)
  
  # without WTP with subset 
  wtp_wo <- mreg(data %>% 
                   filter(!is.na(!!sym(wtp_var[k]))), 
                 remove = wtp_var,
                 i = k,
                 future = ffu)
  
  plot[[k-1]] <- tot[[1]] %>% 
    mutate(class = "Total") %>% 
    rbind(
      wtp_w[[1]] %>% 
        mutate(class = "WTP subset (+WTP)"),
      wtp_wo[[1]] %>% 
        mutate(class = "WTP subset") 
    ) %>% 
    mutate(domain = as.character(domain),
           domain = ifelse(str_detect(var, "peer|Older home|BrandNew|EV range|Newer home|Fast"), "Scenario", domain),
           domain = factor(domain, levels = c("Housing","Heat/Cool","Cook","Vehicle","Resilience","Social","Behavior","Scenario"))) %>% 
    com_reg_plot(tech[[k]])
  
  
  tt <- tot[[1]] %>% 
    mutate(class = "Total") %>% 
    rbind(
      wtp_w[[1]] %>% 
        mutate(class = "WTP subset (+WTP)"),
      wtp_wo[[1]] %>% 
        mutate(class = "WTP subset") 
    ) %>% 
    mutate(domain = as.character(domain),
           domain = ifelse(str_detect(var, "peer|Older home|BrandNew|EV range|Newer home|Fast"), "Scenario", domain),
           domain = factor(domain, levels = c("Housing","Heat/Cool","Cook","Vehicle","Resilience","Social","Behavior","Scenario"))) %>% 
    mutate(tech = ipt[k])
  
  tp <- rbind(tt, tp)
  
  # reg_plot(wtp_w[[1]], tech[k])
  
}

# Total, WTP subset + WTP, WTP subset
tp %>% 
  mutate(domain = as.character(domain),
         domain = ifelse(is.na(domain), "Scenario", domain)) %>% 
  filter(domain == "Scenario") %>% 
  ggplot(aes(x = pe, y = reorder(var, pe), xmin=lwr, xmax=upr)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  geom_pointrangeh(aes(fill = color, color = class), 
                   position = position_dodge2v(height = 0.4), fatten = 4, size = 0.5,
                   pch=21) +
  
  facet_wrap(~ tech, scale = "free", nrow = 2) + 
  theme_bw() +
  
  coord_flip() +
  
  labs(x = "", y ="", fill = "Significant", color = "",
       title = "") +
  
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


# plot[[1]]

### Total +HO, homeown subset
### variable selection
data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
  data_process(ev = c("Fully electric")) %>% 
  data_cl() %>% 
  mutate(
    # future adoption
    # future_EV10 = ifelse((vehicle_next_when > 1 & vehicle_next_fuel == 1)| EV == 1, 1, 0),
    future_EV_0 = ifelse((vehicle_next_when == 1 & vehicle_next_fuel == 1)| EV == 1, 1, 0),
    future_EV_low = ifelse((vehicle_next_when == 1 & vehicle_next_fuel == 1 &
                              ev_wtp_pc <= 8000)| EV == 1, 1, 0),
    future_EV_high = ifelse((vehicle_next_when == 1 & vehicle_next_fuel == 1 &
                               ev_wtp_pc <= 12500)| EV == 1, 1, 0),
    
    future_PV = ifelse(solar_pv_plans %in% c("Yes", "Maybe") | PV == 1, 1, 0),
    future_PS_0 = ifelse((storage_plans %in% c("Yes", "Maybe")) | 
                           (solar_pv_plans %in% c("Yes", "Maybe")) |
                           PS == 1, 1, 0),
    future_PS_low = ifelse((storage_plans %in% c("Yes", "Maybe") & solstor_wtp_dv <= 5000) |
                             (solar_pv_plans %in% c("Yes", "Maybe") & solstor_wtp_dv <= 5000) |
                             PS == 1, 1, 0),
    future_PS_high = ifelse((storage_plans %in% c("Yes", "Maybe") & solstor_wtp_dv <= 12500) |
                              (solar_pv_plans %in% c("Yes", "Maybe") & solstor_wtp_dv <= 12500) |
                              PS == 1, 1, 0),
    
    future_HP_0 = ifelse(heatpump_direct == 1| HP == 1, 1, 0),
    future_HP_low = ifelse(heatpump_direct == 1 & 
                             heatpump_wtp_pc <= 4000| HP == 1, 1, 0),
    future_HP_high = ifelse(heatpump_direct == 1 & 
                              heatpump_wtp_pc <= 8000| HP == 1, 1, 0),
    
    future_IC_0 = ifelse(induction_direct == 1| IC == 1, 1, 0),
    future_IC_low = ifelse(induction_direct == 1 &
                             induction_dv <= 300| IC == 1, 1, 0),
    future_IC_high = ifelse(induction_direct == 1 &
                              induction_dv <= 800| IC == 1, 1, 0)
  ) %>% 
  # mutate(across(matches("future"), ~ ifelse(is.na(.), 0, .))) %>% 
  
  
  # remove variables
  dplyr::select(-pid,-pid_dem,-pid_ind,-pid_rep,
                -solar_install_owner,-solar_install_renter,-solar_install,-solar_date_owner,-solar_pv_plans,-storage_plans,-storage_own,
                -vehicle_1_miles,-vehicle_2_miles,-workfromhome,-vehicle_charging_own:-charging_install,-fastcharge_likely,
                -charging_5mile_non_1:-charging_5mile_own_2,
                -vehicle_wherecharge_1,-vehicle_wherecharge_2,-vehicle_wherecharge_3,-vehicle_whencharge,
                -vehicle_next_fuel,
                
                -replan_heat_owner:-replan_nocool_renter,-elec_savemoney_rent,-elec_health_renter,
                -natgas_ypccc,
                
                -elec_home_cost_1:-elec_home_cost_4,-elec_home_others_1:-elec_home_who_4,
                -heatpump_motiv,-induction_appeal,-induction_downsides,
                
                -cost_fuel_winter,-cost_electric_winter,-cost_combo_winter_summed,-cost_combo_winter,
                -cost_combo_summer,-cost_electric_summer,
                -ccinsure_owner,-ccinsure_renter,
                
                -elec_home_cost_PV,
                -elec_home_cost_EV,-vehicle_next_when,-vehicle_1_class,
                -elec_home_cost_HP,-elec_home_cost_IC,
                -cooling_plan,-heating_plan,
                
                -solstor_wtp_payback,-heatpump_wtp_payback,-ss_upfr_text,-ssupfront_num,-hpupfront_num,-evupfront_num,
                -primary_cooling_type_25_TEXT,-primary_heating_type_64_TEXT,-heatenergy_furnace,-heatenergy_boiler,-hotwater_energy_6_TEXT,
                -heatpump_adopter
                # -future_EV10
  ) %>%
  relocate(wt_ca, .after = last_col()) %>%
  relocate(tractid, .after = last_col()) %>% 
  dplyr::select(-ExternalReference, -tractid,
                # remove NAs
                -induction_direct, -heatpump_direct, -ac_unit_number,-hotwater_nextelec,
                # remove not significant or collinear variables
                -charging_access,-charging_5mile_r, -ccinsure_both,-yale_worried,-ideology,-hvac_avail
                
                # -charging_work, -vehicle_num,
                # -born_us, -employment, -education, -race, -gender, -lowincome_sup,
                # -elec_health, -upfrontpayback, -elec_safety, -ccinsure_lost,
                # -ccpastmove, -elec_avail, -ccmove_where, -homevac
  ) %>% 
  relocate(wt_ca, .after = last_col())


### with Home own
data <- data %>% 
  dplyr::select(-matches("future"), matches(paste0("_",ffu)),
                -matches("combo")) # collinear with wtp

categorical_vars <- c(
  "climatezone","dac","gender","race","education","born_us","employment","home_type","home_age","home_own","charging_work",
  "vehicle_next_used","primary_heating_type","hotwater_energy","previous_heating_type",
  "primary_cooling_type","kitchen_range_type",
  "upfrontpayback","lowincome_sup","outage_generatorown",
  "ccinsure_lost","ccmove_where","charging_5mile_f",
  "electrification","peer_EV","peer_PV","peer_HP","peer_IC","home_own"
)

for (var in categorical_vars) {
  # Check if var is in the dataframe colnames first
  if (var %in% colnames(data)) { # Check in 'x' now, not 'data'
    data[[var]] <- as.factor(data[[var]])
  }
}


tech <- c("PV","EV","HP","IC","PS")
plot <- list()
tp1 <- data.frame()
for(k in 2:5){
  
  # with HW
  tot <- mreg(data, 
              remove = wtp_var,
              i = k,
              future = ffu)
  
  # without WH and subset 
  wtp_w <- mreg(data %>% 
                  filter(!is.na(!!sym(wtp_var[k]))), 
                remove = c(wtp_var, "home_own"),
                i = k,
                future = ffu)
  
  # with HW and subset 
  wtp_wo <- mreg(data %>% 
                   filter(home_own == 1), 
                 remove = c(wtp_var, "home_own"),
                 i = k,
                 future = ffu)
  
  plot[[k-1]] <- tot[[1]] %>% 
    mutate(class = "Total (+HO)") %>% 
    rbind(
      wtp_w[[1]] %>% 
        mutate(class = "WTP subset"),
      wtp_wo[[1]] %>% 
        mutate(class = "Homeown subset") 
    ) %>% 
    mutate(domain = as.character(domain),
           domain = ifelse(str_detect(var, "peer|Older home|BrandNew|EV range|Newer home|Fast"), "Scenario", domain),
           domain = factor(domain, levels = c("Housing","Heat/Cool","Cook","Vehicle","Resilience","Social","Behavior","Scenario"))) %>% 
    com_reg_plot(tech[[k]])
  
  
  tt <- tot[[1]] %>% 
    mutate(class = "Total (+HO)") %>% 
    rbind(
      wtp_w[[1]] %>% 
        mutate(class = "WTP subset"),
      wtp_wo[[1]] %>% 
        mutate(class = "Homeown subset") 
    ) %>% 
    mutate(domain = as.character(domain),
           domain = ifelse(str_detect(var, "peer|Older home|BrandNew|EV range|Newer home|Fast"), "Scenario", domain),
           domain = factor(domain, levels = c("Housing","Heat/Cool","Cook","Vehicle","Resilience","Social","Behavior","Scenario"))) %>% 
    mutate(tech = ipt[k])
  
  tp1 <- rbind(tt, tp1)
  
  # reg_plot(wtp_w[[1]], tech[k])
  
}

# Total +HO, homeown subset
tp1 %>% 
  mutate(class = factor(class, levels = c("Total (+HO)", "Homeown subset", "WTP subset"))) %>% 
  filter(domain == "Scenario") %>% 
  
  ggplot(aes(x = pe, y = reorder(var, pe), xmin=lwr, xmax=upr)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  geom_pointrangeh(aes(fill = color, color = class), 
                   position = position_dodge2v(height = 0.4), fatten = 4, size = 0.5,
                   pch=21) +
  
  facet_wrap(~ tech, scale = "free", nrow = 2) + 
  theme_bw() +
  
  coord_flip() +
  
  labs(x = "", y ="", fill = "Significant", color = "",
       title = "") +
  
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


# plot[[1]]

### combine
rbind(
  tp %>% 
    mutate(domain = as.character(domain),
           domain = ifelse(is.na(domain), "Scenario", domain)) %>% 
    filter(domain == "Scenario"),
  tp1 %>% 
    filter(domain == "Scenario") %>% 
    filter(class != "WTP subset")
) %>% 
  mutate(class = factor(class, levels = c("Total", "Total (+HO)", "Homeown subset", "WTP subset", "WTP subset (+WTP)"))) %>% 
  
  ggplot(aes(x = pe, y = reorder(var, pe), xmin=lwr, xmax=upr)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  geom_pointrangeh(aes(fill = color, color = class), 
                   position = position_dodge2v(height = 0.4), fatten = 4, size = 0.5,
                   pch=21) +
  
  facet_wrap(~ tech, scale = "free", nrow = 2) + 
  theme_bw() +
  
  coord_flip() +
  
  labs(x = "", y ="", fill = "Significant", color = "",
       title = "") +
  
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

### combine
dd1 <- rbind(
  tp %>% 
    mutate(domain = as.character(domain),
           domain = ifelse(is.na(domain), "Scenario", domain)) %>% 
    filter(domain == "Scenario"),
  tp1 %>% 
    filter(domain == "Scenario") %>% 
    filter(class != "WTP subset")
) %>% 
  mutate(model = "intended")


dd2 <- rbind(
  tp %>% 
    mutate(domain = as.character(domain),
           domain = ifelse(is.na(domain), "Scenario", domain)) %>% 
    filter(domain == "Scenario"),
  tp1 %>% 
    filter(domain == "Scenario") %>% 
    filter(class != "WTP subset")
) %>% 
  mutate(model = "high")


dd <- dd1 %>% 
  filter(class == "WTP subset (+WTP)") %>% 
  rbind(dd2 %>% 
  filter(class %in% c("Total (+HO)","WTP subset")))



# HP
php <-  dd %>% 
  mutate(class = factor(class, levels = c("Total", "Total (+HO)", "Homeown subset", "WTP subset", "WTP subset (+WTP)"))) %>% 
  filter(tech == "HP",
         !var %in% c("More EV range","Fast charger",
                     "Newer home","BrandNew home","Older home")) %>% 
  
  ggplot(aes(x = pe, y = reorder(var, pe), xmin=lwr, xmax=upr)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  geom_pointrangeh(aes(fill = color, color = class), 
                   position = position_dodge2v(height = 0.4), fatten = 4, size = 0.5,
                   pch=21) +
  
  facet_wrap(~ tech, scale = "free", nrow = 2) + 
  theme_bw() +
  
  coord_flip() +
  
  labs(x = "", y ="", fill = "Significant", color = "",
       title = "") +
  
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

# IC
pic <- dd %>% 
  mutate(class = factor(class, levels = c("Total", "Total (+HO)", "Homeown subset", "WTP subset", "WTP subset (+WTP)"))) %>% 
  filter(tech == "IC",
         !var %in% c("More EV range","Fast charger",
                     "Newer home","BrandNew home","Older home")) %>% 
  
  ggplot(aes(x = pe, y = reorder(var, pe), xmin=lwr, xmax=upr)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  geom_pointrangeh(aes(fill = color, color = class), 
                   position = position_dodge2v(height = 0.4), fatten = 4, size = 0.5,
                   pch=21) +
  
  facet_wrap(~ tech, scale = "free", nrow = 2) + 
  theme_bw() +
  
  coord_flip() +
  
  labs(x = "", y ="", fill = "Significant", color = "",
       title = "") +
  
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

# PS
pps <- dd %>% 
  mutate(class = factor(class, levels = c("Total", "Total (+HO)", "Homeown subset", "WTP subset", "WTP subset (+WTP)"))) %>% 
  filter(tech == "PS",
         !var %in% c("More EV range","Fast charger") ) %>% 
  
  ggplot(aes(x = pe, y = reorder(var, pe), xmin=lwr, xmax=upr)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  geom_pointrangeh(aes(fill = color, color = class), 
                   position = position_dodge2v(height = 0.4), fatten = 4, size = 0.5,
                   pch=21) +
  
  facet_wrap(~ tech, scale = "free", nrow = 2) + 
  theme_bw() +
  
  coord_flip() +
  
  labs(x = "", y ="", fill = "Significant", color = "",
       title = "") +
  
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


# EV
pev <- dd %>% 
  mutate(class = factor(class, levels = c("Total", "Total (+HO)", "Homeown subset", "WTP subset", "WTP subset (+WTP)"))) %>% 
  filter(tech == "EV") %>% 
  
  ggplot(aes(x = pe, y = reorder(var, pe), xmin=lwr, xmax=upr)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  geom_pointrangeh(aes(fill = color, color = class), 
                   position = position_dodge2v(height = 0.4), fatten = 4, size = 0.5,
                   pch=21) +
  
  facet_wrap(~ tech, scale = "free", nrow = 2) + 
  theme_bw() +
  
  coord_flip() +
  
  labs(x = "", y ="", fill = "Significant", color = "",
       title = "") +
  
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

ggarrange(pps, pev, php, pic,
          common.legend = T, legend = "bottom")


##############################################################################

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
                                       heatpump_direct == "Somewhat interested"~ 0,
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
                                        induction_direct == "Somewhat interested"~ 0,
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
                                 ev_wtp_pc == "never"~ NA),
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
                                      solstor_wtp_dv == "never"~ NA),
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
                                       heatpump_wtp_pc == "never"~ NA),
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
                                    induction_dv == "never" ~ NA),
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


data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
  data_process(ev = c("Fully electric")) %>% 
  data_cl() %>% 
  mutate(
    # future adoption
    # future_EV10 = ifelse((vehicle_next_when > 1 & vehicle_next_fuel == 1)| EV == 1, 1, 0),
    future_EV_0 = ifelse((vehicle_next_when == 1 & vehicle_next_fuel == 1)| EV == 1, 1, 0),
    future_EV_low = ifelse((vehicle_next_when == 1 & vehicle_next_fuel == 1 |
                              ev_wtp_pc <= 8000)| EV == 1, 1, 0),
    future_EV_high = ifelse((vehicle_next_when == 1 & vehicle_next_fuel == 1 |
                               ev_wtp_pc <= 12500)| EV == 1, 1, 0),
    
    future_PV = ifelse(solar_pv_plans %in% c("Yes") | PV == 1, 1, 0),
    future_PS_0 = ifelse((storage_plans %in% c("Yes")) | 
                           (solar_pv_plans %in% c("Yes")) |
                           PS == 1, 1, 0),
    future_PS_low = ifelse((storage_plans %in% c("Yes") | solstor_wtp_dv <= 5000) |
                             (solar_pv_plans %in% c("Yes") | solstor_wtp_dv <= 5000) |
                             PS == 1, 1, 0),
    future_PS_high = ifelse((storage_plans %in% c("Yes") | solstor_wtp_dv <= 12500) |
                              (solar_pv_plans %in% c("Yes") | solstor_wtp_dv <= 12500) |
                              PS == 1, 1, 0),
    
    future_HP_0 = ifelse(heatpump_direct == 1 | HP == 1, 1, 0),
    future_HP_low = ifelse(heatpump_direct == 1 | 
                             heatpump_wtp_pc <= 4000| HP == 1, 1, 0),
    future_HP_high = ifelse(heatpump_direct == 1 | 
                              heatpump_wtp_pc <= 8000| HP == 1, 1, 0),
    
    future_IC_0 = ifelse(induction_direct == 1| IC == 1, 1, 0),
    future_IC_low = ifelse(induction_direct == 1 |
                             induction_dv <= 300| IC == 1, 1, 0),
    future_IC_high = ifelse(induction_direct == 1 |
                              induction_dv <= 800| IC == 1, 1, 0)
  ) %>% 
  # mutate(across(matches("future"), ~ ifelse(is.na(.), 0, .))) %>% 
  
  
  # remove variables
  dplyr::select(-pid,-pid_dem,-pid_ind,-pid_rep,
                -solar_install_owner,-solar_install_renter,-solar_install,-solar_date_owner,-solar_pv_plans,-storage_plans,-storage_own,
                -vehicle_1_miles,-vehicle_2_miles,-workfromhome,-vehicle_charging_own:-charging_install,-fastcharge_likely,
                -charging_5mile_non_1:-charging_5mile_own_2,
                -vehicle_wherecharge_1,-vehicle_wherecharge_2,-vehicle_wherecharge_3,-vehicle_whencharge,
                -vehicle_next_fuel,
                
                -replan_heat_owner:-replan_nocool_renter,-elec_savemoney_rent,-elec_health_renter,
                -natgas_ypccc,
                
                -elec_home_cost_1:-elec_home_cost_4,-elec_home_others_1:-elec_home_who_4,
                -heatpump_motiv,-induction_appeal,-induction_downsides,
                
                -cost_fuel_winter,-cost_electric_winter,-cost_combo_winter_summed,-cost_combo_winter,
                -cost_combo_summer,-cost_electric_summer,
                -ccinsure_owner,-ccinsure_renter,
                
                -elec_home_cost_PV,
                -elec_home_cost_EV,-vehicle_next_when,-vehicle_1_class,
                -elec_home_cost_HP,-elec_home_cost_IC,
                -cooling_plan,-heating_plan,
                
                -solstor_wtp_payback,-heatpump_wtp_payback,-ss_upfr_text,-ssupfront_num,-hpupfront_num,-evupfront_num,
                -primary_cooling_type_25_TEXT,-primary_heating_type_64_TEXT,-heatenergy_furnace,-heatenergy_boiler,-hotwater_energy_6_TEXT,
                -heatpump_adopter
                # -future_EV10
  ) %>%
  relocate(wt_ca, .after = last_col()) %>%
  relocate(tractid, .after = last_col()) %>% 
  dplyr::select(-ExternalReference, -tractid,
                # remove NAs
                -induction_direct, -heatpump_direct, -ac_unit_number,-hotwater_nextelec,
                # remove not significant or collinear variables
                -charging_access,-charging_5mile_r, -ccinsure_both,-yale_worried,-ideology,-hvac_avail
                
                # -charging_work, -vehicle_num,
                # -born_us, -employment, -education, -race, -gender, -lowincome_sup,
                # -elec_health, -upfrontpayback, -elec_safety, -ccinsure_lost,
                # -ccpastmove, -elec_avail, -ccmove_where, -homevac
  ) %>% 
  relocate(wt_ca, .after = last_col())




