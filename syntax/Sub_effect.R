source("./syntax/Function.R")
source("./syntax/Data.R")

data <- dat1
### peer effects 
select_scene <- list(c("peer_PV"),
                     c("peer_EV"),
                     c("peer_HP"),
                     c("peer_IC"),
                     c("peer_PV"))

peer <- data.frame()
# for(k in 1:5){
#   
#   # no sum contrasts
#   if(k == 1){
#     dat <- data %>% 
#       dplyr::select(VAR[[4]], climatezone, dac, matches("PV|PS|EV|HP|IC", ignore.case = FALSE),wt_ca)
#   }else{
#     dat <- data %>% 
#       dplyr::select(VAR[[k-1]], climatezone, dac, matches("PV|PS|EV|HP|IC", ignore.case = FALSE),wt_ca)
#   }   
#   
#   b_ev <- mreg_dac(dat, 
#           
#                    i = k,
#                    scenario = select_scene[[k]],
#                    # future = 0,
#                    peer = 1)
#   
#   num <- length(b_ev[[1]])
#   
#   df <- b_ev[[1]] %>% 
#     dplyr::select(matches("peer")) %>% 
#     dplyr::rename(Peer = names(.)[[1]],
#                   Neighbor = names(.)[[2]],
#                   Both = names(.)[[3]]) %>% 
#   
#     mutate(dac = row.names(.),
#            tech = ipt[k]) %>% 
#     gather(key, value, Peer:Both) %>% 
#     mutate(SE = c(b_ev[[2]][num-2,num-2,],
#                   b_ev[[2]][num-1,num-1,],
#                   b_ev[[2]][num,num,]))
#   
#   peer <- rbind(df, peer)
# }
for(k in 1:5){
  
  # no sum contrasts
  if(k == 1){
    da <- data %>% 
      dplyr::select(VAR[[4]], climatezone, dac, matches("PV|PS|EV|HP|IC", ignore.case = FALSE),wt_ca)
  }else{
    da <- data %>% 
      dplyr::select(VAR[[k-1]], climatezone, dac, matches("PV|PS|EV|HP|IC", ignore.case = FALSE),wt_ca)
  }   
  
  df <- mreg_peer(da, 
                   i = k,
                   scenario = select_scene[[k]],
                   # future = 0,
                   peer = 1)
  
  peer <- rbind(df, peer)
}


# f4a <- peer %>%
#   mutate(urban = ifelse(dac %in% c("Urban_DAC","Urban_Non_DAC"), "Urban", "Rural"),
#          dac = ifelse(dac %in% c("Urban_DAC","Rural_DAC"), "DAC", "Non-DAC")) %>% 
#   mutate(
#     tech = recode(tech,
#                   "PS" = "PV + Storage",
#                   "IC" = "Induction stoves",
#                   "HP" = "Heat pumps",
#                   "EV" = "Electric vehicles",
#                   "PV" = "Photovoltaics"),
#     tech = factor(tech, levels = c("Photovoltaics","PV + Storage", "Electric vehicles", "Heat pumps", "Induction stoves")),
#     # dac = factor(dac, levels = c("Urban_DAC", "Urban_Non_DAC", "Rural_DAC","Rural_Non_DAC")),
#     key = factor(key, levels = c("Both","Neighbor", "Peer"))
#   ) %>% 
#   ggplot(aes(x = key, y = estimate, fill = dac)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
#   geom_errorbar(
#     aes(ymin = conf.low, ymax = conf.high),
#     position = position_dodge(width = 0.7),
#     width = 0.2,
#     color = "gray50"
#   ) +
#   
#   facet_grid(urban ~ tech, switch = "y") +
#   # scale_fill_manual(values = c(
#   #   "Urban_DAC"      = "#d94801",  # rich burnt orange
#   #   "Rural_DAC"      = "#fdae6b",  # soft apricot
#   #   "Urban_Non_DAC"  = "#238b45",  # deep forest green
#   #   "Rural_Non_DAC"  = "#a1d99b"   # light sage green
#   # ),
#   # 
#   # labels = c("Urban DAC", "Urban Non-DAC", "Rural DAC", "Rural Non-DAC")) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   scale_fill_manual(values = c("#C2A385","#4A6273")) +
#   labs(
#     x = "",
#     y = "Effect magnitude",
#     fill = "",
#     title = ""
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     axis.text = element_text(size = 12),
#     axis.title = element_text(size = 12),
#     plot.title = element_text(face = "bold", size = 16, hjust = -0.1),
#     legend.text = element_text(size = 10),
#     legend.position = "bottom",
#     
#     strip.placement = "outside", # Keep labels on the outside
#     strip.background =element_rect(fill="gray22",color="gray22"),
#     strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=14, face = "bold"),
#     strip.text.y.left = element_text(angle = 0), # Ensure domain labels are horizontal
#     
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank()
#   ) 



f4a <- peer %>%
  filter(key == "Both") %>% 
  filter(!tech == "PS") %>% 
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
    key = "Peer effects"
  ) %>% 
  ggplot(aes(x = key, y = estimate, fill = dac)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.7),
    width = 0.2,
    color = "gray50"
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
  scale_fill_manual(values = c("#C2A385","#4A6273")) +
  labs(
    x = "Peer effect",
    y = "Effect magnitude",
    fill = "",
    title = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 16, hjust = -0.1),
    legend.text = element_text(size = 10),
    legend.position = "right",
    
    strip.placement = "outside", # Keep labels on the outside
    strip.background =element_rect(fill="gray22",color="gray22"),
    strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=14, face = "bold"),
    strip.text.y.left = element_text(angle = 0), # Ensure domain labels are horizontal
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) 


# ### add urban variable
# data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
#   data_process(ev = c("Fully electric")) %>% 
#   left_join(CA_t %>% 
#               st_drop_geometry() %>% 
#               dplyr::select(GEOID, estimate), by = c("tractid" = "GEOID")) %>% 
#   mutate(urban = ifelse(estimate > 5000, "Urban", "Rural")) %>% 
#   mutate(dac = ifelse(dac == 1 & urban == "Urban", "Urban_DAC",
#                       ifelse(dac == 1 & urban == "Rural", "Rural_DAC",
#                              ifelse(dac == 0 & urban == "Urban", "Urban_Non_DAC", "Rural_Non_DAC")))) %>% 
#   dplyr::select(-estimate, -urban) %>% 
#   data_clean(1)
# 
# 
# ### EV charger
# select_scene <- list(c("home_age","peer_PV"),
#                      c("home_age","peer_EV","charging_5mile_f","rangeanxiety"),
#                      c("peer_HP"),
#                      c("peer_IC"),
#                      c("home_age","peer_PV"))
# 
# k <- 2
# b_ev <- mreg_dac(data %>% 
#                    dplyr::select(VAR[[k-1]], climatezone, dac, matches("PV|PS|EV|HP|IC", ignore.case = FALSE),wt_ca), 
#                  scenario = select_scene[[k]],
#                  i = k)
#                  # future = 0 # change the scenarios
# 
# num <- length(b_ev[[1]])
# 
# df <- b_ev[[1]] %>% 
#   dplyr::select(matches("charging")) %>% 
#   dplyr::rename(Charging = names(.)[[1]]) %>% 
#   
#   mutate(dac = row.names(.)) %>% 
#   mutate(SE = b_ev[[2]][num-1,num-1,])
# 
# 
# f4b1 <- df %>%
#   mutate(urban = ifelse(dac %in% c("Urban_DAC","Urban_Non_DAC"), "Urban", "Rural"),
#          dac = ifelse(dac %in% c("Urban_DAC","Rural_DAC"), "DAC", "Non-DAC")) %>% 
#   ggplot(aes(x = urban, y = Charging, fill = dac)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
#   geom_errorbar(
#     aes(ymin = Charging - SE, ymax = Charging + SE),
#     position = position_dodge(width = 0.7),
#     width = 0.2
#   ) +
#   
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(
#     x = "",
#     y = "Effect magnitude",
#     fill = "",
#     title = "Charging density effect"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     axis.text = element_text(size = 12),
#     axis.title = element_text(size = 12),
#     plot.title = element_text(face = "bold", size = 16),
#     legend.text = element_text(size = 10),
#     legend.position = "bottom",
#     
#     strip.placement = "outside", # Keep labels on the outside
#     strip.background =element_rect(fill="gray22",color="gray22"),
#     strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=14, face = "bold"),
#     strip.text.y.left = element_text(angle = 0), # Ensure domain labels are horizontal
#     
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank()
#   ) 
# 
# 
# 
# ### PSPS 
# ### add psps variable
# crss <- CA_t %>% 
#   st_make_valid() %>% 
#   st_intersection(CA_2010 %>%
#                     left_join(psps %>% 
#                                 mutate(ps = winter+fall+spring+summer) %>% 
#                                 dplyr::select(GEOID, ps), by = "GEOID") %>% 
#                     dplyr::select(ps) %>% 
#                     st_make_valid())
# 
# # calculate intersected areas
# crss$area <- st_area(crss) %>% as.numeric()
# 
# # aggregate at tract level with area weights
# f_crss <- crss %>% 
#   st_drop_geometry() %>% 
#   group_by(GEOID) %>% 
#   summarise(ps = weighted.mean(ps, area, na.rm = T)) %>% 
#   ungroup() %>% 
#   mutate(psps = ifelse(is.na(ps), "none",
#                        ifelse(ps>5, "high", "low")))
# 
# 
# ### dac is combination of dac and psps
# data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
#   data_process(ev = c("Fully electric")) %>% 
#   left_join(f_crss %>% 
#               dplyr::select(-ps), by = c("tractid" = "GEOID")) %>% 
#   mutate(dac = ifelse(dac == 1 & psps == "high", "High_DAC",
#                       ifelse(dac == 1 & psps == "low", "Low_DAC",
#                              ifelse(dac == 1 & psps == "none", "None_DAC",
#                                     ifelse(dac == 0 & psps == "high", "High_Non_DAC", 
#                                            ifelse(dac == 0 & psps == "low", "Low_Non_DAC", "None_Non_DAC")))))) %>% 
#   data_clean(1)
# 
# 
# ### DAC, PSPS respondent distribution
# data$dac %>% table() %>% 
#   as.data.frame() %>% 
#   dplyr::rename(Category = ".",
#                 Count = "Freq") %>% 
#   mutate(
#     Level = case_when(
#       grepl("High", Category) ~ "High",
#       grepl("Low", Category) ~ "Low",
#       grepl("None", Category) ~ "None"
#     ),
#     DAC = case_when(
#       grepl("Non_DAC", Category) ~ "Non_DAC",
#       grepl("DAC", Category) ~ "DAC"
#     )
#   ) %>% 
#   ggplot(aes(x = Level, y = Count, fill = DAC)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(title = "",
#        x = "PSPS",
#        y = "Count",
#        fill = "") +
#   theme_minimal()
# 
# 
# # ### dac is just dac
# # data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>%
# #   data_process(ev = c("Fully electric")) %>%
# #   data_clean(1)
# 
# 
# select_scene <- list(c("peer_PV"),
#                      c("peer_EV","rangeanxiety"),
#                      c("peer_HP"),
#                      c("peer_IC"),
#                      c("peer_PV"))
# 
# k <- 5
# b_ev <- mreg_dac(data %>% 
#                    dplyr::select(VAR[[k-1]], climatezone, dac, matches("PV|PS|EV|HP|IC", ignore.case = FALSE),wt_ca), 
#                  scenario = select_scene[[k]],
#                  i = k,
#                  future = 0)
# 
# num <- length(b_ev[[1]])
# 
# df <- b_ev[[1]] %>% 
#   dplyr::select(matches("Inter")) %>% 
#   dplyr::rename(`Regional Effect` = "(Intercept)") %>% 
#   mutate(dac = row.names(.)) %>% 
#   mutate(SE = b_ev[[2]][num-2,num-2,])
# 
# 
# ### newer home effect on dac 
# f4b2 <- df %>%
#     mutate(psps = ifelse(str_detect(dac, "High"), "High PSPS",
#                          ifelse(str_detect(dac, "Low"), "Low PSPS", "No PSPS")),
#            dac = ifelse(str_detect(dac, "Non_DAC"), "Non-DAC", "DAC")) %>%
#   ggplot(aes(x = psps, y = `Regional Effect`, fill = dac)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
#   geom_errorbar(
#     aes(ymin = `Regional Effect` - SE, ymax = `Regional Effect` + SE),
#     position = position_dodge(width = 0.7),
#     width = 0.2
#   ) +
#   
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(
#     x = "",
#     y = "Effect magnitude",
#     fill = "",
#     title = "PV + Storage by PSPS and DAC status"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     axis.text = element_text(size = 12),
#     axis.title = element_text(size = 12),
#     plot.title = element_text(face = "bold", size = 16),
#     legend.text = element_text(size = 10),
#     legend.position = "bottom",
#     
#     strip.placement = "outside", # Keep labels on the outside
#     strip.background =element_rect(fill="gray22",color="gray22"),
#     strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=14, face = "bold"),
#     strip.text.y.left = element_text(angle = 0), # Ensure domain labels are horizontal
#     
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank()
#   ) 
#
# ### random effect of psps and dac
# ### dac is combination of dac and psps
# data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>%
#   data_process(ev = c("Fully electric")) %>%
#   left_join(f_crss %>%
#               dplyr::select(-ps), by = c("tractid" = "GEOID")) %>%
#   mutate(dac = ifelse(dac == 1 & psps == "high", "High_DAC",
#                       ifelse(dac == 1 & psps == "low", "Low_DAC",
#                              ifelse(dac == 1 & psps == "none", "None_DAC",
#                                     ifelse(dac == 0 & psps == "high", "High_Non_DAC",
#                                            ifelse(dac == 0 & psps == "low", "Low_Non_DAC", "None_Non_DAC")))))) %>%
#   dplyr::select(-psps) %>%
#   data_clean(1)
# 
# 
# k <- 5
# b_ev <- mreg(data,
#              remove = c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv",
#                         "education","employment"),
#              i = k,
#              future = 0)
# 
# b_ev[[3]] %>%
#   mutate(psps = ifelse(str_detect(zone, "High"), "High PSPS",
#                        ifelse(str_detect(zone, "Low"), "Low PSPS", "No PSPS")),
#          dac = ifelse(str_detect(zone, "Non_DAC"), "Non-DAC", "DAC")) %>%
#   ggplot(aes(x = psps, y = R_effect, fill = dac)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
#   geom_errorbar(
#     aes(ymin = R_effect - SE, ymax = R_effect + SE),
#     position = position_dodge(width = 0.7),
#     width = 0.2
#   ) +
# 
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(
#     x = "",
#     y = "Effect magnitude",
#     fill = "",
#     title = "Random effect"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     axis.text = element_text(size = 12),
#     axis.title = element_text(size = 12),
#     plot.title = element_text(face = "bold", size = 16),
#     legend.text = element_text(size = 10),
#     legend.position = "bottom",
# 
#     strip.placement = "outside", # Keep labels on the outside
#     strip.background =element_rect(fill="gray22",color="gray22"),
#     strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=14, face = "bold"),
#     strip.text.y.left = element_text(angle = 0), # Ensure domain labels are horizontal
# 
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank()
#   )
# 
# 
# ### random effect of psps and home type
# ### dac is combination of psps and home type
# data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
#   data_process(ev = c("Fully electric")) %>% 
#   left_join(f_crss %>% 
#               dplyr::select(-ps), by = c("tractid" = "GEOID")) %>% 
#   data_clean(1) %>% 
#   mutate(dac = ifelse(home_type == "SF" & psps == "high", "High_SF",
#                       ifelse(home_type == "SF" & psps == "low", "Low_SF",
#                              ifelse(home_type == "SF" & psps == "none", "None_SF",
#                                     ifelse(home_type == "MF" & psps == "high", "High_MF", 
#                                            ifelse(home_type == "MF" & psps == "low", "Low_MF", 
#                                                   ifelse(home_type == "MF" & psps == "none", "None_MF", 
#                                                          ifelse(home_type == "Others" & psps == "high", "High_Oth", 
#                                                                 ifelse(home_type == "Others" & psps == "low", "Low_Oth","None_Oth"))))))))) %>% 
#   dplyr::select(-psps, -home_type) 
# 
# 
# mreg_ht <- function(data, remove = NULL, i, scenario = NULL, future = NULL){
#   
#   # data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1)
#   # remove <- c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv", "education","employment")
#   # scenario <- c("peer_PV")
#   # i <- 5
#   # future <- 0
#   
#   da_r <- data %>% 
#     dplyr::select(# remove multicollinear variables
#       
#       # remove predictors with substantial NAs
#       -heatpump_direct,-induction_direct,
#       
#       # remove future adoption
#       -starts_with("future")
#     ) %>% 
#     dplyr::select(-remove)
#   
#   if(i == 1){
#     future_var <- sym(paste0("future_", ipt[i]))
#   }else{
#     future_var <- sym(paste0("future_", ipt[i],"_",future))
#   }
#   
#   
#   if(!is.null(future)){
#     da_r <- data %>% 
#       mutate(!!ipt[i] := .data[[future_var]]) %>% 
#       dplyr::select(# remove multicollinear variables
#         
#         # remove predictors with substantial NAs
#         -heatpump_direct,-induction_direct,
#         
#         # remove future adoption
#         -starts_with("future")
#       ) %>% 
#       dplyr::select(-remove)
#   }
#   
#   
#   # binary
#   bina <- c(
#     "born_us",
#     
#     "charging_5mile_f",
#     "vehicle_next_used",
#     # "vehicle_next_fuel",
#     "charging_work",
#     
#     "therm_winter",
#     
#     "upfrontpayback",
#     "outage_generatorown",
#     "ccmove_where"
#   ) %>% 
#     setdiff(remove)
#   
#   # categorical variables: 
#   cat <- c("race",
#            # "born_us",
#            # "home_type",
#            "employment",
#            
#            "peer_EV",
#            "peer_PV",
#            "peer_HP",
#            "peer_IC",
#            
#            # "charging_5mile_f",
#            # "vehicle_next_used",
#            # "vehicle_next_fuel",
#            # "charging_work",
#            
#            "primary_heating_type",
#            "primary_cooling_type",
#            # "therm_winter",
#            
#            "kitchen_range_type",
#            "electrification"
#            # "upfrontpayback",
#            # "outage_generatorown",
#            # "ccmove_where"
#   ) %>% 
#     setdiff(remove)
#   
#   exclusions <- list(
#     c("peer_EV", "peer_HP", "peer_IC"),
#     c("peer_PV", "peer_HP", "peer_IC"),
#     c("peer_EV", "peer_PV", "peer_IC"),
#     c("peer_EV", "peer_HP", "peer_PV")
#   )
#   
#   if(i %in% c(1,5)){
#     cat <- cat %>% setdiff(exclusions[[1]])
#   }else{
#     cat <- cat %>% setdiff(exclusions[[i]])
#   }
#   
#   
#   ftr <- c("climatezone",
#            "dac",
#            "race",
#            "born_us",
#            # "home_type",
#            "employment",
#            
#            "peer_EV",
#            "peer_PV",
#            "peer_HP",
#            "peer_IC",
#            
#            "charging_5mile_f",
#            "vehicle_next_used",
#            # "vehicle_next_fuel",
#            "charging_work",
#            
#            "primary_heating_type",
#            "primary_cooling_type",
#            "therm_winter",
#            
#            "kitchen_range_type",
#            "electrification",
#            "upfrontpayback",
#            "outage_generatorown",
#            "ccmove_where"
#   ) %>% 
#     setdiff(remove)
#   
#   # da$race %>% unique
#   da_r[,ftr] <- data.frame(lapply(da_r[ftr],as.factor)) 
#   
#   # dummy sum contrast based on reference category (-1)
#   for (var in cat) {
#     k <- length(levels(da_r[[var]]))
#     contrasts(da_r[[var]]) <- contr.sum(k)
#     colnames(contrasts(da_r[[var]])) <- levels(da_r[[var]])[1:(k-1)]
#   }
#   
#   # for binary
#   for (var in bina) {
#     da_r[[var]] <- factor(da_r[[var]], levels = c(1,0))
#     contrasts(da_r[[var]]) <- contr.sum(2)
#     colnames(contrasts(da_r[[var]])) <- levels(da_r[[var]])[1]
#   }
#   
#   da_r <- da_r %>% 
#   mutate(across(where(is.numeric) & !c("PV","PS","EV","HP","IC","wt_ca"), ~ scale(.) %>% as.numeric())) %>% 
#     mutate(across(any_of(c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv")), ~ .x * -1))
#   
#   # lapply(da_r, unique)
#   # summary(da_r)
#   
#   if(i %in% c(1,5)){
#     # for PV, remove zone effect, tech
#     tract <- c("climatezone","dac","ev_wtp_pc","heatpump_wtp_pc","induction_dv","wt_ca",
#                "peer_EV","peer_HP","peer_IC",
#                "PV","PS","EV","HP","IC")
#     
#     model1vars <- setdiff(names(da_r), tract)
#     
#   }else if(i == 2){
#     # for EV, remove zone effect, tech
#     tract <- c("climatezone","dac","solstor_wtp_dv","heatpump_wtp_pc","induction_dv","wt_ca",
#                "peer_IC","peer_HP","peer_PV",
#                "PV","PS","EV","HP","IC")
#     model1vars <- setdiff(names(da_r), tract)
#     
#   }else if(i == 3){
#     # for HP, remove zone effect, tech, heating/cooling type
#     tract <- c("climatezone","dac","ev_wtp_pc","solstor_wtp_dv","induction_dv","wt_ca",
#                "peer_EV","peer_IC","peer_PV",
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
#   model1vars <- setdiff(model1vars, scenario)
#   
#   if (is.null(scenario)) {
#     fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), 
#                              "+ (1|climatezone) + (1|dac)"))
#   } else {
#     fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), 
#                              "+ (1+",paste(scenario, collapse = " + "),"|climatezone) + (1+",paste(scenario, collapse = " + "),
#                              "|dac)"))
#   }
#   
#   fit <- lmer(fvar, weights = wt_ca, data = da_r)
#   # summary(fit)
#   sum_fit <- summary(fit)$coef %>% 
#     as.data.frame() %>% 
#     tibble::rownames_to_column("var") %>% 
#     dplyr::select(var, Estimate)
#   
#   m <- dim(fit %>% tidy() %>% filter(is.na(group)))[[1]]
#   
#   sims <- 1000
#   pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
#   pe <- pe[1:m,]
#   
#   vc <- vcov(fit)
#   simbetas <- mvrnorm(sims, pe, vc)
#   colnames(simbetas)[1] <- "Cont"
#   
#   # Create an empty data frame with NA values and the specified row and column names
#   df <- as.data.frame(matrix(NA, nrow = length(colnames(simbetas)), ncol = length(colnames(simbetas))))
#   rownames(df) <- colnames(simbetas)
#   colnames(df) <- colnames(simbetas)
#   
#   ### adding reference categories
#   reference_levels <- c()
#   for (var in cat) {
#     k <- length(levels(da_r[[var]]))
#     reference_levels[var] <- levels(da_r[[var]])[k]
#   }
#   
#   cat_ref <- paste0(names(reference_levels),reference_levels)
#   
#   df_r <- as.data.frame(matrix(NA, nrow = length(cat_ref), ncol = length(colnames(simbetas))))
#   rownames(df_r) <- cat_ref
#   colnames(df_r) <- colnames(simbetas)
#   
#   df_r1 <- df_r2 <- df_r
#   df_r2[is.na(df_r2)] <- 0
#   
#   for(v in names(reference_levels)){
#     # Identify target row(s) starting with "race"
#     target_rows <- grep(paste0("^",v), rownames(df_r))
#     
#     # Identify target columns starting with "race"
#     target_cols <- grep(paste0("^",v), colnames(df_r))
#     
#     # Replace NAs with -1 for matching row-column combinations
#     for (r in target_rows) {
#       
#       df_r1[r, target_cols] <- ifelse(is.na(df_r[r, target_cols]), -1, 0)
#     }
#   }
#   df_r1[is.na(df_r1)] <- 0
#   
#   
#   df1 <- df2 <- df 
#   # Replace values in each row
#   for (j in seq_along(colnames(simbetas))) {
#     df1[j, ] <- 0              # Set all elements in the row to 0
#     df1[j, colnames(simbetas)[j]] <- 1  # Set the element in the column matching the row name to 1
#   }
#   
#   df1 <- df1[-1,] %>% rbind(df_r1)
#   
#   for (j in seq_along(colnames(simbetas))) {
#     df2[j, ] <- 0              # Set all elements in the row to 0
#     df2[j, colnames(simbetas)[j]] <- 0  # Set the element in the column matching the row name to 0
#   }
#   
#   df2 <- df2[-1,] %>% rbind(df_r2)
#   
#   x <- df1 %>% rbind(df2) %>% 
#     mutate(Cont = 1) %>% as.matrix()
#   
#   m <- m + length(cat_ref)
#   xbeta <- x %*% t(simbetas) 
#   
#   xbeta <- xbeta[1:(m-1),] - xbeta[m:(2*(m-1)),]
#   
#   pe <- apply(xbeta, 1, mean) 
#   lwr <- apply(xbeta, 1, quantile, probs= 0.975) 
#   upr <- apply(xbeta, 1, quantile, probs= 0.025)
#   
#   dd <- cbind(pe,upr,lwr) %>%
#     as.data.frame() %>%
#     tibble::rownames_to_column("var") %>%
#     mutate(color = ifelse(upr*lwr >0, "Y", "N")) %>%
#     mutate(IV = ipt[i])
#   
#   if (is.null(scenario)) {
#     tp <- sqrt(attr(ranef(fit, condVar=T)[[1]], "postVar"))*1.96 
#     
#     ds <- as.data.frame(ranef(fit)$climatezone) %>% 
#       dplyr::rename(R_effect = "(Intercept)") %>% 
#       tibble::rownames_to_column("zone") %>% 
#       mutate(SE = tp[1,1,],
#              lower = R_effect - SE,
#              upper = R_effect + SE) %>% 
#       mutate(IV = ipt[i])
#     
#     
#     tp <- sqrt(attr(ranef(fit, condVar=T)[[2]], "postVar"))*1.96 
#     
#     dc <- as.data.frame(ranef(fit)$dac) %>% 
#       dplyr::rename(R_effect = "(Intercept)") %>% 
#       tibble::rownames_to_column("zone") %>% 
#       mutate(SE = tp[1,1,],
#              lower = R_effect - SE,
#              upper = R_effect + SE) %>% 
#       mutate(IV = ipt[i])
#     
#     
#   } else {
#     tp <- sqrt(attr(ranef(fit, condVar=T)[[1]], "postVar"))*1.96 
#     
#     d <- as.data.frame(ranef(fit)$climatezone) %>%
#       tibble::rownames_to_column("zone") 
#     
#     colnames(d)[1:2] <- c("zone","RE")
#     
#     n <- length(d)-1
#     
#     # extract relevant SE values dynamically
#     se_vec <- unlist(lapply(1:n, function(i) tp[i, i, ]))
#     
#     ds <- d %>% 
#       gather(scene, R_effect, -zone) %>% 
#       mutate(SE = se_vec,
#              lower = R_effect - SE,
#              upper = R_effect + SE) %>%
#       mutate(IV = ipt[i])
#     
#     
#     tp <- sqrt(attr(ranef(fit, condVar=T)[[2]], "postVar"))*1.96 
#     
#     d <- as.data.frame(ranef(fit)$dac) %>%
#       tibble::rownames_to_column("zone") 
#     
#     colnames(d)[1:2] <- c("zone","RE")
#     
#     n <- length(d)-1
#     
#     # extract relevant SE values dynamically
#     se_vec <- unlist(lapply(1:n, function(i) tp[i, i, ]))
#     
#     dc <- d %>% 
#       gather(scene, R_effect, -zone) %>% 
#       mutate(SE = se_vec,
#              lower = R_effect - SE,
#              upper = R_effect + SE) %>%
#       mutate(IV = ipt[i])
#     
#   }
#   
#   
#   # dd$var %>% unique() 
#   # da$primary_heating_type %>% unique() 
#   name_mapping <- c(
#     "PV" = "PV", 
#     "PS" = "PS",
#     "EV" = "EV", 
#     "HP" = "HP",
#     "IC" = "IC",
#     
#     "born_us1" = "Born US",
#     "upfrontpayback1" = "Upfront prefer",
#     "therm_winter1" = "Thermostat use",
#     "outage_generatorown1" = "Own generator",
#     
#     "charging_5mile_f1" = "Fast charger",
#     # "vehicle_next_fuel1" = "EV to buy",
#     "vehicle_next_used1" = "New car",
#     # "vehicle_next_when" = "Later to buy car",
#     "vehicle_1_miles" = "Milage",
#     "charging_work1" = "Charger at work",
#     
#     "home_age" = "Newer home",
#     "rangeanxiety" = "more range for EV",
#     "ccmove_where1" = "Leave CA",
#     "homevac" = "Evacuation"
#     
#   )
#   
#   reg <- dd %>% 
#     mutate(var = recode(var, !!!name_mapping),
#            domain = ifelse(str_detect(var,"income|education|race|ideology|pid|US|employment"), "Sociodemo",
#                            
#                            ifelse(str_detect(var, "home|household"), "Housing",
#                                   
#                                   ifelse(str_detect(var,"primary|Thermostat"), "Heat/Cool",
#                                          
#                                          ifelse(var %in% dd$var[grep(pattern="kitchen",
#                                                                      dd$var)], "Cook",
#                                                 
#                                                 ifelse(str_detect(var,"therm|burden|savemoney|health|safety|Upfront|direct|plan|electrification"), "Behavior",
#                                                        
#                                                        ifelse(str_detect(var, "outage_impact|freq|dur|generator"), "Resilience", 
#                                                               
#                                                               ifelse(str_detect(var, "peer"), "Peer Effect", 
#                                                                      ifelse(str_detect(var, "Milage|charger|Charger|car|EV|l1l2|dc"), "Vehicle", 
#                                                                             ifelse(str_detect(var, "payback|dv|pc"), "WTP",
#                                                                                    ifelse(str_detect(var, "cc|CA|Evacuation"), "Climate", var)))))))))),
#            domain = factor(domain, levels = c("Housing","Heat/Cool","Cook","Vehicle","Sociodemo","Behavior","Resilience","Climate","Peer Effect","WTP"))) %>% 
#     
#     mutate(IV = factor(IV, levels = ipt)) %>% 
#     
#     mutate(color = factor(color, levels = c("Y","N")))
#   
#   return(list(reg, ds, dc, sum_fit))
# }
# 
# 
# k <- 5
# b_ev <- mreg_ht(data, 
#              remove = c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv",
#                         "education","employment"),
#              i = k,
#              future = 0)
# 
# b_ev[[3]] %>%
#   mutate(psps = ifelse(str_detect(zone, "High"), "High PSPS", 
#                        ifelse(str_detect(zone, "Low"), "Low PSPS", "No PSPS")),
#          dac = ifelse(str_detect(zone, "SF"), "SF", 
#                       ifelse(str_detect(zone, "MF"), "MF", "Others")),
#          dac = factor(dac, levels = c("SF","MF","Others"))) %>% 
#   ggplot(aes(x = psps, y = R_effect, fill = dac)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
#   geom_errorbar(
#     aes(ymin = R_effect - SE, ymax = R_effect + SE),
#     position = position_dodge(width = 0.7),
#     width = 0.2
#   ) +
#   
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(
#     x = "",
#     y = "Effect magnitude",
#     fill = "",
#     title = "Random effect"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     axis.text = element_text(size = 12),
#     axis.title = element_text(size = 12),
#     plot.title = element_text(face = "bold", size = 16),
#     legend.text = element_text(size = 10),
#     legend.position = "bottom",
#     
#     strip.placement = "outside", # Keep labels on the outside
#     strip.background =element_rect(fill="gray22",color="gray22"),
#     strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=14, face = "bold"),
#     strip.text.y.left = element_text(angle = 0), # Ensure domain labels are horizontal
#     
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank()
#   ) 
# 
# 
# 
# ### random effect of psps and race
# ### dac is combination of psps and race
# data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
#   data_process(ev = c("Fully electric")) %>% 
#   left_join(f_crss %>% 
#               dplyr::select(-ps), by = c("tractid" = "GEOID")) %>% 
#   data_clean(1) %>% 
#   mutate(dac = ifelse(race == "Multirace" & psps == "high", "High_Multirace",
#                       ifelse(race == "Multirace" & psps == "low", "Low_Multirace",
#                              ifelse(race == "Multirace" & psps == "none", "None_Multirace",
#                                     ifelse(race == "White" & psps == "high", "High_White",
#                                            ifelse(race == "White" & psps == "low", "Low_White",
#                                                   ifelse(race == "White" & psps == "none", "None_White",
#                                                          ifelse(race == "Asian" & psps == "high", "High_Asian",
#                                                                 ifelse(race == "Asian" & psps == "low", "Low_Asian",
#                                                                        ifelse(race == "Asian" & psps == "none", "None_Asian",
#                                                                               ifelse(race == "Hispanic" & psps == "high", "High_Hispanic",
#                                                                                      ifelse(race == "Hispanic" & psps == "low", "Low_Hispanic",
#                                                                                             ifelse(race == "Hispanic" & psps == "none", "None_Hispanic",
#                                                                                                    ifelse(race == "Natives" & psps == "high", "High_Natives",
#                                                                                                           ifelse(race == "Natives" & psps == "low", "Low_Natives",
#                                                                                                                  ifelse(race == "Natives" & psps == "none", "None_Natives",
#                                                                                                                         ifelse(race == "Black" & psps == "high", "High_Black",
#                                                                                                                                ifelse(race == "Black" & psps == "low", "Low_Black", "None_Black")))))))))))))))))) %>% 
#   dplyr::select(-psps, -race) 
# 
# 
# mreg_race <- function(data, remove = NULL, i, scenario = NULL, future = NULL){
#   
#   # data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1)
#   # remove <- c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv", "education","employment")
#   # scenario <- c("peer_PV")
#   # i <- 5
#   # future <- 127
#   
#   da_r <- data %>% 
#     dplyr::select(# remove multicollinear variables
#       
#       # remove predictors with substantial NAs
#       -heatpump_direct,-induction_direct,
#       
#       # remove future adoption
#       -starts_with("future")
#     ) %>% 
#     dplyr::select(-remove)
#   
#   if(i == 1){
#     future_var <- sym(paste0("future_", ipt[i]))
#   }else{
#     future_var <- sym(paste0("future_", ipt[i],"_",future))
#   }
#   
#   
#   if(!is.null(future)){
#     da_r <- data %>% 
#       mutate(!!ipt[i] := .data[[future_var]]) %>% 
#       dplyr::select(# remove multicollinear variables
#         
#         # remove predictors with substantial NAs
#         -heatpump_direct,-induction_direct,
#         
#         # remove future adoption
#         -starts_with("future")
#       ) %>% 
#       dplyr::select(-remove)
#   }
#   
#   
#   # binary
#   bina <- c(
#     "born_us",
#     
#     "charging_5mile_f",
#     "vehicle_next_used",
#     # "vehicle_next_fuel",
#     "charging_work",
#     
#     "therm_winter",
#     
#     "upfrontpayback",
#     "outage_generatorown",
#     "ccmove_where"
#   ) %>% 
#     setdiff(remove)
#   
#   # categorical variables: 
#   cat <- c(
#     # "race",
#            # "born_us",
#            "home_type",
#            "employment",
#            
#            "peer_EV",
#            "peer_PV",
#            "peer_HP",
#            "peer_IC",
#            
#            # "charging_5mile_f",
#            # "vehicle_next_used",
#            # "vehicle_next_fuel",
#            # "charging_work",
#            
#            "primary_heating_type",
#            "primary_cooling_type",
#            # "therm_winter",
#            
#            "kitchen_range_type",
#            "electrification"
#            # "upfrontpayback",
#            # "outage_generatorown",
#            # "ccmove_where"
#   ) %>% 
#     setdiff(remove)
#   
#   exclusions <- list(
#     c("peer_EV", "peer_HP", "peer_IC"),
#     c("peer_PV", "peer_HP", "peer_IC"),
#     c("peer_EV", "peer_PV", "peer_IC"),
#     c("peer_EV", "peer_HP", "peer_PV")
#   )
#   
#   if(i %in% c(1,5)){
#     cat <- cat %>% setdiff(exclusions[[1]])
#   }else{
#     cat <- cat %>% setdiff(exclusions[[i]])
#   }
#   
#   
#   ftr <- c("climatezone",
#            "dac",
#            # "race",
#            "born_us",
#            "home_type",
#            "employment",
#            
#            "peer_EV",
#            "peer_PV",
#            "peer_HP",
#            "peer_IC",
#            
#            "charging_5mile_f",
#            "vehicle_next_used",
#            # "vehicle_next_fuel",
#            "charging_work",
#            
#            "primary_heating_type",
#            "primary_cooling_type",
#            "therm_winter",
#            
#            "kitchen_range_type",
#            "electrification",
#            "upfrontpayback",
#            "outage_generatorown",
#            "ccmove_where"
#   ) %>% 
#     setdiff(remove)
#   
#   # da$race %>% unique
#   da_r[,ftr] <- data.frame(lapply(da_r[ftr],as.factor)) 
#   
#   # dummy sum contrast based on reference category (-1)
#   for (var in cat) {
#     k <- length(levels(da_r[[var]]))
#     contrasts(da_r[[var]]) <- contr.sum(k)
#     colnames(contrasts(da_r[[var]])) <- levels(da_r[[var]])[1:(k-1)]
#   }
#   
#   # for binary
#   for (var in bina) {
#     da_r[[var]] <- factor(da_r[[var]], levels = c(1,0))
#     contrasts(da_r[[var]]) <- contr.sum(2)
#     colnames(contrasts(da_r[[var]])) <- levels(da_r[[var]])[1]
#   }
#   
#   da_r <- da_r %>% 
#     # mutate(race = relevel(race, ref = "White"),
#     #        peer_PV = relevel(peer_PV, ref = "none"),
#     #        peer_EV = relevel(peer_EV, ref = "none"),
#     #        peer_HP = relevel(peer_HP, ref = "none"),
#     #        peer_IC = relevel(peer_IC, ref = "none"),
#     #        primary_heating_type = relevel(primary_heating_type, ref = "Central"),
#     #        primary_cooling_type = relevel(primary_cooling_type, ref = "None"),
#     #        kitchen_range_type = relevel(kitchen_range_type, ref = "Natural_gas"),
#     #        electrification = relevel(electrification, ref = "No_prefer")
#     # ) %>%  # relevel for categorical variables 
#     # 
#   # fastDummies::dummy_cols(
#   #   select_columns = cat[!cat %in% c("climatezone","dac")],  # categorical variables
#   #   remove_first_dummy = TRUE,    # Avoid multicollinearity
#   #   remove_selected_columns = TRUE  # Drop original factor columns
#   # ) %>% 
#   
#   mutate(across(where(is.numeric) & !c("PV","PS","EV","HP","IC","wt_ca"), ~ scale(.) %>% as.numeric())) %>% 
#     mutate(across(any_of(c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv")), ~ .x * -1))
#   
#   # lapply(da_r, unique)
#   # summary(da_r)
#   
#   if(i %in% c(1,5)){
#     # for PV, remove zone effect, tech
#     tract <- c("climatezone","dac","ev_wtp_pc","heatpump_wtp_pc","induction_dv","wt_ca",
#                "peer_EV","peer_HP","peer_IC",
#                "PV","PS","EV","HP","IC")
#     
#     model1vars <- setdiff(names(da_r), tract)
#     
#   }else if(i == 2){
#     # for EV, remove zone effect, tech
#     tract <- c("climatezone","dac","solstor_wtp_dv","heatpump_wtp_pc","induction_dv","wt_ca",
#                "peer_IC","peer_HP","peer_PV",
#                "PV","PS","EV","HP","IC")
#     model1vars <- setdiff(names(da_r), tract)
#     
#   }else if(i == 3){
#     # for HP, remove zone effect, tech, heating/cooling type
#     tract <- c("climatezone","dac","ev_wtp_pc","solstor_wtp_dv","induction_dv","wt_ca",
#                "peer_EV","peer_IC","peer_PV",
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
#   model1vars <- setdiff(model1vars, scenario)
#   
#   if (is.null(scenario)) {
#     fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), 
#                              "+ (1|climatezone) + (1|dac)"))
#   } else {
#     fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), 
#                              "+ (1+",paste(scenario, collapse = " + "),"|climatezone) + (1+",paste(scenario, collapse = " + "),
#                              "|dac)"))
#   }
#   
#   fit <- lmer(fvar, weights = wt_ca, data = da_r)
#   # summary(fit)
#   sum_fit <- summary(fit)$coef %>% 
#     as.data.frame() %>% 
#     tibble::rownames_to_column("var") %>% 
#     dplyr::select(var, Estimate)
#   
#   m <- dim(fit %>% tidy() %>% filter(is.na(group)))[[1]]
#   
#   sims <- 1000
#   pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
#   pe <- pe[1:m,]
#   
#   vc <- vcov(fit)
#   simbetas <- mvrnorm(sims, pe, vc)
#   colnames(simbetas)[1] <- "Cont"
#   
#   # Create an empty data frame with NA values and the specified row and column names
#   df <- as.data.frame(matrix(NA, nrow = length(colnames(simbetas)), ncol = length(colnames(simbetas))))
#   rownames(df) <- colnames(simbetas)
#   colnames(df) <- colnames(simbetas)
#   
#   ### adding reference categories
#   reference_levels <- c()
#   for (var in cat) {
#     k <- length(levels(da_r[[var]]))
#     reference_levels[var] <- levels(da_r[[var]])[k]
#   }
#   
#   cat_ref <- paste0(names(reference_levels),reference_levels)
#   
#   df_r <- as.data.frame(matrix(NA, nrow = length(cat_ref), ncol = length(colnames(simbetas))))
#   rownames(df_r) <- cat_ref
#   colnames(df_r) <- colnames(simbetas)
#   
#   df_r1 <- df_r2 <- df_r
#   df_r2[is.na(df_r2)] <- 0
#   
#   for(v in names(reference_levels)){
#     # Identify target row(s) starting with "race"
#     target_rows <- grep(paste0("^",v), rownames(df_r))
#     
#     # Identify target columns starting with "race"
#     target_cols <- grep(paste0("^",v), colnames(df_r))
#     
#     # Replace NAs with -1 for matching row-column combinations
#     for (r in target_rows) {
#       
#       df_r1[r, target_cols] <- ifelse(is.na(df_r[r, target_cols]), -1, 0)
#     }
#   }
#   df_r1[is.na(df_r1)] <- 0
#   
#   
#   df1 <- df2 <- df 
#   # Replace values in each row
#   for (j in seq_along(colnames(simbetas))) {
#     df1[j, ] <- 0              # Set all elements in the row to 0
#     df1[j, colnames(simbetas)[j]] <- 1  # Set the element in the column matching the row name to 1
#   }
#   
#   df1 <- df1[-1,] %>% rbind(df_r1)
#   
#   for (j in seq_along(colnames(simbetas))) {
#     df2[j, ] <- 0              # Set all elements in the row to 0
#     df2[j, colnames(simbetas)[j]] <- 0  # Set the element in the column matching the row name to 0
#   }
#   
#   df2 <- df2[-1,] %>% rbind(df_r2)
#   
#   x <- df1 %>% rbind(df2) %>% 
#     mutate(Cont = 1) %>% as.matrix()
#   
#   m <- m + length(cat_ref)
#   xbeta <- x %*% t(simbetas) 
#   
#   xbeta <- xbeta[1:(m-1),] - xbeta[m:(2*(m-1)),]
#   
#   pe <- apply(xbeta, 1, mean) 
#   lwr <- apply(xbeta, 1, quantile, probs= 0.975) 
#   upr <- apply(xbeta, 1, quantile, probs= 0.025)
#   
#   dd <- cbind(pe,upr,lwr) %>%
#     as.data.frame() %>%
#     tibble::rownames_to_column("var") %>%
#     mutate(color = ifelse(upr*lwr >0, "Y", "N")) %>%
#     mutate(IV = ipt[i])
#   
#   if (is.null(scenario)) {
#     tp <- sqrt(attr(ranef(fit, condVar=T)[[2]], "postVar"))*1.96 
#     
#     ds <- as.data.frame(ranef(fit)$climatezone) %>% 
#       dplyr::rename(R_effect = "(Intercept)") %>% 
#       tibble::rownames_to_column("zone") %>% 
#       mutate(SE = tp[1,1,],
#              lower = R_effect - SE,
#              upper = R_effect + SE) %>% 
#       mutate(IV = ipt[i])
#     
#     
#     tp <- sqrt(attr(ranef(fit, condVar=T)[[1]], "postVar"))*1.96 
#     
#     dc <- as.data.frame(ranef(fit)$dac) %>% 
#       dplyr::rename(R_effect = "(Intercept)") %>% 
#       tibble::rownames_to_column("zone") %>% 
#       mutate(SE = tp[1,1,],
#              lower = R_effect - SE,
#              upper = R_effect + SE) %>% 
#       mutate(IV = ipt[i])
#     
#     
#   } else {
#     tp <- sqrt(attr(ranef(fit, condVar=T)[[1]], "postVar"))*1.96 
#     
#     d <- as.data.frame(ranef(fit)$climatezone) %>%
#       tibble::rownames_to_column("zone") 
#     
#     colnames(d)[1:2] <- c("zone","RE")
#     
#     n <- length(d)-1
#     
#     # extract relevant SE values dynamically
#     se_vec <- unlist(lapply(1:n, function(i) tp[i, i, ]))
#     
#     ds <- d %>% 
#       gather(scene, R_effect, -zone) %>% 
#       mutate(SE = se_vec,
#              lower = R_effect - SE,
#              upper = R_effect + SE) %>%
#       mutate(IV = ipt[i])
#     
#     
#     tp <- sqrt(attr(ranef(fit, condVar=T)[[2]], "postVar"))*1.96 
#     
#     d <- as.data.frame(ranef(fit)$dac) %>%
#       tibble::rownames_to_column("zone") 
#     
#     colnames(d)[1:2] <- c("zone","RE")
#     
#     n <- length(d)-1
#     
#     # extract relevant SE values dynamically
#     se_vec <- unlist(lapply(1:n, function(i) tp[i, i, ]))
#     
#     dc <- d %>% 
#       gather(scene, R_effect, -zone) %>% 
#       mutate(SE = se_vec,
#              lower = R_effect - SE,
#              upper = R_effect + SE) %>%
#       mutate(IV = ipt[i])
#     
#   }
#   
#   
#   # dd$var %>% unique() 
#   # da$primary_heating_type %>% unique() 
#   name_mapping <- c(
#     "PV" = "PV", 
#     "PS" = "PS",
#     "EV" = "EV", 
#     "HP" = "HP",
#     "IC" = "IC",
#     
#     "born_us1" = "Born US",
#     "upfrontpayback1" = "Upfront prefer",
#     "therm_winter1" = "Thermostat use",
#     "outage_generatorown1" = "Own generator",
#     
#     "charging_5mile_f1" = "Fast charger",
#     # "vehicle_next_fuel1" = "EV to buy",
#     "vehicle_next_used1" = "New car",
#     # "vehicle_next_when" = "Later to buy car",
#     "vehicle_1_miles" = "Milage",
#     "charging_work1" = "Charger at work",
#     
#     "home_age" = "Newer home",
#     "rangeanxiety" = "more range for EV",
#     "ccmove_where1" = "Leave CA",
#     "homevac" = "Evacuation"
#     
#   )
#   
#   reg <- dd %>% 
#     mutate(var = recode(var, !!!name_mapping),
#            domain = ifelse(str_detect(var,"income|education|race|ideology|pid|US|employment"), "Sociodemo",
#                            
#                            ifelse(str_detect(var, "home|household"), "Housing",
#                                   
#                                   ifelse(str_detect(var,"primary|Thermostat"), "Heat/Cool",
#                                          
#                                          ifelse(var %in% dd$var[grep(pattern="kitchen",
#                                                                      dd$var)], "Cook",
#                                                 
#                                                 ifelse(str_detect(var,"therm|burden|savemoney|health|safety|Upfront|direct|plan|electrification"), "Behavior",
#                                                        
#                                                        ifelse(str_detect(var, "outage_impact|freq|dur|generator"), "Resilience", 
#                                                               
#                                                               ifelse(str_detect(var, "peer"), "Peer Effect", 
#                                                                      ifelse(str_detect(var, "Milage|charger|Charger|car|EV|l1l2|dc"), "Vehicle", 
#                                                                             ifelse(str_detect(var, "payback|dv|pc"), "WTP",
#                                                                                    ifelse(str_detect(var, "cc|CA|Evacuation"), "Climate", var)))))))))),
#            domain = factor(domain, levels = c("Housing","Heat/Cool","Cook","Vehicle","Sociodemo","Behavior","Resilience","Climate","Peer Effect","WTP"))) %>% 
#     
#     mutate(IV = factor(IV, levels = ipt)) %>% 
#     
#     mutate(color = factor(color, levels = c("Y","N")))
#   
#   return(list(reg, ds, dc, sum_fit))
# }
# 
# 
# k <- 5
# b_ev <- mreg_race(data, 
#                 remove = c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv",
#                            "education","employment"),
#                 i = k,
#                 future = 0)
# 
# b_ev[[3]] %>%
#   mutate(psps = ifelse(str_detect(zone, "High"), "High PSPS", 
#                        ifelse(str_detect(zone, "Low"), "Low PSPS", "No PSPS")),
#          dac = ifelse(str_detect(zone, "Asian"), "Asian", 
#                       ifelse(str_detect(zone, "Black"), "Black", 
#                              ifelse(str_detect(zone, "Hispanic"), "Hispanic", 
#                                     ifelse(str_detect(zone, "Multirace"), "Multirace", 
#                                            ifelse(str_detect(zone, "Natives"), "Natives", "White"))))),
#          dac = factor(dac, levels = c("Hispanic","White","Black","Asian","Multirace","Natives"))) %>% 
#   ggplot(aes(x = psps, y = R_effect, fill = dac)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
#   geom_errorbar(
#     aes(ymin = R_effect - SE, ymax = R_effect + SE),
#     position = position_dodge(width = 0.7),
#     width = 0.2
#   ) +
#   
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(
#     x = "",
#     y = "Effect magnitude",
#     fill = "",
#     title = "Random effect"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     axis.text = element_text(size = 12),
#     axis.title = element_text(size = 12),
#     plot.title = element_text(face = "bold", size = 16),
#     legend.text = element_text(size = 10),
#     legend.position = "bottom",
#     
#     strip.placement = "outside", # Keep labels on the outside
#     strip.background =element_rect(fill="gray22",color="gray22"),
#     strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=14, face = "bold"),
#     strip.text.y.left = element_text(angle = 0), # Ensure domain labels are horizontal
#     
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank()
#   ) 
# 
# 
# 
# ### random effect of race
# ### dac is race
# data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
#   data_process(ev = c("Fully electric")) %>% 
#   data_clean(1) %>% 
#   mutate(dac = race) %>% 
#   dplyr::select(-race) 
# 
# 
# mreg_race <- function(data, remove = NULL, i, scenario = NULL, future = NULL){
#   
#   # data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1)
#   # remove <- c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv", "education","employment")
#   # scenario <- c("peer_PV")
#   # i <- 5
#   # future <- 127
#   
#   da_r <- data %>% 
#     dplyr::select(# remove multicollinear variables
#       
#       # remove predictors with substantial NAs
#       -heatpump_direct,-induction_direct,
#       
#       # remove future adoption
#       -starts_with("future")
#     ) %>% 
#     dplyr::select(-remove)
#   
#   if(i == 1){
#     future_var <- sym(paste0("future_", ipt[i]))
#   }else{
#     future_var <- sym(paste0("future_", ipt[i],"_",future))
#   }
#   
#   
#   if(!is.null(future)){
#     da_r <- data %>% 
#       mutate(!!ipt[i] := .data[[future_var]]) %>% 
#       dplyr::select(# remove multicollinear variables
#         
#         # remove predictors with substantial NAs
#         -heatpump_direct,-induction_direct,
#         
#         # remove future adoption
#         -starts_with("future")
#       ) %>% 
#       dplyr::select(-remove)
#   }
#   
#   
#   # binary
#   bina <- c(
#     "born_us",
#     
#     "charging_5mile_f",
#     "vehicle_next_used",
#     # "vehicle_next_fuel",
#     "charging_work",
#     
#     "therm_winter",
#     
#     "upfrontpayback",
#     "outage_generatorown",
#     "ccmove_where"
#   ) %>% 
#     setdiff(remove)
#   
#   # categorical variables: 
#   cat <- c(
#     # "race",
#     # "born_us",
#     "home_type",
#     "employment",
#     
#     "peer_EV",
#     "peer_PV",
#     "peer_HP",
#     "peer_IC",
#     
#     # "charging_5mile_f",
#     # "vehicle_next_used",
#     # "vehicle_next_fuel",
#     # "charging_work",
#     
#     "primary_heating_type",
#     "primary_cooling_type",
#     # "therm_winter",
#     
#     "kitchen_range_type",
#     "electrification"
#     # "upfrontpayback",
#     # "outage_generatorown",
#     # "ccmove_where"
#   ) %>% 
#     setdiff(remove)
#   
#   exclusions <- list(
#     c("peer_EV", "peer_HP", "peer_IC"),
#     c("peer_PV", "peer_HP", "peer_IC"),
#     c("peer_EV", "peer_PV", "peer_IC"),
#     c("peer_EV", "peer_HP", "peer_PV")
#   )
#   
#   if(i %in% c(1,5)){
#     cat <- cat %>% setdiff(exclusions[[1]])
#   }else{
#     cat <- cat %>% setdiff(exclusions[[i]])
#   }
#   
#   
#   ftr <- c("climatezone",
#            "dac",
#            # "race",
#            "born_us",
#            "home_type",
#            "employment",
#            
#            "peer_EV",
#            "peer_PV",
#            "peer_HP",
#            "peer_IC",
#            
#            "charging_5mile_f",
#            "vehicle_next_used",
#            # "vehicle_next_fuel",
#            "charging_work",
#            
#            "primary_heating_type",
#            "primary_cooling_type",
#            "therm_winter",
#            
#            "kitchen_range_type",
#            "electrification",
#            "upfrontpayback",
#            "outage_generatorown",
#            "ccmove_where"
#   ) %>% 
#     setdiff(remove)
#   
#   # da$race %>% unique
#   da_r[,ftr] <- data.frame(lapply(da_r[ftr],as.factor)) 
#   
#   # dummy sum contrast based on reference category (-1)
#   for (var in cat) {
#     k <- length(levels(da_r[[var]]))
#     contrasts(da_r[[var]]) <- contr.sum(k)
#     colnames(contrasts(da_r[[var]])) <- levels(da_r[[var]])[1:(k-1)]
#   }
#   
#   # for binary
#   for (var in bina) {
#     da_r[[var]] <- factor(da_r[[var]], levels = c(1,0))
#     contrasts(da_r[[var]]) <- contr.sum(2)
#     colnames(contrasts(da_r[[var]])) <- levels(da_r[[var]])[1]
#   }
#   
#   da_r <- da_r %>% 
#     # mutate(race = relevel(race, ref = "White"),
#     #        peer_PV = relevel(peer_PV, ref = "none"),
#     #        peer_EV = relevel(peer_EV, ref = "none"),
#     #        peer_HP = relevel(peer_HP, ref = "none"),
#     #        peer_IC = relevel(peer_IC, ref = "none"),
#     #        primary_heating_type = relevel(primary_heating_type, ref = "Central"),
#     #        primary_cooling_type = relevel(primary_cooling_type, ref = "None"),
#     #        kitchen_range_type = relevel(kitchen_range_type, ref = "Natural_gas"),
#     #        electrification = relevel(electrification, ref = "No_prefer")
#     # ) %>%  # relevel for categorical variables 
#     # 
#   # fastDummies::dummy_cols(
#   #   select_columns = cat[!cat %in% c("climatezone","dac")],  # categorical variables
#   #   remove_first_dummy = TRUE,    # Avoid multicollinearity
#   #   remove_selected_columns = TRUE  # Drop original factor columns
#   # ) %>% 
#   
#   mutate(across(where(is.numeric) & !c("PV","PS","EV","HP","IC","wt_ca"), ~ scale(.) %>% as.numeric())) %>% 
#     mutate(across(any_of(c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv")), ~ .x * -1))
#   
#   # lapply(da_r, unique)
#   # summary(da_r)
#   
#   if(i %in% c(1,5)){
#     # for PV, remove zone effect, tech
#     tract <- c("climatezone","dac","ev_wtp_pc","heatpump_wtp_pc","induction_dv","wt_ca",
#                "peer_EV","peer_HP","peer_IC",
#                "PV","PS","EV","HP","IC")
#     
#     model1vars <- setdiff(names(da_r), tract)
#     
#   }else if(i == 2){
#     # for EV, remove zone effect, tech
#     tract <- c("climatezone","dac","solstor_wtp_dv","heatpump_wtp_pc","induction_dv","wt_ca",
#                "peer_IC","peer_HP","peer_PV",
#                "PV","PS","EV","HP","IC")
#     model1vars <- setdiff(names(da_r), tract)
#     
#   }else if(i == 3){
#     # for HP, remove zone effect, tech, heating/cooling type
#     tract <- c("climatezone","dac","ev_wtp_pc","solstor_wtp_dv","induction_dv","wt_ca",
#                "peer_EV","peer_IC","peer_PV",
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
#   model1vars <- setdiff(model1vars, scenario)
#   
#   if (is.null(scenario)) {
#     fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), 
#                              "+ (1|climatezone) + (1|dac)"))
#   } else {
#     fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), 
#                              "+ (1+",paste(scenario, collapse = " + "),"|climatezone) + (1+",paste(scenario, collapse = " + "),
#                              "|dac)"))
#   }
#   
#   fit <- lmer(fvar, weights = wt_ca, data = da_r)
#   # summary(fit)
#   sum_fit <- summary(fit)$coef %>% 
#     as.data.frame() %>% 
#     tibble::rownames_to_column("var") %>% 
#     dplyr::select(var, Estimate)
#   
#   m <- dim(fit %>% tidy() %>% filter(is.na(group)))[[1]]
#   
#   sims <- 1000
#   pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
#   pe <- pe[1:m,]
#   
#   vc <- vcov(fit)
#   simbetas <- mvrnorm(sims, pe, vc)
#   colnames(simbetas)[1] <- "Cont"
#   
#   # Create an empty data frame with NA values and the specified row and column names
#   df <- as.data.frame(matrix(NA, nrow = length(colnames(simbetas)), ncol = length(colnames(simbetas))))
#   rownames(df) <- colnames(simbetas)
#   colnames(df) <- colnames(simbetas)
#   
#   ### adding reference categories
#   reference_levels <- c()
#   for (var in cat) {
#     k <- length(levels(da_r[[var]]))
#     reference_levels[var] <- levels(da_r[[var]])[k]
#   }
#   
#   cat_ref <- paste0(names(reference_levels),reference_levels)
#   
#   df_r <- as.data.frame(matrix(NA, nrow = length(cat_ref), ncol = length(colnames(simbetas))))
#   rownames(df_r) <- cat_ref
#   colnames(df_r) <- colnames(simbetas)
#   
#   df_r1 <- df_r2 <- df_r
#   df_r2[is.na(df_r2)] <- 0
#   
#   for(v in names(reference_levels)){
#     # Identify target row(s) starting with "race"
#     target_rows <- grep(paste0("^",v), rownames(df_r))
#     
#     # Identify target columns starting with "race"
#     target_cols <- grep(paste0("^",v), colnames(df_r))
#     
#     # Replace NAs with -1 for matching row-column combinations
#     for (r in target_rows) {
#       
#       df_r1[r, target_cols] <- ifelse(is.na(df_r[r, target_cols]), -1, 0)
#     }
#   }
#   df_r1[is.na(df_r1)] <- 0
#   
#   
#   df1 <- df2 <- df 
#   # Replace values in each row
#   for (j in seq_along(colnames(simbetas))) {
#     df1[j, ] <- 0              # Set all elements in the row to 0
#     df1[j, colnames(simbetas)[j]] <- 1  # Set the element in the column matching the row name to 1
#   }
#   
#   df1 <- df1[-1,] %>% rbind(df_r1)
#   
#   for (j in seq_along(colnames(simbetas))) {
#     df2[j, ] <- 0              # Set all elements in the row to 0
#     df2[j, colnames(simbetas)[j]] <- 0  # Set the element in the column matching the row name to 0
#   }
#   
#   df2 <- df2[-1,] %>% rbind(df_r2)
#   
#   x <- df1 %>% rbind(df2) %>% 
#     mutate(Cont = 1) %>% as.matrix()
#   
#   m <- m + length(cat_ref)
#   xbeta <- x %*% t(simbetas) 
#   
#   xbeta <- xbeta[1:(m-1),] - xbeta[m:(2*(m-1)),]
#   
#   pe <- apply(xbeta, 1, mean) 
#   lwr <- apply(xbeta, 1, quantile, probs= 0.975) 
#   upr <- apply(xbeta, 1, quantile, probs= 0.025)
#   
#   dd <- cbind(pe,upr,lwr) %>%
#     as.data.frame() %>%
#     tibble::rownames_to_column("var") %>%
#     mutate(color = ifelse(upr*lwr >0, "Y", "N")) %>%
#     mutate(IV = ipt[i])
#   
#   if (is.null(scenario)) {
#     tp <- sqrt(attr(ranef(fit, condVar=T)[[1]], "postVar"))*1.96 
#     
#     ds <- as.data.frame(ranef(fit)$climatezone) %>% 
#       dplyr::rename(R_effect = "(Intercept)") %>% 
#       tibble::rownames_to_column("zone") %>% 
#       mutate(SE = tp[1,1,],
#              lower = R_effect - SE,
#              upper = R_effect + SE) %>% 
#       mutate(IV = ipt[i])
#     
#     
#     tp <- sqrt(attr(ranef(fit, condVar=T)[[2]], "postVar"))*1.96 
#     
#     dc <- as.data.frame(ranef(fit)$dac) %>% 
#       dplyr::rename(R_effect = "(Intercept)") %>% 
#       tibble::rownames_to_column("zone") %>% 
#       mutate(SE = tp[1,1,],
#              lower = R_effect - SE,
#              upper = R_effect + SE) %>% 
#       mutate(IV = ipt[i])
#     
#     
#   } else {
#     tp <- sqrt(attr(ranef(fit, condVar=T)[[1]], "postVar"))*1.96 
#     
#     d <- as.data.frame(ranef(fit)$climatezone) %>%
#       tibble::rownames_to_column("zone") 
#     
#     colnames(d)[1:2] <- c("zone","RE")
#     
#     n <- length(d)-1
#     
#     # extract relevant SE values dynamically
#     se_vec <- unlist(lapply(1:n, function(i) tp[i, i, ]))
#     
#     ds <- d %>% 
#       gather(scene, R_effect, -zone) %>% 
#       mutate(SE = se_vec,
#              lower = R_effect - SE,
#              upper = R_effect + SE) %>%
#       mutate(IV = ipt[i])
#     
#     
#     tp <- sqrt(attr(ranef(fit, condVar=T)[[2]], "postVar"))*1.96 
#     
#     d <- as.data.frame(ranef(fit)$dac) %>%
#       tibble::rownames_to_column("zone") 
#     
#     colnames(d)[1:2] <- c("zone","RE")
#     
#     n <- length(d)-1
#     
#     # extract relevant SE values dynamically
#     se_vec <- unlist(lapply(1:n, function(i) tp[i, i, ]))
#     
#     dc <- d %>% 
#       gather(scene, R_effect, -zone) %>% 
#       mutate(SE = se_vec,
#              lower = R_effect - SE,
#              upper = R_effect + SE) %>%
#       mutate(IV = ipt[i])
#     
#   }
#   
#   
#   # dd$var %>% unique() 
#   # da$primary_heating_type %>% unique() 
#   name_mapping <- c(
#     "PV" = "PV", 
#     "PS" = "PS",
#     "EV" = "EV", 
#     "HP" = "HP",
#     "IC" = "IC",
#     
#     "born_us1" = "Born US",
#     "upfrontpayback1" = "Upfront prefer",
#     "therm_winter1" = "Thermostat use",
#     "outage_generatorown1" = "Own generator",
#     
#     "charging_5mile_f1" = "Fast charger",
#     # "vehicle_next_fuel1" = "EV to buy",
#     "vehicle_next_used1" = "New car",
#     # "vehicle_next_when" = "Later to buy car",
#     "vehicle_1_miles" = "Milage",
#     "charging_work1" = "Charger at work",
#     
#     "home_age" = "Newer home",
#     "rangeanxiety" = "more range for EV",
#     "ccmove_where1" = "Leave CA",
#     "homevac" = "Evacuation"
#     
#   )
#   
#   reg <- dd %>% 
#     mutate(var = recode(var, !!!name_mapping),
#            domain = ifelse(str_detect(var,"income|education|race|ideology|pid|US|employment"), "Sociodemo",
#                            
#                            ifelse(str_detect(var, "home|household"), "Housing",
#                                   
#                                   ifelse(str_detect(var,"primary|Thermostat"), "Heat/Cool",
#                                          
#                                          ifelse(var %in% dd$var[grep(pattern="kitchen",
#                                                                      dd$var)], "Cook",
#                                                 
#                                                 ifelse(str_detect(var,"therm|burden|savemoney|health|safety|Upfront|direct|plan|electrification"), "Behavior",
#                                                        
#                                                        ifelse(str_detect(var, "outage_impact|freq|dur|generator"), "Resilience", 
#                                                               
#                                                               ifelse(str_detect(var, "peer"), "Peer Effect", 
#                                                                      ifelse(str_detect(var, "Milage|charger|Charger|car|EV|l1l2|dc"), "Vehicle", 
#                                                                             ifelse(str_detect(var, "payback|dv|pc"), "WTP",
#                                                                                    ifelse(str_detect(var, "cc|CA|Evacuation"), "Climate", var)))))))))),
#            domain = factor(domain, levels = c("Housing","Heat/Cool","Cook","Vehicle","Sociodemo","Behavior","Resilience","Climate","Peer Effect","WTP"))) %>% 
#     
#     mutate(IV = factor(IV, levels = ipt)) %>% 
#     
#     mutate(color = factor(color, levels = c("Y","N")))
#   
#   return(list(reg, ds, dc, sum_fit))
# }
# 
# 
# k <- 5
# b_ev <- mreg_race(data, 
#                   remove = c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv",
#                              "education","employment"),
#                   i = k,
#                   future = 0)
# 
# b_ev[[3]] %>%
#   mutate(dac = ifelse(str_detect(zone, "Asian"), "Asian", 
#                       ifelse(str_detect(zone, "Black"), "Black", 
#                              ifelse(str_detect(zone, "Hispanic"), "Hispanic", 
#                                     ifelse(str_detect(zone, "Multirace"), "Multirace", 
#                                            ifelse(str_detect(zone, "Natives"), "Natives", "White"))))),
#          dac = factor(dac, levels = c("Hispanic","White","Black","Asian","Multirace","Natives"))) %>% 
#   ggplot(aes(x = dac, y = R_effect, fill = dac)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
#   geom_errorbar(
#     aes(ymin = R_effect - SE, ymax = R_effect + SE),
#     position = position_dodge(width = 0.7),
#     width = 0.2
#   ) +
#   
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(
#     x = "",
#     y = "Effect magnitude",
#     fill = "",
#     title = "Random effect"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     axis.text = element_text(size = 12),
#     axis.title = element_text(size = 12),
#     plot.title = element_text(face = "bold", size = 16),
#     legend.text = element_text(size = 10),
#     legend.position = "bottom",
#     
#     strip.placement = "outside", # Keep labels on the outside
#     strip.background =element_rect(fill="gray22",color="gray22"),
#     strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=14, face = "bold"),
#     strip.text.y.left = element_text(angle = 0), # Ensure domain labels are horizontal
#     
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank()
#   ) 

### heat pump 
tech <- c("PV","EV","HP","IC","PS")
k <- 3 

b_ev <- mreg(dat %>% 
               dplyr::select(-c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv","solstor_wtp_dv"),
                             -c("PS_int","EV_int","HP_int","IC_int")) %>% 
               dplyr::select(VAR[[k-1]], climatezone, dac, matches("PV|PS|EV|HP|IC", ignore.case = FALSE),wt_ca), 
             i = k)
             # future = 0)


climate_zone_data <- tibble(
  zone = 1:16,
  Representative_City = c(
    "Arcata", "Santa Rosa", "Oakland", "San Jose (Reid-Hillview)",
    "Santa Maria", "Torrance", "San Diego (Lindbergh)", "Fullerton",
    "Burbank (Glendale)", "Riverside", "Red Bluff", "Sacramento",
    "Fresno", "Palmdale", "Palm Springs (Intl)", "Blue Canyon"
  ),
  HDD = c(
    4496, 2844, 2909, 2335, 2844, 1856, 1362, 1500,
    1656, 1718, 2548, 2588, 2470, 3703, 1004, 6088
  ),
  CDD = c(
    0, 456, 128, 574, 456, 215, 227, 567,
    771, 1324, 1819, 1292, 1891, 1141, 4133, 148
  )
) %>% 
  mutate(zone = as.character(zone))


a <- b_ev[[2]] %>% 
  left_join(climate_zone_data, by = "zone") %>% 
  select(zone, R_effect, lower, upper, HDD, CDD) %>%
  pivot_longer(cols = c(HDD, CDD), names_to = "Degree_Type", values_to = "Degree_Days") %>% 
  ggplot(aes(x = Degree_Days, y = R_effect, color = Degree_Type, fill = Degree_Type)) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(title = "",
       x = "Degree days",
       y = "Heat pump random effect",
       color = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom")


# d <- map_pl(dat_pr(b_ev[[2]], b_ev[[3]]), tech[k]) +
#   labs(title = "", fill = "") 
b <- cz %>% 
  left_join(b_ev[[2]], by = c("BZone" = "zone")) %>% 
  mutate(centroid = st_point_on_surface(geometry)) %>%   # safer than st_centroid
  ggplot() +
  geom_sf(fill = "white", color = "gray0") + # US border
  geom_sf(aes(fill = R_effect), color = NA, size = 0.3) +
  
  geom_shadowtext(
    aes(label = BZone, geometry = centroid),
    stat = "sf_coordinates",
    color = "white",        # text color
    bg.color = "black",     # outline color
    bg.r = 0.15,            # outline thickness
    size = 4
  ) +

  # geom_sf_text(
  #   aes(label = BZone, geometry = centroid),
  #   size = 4,
  #   fontface = "bold",
  #   color = "blue"
  # ) +
  
  theme_minimal() +
  scale_fill_viridis_c(option = "magma") +
  # scale_fill_distiller(palette = "RdBu", direction = -1) +

  labs(title = "", fill = "", x = "", y = "") +
  theme(legend.position = "bottom",
        # legend.text=element_text(size=6),
        # legend.key.size = unit(0.3, 'cm'),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title=element_text(family="Franklin Gothic Demi", size=15, hjust = 0.2))


c <- b_ev[[2]] %>% 
  left_join(climate_zone_data, by = "zone") %>% 
  mutate(IV = "Heat pumps") %>% 
  zone_plot() +
  labs(title = "", y = "Random effect in order of HDD")


f4c <- ggarrange(a,b,c, nrow = 1, widths = c(1.7,1,1.5))


ggsave("./fig/f4.png",
       ggarrange(f4a, f4c, nrow = 2,
                 heights = c(1,1),
                 labels = c("A", "B"),  # Adds labels to plots
                 label.x = 0,        # Adjust horizontal position of labels
                 label.y = 1,        # Adjust vertical position of labels
                 vjust = 1,
                 hjust = -1,
                 font.label = list(size = 14, face = "bold")),
       width = 12, height = 10)


# ### HP via AC
# k <- 3
# mreg_hp <- function(data, remove = NULL, i, scenario = NULL, future = NULL){
#   
#   # data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1)
#   # remove <- c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv", "education","employment")
#   # scenario <- c("peer_PV")
#   # i <- 5
#   # future <- 127
#   
#   da_r <- data %>% 
#     dplyr::select(# remove multicollinear variables
#       
#       # remove predictors with substantial NAs
#       -heatpump_direct,-induction_direct,
#       
#       # remove future adoption
#       -starts_with("future")
#     ) %>% 
#     dplyr::select(-remove)
#   
#   if(i == 1){
#     future_var <- sym(paste0("future_", ipt[i]))
#   }else{
#     future_var <- sym(paste0("future_", ipt[i],"_",future))
#   }
#   
#   
#   if(!is.null(future)){
#     da_r <- data %>% 
#       mutate(!!ipt[i] := .data[[future_var]]) %>% 
#       dplyr::select(# remove multicollinear variables
#         
#         # remove predictors with substantial NAs
#         -heatpump_direct,-induction_direct,
#         
#         # remove future adoption
#         -starts_with("future")
#       ) %>% 
#       dplyr::select(-remove)
#   }
#   
#   
#   # binary
#   bina <- c(
#     "born_us",
#     
#     "charging_5mile_f",
#     "vehicle_next_used",
#     # "vehicle_next_fuel",
#     "charging_work",
#     
#     "therm_winter",
#     
#     "upfrontpayback",
#     "outage_generatorown",
#     "ccmove_where"
#   ) %>% 
#     setdiff(remove)
#   
#   # categorical variables: 
#   cat <- c("race",
#            # "born_us",
#            "home_type",
#            "employment",
#            
#            "peer_EV",
#            "peer_PV",
#            "peer_HP",
#            "peer_IC",
#            
#            # "charging_5mile_f",
#            # "vehicle_next_used",
#            # "vehicle_next_fuel",
#            # "charging_work",
#            
#            "primary_heating_type",
#            "primary_cooling_type",
#            # "therm_winter",
#            
#            "kitchen_range_type",
#            "electrification",
#            # "upfrontpayback",
#            # "outage_generatorown",
#            # "ccmove_where"
#            "ac"
#   ) %>% 
#     setdiff(remove)
#   
#   exclusions <- list(
#     c("peer_EV", "peer_HP", "peer_IC"),
#     c("peer_PV", "peer_HP", "peer_IC"),
#     c("peer_EV", "peer_PV", "peer_IC"),
#     c("peer_EV", "peer_HP", "peer_PV")
#   )
#   
#   if(i %in% c(1,5)){
#     cat <- cat %>% setdiff(exclusions[[1]])
#   }else{
#     cat <- cat %>% setdiff(exclusions[[i]])
#   }
#   
#   
#   ftr <- c("climatezone",
#            "dac",
#            "race",
#            "born_us",
#            "home_type",
#            "employment",
#            
#            "peer_EV",
#            "peer_PV",
#            "peer_HP",
#            "peer_IC",
#            
#            "charging_5mile_f",
#            "vehicle_next_used",
#            # "vehicle_next_fuel",
#            "charging_work",
#            
#            "primary_heating_type",
#            "primary_cooling_type",
#            "therm_winter",
#            
#            "kitchen_range_type",
#            "electrification",
#            "upfrontpayback",
#            "outage_generatorown",
#            "ccmove_where",
#            "ac"
#   ) %>% 
#     setdiff(remove)
#   
#   # da$race %>% unique
#   da_r[,ftr] <- data.frame(lapply(da_r[ftr],as.factor)) 
#   
#   # dummy sum contrast based on reference category (-1)
#   for (var in cat) {
#     k <- length(levels(da_r[[var]]))
#     contrasts(da_r[[var]]) <- contr.sum(k)
#     colnames(contrasts(da_r[[var]])) <- levels(da_r[[var]])[1:(k-1)]
#   }
#   
#   # for binary
#   for (var in bina) {
#     da_r[[var]] <- factor(da_r[[var]], levels = c(1,0))
#     contrasts(da_r[[var]]) <- contr.sum(2)
#     colnames(contrasts(da_r[[var]])) <- levels(da_r[[var]])[1]
#   }
#   
#   da_r <- da_r %>% 
#     mutate(across(starts_with("peer"), ~factor(.x, levels = c("neighbor","peer","none")))) %>% # avoid the sum contrast
#   
#   mutate(across(where(is.numeric) & !c("PV","PS","EV","HP","IC","wt_ca"), ~ scale(.) %>% as.numeric())) %>% 
#     mutate(across(any_of(c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv")), ~ .x * -1))
#   
#   # lapply(da_r, unique)
#   # summary(da_r)
#   
#   if(i %in% c(1,5)){
#     # for PV, remove zone effect, tech
#     tract <- c("climatezone","dac","ev_wtp_pc","heatpump_wtp_pc","induction_dv","wt_ca",
#                "peer_EV","peer_HP","peer_IC",
#                "PV","PS","EV","HP","IC")
#     
#     model1vars <- setdiff(names(da_r), tract)
#     
#   }else if(i == 2){
#     # for EV, remove zone effect, tech
#     tract <- c("climatezone","dac","solstor_wtp_dv","heatpump_wtp_pc","induction_dv","wt_ca",
#                "peer_IC","peer_HP","peer_PV",
#                "PV","PS","EV","HP","IC")
#     model1vars <- setdiff(names(da_r), tract)
#     
#   }else if(i == 3){
#     # for HP, remove zone effect, tech, heating/cooling type
#     tract <- c("climatezone","dac","ev_wtp_pc","solstor_wtp_dv","induction_dv","wt_ca",
#                "peer_EV","peer_IC","peer_PV",
#                "PV","PS","EV","HP","IC","primary_heating_type","primary_cooling_type","ac")
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
#   model1vars <- setdiff(model1vars, scenario)
#   
# 
#     fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), 
#                              "+ (ac|climatezone) + dac"))
#     
#     fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), 
#                              "+ (1|climatezone) + dac + ac"))
# 
#   
#   fit <- lmer(fvar, weights = wt_ca, data = da_r)
#   # summary(fit)
#   sum_fit <- summary(fit)$coef %>% 
#     as.data.frame() %>% 
#     tibble::rownames_to_column("var") %>% 
#     dplyr::select(var, Estimate)
#   
#   m <- dim(fit %>% tidy() %>% filter(is.na(group)))[[1]]
#   
#   sims <- 1000
#   pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
#   pe <- pe[1:m,]
#   
#   vc <- vcov(fit)
#   simbetas <- mvrnorm(sims, pe, vc)
#   colnames(simbetas)[1] <- "Cont"
#   
#   # Create an empty data frame with NA values and the specified row and column names
#   df <- as.data.frame(matrix(NA, nrow = length(colnames(simbetas)), ncol = length(colnames(simbetas))))
#   rownames(df) <- colnames(simbetas)
#   colnames(df) <- colnames(simbetas)
#   
#   ### adding reference categories
#   reference_levels <- c()
#   for (var in cat) {
#     k <- length(levels(da_r[[var]]))
#     reference_levels[var] <- levels(da_r[[var]])[k]
#   }
#   
#   cat_ref <- paste0(names(reference_levels),reference_levels)
#   
#   df_r <- as.data.frame(matrix(NA, nrow = length(cat_ref), ncol = length(colnames(simbetas))))
#   rownames(df_r) <- cat_ref
#   colnames(df_r) <- colnames(simbetas)
#   
#   df_r1 <- df_r2 <- df_r
#   df_r2[is.na(df_r2)] <- 0
#   
#   for(v in names(reference_levels)){
#     # Identify target row(s) starting with "race"
#     target_rows <- grep(paste0("^",v), rownames(df_r))
#     
#     # Identify target columns starting with "race"
#     target_cols <- grep(paste0("^",v), colnames(df_r))
#     
#     # Replace NAs with -1 for matching row-column combinations
#     for (r in target_rows) {
#       
#       df_r1[r, target_cols] <- ifelse(is.na(df_r[r, target_cols]), -1, 0)
#     }
#   }
#   df_r1[is.na(df_r1)] <- 0
#   
#   
#   df1 <- df2 <- df 
#   # Replace values in each row
#   for (j in seq_along(colnames(simbetas))) {
#     df1[j, ] <- 0              # Set all elements in the row to 0
#     df1[j, colnames(simbetas)[j]] <- 1  # Set the element in the column matching the row name to 1
#   }
#   
#   df1 <- df1[-1,] %>% rbind(df_r1)
#   
#   for (j in seq_along(colnames(simbetas))) {
#     df2[j, ] <- 0              # Set all elements in the row to 0
#     df2[j, colnames(simbetas)[j]] <- 0  # Set the element in the column matching the row name to 0
#   }
#   
#   df2 <- df2[-1,] %>% rbind(df_r2)
#   
#   x <- df1 %>% rbind(df2) %>% 
#     mutate(Cont = 1) %>% as.matrix()
#   
#   m <- m + length(cat_ref)
#   xbeta <- x %*% t(simbetas) 
#   
#   xbeta <- xbeta[1:(m-1),] - xbeta[m:(2*(m-1)),]
#   
#   pe <- apply(xbeta, 1, mean) 
#   lwr <- apply(xbeta, 1, quantile, probs= 0.975) 
#   upr <- apply(xbeta, 1, quantile, probs= 0.025)
#   
#   dd <- cbind(pe,upr,lwr) %>%
#     as.data.frame() %>%
#     tibble::rownames_to_column("var") %>%
#     mutate(color = ifelse(upr*lwr >0, "Y", "N")) %>%
#     mutate(IV = ipt[i])
#   
#     tp <- sqrt(attr(ranef(fit, condVar=T)[[1]], "postVar"))*1.96 
#     
#     ds <- as.data.frame(ranef(fit)$climatezone) %>% 
#       dplyr::rename(R_effect = "(Intercept)") %>% 
#       tibble::rownames_to_column("zone") %>% 
#       gather(key, value, R_effect:acNone) %>% 
#       mutate(SE = c(tp[1,1,],tp[2,2,],tp[3,3,]),
#              lower = value - SE,
#              upper = value + SE)
#     
#   
#   
#   # dd$var %>% unique() 
#   # da$primary_heating_type %>% unique() 
#   name_mapping <- c(
#     "PV" = "PV", 
#     "PS" = "PS",
#     "EV" = "EV", 
#     "HP" = "HP",
#     "IC" = "IC",
#     
#     "born_us1" = "Born US",
#     "upfrontpayback1" = "Upfront prefer",
#     "therm_winter1" = "Thermostat use",
#     "outage_generatorown1" = "Own generator",
#     
#     "charging_5mile_f1" = "Fast charger",
#     # "vehicle_next_fuel1" = "EV to buy",
#     "vehicle_next_used1" = "New car",
#     # "vehicle_next_when" = "Later to buy car",
#     "vehicle_1_miles" = "Milage",
#     "charging_work1" = "Charger at work",
#     
#     "home_age" = "Newer home",
#     "rangeanxiety" = "more range for EV",
#     "ccmove_where1" = "Leave CA",
#     "homevac" = "Evacuation"
#     
#   )
#   
#   reg <- dd %>% 
#     mutate(var = recode(var, !!!name_mapping),
#            domain = ifelse(str_detect(var,"income|education|race|ideology|pid|US|employment"), "Sociodemo",
#                            
#                            ifelse(str_detect(var, "home|household"), "Housing",
#                                   
#                                   ifelse(str_detect(var,"primary|Thermostat"), "Heat/Cool",
#                                          
#                                          ifelse(var %in% dd$var[grep(pattern="kitchen",
#                                                                      dd$var)], "Cook",
#                                                 
#                                                 ifelse(str_detect(var,"therm|burden|savemoney|health|safety|Upfront|direct|plan|electrification"), "Behavior",
#                                                        
#                                                        ifelse(str_detect(var, "outage_impact|freq|dur|generator"), "Resilience", 
#                                                               
#                                                               ifelse(str_detect(var, "peer"), "Peer Effect", 
#                                                                      ifelse(str_detect(var, "Milage|charger|Charger|car|EV|l1l2|dc"), "Vehicle", 
#                                                                             ifelse(str_detect(var, "payback|dv|pc"), "WTP",
#                                                                                    ifelse(str_detect(var, "cc|CA|Evacuation"), "Climate", var)))))))))),
#            domain = factor(domain, levels = c("Housing","Heat/Cool","Cook","Vehicle","Sociodemo","Behavior","Resilience","Climate","Peer Effect","WTP"))) %>% 
#     
#     mutate(IV = factor(IV, levels = ipt)) %>% 
#     
#     mutate(color = factor(color, levels = c("Y","N")))
#   
#   return(list(reg, ds, sum_fit))
# }
# 
# 
# ### those having central air cooling as heat pumps
# data %>% 
#   dplyr::select(primary_cooling_cent, primary_cooling_type) %>% View
# 
# data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
#   data_process(ev = c("Fully electric")) %>% 
#   data_clean(1) %>% 
#   mutate(ac = ifelse(primary_cooling_type %in% c("Room","Central"), "AC", primary_cooling_type))
# 
# b_ev <- mreg_hp(data, 
#              remove = c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv",
#                         "education","employment"),
#              i = k)
# 
# b_ev[[2]] %>%
#   ggplot(aes(x = reorder(factor(zone), value), y = value, color = key)) +
#   geom_point(position = position_dodge(width = 0.5)) +
#   geom_errorbar(aes(ymin = lower, ymax = upper), 
#                 width = 0.2, 
#                 position = position_dodge(width = 0.5)) +
#   theme_minimal() +
#   labs(
#     title = "",
#     x = "Zone",
#     y = "HP adoption",
#     color = ""
#   )
# 
# 
# b_ev[[2]] %>% 
#   left_join(climate_zone_data, by = "zone") %>% 
#   select(zone, key, value, lower, upper, HDD, CDD) %>%
#   pivot_longer(cols = c(HDD, CDD), names_to = "Degree_Type", values_to = "Degree_Days") %>% 
#   ggplot(aes(x = Degree_Days, y = value, color = Degree_Type, fill = Degree_Type)) +
#   geom_point(size = 2) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#   geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
#   facet_wrap(~key, scales = "free") +
#   labs(title = "",
#        x = "Degree days",
#        y = "Random effect",
#        color = "",
#        fill = "") +
#   theme_minimal() +
#   theme(legend.position = "bottom")
# 
# 
# b_ev[[2]] %>% 
#   filter(key != "R_effect") %>% 
#   dplyr::select(-lower, -upper) %>% 
#   group_by(zone) %>% 
#   mutate(SE = mean(SE)) %>% 
#   pivot_wider(names_from = key, values_from = value) %>% 
#   mutate(acOther = -acAC - acNone,
#          lower = acOther - SE,
#          upper = acOther + SE) %>% 
#   left_join(climate_zone_data, by = "zone") %>% 
#   select(zone, acOther, lower, upper, HDD, CDD) %>%
#   pivot_longer(cols = c(HDD, CDD), names_to = "Degree_Type", values_to = "Degree_Days") %>% 
#   
#   ggplot(aes(x = Degree_Days, y = acOther, color = Degree_Type, fill = Degree_Type)) +
#   geom_point(size = 2) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#   geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
#   labs(title = "",
#        x = "Degree days",
#        y = "Random effect",
#        color = "",
#        fill = "") +
#   theme_minimal() +
#   theme(legend.position = "bottom")
#   
#   
# 
# 
# f4 <- ggarrange(f4a, ggarrange(f4b1, f4b2, widths = c(1,1.5), nrow = 1, common.legend = T, legend = "bottom"), f4c,
#                 nrow = 3,
#                 heights = c(1.8,1,1),
#                 labels = c("A", "B", "C"),  # Adds labels to plots
#                 label.x = 0,        # Adjust horizontal position of labels
#                 label.y = 1,        # Adjust vertical position of labels
#                 font.label = list(size = 14, face = "bold")
# )


