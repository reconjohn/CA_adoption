###################################################################### Three tech.
data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1) %>% 
  dplyr::select(-c("cost_combo_winter_final","cost_combo_summer_final"),
                -c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv","solstor_wtp_dv")) %>% 
  
## average adoption
d1 <- data %>% # only homeowners 
  filter(home_own == 1) %>% 
  summarise(across(
    .cols = all_of(names(.)[str_detect(names(.), "HP|IC|PS")& !str_detect(names(.), "peer")]),
    .fns = ~ weighted.mean(.x, wt_ca, na.rm = TRUE)
  )) %>%
  gather(key, adoption) %>% 

  mutate(
    tech = str_extract(key, "(PV|PS|EV|HP|IC)"),
    scenario = case_when(
      str_detect(key, "_int") ~ "Intended",
      str_detect(key, "^(PS|EV|HP|IC)") ~ "current",
      str_detect(key, "0|(PV$)") ~ "Future",
      str_detect(key, "low") ~ "Lsubsidy",
      str_detect(key, "high") ~ "Hsubsidy",
      TRUE ~ "future"  # fallback for keys like "future_PV"
    )
  ) %>%
  dplyr::select(-key) %>%
  pivot_wider(names_from = scenario, values_from = adoption) %>%
  mutate(
    future = Future - current,
    low.subsidy = Lsubsidy - Future,
    high.subsidy = Hsubsidy - Lsubsidy,
    intended = Intended - Hsubsidy
  ) %>%
  dplyr::select(-Future,-Lsubsidy,-Hsubsidy,-Intended) %>% 
  mutate(across(-tech, ~ .x * 0.55)) %>% 
  
  left_join(
    data %>% # only homeowners 
      filter(home_own == 0) %>% 
      summarise(across(
        .cols = c("PS","HP","IC"),
        .fns = ~ weighted.mean(.x, wt_ca, na.rm = TRUE)
      )) %>%
      gather(tech, rent) %>% 
      mutate(across(-tech, ~ .x * 0.45)), by = "tech"
  )
  


### optimistic scenarios
select_var <- list(c("peer"),
                   c("peer"),
                   c("peer"))

rates <- list(c(-0.25),
              c(-0.17),
              c(-0.14,-0.23))


data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1) %>% 
  dplyr::select(-c("cost_combo_winter_final","cost_combo_summer_final"),
                -c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv","solstor_wtp_dv"),
                -c("PS_int","EV_int","HP_int","IC_int"))

result <- data.frame()
for(k in 1:3){
  
  ### without sum contrasts for scenario variables
  b_ev <- mreg_scene(data = data %>% 
                       filter(home_own == 1) %>% 
                       dplyr::select(VAR[[k+1]], matches("home_own", ignore.case = FALSE),
                                     climatezone, dac, matches("PV|EV|PS|HP|IC", ignore.case = FALSE),wt_ca),
                     remove = "home_own",
                     i = k+2,
                     scenario = select_var[[k]],
                     future = "high")
  
  pattern <- paste(select_var[[k]], collapse = "|")
  
  df <- b_ev %>% 
    filter(str_detect(var, pattern)) %>%
    dplyr::select(var, Estimate) %>% 
    cbind(tibble(rate = rates[[k]])) %>% 
    mutate(effect = Estimate*rate) 
  
  rd <- df %>%
    summarise(
      peer            = sum(effect[str_detect(var, "peer")], na.rm = TRUE)
      # home_age            = sum(effect[str_detect(var, "home_age")], na.rm = TRUE),
      # charging_5mile  = sum(effect[str_detect(var, "charging_5mile_f1")], na.rm = TRUE),
      # rangeanxiety    = sum(effect[str_detect(var, "rangeanxiety")], na.rm = TRUE)
    ) %>% 
    mutate(tech = ipt[k+2])
  
  result <- rbind(rd, result)
}


stage_order <- c("Renters", "Current", "No subsidy", "Low support", "High support", 
                 "Peer effects","Extra high subsidy")

df_long <- d1 %>% 
  left_join(result, by = "tech")  %>%
  dplyr::select(-intended) %>% 
  pivot_longer(cols = -tech, names_to = "stage", values_to = "value") %>% 
  mutate(stage = recode(stage,
                        "current" = "Current",
                        "rent" = "Renters",
                        "future" = "No subsidy",
                        "low.subsidy" = "Low support",
                        "high.subsidy" = "High support",
                        
                        "peer" = "Peer effects")) %>% 
  
  mutate(stage = factor(stage, levels = stage_order)) %>% 
  arrange(stage) %>% 
  group_by(tech) %>%
  mutate(
    value = replace_na(value, 0),
    start = cumsum(lag(value, default = 0)),  # cumulative start
    end = start + value
  ) %>% 
  
  rbind(
    d1 %>% 
      left_join(result, by = "tech")  %>%
      dplyr::select(-peer) %>% 
      pivot_longer(cols = -tech, names_to = "stage", values_to = "value") %>% 
      mutate(stage = recode(stage,
                            "current" = "Current",
                            "rent" = "Renters",
                            "future" = "No subsidy",
                            "low.subsidy" = "Low support",
                            "high.subsidy" = "High support",
                            "intended" = "Extra high subsidy")) %>% 
      
      mutate(stage = factor(stage, levels = stage_order)) %>% 
      arrange(stage) %>% 
      group_by(tech) %>%
      mutate(
        value = replace_na(value, 0),
        start = cumsum(lag(value, default = 0)),  # cumulative start
        end = start + value
      ) %>% 
      filter(stage == "Extra high subsidy") 
  ) %>% 
  mutate(tech = factor(tech, levels = c("PS","HP","IC"))) %>% 
  mutate(scene = "Optimistic") 


dff <- df_long %>% 
  filter(value >0) %>% 
  mutate(across(where(is.numeric), ~ .x * 100)) %>% 
  group_by(tech) %>%
  mutate(id = row_number(),
         label_val = if_else(value != 0, as.character(round(value, 0)), "")) %>%
  ungroup() %>% # Ungrouping is good practice after mutations
  mutate(class = ifelse(stage %in% c("Current","No subsidy","Renters"), "No subsidy",
                        ifelse(stage %in% c("Low support","High support"), "Policy intervention",
                               ifelse(stage %in% c("Extra high subsidy"), "Rest",
                                      "Time variant"))),
         class = factor(class, levels = c("No subsidy","Policy intervention","Time variant","Rest")))


segment_data <- dff %>%
  group_by(tech) %>%
  filter(id < max(id)) %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Heat pumps","Induction stoves"))) 

hlines <- tibble::tribble(
  ~tech, ~y_intercept, ~line_label,
  "PV + Storage", 20, "Base: 0.20",
  "Heat pumps", 46,   "Base: 46" # This line will not appear as there is no "HP" data
) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps")))



opt_result <- dff %>% 
  filter(tech != "PV") %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Heat pumps","Induction stoves"))) 


### pessimistic scenarios
select_var <- list(c("peer"),
                   c("peer"),
                   c("peer"))


rates_p <- list(c(0,-0.11), # HP
                c(0,-0.07), # IC
                c(0,-0.18)) # PS

result_p <- data.frame()
for(k in 1:3){
  
  b_ev <- mreg_scene(data = data %>% 
                       filter(home_own == 1) %>% 
                       dplyr::select(VAR[[k+1]], matches("home_own", ignore.case = FALSE),
                                     climatezone, dac, matches("PV|PS|EV|HP|IC", ignore.case = FALSE),wt_ca),
                     remove = "home_own",
                     i = k+2,
                     scenario = select_var[[k]],
                     future = "low")
  
  pattern <- paste(select_var[[k]], collapse = "|")
  
  df <- b_ev %>% 
    filter(str_detect(var, pattern)) %>%
    dplyr::select(var, Estimate) %>% 
    cbind(tibble(rate = rates_p[[k]])) %>% 
    mutate(effect = Estimate*rate) 
  
  rd <- df %>%
    summarise(
      peer = sum(effect[str_detect(var, "peer")], na.rm = TRUE)
    ) %>% 
    mutate(tech = ipt[k+2])
  
  result_p <- rbind(rd, result_p)
}


df_long_p <- d1 %>% 
  dplyr::select(-intended) %>% 
  dplyr::select(-high.subsidy) %>% 
  left_join(result_p, by = "tech")  %>%
  pivot_longer(cols = -tech, names_to = "stage", values_to = "value") %>%
  mutate(stage = recode(stage,
                        "current" = "Current",
                        "rent" = "Renters",
                        "future" = "No subsidy",
                        "low.subsidy" = "Low support",
                        "high.subsidy" = "High support",
              
                        "peer" = "Peer effects")) %>% 
  mutate(stage = factor(stage, levels = stage_order)) %>% 
  arrange(stage) %>% 
  group_by(tech) %>%
  mutate(
    value = replace_na(value, 0),
    start = cumsum(lag(value, default = 0)),  # cumulative start
    end = start + value
  ) %>% 
  mutate(tech = factor(tech, levels = c("PS","PV","EV","HP","IC"))) %>% 
  mutate(scene = "Pessimistic")


dff <- df_long_p %>% 
  filter(value >0) %>% 
  mutate(across(where(is.numeric), ~ .x * 100)) %>% 
  group_by(tech) %>%
  mutate(id = row_number(),
         label_val = if_else(value != 0, as.character(round(value, 0)), "")) %>%
  ungroup() %>% # Ungrouping is good practice after mutations
  mutate(class = ifelse(stage %in% c("Current","No subsidy","Renters"), "No subsidy",
                        ifelse(stage %in% c("Low support"), "Policy intervention",
                               "Time variant")),
         class = factor(class, levels = c("No subsidy","Policy intervention","Time variant")))


segment_data <- dff %>%
  group_by(tech) %>%
  filter(id < max(id)) %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Heat pumps","Induction stoves"))) 

hlines <- tibble::tribble(
  ~tech, ~y_intercept, ~line_label,
  "PV + Storage", 20, "Base: 0.20",
  "Heat pumps", 46,   "Base: 46" # This line will not appear as there is no "HP" data
) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps")))


pess_result <- dff %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Heat pumps","Induction stoves"))) 


d <- opt_result %>% 
  mutate(id = ifelse(stage == "Extra high subsidy", 5, id)) %>% 
  mutate(
    id = ifelse(class == "Time variant", 4, id)   # stack on top of More subsidy
  ) %>% 
  rbind(
    pess_result %>%
      filter(class == "Time variant") %>% 
      mutate(
        id = 3
      )
  ) %>% 
  mutate(id = ifelse(stage == "Current", 1, 
                     ifelse(stage == "No subsidy", 2,
                            ifelse(stage == "Low support", 3,
                                   ifelse(stage == "High support", 4, id)))))


time_variant_stages <- c("Peer effects","Renters")
fill_vals <- c(
  setNames(RColorBrewer::brewer.pal(length(time_variant_stages), "Set2"),
           time_variant_stages),
  "Other" = "azure3" 
)

pattern_vals <- c(
  setNames(rep("stripe", length(time_variant_stages)), time_variant_stages),
  "Other" = "none"
)

# d %>%
#   mutate(sta = as.character(stage),
#          ST = ifelse(sta == "Peer effects", "Peer effects", 
#                      ifelse(sta == "Renters", "Renters","Other")),
#          ST = factor(ST, levels = c("Peer effects","Renters", "Other"))) %>% 
#   ggplot(aes(x = stage)) +
#   
#   geom_rect_pattern(
#     aes(
#       xmin = id - 0.45,
#       xmax = id + 0.45,
#       ymin = start,
#       ymax = end,
#       fill = ST,
#       pattern = ST
#     ),
#     pattern_fill = "black",
#     pattern_angle = 45,
#     pattern_density = 0.02,
#     pattern_spacing = 0.03,
#     color = NA
#   ) +
#   
#   geom_hline(
#     data = hlines %>% filter(tech != "PV"),
#     aes(yintercept = y_intercept),
#     color = "darkred",
#     linetype = "dashed",
#     linewidth = 1
#   ) +
#   
#   facet_wrap(~ tech, scales = "free_x", nrow = 1) +
#   
#   scale_x_discrete(limits = c("Current", "No subsidy", "Low support", "High support","Extra high subsidy")) +
#   
#   scale_y_continuous(labels = scales::comma) +
#   
#   scale_fill_manual(values = fill_vals, name = "",
#                     breaks = time_variant_stages) +
#   scale_pattern_manual(values = pattern_vals, name = "",
#                        breaks = time_variant_stages) +
#   
#   labs(
#     title = "",
#     x = "",
#     y = "Cumulative Adoption (%)"
#   ) +
#   
#   theme_minimal(base_size = 12) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
#     plot.title = element_text(face = "bold", size = 16),
#     legend.position = "right",
#     strip.text = element_text(size = 14, face = "bold"),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank()
#   )


daa1 <- d %>% 
  mutate(across(c(value,start,end), ~ .x*0.925))

add_values <- tibble(
  tech = rep(c("PV + Storage","Heat pumps","Induction stoves"), each = 3),
  id    = rep(c(2, 3, 4), 3),
  value = rep(0.5, 9)* 7.5  # new built proportion * homeowner proportion 
)


# extract the correct 'start' for each id (the last end value)
last_end <- daa1 %>%
  group_by(tech, id) %>%
  summarise(start = max(end), .groups = "drop") 

# compute end = value + start
new_rows <- add_values %>%
  left_join(last_end, by = c("tech","id")) %>%
  mutate(end = start + value,
         stage = "New build-50% adoption",
         scene = "Optimistic",
         label_val = NA,
         class = "Time variant")


last_end1 <- new_rows %>%
  group_by(tech, id) %>%
  summarise(start = max(end), .groups = "drop") 


new_rows <- add_values %>%
  left_join(last_end1, by = c("tech","id")) %>%
  mutate(end = start + value,
         stage = "New build-100% adoption",
         scene = "Optimistic",
         label_val = NA,
         class = "Time variant") %>% 
  rbind(new_rows)


# bind to original data
daa <- bind_rows(daa1, new_rows) %>%
  arrange(id, start) 


# Identify the time-variant stages
time_variant_stages <- c("New build-100% adoption", "New build-50% adoption", "Peer effects")

fill_vals <- c(
  setNames(RColorBrewer::brewer.pal(length(time_variant_stages), "Set2")[c(3,1,2)],
           time_variant_stages),
  "Renters" = "brown4",
  "Homeowners" = "burlywood",
  "Subsidy" = "azure3"
)

pattern_vals <- c(
  setNames(rep("stripe", length(time_variant_stages)), time_variant_stages),
  "Renters" = "none",
  "Homeowners" = "none",
  "Subsidy" = "none"
)

f1a <- daa %>%
  mutate(sta = as.character(stage),
         ST = ifelse(sta == "Peer effects", "Peer effects", 
                     ifelse(sta == "New build-50% adoption", "New build-50% adoption",
                            ifelse(sta == "New build-100% adoption", "New build-100% adoption",
                     ifelse(sta == "Renters", "Renters","Homeowners")))),
         ST = ifelse(ST == "Homeowners" & sta %in% c("No subsidy","Low support","High support","Extra high subsidy"), "Subsidy", ST),
         tech = factor(tech, levels = c("PV + Storage","Heat pumps", "Induction stoves"))) %>% 
  mutate(stage = factor(stage,
                        levels = c("Current", "No subsidy", "Low support", "High support", "Extra high subsidy"))) %>% 
  ggplot(aes(x = stage)) +
  
  geom_rect(aes(xmin = id - 0.45, xmax = id + 0.45, ymin = start, ymax = end, fill = ST)) +

  geom_rect_pattern(
    aes(
      xmin = id - 0.45,
      xmax = id + 0.45,
      ymin = start,
      ymax = end,
      fill = ST,
      pattern = ST
    ),
    pattern_fill = "white",
    pattern_colour = "white", 
    pattern_angle = 45,
    pattern_density = 0.02,
    pattern_spacing = 0.03,
    color = NA
  ) +
  
  geom_hline(
    data = hlines %>% filter(tech != "PV"),
    aes(yintercept = y_intercept),
    color = "darkred",
    linetype = "dashed",
    linewidth = 1
  ) +
  
  facet_wrap(~ tech, scales = "fixed", nrow = 1) +
  
  scale_x_discrete(drop = FALSE,
                   limits = c("Current", "No subsidy", "Low support", "High support", "Extra high subsidy")) +
  
  scale_y_continuous(labels = scales::comma) +
  
  scale_fill_manual(values = fill_vals, name = "",
                    breaks = c(time_variant_stages,"Subsidy", "Homeowners", "Renters")) +
  scale_pattern_manual(values = pattern_vals, name = "",
                       breaks = c(time_variant_stages, "Subsidy", "Homeowners", "Renters")) +

  labs(
    title = "",
    x = "",
    y = "Cumulative Adoption (%)"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) + 
  guides(
    fill = guide_legend(nrow = 1)
  )



# daa1 <- d %>% 
#   mutate(across(c(value,start,end), ~ .x*0.816))
# 
# add_values <- tibble(
#   tech = rep(c("PV + Storage","Heat pumps","Induction stoves"), each = 3),
#   id    = rep(c(2, 3, 4), 3),
#   value = c(c(0.9, 0.9, 0.95),
#             c(0.75, 0.75, 0.85),
#             c(0.3, 0.3, 0.4))* 18.4
# )
# 
# # extract the correct 'start' for each id (the last end value)
# last_end <- daa1 %>%
#   group_by(tech, id) %>%
#   summarise(start = max(end), .groups = "drop") 
# 
# # compute end = value + start
# new_rows <- add_values %>%
#   left_join(last_end, by = c("tech","id")) %>%
#   mutate(end = start + value,
#          stage = "New built",
#          scene = "Optimistic",
#          label_val = NA,
#          class = "Time variant")
# 
# # bind to original data
# daa <- bind_rows(daa1, new_rows) %>%
#   arrange(id, start) 
# 
# 
# # Identify the time-variant stages
# time_variant_stages <- c("New built", "Peer effects", "Rent")
# 
# fill_vals <- c(
#   setNames(RColorBrewer::brewer.pal(length(time_variant_stages), "Set2"),
#            time_variant_stages),
#   "Other" = "azure3" 
# )
# 
# pattern_vals <- c(
#   setNames(rep("stripe", length(time_variant_stages)), time_variant_stages),
#   "Other" = "none"
# )
# 
# daa %>%
#   mutate(sta = as.character(stage),
#          ST = ifelse(sta == "Peer effects", "Peer effects", 
#                      ifelse(sta == "New built", "New built",
#                             ifelse(sta == "Rent", "Rent","Other"))),
#          ST = factor(ST, levels = c("Peer effects","New built", "Rent", "Other")),
#          tech = factor(tech, levels = c("PV + Storage","Heat pumps", "Induction stoves"))) %>% 
#   ggplot(aes(x = stage)) +
#   
#   geom_rect_pattern(
#     aes(
#       xmin = id - 0.45,
#       xmax = id + 0.45,
#       ymin = start,
#       ymax = end,
#       fill = ST,
#       pattern = ST
#     ),
#     pattern_fill = "black",
#     pattern_angle = 45,
#     pattern_density = 0.02,
#     pattern_spacing = 0.03,
#     color = NA
#   ) +
#   
#   geom_hline(
#     data = hlines %>% filter(tech != "PV"),
#     aes(yintercept = y_intercept),
#     color = "darkred",
#     linetype = "dashed",
#     linewidth = 1
#   ) +
#   
#   facet_wrap(~ tech, scales = "free_x", nrow = 1) +
#   
#   scale_x_discrete(limits = c("Current", "Future", "Low", "High", "Intended")) +
#   
#   scale_y_continuous(labels = scales::comma) +
#   
#   scale_fill_manual(values = fill_vals, name = "",
#                     breaks = time_variant_stages) +
#   scale_pattern_manual(values = pattern_vals, name = "",
#                        breaks = time_variant_stages) +
#   
#   labs(
#     title = "",
#     x = "",
#     y = "Cumulative Adoption (%)"
#   ) +
#   
#   theme_minimal(base_size = 12) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
#     plot.title = element_text(face = "bold", size = 16),
#     legend.position = "right",
#     strip.text = element_text(size = 14, face = "bold"),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank()
#   )


################################################################################ EV 
data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1) %>% 
  dplyr::select(-c("cost_combo_winter_final","cost_combo_summer_final"),
                -c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv","solstor_wtp_dv"))
## average adoption
d1 <- data %>% # only homeowners 
  summarise(across(
    .cols = all_of(names(.)[str_detect(names(.), "EV")& !str_detect(names(.), "peer")]),
    .fns = ~ weighted.mean(.x, wt_ca, na.rm = TRUE)
  )) %>%
  gather(key, adoption) %>% 
  
  mutate(
    tech = str_extract(key, "(PV|PS|EV|HP|IC)"),
    scenario = case_when(
      str_detect(key, "_int") ~ "Intended",
      str_detect(key, "^(PS|EV|HP|IC)") ~ "current",
      str_detect(key, "0|(PV$)") ~ "Future",
      str_detect(key, "low") ~ "Lsubsidy",
      str_detect(key, "high") ~ "Hsubsidy",
      TRUE ~ "future"  # fallback for keys like "future_PV"
    )
  ) %>%
  dplyr::select(-key) %>%
  pivot_wider(names_from = scenario, values_from = adoption) %>%
  mutate(
    future = Future - current,
    low.subsidy = Lsubsidy - Future,
    high.subsidy = Hsubsidy - Lsubsidy,
    intended = Intended - Hsubsidy
  ) %>%
  dplyr::select(-Future,-Lsubsidy,-Hsubsidy,-Intended) %>% 
  mutate(rent = current * 0.11,
         current = current * 0.89) # splite renter and homeowners




### optimistic scenarios
select_var <- list(c("peer_EV","rangeanxiety"))

rates <- list(c(-2,-0.04,-0.26))


data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1) %>% 
  dplyr::select(-c("cost_combo_winter_final","cost_combo_summer_final"),
                -c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv","solstor_wtp_dv"),
                -c("PS_int","EV_int","HP_int","IC_int"))

result <- data.frame()
for(k in 1){
  
  ### without sum contrasts for scenario variables
  b_ev <- mreg_scene(data = data %>% 
                       dplyr::select(VAR[[k]], matches("home_own", ignore.case = FALSE),
                                     climatezone, dac, matches("PV|EV|PS|HP|IC", ignore.case = FALSE),wt_ca),
                     remove = NULL,
                     i = k+1,
                     scenario = select_var[[k]],
                     future = "high")
  
  pattern <- paste(select_var[[k]], collapse = "|")
  
  df <- b_ev %>% 
    filter(str_detect(var, pattern)) %>%
    dplyr::select(var, Estimate) %>% 
    cbind(tibble(rate = rates[[k]])) %>% 
    mutate(effect = Estimate*rate) 
  
  rd <- df %>%
    summarise(
      peer            = sum(effect[str_detect(var, "peer")], na.rm = TRUE),
      # home_age            = sum(effect[str_detect(var, "home_age")], na.rm = TRUE),
      # charging_5mile  = sum(effect[str_detect(var, "charging_5mile_f1")], na.rm = TRUE),
      rangeanxiety    = sum(effect[str_detect(var, "rangeanxiety")], na.rm = TRUE)
    ) %>% 
    mutate(tech = ipt[k+1])
  
  result <- rbind(rd, result)
}


stage_order <- c("Renters", "Current", "No subsidy", "Low support", "High support", 
                 "Peer effects","Range", "Extra high subsidy")

df_long <- d1 %>% 
  left_join(result, by = "tech")  %>%
  dplyr::select(-intended) %>% 
  pivot_longer(cols = -tech, names_to = "stage", values_to = "value") %>% 
  mutate(stage = recode(stage,
                        "current" = "Current",
                        "rent" = "Renters",
                        "future" = "No subsidy",
                        "low.subsidy" = "Low support",
                        "high.subsidy" = "High support",
                        
                        "peer" = "Peer effects",
                        "rangeanxiety" = "Range")) %>% 
  
  mutate(stage = factor(stage, levels = stage_order)) %>% 
  arrange(stage) %>% 
  group_by(tech) %>%
  mutate(
    value = replace_na(value, 0),
    start = cumsum(lag(value, default = 0)),  # cumulative start
    end = start + value
  ) %>% 
  
  rbind(
    d1 %>% 
      left_join(result, by = "tech")  %>%
      dplyr::select(-peer,-rangeanxiety) %>% 
      pivot_longer(cols = -tech, names_to = "stage", values_to = "value") %>% 
      mutate(stage = recode(stage,
                            "current" = "Current",
                            "rent" = "Renters",
                           
                            "future" = "No subsidy",
                            "low.subsidy" = "Low support",
                            "high.subsidy" = "High support",
                            "intended" = "Extra high subsidy")) %>% 
      
      mutate(stage = factor(stage, levels = stage_order)) %>% 
      arrange(stage) %>% 
      group_by(tech) %>%
      mutate(
        value = replace_na(value, 0),
        start = cumsum(lag(value, default = 0)),  # cumulative start
        end = start + value
      ) %>% 
      filter(stage == "Extra high subsidy") 
  ) %>% 
  mutate(scene = "Optimistic") 


dff <- df_long %>% 
  filter(value >0) %>% 
  mutate(across(where(is.numeric), ~ .x * 100)) %>% 
  group_by(tech) %>%
  mutate(id = row_number(),
         label_val = if_else(value != 0, as.character(round(value, 0)), "")) %>%
  ungroup() %>% # Ungrouping is good practice after mutations
  mutate(class = ifelse(stage %in% c("Current","No subsidy","Renters"), "No subsidy",
                        ifelse(stage %in% c("Low support","High support"), "Policy intervention",
                               ifelse(stage %in% c("Extra high subsidy"), "Rest",
                                      "Time variant"))),
         class = factor(class, levels = c("No subsidy","Policy intervention","Time variant","Rest")))


segment_data <- dff %>%
  group_by(tech) %>%
  filter(id < max(id)) %>% 
  mutate(tech = recode(tech, 
                       "EV" = "Electric vehicle"))

hlines <- tibble::tribble(
  ~tech, ~y_intercept, ~line_label,
  "Electric vehicles", 42, "Base: 0.42"
) 



opt_result <- dff %>% 
  mutate(tech = recode(tech, 
                       "EV" = "Electric vehicle")) 


### pessimistic scenarios
select_var <- list(c("peer_EV","rangeanxiety"))

rates_p <- list(c(-1,0,-0.15))

result_p <- data.frame()
for(k in 1){
  
  b_ev <- mreg_scene(data = data %>% 
                   
                       dplyr::select(VAR[[k+1]], matches("home_own", ignore.case = FALSE),
                                     climatezone, dac, matches("PV|PS|EV|HP|IC", ignore.case = FALSE),wt_ca),
                     remove = NULL,
                     i = k+1,
                     scenario = select_var[[k]],
                     future = "low")
  
  pattern <- paste(select_var[[k]], collapse = "|")
  
  df <- b_ev %>% 
    filter(str_detect(var, pattern)) %>%
    dplyr::select(var, Estimate) %>% 
    cbind(tibble(rate = rates_p[[k]])) %>% 
    mutate(effect = Estimate*rate) 
  
  rd <- df %>%
    summarise(
      peer = sum(effect[str_detect(var, "peer")], na.rm = TRUE),
      rangeanxiety    = sum(effect[str_detect(var, "rangeanxiety")], na.rm = TRUE)
    ) %>% 
    mutate(tech = ipt[k+1])
  
  result_p <- rbind(rd, result_p)
}


df_long_p <- d1 %>% 
  dplyr::select(-intended) %>% 
  dplyr::select(-high.subsidy) %>% 
  left_join(result_p, by = "tech")  %>%
  pivot_longer(cols = -tech, names_to = "stage", values_to = "value") %>%
  mutate(stage = recode(stage,
                        "current" = "Current",
                        "rent" = "Renters",
                        "future" = "No subsidy",
                        "low.subsidy" = "Low support",
                        "high.subsidy" = "High support",
                        
                        "intended" = "Extra high subsidy",
                        
                        "peer" = "Peer effects",
                        "rangeanxiety" = "Range")) %>% 
  mutate(stage = factor(stage, levels = stage_order)) %>% 
  arrange(stage) %>% 
  group_by(tech) %>%
  mutate(
    value = replace_na(value, 0),
    start = cumsum(lag(value, default = 0)),  # cumulative start
    end = start + value
  ) %>% 
  mutate(scene = "Pessimistic")


dff <- df_long_p %>% 
  filter(value >0) %>% 
  mutate(across(where(is.numeric), ~ .x * 100)) %>% 
  group_by(tech) %>%
  mutate(id = row_number(),
         label_val = if_else(value != 0, as.character(round(value, 0)), "")) %>%
  ungroup() %>% # Ungrouping is good practice after mutations
  mutate(class = ifelse(stage %in% c("Current","No subsidy","Renters"), "No subsidy",
                        ifelse(stage %in% c("Low support"), "Policy intervention",
                               "Time variant")),
         class = factor(class, levels = c("No subsidy","Policy intervention","Time variant")))


segment_data <- dff %>%
  group_by(tech) %>%
  filter(id < max(id)) %>% 
  mutate(tech = recode(tech, 
                       "EV" = "Electric vehicle"))

hlines <- tibble::tribble(
  ~tech, ~y_intercept, ~line_label,
  "Electric vehicles", 42, "Base: 0.42"
) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps")))


pess_result <- dff %>% 
  mutate(tech = recode(tech, 
                       "EV" = "Electric vehicle"))


d <- opt_result %>% 
  mutate(id = ifelse(stage == "Extra high subsidy", 5, id)) %>% 
  mutate(
    id = ifelse(class == "Time variant", 4, id)   # stack on top of More subsidy
  ) %>% 
  rbind(
    pess_result %>%
      filter(class == "Time variant") %>% 
      mutate(
        id = 3
      )
  ) %>% 
  mutate(id = ifelse(stage == "Current", 1, 
                     ifelse(stage == "No subsidy", 2,
                            ifelse(stage == "Low support", 3,
                                   ifelse(stage == "High support", 4, id)))))


time_variant_stages <- c("Range", "Peer effects")
fill_vals <- c(
  # setNames(RColorBrewer::brewer.pal(length(time_variant_stages), "Set2"),
  #          time_variant_stages),
  "Renters" = "brown4",
  "Homeowners" = "burlywood",
  "Range" = "gold4",
  "Peer effects" = "#FC8D62",
  "Subsidy" = "azure3" 
)

pattern_vals <- c(
  setNames(rep("stripe", length(time_variant_stages)), time_variant_stages),
  "Renters" = "none",
  "Homeowners" = "none",
  "Subsidy" = "none"
)

f1b <- d %>%
  mutate(stage = as.character(stage),
         stage = ifelse(stage == "Future", "No subsidy",
                        ifelse(stage == "Low", "Low support",
                               ifelse(stage == "High", "High support", 
                                      ifelse(stage == "Intended", "Extra high subsidy", stage))))) %>% 
  mutate(sta = as.character(stage),
         ST = ifelse(sta == "Peer effects", "Peer effects", 
                     ifelse(sta == "Range", "Range","Subsidy")),
         ST = ifelse(stage == "Renters", "Renters",
                     ifelse(stage == "Current", "Homeowners", ST)),
         ST = factor(ST, levels = c("Range", "Peer effects", "Subsidy","Homeowners","Renters"))) %>% 
  ggplot(aes(x = stage)) +
  
  geom_rect_pattern(
    aes(
      xmin = id - 0.45,
      xmax = id + 0.45,
      ymin = start,
      ymax = end,
      fill = ST,
      pattern = ST
    ),
    pattern_fill = "white",
    pattern_color = "white",
    pattern_angle = 45,
    pattern_density = 0.02,
    pattern_spacing = 0.03,
    color = NA
  ) +
  
  geom_hline(
    data = hlines %>% filter(tech != "PV") %>% 
      mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps"))),
    aes(yintercept = y_intercept),
    color = "darkred",
    linetype = "dashed",
    linewidth = 1
  ) +
  
  # facet_wrap( ~ tech) +
  
  scale_x_discrete(limits = c("Current", "No subsidy", "Low support", "High support","Extra high subsidy")) +
  
  scale_y_continuous(labels = scales::comma) +
  
  scale_fill_manual(values = fill_vals, name = "",
                    breaks = "Range") +
  scale_pattern_manual(values = pattern_vals, name = "",
                       breaks = "Range") +
  
  # scale_fill_manual(values = fill_vals, name = "",
  #                   breaks = c(time_variant_stages, "Homeowns+Renters")) +
  # scale_pattern_manual(values = pattern_vals, name = "",
  #                      breaks = c(time_variant_stages, "Homeowns+Renters")) +
  
  
  labs(
    title = "\n\nElectric vehicles",
    x = "",
    y = "Cumulative Adoption (%)"
  ) +
  
  ylim(0,45) + 
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


f2 <- ggarrange(f1a, f1b, nrow = 1,
          widths = c(3,1))

table_plot_data <- data.frame(
  Component = c(
    "Current", "Homeowners", "Renters", "Subsidy",
    "Peer effects", "Range (EV only)", "New build"
  ),
  Description = c(
    "Baseline cumulative share of current adopters, segmented by tenure.",
    "Current adopters who identify as homeowners.",
    "Current adopters who identify as renters.",
    "Under support: Low ($8k EV; $5k PV+SS; $4k HP; $300 IC), High ($12.5k EV/PV+SS; $8k HP; $800 IC), and Extra (above High).",
    "Additional adoption attributable to neighbor/peer influence, estimated via mixed-effects regression.",
    "Projected adoption increase from EV range improvements (100–200 miles), based on mixed-effects regression.",
    "Expected adoption from 2025–2035 new housing construction assuming 50-100% technology penetration."
  )
) %>%
  mutate(
    y = n():1,
    x_pos = 0.8
  )

# Start with a base plot for text and lines
table_plot <- ggplot(table_plot_data) +
  # Add component labels (bold)
  geom_text(aes(x = x_pos - 0.7, y = y, label = Component),
            hjust = 0, vjust = 1, fontface = "bold", size = 5) +
  # Add description text (not bold)
  geom_text(aes(x = x_pos - 0.08, y = y, label = Description),
            hjust = 0, vjust = 0.8, size = 4) +
  # Scale axes to fit the text and create a table-like aspect ratio
  scale_x_continuous(limits = c(0, 3.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.5, nrow(table_plot_data) + 0.5), expand = c(0, 0)) +
  # Use theme_void to remove all standard axis components
  theme_void() +
  # Add margin padding for internal space
  theme(plot.margin = margin(20, 20, 20, 20, "pt"))

# Add lines manually with geom_segment to create a grid appearance
table_panel_b <- table_plot +
  # Vertical line separator after component names
  geom_segment(data = tibble(x = c(0.65)),
               aes(x = x, y = 0.5, xend = x, yend = nrow(table_plot_data) + 0.5),
               color = "lightgrey", size = 0.5) +
  # Horizontal line at the very top of the table
  geom_segment(data = tibble(y = c(nrow(table_plot_data) + 0.5)),
               aes(x = 0, y = y, xend = 3.5, yend = y),
               color = "black", size = 1) +
  # Horizontal lines between rows for legibility
  geom_segment(data = tibble(y = seq(nrow(table_plot_data) - 0.5, 0.5, by = -1)),
               aes(x = 0, y = y, xend = 3.5, yend = y),
               color = "lightgrey", size = 0.5) +
  geom_segment(data = tibble(y = 0.5),
               aes(x = 0, y = y, xend = 3.5, yend = y),
               color = "black", size = 1) 



# Combine Panels A and B using ggarrange
combined_plot <- ggarrange(f2, table_panel_b,
                           labels = c("A", "B"),
                           ncol = 1, nrow = 2,
                           heights = c(2, 1)) # Adjust relative heights as needed

ggsave("./fig/f2.png",
       combined_plot,
       width = 12, height = 10)


############################################################################## DAC
data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1) %>% 
  dplyr::select(-c("cost_combo_winter_final","cost_combo_summer_final"),
                -c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv","solstor_wtp_dv"))

d1_dac <- data %>%
  filter(home_own == 1) %>% 
  group_by(dac) %>% 
  summarise(across(
    .cols = all_of(names(.)[str_detect(names(.), "HP|IC|PS")& !str_detect(names(.), "peer")]),
    .fns = ~ weighted.mean(.x, wt_ca, na.rm = TRUE)
  )) %>%
  gather(key, adoption, -dac) %>%
  mutate(
    tech = str_extract(key, "(PV|PS|EV|HP|IC)"),
    scenario = case_when(
      str_detect(key, "_int") ~ "Intended",
      str_detect(key, "^(PS|EV|HP|IC)") ~ "current",
      str_detect(key, "0|(PV$)") ~ "Future",
      str_detect(key, "low") ~ "Lsubsidy",
      str_detect(key, "high") ~ "Hsubsidy",
      TRUE ~ "future"  # fallback for keys like "future_PV"
    )
  ) %>%
  dplyr::select(-key) %>%
  pivot_wider(names_from = scenario, values_from = adoption) %>%
  mutate(
    future = Future - current,
    low.subsidy = Lsubsidy - Future,
    high.subsidy = Hsubsidy - Lsubsidy,
    intended = Intended - Hsubsidy
  ) %>%
  dplyr::select(-Future,-Lsubsidy,-Hsubsidy,-Intended) %>% 
  mutate(across(-c(tech,dac), ~ .x * 0.55)) %>% 
  
  left_join(
    data %>% # only homeowners 
      filter(home_own == 0) %>% 
      group_by(dac) %>% 
      summarise(across(
        .cols = c("PS","HP","IC"),
        .fns = ~ weighted.mean(.x, wt_ca, na.rm = TRUE)
      )) %>%
      gather(tech, rent, -dac) %>% 
      mutate(across(rent, ~ .x * 0.45)), by = c("tech","dac")
  ) %>% 
  
  mutate(dac = ifelse(dac == "0", "Non_DAC", "DAC"))  


data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1) %>% 
  dplyr::select(-c("cost_combo_winter_final","cost_combo_summer_final"),
                -c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv","solstor_wtp_dv"),
                -c("PS_int","EV_int","HP_int","IC_int"))

### optimistic scenarios
select_scene <- list(c("peer_HP"),
                     c("peer_IC"),
                     c("peer_PV"))

rates <- list(c(-0.25, 0),
              c(-0.17, 0),
              c(-0.23, -0.14))


result_dac <- data.frame()
for(k in 1:3){
  
  ### scenario variable impact by DAC - random slope model 
  b_ev <- mreg_dac_r(data = data %>% 
                     filter(home_own == 1) %>% 
                     dplyr::select(VAR[[k+1]], matches("home_own", ignore.case = FALSE),
                                   climatezone, dac, matches("PV|PS|EV|HP|IC", ignore.case = FALSE),wt_ca), 
                   
                   remove = "home_own",
                   i = k+2,
                   scenario = select_scene[[k]],
                   future = "high") # variable effects and SE 
  
  df <- b_ev %>% 
    dplyr::select(-std.error:-tech) %>% 
    pivot_wider(names_from = dac, values_from = estimate) %>% 
    dplyr::rename(var = key) %>% 
    cbind(tibble(rate = rates[[k]])) %>% 
    mutate(non_DAC = `0`*rate,
           DAC = `1`*rate) 

  
  rd <- df %>%
    summarise(
      # charging_5mile  = sum(non_DAC[str_detect(var, "charging_5mile_f1")], na.rm = TRUE),
      peer            = sum(non_DAC[str_detect(var, "peer")], na.rm = TRUE)
      # home_age            = sum(non_DAC[str_detect(var, "home_age")], na.rm = TRUE),
      # rangeanxiety    = sum(non_DAC[str_detect(var, "rangeanxiety")], na.rm = TRUE)
    ) %>% 
    mutate(tech = ipt[k+2],
           dac = "Non_DAC") %>% 
    rbind(
      df %>%
        summarise(
          # charging_5mile  = sum(DAC[str_detect(var, "charging_5mile_f1")], na.rm = TRUE),
          peer            = sum(DAC[str_detect(var, "peer")], na.rm = TRUE)
          # home_age            = sum(DAC[str_detect(var, "home_age")], na.rm = TRUE),
          # rangeanxiety    = sum(DAC[str_detect(var, "rangeanxiety")], na.rm = TRUE)
        ) %>% 
        mutate(tech = ipt[k+2],
               dac = "DAC") 
    )
  
  result_dac <- rbind(rd, result_dac)
}


stage_order <- c("Rent", "Current",  "Future", "Low", "High", 
                 "Peer effects","Intended")

df_long <- d1_dac %>%
  left_join(result_dac, by = c("tech", "dac")) %>% 
  dplyr::select(-intended) %>% 
  pivot_longer(cols = -c("dac","tech"), names_to = "stage", values_to = "value") %>%
  mutate(stage = recode(stage,
                        "current" = "Current",
                        "rent" = "Rent",
                        "future" = "Future",
                        "low.subsidy" = "Low",
                        "high.subsidy" = "High",
                        "peer" = "Peer effects")) %>% 
  mutate(stage = factor(stage, levels = stage_order)) %>% 
  arrange(stage) %>% 
  group_by(dac, tech) %>%
  mutate(
    value = replace_na(value, 0),
    start = cumsum(lag(value, default = 0)),  # cumulative start
    end = start + value
  ) %>% 
  
  rbind(
    d1_dac %>% 
      left_join(result_dac, by = c("tech", "dac"))  %>%
      dplyr::select(-peer) %>% 
      pivot_longer(cols = -c("dac","tech"), names_to = "stage", values_to = "value") %>% 
      mutate(stage = recode(stage,
                            "current" = "Current",
                            "rent" = "Rent",
                            "future" = "Future",
                            "low.subsidy" = "Low",
                            "high.subsidy" = "High",
                            "intended" = "Intended")) %>% 
      
      mutate(stage = factor(stage, levels = stage_order)) %>% 
      arrange(stage) %>% 
      group_by(dac, tech) %>%
      mutate(
        value = replace_na(value, 0),
        start = cumsum(lag(value, default = 0)),  # cumulative start
        end = start + value
      ) %>% 
      filter(stage == "Intended") 
  ) %>% 

  mutate(tech = factor(tech, levels = c("PS","HP","IC"))) %>% 
  mutate(scene = "Optimistic")


dff <- df_long %>%
  mutate(across(where(is.numeric), ~ .x * 100)) %>% 
  group_by(dac, tech) %>%
  mutate(id = row_number(),
         label_val = if_else(value != 0, as.character(round(value, 0)), "")) %>%
  ungroup() %>% # Ungrouping is good practice after mutations
  mutate(class = ifelse(stage %in% c("Current","Future","Rent"), "No subsidy",
                        ifelse(stage %in% c("Low","High"), "Policy intervention",
                               ifelse(stage %in% c("Intended"), "Rest",
                                      "Time variant"))),
         class = factor(class, levels = c("No subsidy","Policy intervention","Time variant","Rest")))

segment_data <- dff %>%
  group_by(dac, tech) %>%
  filter(id < max(id)) %>% 
  filter(tech != "PV") %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Heat pumps","Induction stoves"))) 

hlines <- tibble::tribble(
  ~tech, ~y_intercept, ~line_label,
  "PV + Storage", 20, "Base: 0.20",
  "Heat pumps", 46,   "Base: 46" # This line will not appear as there is no "HP" data
) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps")))


opt_result <- dff %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Heat pumps","Induction stoves"))) 


### pessimistic scenarios
select_scene <- list(c("peer_HP"),
                     c("peer_IC"),
                     c("peer_PV"))


rates <- list(c(-0.11, 0), # HP
                c(-0.07, 0), # IC
                c(-0.18, 0)) # PS

result_dac_p <- data.frame()
for(k in 1:3){
  
  b_ev <- mreg_dac_r(data %>% 
                       filter(home_own == 1) %>% 
                       dplyr::select(VAR[[k+1]], matches("home_own", ignore.case = FALSE),
                                     climatezone, dac, matches("PV|PS|EV|HP|IC", ignore.case = FALSE),wt_ca),
                     remove = "home_own",
                     i = k+2,
                     scenario = select_scene[[k]],
                     future = "low")
  
  df <- b_ev %>% 
    dplyr::select(-std.error:-tech) %>% 
    pivot_wider(names_from = dac, values_from = estimate) %>% 
    dplyr::rename(var = key) %>% 
    cbind(tibble(rate = rates[[k]])) %>% 
    mutate(non_DAC = `0`*rate,
           DAC = `1`*rate) 
  
  rd <- df %>%
    summarise(
      # charging_5mile  = sum(non_DAC[str_detect(var, "charging_5mile_f1")], na.rm = TRUE),
      peer            = sum(non_DAC[str_detect(var, "peer")], na.rm = TRUE)
      # home_age            = sum(non_DAC[str_detect(var, "home_age")], na.rm = TRUE),
      # rangeanxiety    = sum(non_DAC[str_detect(var, "rangeanxiety")], na.rm = TRUE)
    ) %>% 
    mutate(tech = ipt[k+2],
           dac = "Non_DAC") %>% 
    rbind(
      df %>%
        summarise(
          # charging_5mile  = sum(DAC[str_detect(var, "charging_5mile_f1")], na.rm = TRUE),
          peer            = sum(DAC[str_detect(var, "peer")], na.rm = TRUE)
          # home_age            = sum(DAC[str_detect(var, "home_age")], na.rm = TRUE),
          # rangeanxiety    = sum(DAC[str_detect(var, "rangeanxiety")], na.rm = TRUE)
        ) %>% 
        mutate(tech = ipt[k+2],
               dac = "DAC") 
    )
  
  result_dac_p <- rbind(rd, result_dac_p)
}


stage_order <- c("Rent", "Current", "Future", "Low", "High", 
                 "Peer effects")

df_long_p <- d1_dac %>% 
  dplyr::select(-intended) %>% 
  dplyr::select(-high.subsidy) %>% 
  left_join(result_dac_p, by = c("tech", "dac"))  %>% 
  pivot_longer(cols = -c("dac","tech"), names_to = "stage", values_to = "value") %>%
  mutate(stage = recode(stage,
                        "current" = "Current",
                        "rent" = "Rent",
                        "future" = "Future",
                        "low.subsidy" = "Low",
                        
                        "peer" = "Peer effects")) %>% 
  mutate(stage = factor(stage, levels = stage_order)) %>% 
  arrange(stage) %>% 
  group_by(dac, tech) %>%
  mutate(
    value = replace_na(value, 0),
    start = cumsum(lag(value, default = 0)),  # cumulative start
    end = start + value
  ) %>% 
  mutate(tech = factor(tech, levels = c("PS","HP","IC"))) %>% 
  mutate(scene = "Pessimistic")


dff_p <- df_long_p %>% 
  mutate(across(where(is.numeric), ~ .x * 100)) %>% 
  group_by(dac, tech) %>%
  mutate(id = row_number(),
         label_val = if_else(value != 0, as.character(round(value, 0)), "")) %>%
  ungroup() %>% # Ungrouping is good practice after mutations
  mutate(class = ifelse(stage %in% c("Current","Future","Rent"), "No subsidy",
                        ifelse(stage %in% c("Low","High"), "Policy intervention",
                               ifelse(stage %in% c("Intended"), "Rest",
                                      "Time variant"))),
         class = factor(class, levels = c("No subsidy","Policy intervention","Time variant","Rest")))


segment_data <- dff_p %>%
  group_by(dac, tech) %>%
  filter(id < max(id)) %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Heat pumps","Induction stoves"))) 

hlines <- tibble::tribble(
  ~tech, ~y_intercept, ~line_label,
  "PV + Storage", 20, "Base: 0.20",
  "Heat pumps", 46,   "Base: 46" # This line will not appear as there is no "HP" data
) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Heat pumps")))


pess_result <- dff_p %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Heat pumps","Induction stoves"))) 

### combine
d <- opt_result %>% 
  mutate(id = ifelse(stage == "Intended", 5, id)) %>% 
  mutate(
    id = ifelse(class == "Time variant", 4, id)   # stack on top of More subsidy
  ) %>% 
  rbind(
    pess_result %>%
      filter(class == "Time variant") %>% 
      mutate(
        id = 3
      )
  ) %>% 
  mutate(id = ifelse(stage == "Rent", 1, 
                     ifelse(stage == "Current", 1,
                     ifelse(stage == "Future", 2,
                            ifelse(stage == "Low", 3,
                                   ifelse(stage == "High", 4, id))))))


time_variant_stages <- c("Peer effects","Rent")
fill_vals <- c(
  setNames(RColorBrewer::brewer.pal(length(time_variant_stages), "Set2"),
           time_variant_stages),
  "Other" = "azure3" 
)

pattern_vals <- c(
  setNames(rep("stripe", length(time_variant_stages)), time_variant_stages),
  "Other" = "none"
)


# d %>%
#   mutate(
#     sta = as.character(stage),
#     ST = ifelse(sta == "Peer effects", "Peer effects",
#                 ifelse(sta == "Rent", "Rent", "Other")),
#     ST = factor(ST, levels = c("Peer effects","Rent","Other")),
#     id = as.numeric(id)   
#   ) %>%
#   ggplot(aes(x = stage)) +
#   
#   geom_rect_pattern(
#     aes(
#       xmin = id - 0.45,
#       xmax = id + 0.45,
#       ymin = start,
#       ymax = end,
#       fill = ST,
#       pattern = ST
#     ),
#     pattern_fill = "black",
#     pattern_angle = 45,
#     pattern_density = 0.02,
#     pattern_spacing = 0.03,
#     color = NA
#   ) +
#   
#   geom_hline(
#     data = hlines %>% filter(tech != "PV"),
#     aes(yintercept = y_intercept),
#     color = "darkred",
#     linetype = "dashed",
#     linewidth = 1
#   ) +
#   
#   facet_grid(dac ~ tech, scales = "free_x") +
#   
#   scale_x_discrete(limits = c("Current","Future","Low","High","Intended")) +
#   scale_y_continuous(labels = scales::comma) +
#   
#   scale_fill_manual(values = fill_vals, name = "", breaks = time_variant_stages) +
#   scale_pattern_manual(values = pattern_vals, name = "", breaks = time_variant_stages) +
#   
#   labs(x = "", y = "Cumulative Adoption (%)") +
#   
#   theme_minimal(base_size = 12) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
#     plot.title = element_text(face = "bold", size = 16),
#     legend.position = "right",
#     strip.text = element_text(size = 14, face = "bold"),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank()
#   )

### add new built effects
d_w <- CA_t %>% 
  st_drop_geometry() %>% 
  left_join(dac, by = "GEOID") %>% 
  group_by(sample) %>% 
  summarise(pop = sum(estimate)) %>%
  mutate(prop = pop / sum(pop)) %>% 
  pull(prop)

d_w <- c(1,1)


daa1 <- d %>% 
  mutate(across(c(value,start,end), ~ .x*0.925))

add_values <- tibble(
  tech = rep(c("PV + Storage","Heat pumps","Induction stoves"), each = 3),
  id    = rep(c(2, 3, 4), 3),
  value = rep(0.5, 9) * 7.5 * d_w[2] 
) %>% 
  mutate(dac = "DAC") %>% 
  rbind(
    tibble(
      tech = rep(c("PV + Storage","Heat pumps","Induction stoves"), each = 3),
      id    = rep(c(2, 3, 4), 3),
      value = rep(0.5, 9) * 7.5 * d_w[1]
    ) %>% 
      mutate(dac = "Non_DAC")
  )

# extract the correct 'start' for each id (the last end value)
last_end <- daa1 %>%
  group_by(dac, tech, id) %>%
  summarise(start = max(end), .groups = "drop") 

# compute end = value + start
new_rows <- add_values %>%
  left_join(last_end, by = c("dac","tech","id")) %>%
  mutate(end = start + value,
         stage = "New build 50%",
         scene = "Optimistic",
         label_val = NA,
         class = "Time variant")

last_end1 <- new_rows %>%
  group_by(tech, id, dac) %>%
  summarise(start = max(end), .groups = "drop") 


new_rows <- add_values %>%
  left_join(last_end1, by = c("dac","tech","id")) %>%
  mutate(end = start + value,
         stage = "New build 100%",
         scene = "Optimistic",
         label_val = NA,
         class = "Time variant") %>% 
  rbind(new_rows)


# bind to original data
daa <- bind_rows(daa1, new_rows) %>%
  arrange(id, start) 


# Identify the time-variant stages
time_variant_stages <- c("New build-100% adoption", "New build-50% adoption", "Peer effects")

fill_vals <- c(
  setNames(RColorBrewer::brewer.pal(length(time_variant_stages), "Set2")[c(3,1,2)],
           time_variant_stages),
  "Renters" = "brown4",
  "Homeowners" = "burlywood",
  "Subsidy" = "azure3"
  
)

pattern_vals <- c(
  setNames(rep("stripe", length(time_variant_stages)), time_variant_stages),
  "Renters" = "none",
  "Homeowners" = "none",
  "Subsidy" = "none"
)


f2a <- daa %>%
  mutate(stage = as.character(stage),
         stage = ifelse(stage == "Future", "No subsidy",
                        ifelse(stage == "Low", "Low support",
                               ifelse(stage == "High", "High support", 
                                      ifelse(stage == "Intended", "Extra high subsidy", stage))))) %>% 
  mutate(sta = as.character(stage),
         ST = ifelse(sta == "Peer effects", "Peer effects", 
                     ifelse(sta == "New build 50%", "New build-50% adoption",
                            ifelse(sta == "New build 100%", "New build-100% adoption",
                                   ifelse(sta == "Rent", "Renters","Homeowners")))),
         ST = ifelse(stage %in% c("No subsidy","Low support","High support","Extra high subsidy"), "Subsidy", ST),
         id = as.numeric(id), 
         tech = factor(tech, levels = c("PV + Storage","Heat pumps", "Induction stoves"))) %>% 
  
  mutate(dac = ifelse(dac == "Non_DAC", "Non-DAC", dac),
         dac = factor(dac, levels = c("Non-DAC", "DAC"))) %>% 
  
  ggplot(aes(x = stage)) +
  
  geom_rect(aes(xmin = id - 0.45, xmax = id + 0.45, ymin = start, ymax = end, fill = ST)) +
  
  geom_rect_pattern(
    aes(
      xmin = id - 0.45,
      xmax = id + 0.45,
      ymin = start,
      ymax = end,
      fill = ST,
      pattern = ST
    ),
    pattern_fill = "white",
    pattern_colour = "white",
    pattern_angle = 45,
    pattern_density = 0.02,
    pattern_spacing = 0.03,
    color = NA
  ) +
  
  geom_hline(
    data = hlines %>% filter(tech != "PV"),
    aes(yintercept = y_intercept),
    color = "darkred",
    linetype = "dashed",
    linewidth = 1
  ) +
  
  facet_grid(tech ~ dac, scales = "fixed", switch = "y") +
  
  scale_x_discrete(drop = FALSE,
                   limits = c("Current", "No subsidy", "Low support", "High support", "Extra high subsidy")) +
  
  scale_y_continuous(labels = scales::comma) +
  
  scale_fill_manual(values = fill_vals, name = "",
                    breaks = c(time_variant_stages, "Subsidy", "Homeowners", "Renters")) +
  scale_pattern_manual(values = pattern_vals, name = "",
                       breaks = c(time_variant_stages,"Subsidy", "Homeowners", "Renters")) +
  
  labs(
    title = "Adoption of Non-DAC vs. DAC",
    x = "",
    y = "Cumulative Adoption (%)                     "
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    # axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.x = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "right",
    legend.box.margin = margin(t = 50),
    
    strip.text = element_text(size = 14, face = "bold"),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, size = 14, face = "bold"),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.spacing.y = unit(2, "lines")
  )


### plot difference
df_diff <- daa %>%
  mutate(stage = as.character(stage),
         stage = ifelse(stage == "Future", "No subsidy",
                        ifelse(stage == "Low", "Low support",
                               ifelse(stage == "High", "High support", 
                                      ifelse(stage == "Intended", "Extra high subsidy", stage))))) %>% 
  mutate(sta = as.character(stage),
         ST = ifelse(sta == "Peer effects", "Peer effects", 
                     ifelse(sta == "New build 50%", "New build-50% adoption",
                            ifelse(sta == "New build 100%", "New build-100% adoption",
                                   ifelse(sta == "Rent", "Renters","Homeowners")))),
         ST = ifelse(stage %in% c("No subsidy","Low support","High support","Extra high subsidy"), "Subsidy", ST),
         id = as.numeric(id), 
         tech = factor(tech, levels = c("PV + Storage","Heat pumps", "Induction stoves"))) %>% 
  
  mutate(dac = ifelse(dac == "Non_DAC", "Non-DAC", dac),
         dac = factor(dac, levels = c("Non-DAC", "DAC"))) %>% 
  
  mutate(stage =  ifelse(stage == "Rent", "Current", stage)) %>% 
  
  pivot_wider(
    names_from = dac, 
    values_from = value,
    # Ensure we keep columns that uniquely identify the pair
    id_cols = c(id, tech, ST, stage) 
  ) %>%
  mutate(
    diff_val = `Non-DAC` - DAC
  ) %>%
  # Filter out rows where one side of the pair is missing
  filter(!is.na(diff_val))


stage_labs <- df_diff %>%
  filter(ST %in% c("Homeowners", "Renters", "Subsidy")) %>%
  distinct(id, stage) %>%
  arrange(id)


f2b <- ggplot(df_diff, aes(
  x = id,
  y = diff_val,
  fill = ST,
  pattern = ST
)) +
  
  geom_col_pattern(
    color = "white",
    size = 0.2,
    pattern_fill = "white",
    pattern_colour = "white",
    pattern_angle = 45,
    pattern_density = 0.02,
    pattern_spacing = 0.03
  ) +
  
  facet_wrap(~ tech, nrow = 3) +
  
  scale_x_continuous(
    breaks = stage_labs$id,
    labels = stage_labs$stage
  ) +
  
  scale_fill_manual(
    values = fill_vals,
    name = "",
    breaks = c(time_variant_stages, "Subsidy", "Homeowners", "Renters")
  ) +
  
  scale_pattern_manual(
    values = pattern_vals,
    name = "",
    breaks = c(time_variant_stages, "Subsidy", "Homeowners", "Renters")
  ) +
  
  labs(
    title = "Difference between Non-DAC and DAC",
    x = "",
    y = "Difference (%; Non-DAC - DAC)                   "
  ) +
  
  ylim(c(-2.5, 7)) +
  
  theme_minimal(base_size = 12) +
  theme(
    # axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.x = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "right",
    legend.box.margin = margin(t = 50),
    
    strip.text = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, size = 14, face = "bold"),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.spacing.y = unit(2, "lines")
  ) +
  
  guides(
    fill = guide_legend(nrow = 1),
    pattern = guide_legend(nrow = 1)
  )



############################################################################### EV
data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1) %>% 
  dplyr::select(-c("cost_combo_winter_final","cost_combo_summer_final"),
                -c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv","solstor_wtp_dv"))
## average adoption
d1 <- data %>% # only homeowners 
  group_by(dac) %>% 
  summarise(across(
    .cols = all_of(names(.)[str_detect(names(.), "EV")& !str_detect(names(.), "peer")]),
    .fns = ~ weighted.mean(.x, wt_ca, na.rm = TRUE)
  )) %>%
  gather(key, adoption, -dac) %>% 
  
  mutate(
    tech = str_extract(key, "(PV|PS|EV|HP|IC)"),
    scenario = case_when(
      str_detect(key, "_int") ~ "Intended",
      str_detect(key, "^(PS|EV|HP|IC)") ~ "current",
      str_detect(key, "0|(PV$)") ~ "Future",
      str_detect(key, "low") ~ "Lsubsidy",
      str_detect(key, "high") ~ "Hsubsidy",
      TRUE ~ "future"  # fallback for keys like "future_PV"
    )
  ) %>%
  dplyr::select(-key) %>%
  pivot_wider(names_from = scenario, values_from = adoption) %>%
  mutate(
    future = Future - current,
    low.subsidy = Lsubsidy - Future,
    high.subsidy = Hsubsidy - Lsubsidy,
    intended = Intended - Hsubsidy
  ) %>%
  dplyr::select(-Future,-Lsubsidy,-Hsubsidy,-Intended) %>% 
  mutate(dac = ifelse(dac == "0", "Non_DAC", "DAC")) %>% 
  mutate(current = current * c(0.916, 0.839),
         rent = current * c(0.0844, 0.161))


# data %>%
#   filter(EV == 1) %>%
#   group_by(dac, home_own) %>%
#   summarise(count = n()) %>%
#   mutate(sum = sum(count),
#          prop = count/sum)


### optimistic scenarios
select_var <- list(c("peer_EV","rangeanxiety"))

rates <- list(c(-0.26,-0.04,-2))


data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1) %>% 
  dplyr::select(-c("cost_combo_winter_final","cost_combo_summer_final"),
                -c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv","solstor_wtp_dv"),
                -c("PS_int","EV_int","HP_int","IC_int"))

result <- data.frame()
for(k in 1){
  
  ### without sum contrasts for scenario variables
  b_ev <- mreg_dac_r(data = data %>% 
                       dplyr::select(VAR[[k]], matches("home_own", ignore.case = FALSE),
                                     climatezone, dac, matches("PV|EV|PS|HP|IC", ignore.case = FALSE),wt_ca),
                     remove = NULL,
                     i = k+1,
                     scenario = select_var[[k]],
                     future = "high")
  
  df <- b_ev %>% 
    dplyr::select(-std.error:-tech) %>% 
    pivot_wider(names_from = dac, values_from = estimate) %>% 
    dplyr::rename(var = key) %>% 
    cbind(tibble(rate = rates[[k]])) %>% 
    mutate(non_DAC = `0`*rate,
           DAC = `1`*rate) 
  

  # df <- b_ev[[1]] %>% t() %>% 
  #   as.data.frame() %>% 
  #   dplyr::slice(-1) %>% 
  #   mutate(var = row.names(.)) %>% 
  #   cbind(tibble(rate = rates[[k]])) %>% 
  #   mutate(non_DAC = `0`*rate,
  #          DAC = `1`*rate) 
  

  rd <- df %>%
    summarise(
      # charging_5mile  = sum(non_DAC[str_detect(var, "charging_5mile_f1")], na.rm = TRUE),
      peer            = sum(non_DAC[str_detect(var, "peer")], na.rm = TRUE),
      # home_age            = sum(non_DAC[str_detect(var, "home_age")], na.rm = TRUE),
      rangeanxiety    = sum(non_DAC[str_detect(var, "rangeanxiety")], na.rm = TRUE)
    ) %>% 
    mutate(tech = ipt[k+1],
           dac = "Non_DAC") %>% 
    rbind(
      df %>%
        summarise(
          # charging_5mile  = sum(DAC[str_detect(var, "charging_5mile_f1")], na.rm = TRUE),
          peer            = sum(DAC[str_detect(var, "peer")], na.rm = TRUE),
          # home_age            = sum(DAC[str_detect(var, "home_age")], na.rm = TRUE),
          rangeanxiety    = sum(DAC[str_detect(var, "rangeanxiety")], na.rm = TRUE)
        ) %>% 
        mutate(tech = ipt[k+1],
               dac = "DAC") 
    )
  
  result <- rbind(rd, result)
}


stage_order <- c("Rent", "Current", "Future", "Low", "High", 
                 "Peer effects","Range", "Intended")

df_long <- d1 %>% 
  left_join(result, by = c("tech","dac"))  %>%
  dplyr::select(-intended) %>% 
  pivot_longer(cols = -c(tech,dac), names_to = "stage", values_to = "value") %>% 
  mutate(stage = recode(stage,
                        "current" = "Current",
                        "rent" = "Rent",
                        "future" = "Future",
                        "low.subsidy" = "Low",
                        "high.subsidy" = "High",
                        
                        "peer" = "Peer effects",
                        "rangeanxiety" = "Range")) %>% 
  
  mutate(stage = factor(stage, levels = stage_order)) %>% 
  arrange(stage) %>% 
  group_by(tech,dac) %>%
  mutate(
    value = replace_na(value, 0),
    start = cumsum(lag(value, default = 0)),  # cumulative start
    end = start + value
  ) %>% 
  
  rbind(
    d1 %>% 
      left_join(result, by = c("tech","dac"))  %>%
      dplyr::select(-peer) %>% 
      pivot_longer(cols = -c(tech,dac), names_to = "stage", values_to = "value") %>% 
      mutate(stage = recode(stage,
                            "current" = "Current",
                            "rent" = "Rent",
                            
                            "future" = "Future",
                            "low.subsidy" = "Low",
                            "high.subsidy" = "High",
                            "intended" = "Intended")) %>% 
      
      mutate(stage = factor(stage, levels = stage_order)) %>% 
      arrange(stage) %>% 
      group_by(tech,dac) %>%
      mutate(
        value = replace_na(value, 0),
        start = cumsum(lag(value, default = 0)),  # cumulative start
        end = start + value
      ) %>% 
      filter(stage == "Intended") 
  ) %>% 
  mutate(scene = "Optimistic") 


dff <- df_long %>% 
  filter(value >0) %>% 
  mutate(across(where(is.numeric), ~ .x * 100)) %>% 
  group_by(tech,dac) %>%
  mutate(id = row_number(),
         label_val = if_else(value != 0, as.character(round(value, 0)), "")) %>%
  ungroup() %>% # Ungrouping is good practice after mutations
  mutate(class = ifelse(stage %in% c("Current","Future","Rent"), "No subsidy",
                        ifelse(stage %in% c("Low","High"), "Policy intervention",
                               ifelse(stage %in% c("Intended"), "Rest",
                                      "Time variant"))),
         class = factor(class, levels = c("No subsidy","Policy intervention","Time variant","Rest")))


segment_data <- dff %>%
  group_by(tech,dac) %>%
  filter(id < max(id)) %>% 
  mutate(tech = recode(tech, 
                       "EV" = "Electric vehicles"))

hlines <- tibble::tribble(
  ~tech, ~y_intercept, ~line_label,
  "Electric vehicles", 42, "Base: 0.42"
) 



opt_result <- dff %>% 
  mutate(tech = recode(tech, 
                       "EV" = "Electric vehicles")) 


### pessimistic scenarios
select_var <- list(c("peer_EV","rangeanxiety"))

rates_p <- list(c(-0.15,0,-1))
result_p <- data.frame()
for(k in 1){
  
  b_ev <- mreg_dac_r(data = data %>% 
                       
                       dplyr::select(VAR[[k]], matches("home_own", ignore.case = FALSE),
                                     climatezone, dac, matches("PV|PS|EV|HP|IC", ignore.case = FALSE),wt_ca),
                     remove = NULL,
                     i = k+1,
                     scenario = select_var[[k]],
                     future = "low")
  
  
  df <- b_ev %>% 
    dplyr::select(-std.error:-tech) %>% 
    pivot_wider(names_from = dac, values_from = estimate) %>% 
    dplyr::rename(var = key) %>% 
    cbind(tibble(rate = rates_p[[k]])) %>% 
    mutate(non_DAC = `0`*rate,
           DAC = `1`*rate) 
  
  # b_ev[[1]]
  
  
  rd <- df %>%
    summarise(
      # charging_5mile  = sum(non_DAC[str_detect(var, "charging_5mile_f1")], na.rm = TRUE),
      peer            = sum(non_DAC[str_detect(var, "peer")], na.rm = TRUE),
      # home_age            = sum(non_DAC[str_detect(var, "home_age")], na.rm = TRUE),
      rangeanxiety    = sum(non_DAC[str_detect(var, "rangeanxiety")], na.rm = TRUE)
    ) %>% 
    mutate(tech = ipt[k+1],
           dac = "Non_DAC") %>% 
    rbind(
      df %>%
        summarise(
          # charging_5mile  = sum(DAC[str_detect(var, "charging_5mile_f1")], na.rm = TRUE),
          peer            = sum(DAC[str_detect(var, "peer")], na.rm = TRUE),
          # home_age            = sum(DAC[str_detect(var, "home_age")], na.rm = TRUE),
          rangeanxiety    = sum(DAC[str_detect(var, "rangeanxiety")], na.rm = TRUE)
        ) %>% 
        mutate(tech = ipt[k+1],
               dac = "DAC") 
    )
  
  
  result_p <- rbind(rd, result_p)
}


df_long_p <- d1 %>% 
  dplyr::select(-intended) %>% 
  dplyr::select(-high.subsidy) %>% 
  left_join(result_p, by = c("tech","dac"))  %>%
  pivot_longer(cols = -c(tech,dac), names_to = "stage", values_to = "value") %>%
  mutate(stage = recode(stage,
                        "current" = "Current",
                        "rent" = "Rent",
                        "future" = "Future",
                        "low.subsidy" = "Low",
                        "high.subsidy" = "High",
                        
                        "peer" = "Peer effects",
                        "rangeanxiety" = "Range")) %>% 
  mutate(stage = factor(stage, levels = stage_order)) %>% 
  arrange(stage) %>% 
  group_by(tech,dac) %>%
  mutate(
    value = replace_na(value, 0),
    start = cumsum(lag(value, default = 0)),  # cumulative start
    end = start + value
  ) %>% 
  mutate(scene = "Pessimistic")


dff <- df_long_p %>% 
  filter(value >0) %>% 
  mutate(across(where(is.numeric), ~ .x * 100)) %>% 
  group_by(tech,dac) %>%
  mutate(id = row_number(),
         label_val = if_else(value != 0, as.character(round(value, 0)), "")) %>%
  ungroup() %>% # Ungrouping is good practice after mutations
  mutate(class = ifelse(stage %in% c("Current","Future","Rent"), "No subsidy",
                        ifelse(stage %in% c("Low","High"), "Policy intervention",
                               ifelse(stage %in% c("Intended"), "Rest",
                                      "Time variant"))),
         class = factor(class, levels = c("No subsidy","Policy intervention","Time variant","Rest")))


segment_data <- dff %>%
  group_by(tech,dac) %>%
  filter(id < max(id)) %>% 
  mutate(tech = recode(tech, 
                       "EV" = "Electric vehicles"))

hlines <- tibble::tribble(
  ~tech, ~y_intercept, ~line_label,
  "Electric vehicles", 42, "Base: 0.42"
) 


pess_result <- dff %>% 
  mutate(tech = recode(tech, 
                       "EV" = "Electric vehicles"))


d <- opt_result %>% 
  mutate(id = ifelse(stage == "Intended", 5, id)) %>% 
  mutate(
    id = ifelse(class == "Time variant", 4, id)   # stack on top of More subsidy
  ) %>% 
  rbind(
    pess_result %>%
      filter(class == "Time variant") %>% 
      mutate(
        id = 3
      )
  ) %>% 
  mutate(id = ifelse(stage == "Rent", 1, 
                     ifelse(stage == "Current", 1,
                            ifelse(stage == "Future", 2,
                                   ifelse(stage == "Low", 3,
                                          ifelse(stage == "High", 4, id))))))
  

time_variant_stages <- c("Range", "Peer effects")

fill_vals <- c(
  # setNames(RColorBrewer::brewer.pal(length(time_variant_stages), "Set2"),
  #          time_variant_stages),
  "Renters" = "brown4",
  "Homeowners" = "burlywood",
  "Range" = "gold4",
  "Peer effects" = "#FC8D62",
  "Subsidy" = "azure3" 
)

pattern_vals <- c(
  setNames(rep("stripe", length(time_variant_stages)), time_variant_stages),
  "Renters" = "none",
  "Homeowners" = "none",
  "Subsidy" = "none"
)

f2c <- d %>%
  mutate(stage = as.character(stage),
         stage = ifelse(stage == "Future", "No subsidy",
                        ifelse(stage == "Low", "Low support",
                               ifelse(stage == "High", "High support", 
                                      ifelse(stage == "Intended", "Extra high subsidy", 
                                             ifelse(stage == "Rent", "Renters",stage)))))) %>% 
  mutate(sta = as.character(stage),
         ST = ifelse(sta == "Peer effects", "Peer effects", 
                     ifelse(sta == "Range", "Range","Subsidy")),
         ST = ifelse(stage == "Renters", "Renters",
                     ifelse(stage == "Current", "Homeowners", ST)),
         ST = factor(ST, levels = c("Range", "Peer effects", "Subsidy","Homeowners","Renters"))) %>% 
  
  mutate(dac = ifelse(dac == "Non_DAC", "Non-DAC", dac),
         dac = factor(dac, levels = c("Non-DAC", "DAC"))) %>% 
  
  ggplot(aes(x = stage)) +
  
  geom_rect(aes(xmin = id - 0.45, xmax = id + 0.45, ymin = start, ymax = end, fill = ST)) +
  
  geom_rect_pattern(
    aes(
      xmin = id - 0.45,
      xmax = id + 0.45,
      ymin = start,
      ymax = end,
      fill = ST,
      pattern = ST
    ),
    pattern_fill = "white",
    pattern_color = "white",
    pattern_angle = 45,
    pattern_density = 0.02,
    pattern_spacing = 0.03,
    color = NA
  ) +
  
  geom_hline(
    data = hlines %>% filter(tech != "PV") %>% 
      mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps"))),
    aes(yintercept = y_intercept),
    color = "darkred",
    linetype = "dashed",
    linewidth = 1
  ) +
  
  facet_grid(tech ~dac, scales = "fixed", switch = "y") +
  
  scale_x_discrete(limits = c("Current", "No subsidy", "Low support", "High support","Extra high subsidy")) +
  
  scale_y_continuous(labels = scales::comma) +
  
  scale_fill_manual(values = fill_vals, name = "",
                    breaks = "Range") +
  scale_pattern_manual(values = pattern_vals, name = "",
                       breaks = "Range") +
  ylim(0, 45) +
  
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "right",
    legend.box.margin = margin(t = 0, r = 80, b = 50, l = 0),
    
    strip.text.x = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, size = 14, face = "bold"),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


### plot difference
df_diff <- d %>%
  mutate(stage = as.character(stage),
         stage = ifelse(stage == "Future", "No subsidy",
                        ifelse(stage == "Low", "Low support",
                               ifelse(stage == "High", "High support", 
                                      ifelse(stage == "Intended", "Extra high subsidy", 
                                             ifelse(stage == "Rent", "Renters",stage)))))) %>% 
  mutate(sta = as.character(stage),
         ST = ifelse(sta == "Peer effects", "Peer effects", 
                     ifelse(sta == "Range", "Range","Subsidy")),
         ST = ifelse(stage == "Renters", "Renters",
                     ifelse(stage == "Current", "Homeowners", ST)),
         ST = factor(ST, levels = c("Range", "Peer effects", "Subsidy","Homeowners","Renters"))) %>% 
  
  mutate(dac = ifelse(dac == "Non_DAC", "Non-DAC", dac),
         dac = factor(dac, levels = c("Non-DAC", "DAC"))) %>% 
  
  mutate(stage =  ifelse(stage == "Rent", "Current", stage)) %>% 
  
  pivot_wider(
    names_from = dac, 
    values_from = value,
    # Ensure we keep columns that uniquely identify the pair
    id_cols = c(id, tech, ST, stage) 
  ) %>%
  mutate(
    diff_val = `Non-DAC` - DAC
  ) %>%
  # Filter out rows where one side of the pair is missing
  filter(!is.na(diff_val))


stage_labs <- df_diff %>%
  filter(ST %in% c("Homeowners", "Subsidy")) %>%
  distinct(id, stage) %>%
  arrange(id)


f2d <- ggplot(df_diff, aes(
  x = id,
  y = diff_val,
  fill = ST,
  pattern = ST
)) +
  
  geom_col_pattern(
    color = "white",
    size = 0.2,
    pattern_fill = "white",
    pattern_colour = "white",
    pattern_angle = 45,
    pattern_density = 0.02,
    pattern_spacing = 0.03
  ) +
  
  facet_wrap(~ tech, nrow = 3) +
  
  scale_x_continuous(
    breaks = stage_labs$id,
    labels = stage_labs$stage
  ) +
  
  scale_fill_manual(
    values = fill_vals,
    name = "",
    breaks = c(time_variant_stages, "Subsidy", "Homeowners", "Renters")
  ) +
  
  scale_pattern_manual(
    values = pattern_vals,
    name = "",
    breaks = c(time_variant_stages, "Subsidy", "Homeowners", "Renters")
  ) +
  
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "right",
    legend.box.margin = margin(t = 0, r = 80, b = 50, l = 0),
    
    strip.text = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, size = 14, face = "bold"),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  
  guides(
    fill = guide_legend(nrow = 1),
    pattern = guide_legend(nrow = 1)
  )


library(ggpubr)

# top row: legend toward bottom
f2a2 <- f2a + theme(
  legend.position      = c(1, 0.15),      # right, low
  legend.justification = c(1, 0.15),
  plot.margin = margin(0, 5, 0, 5)
) 



f2b2 <- f2b + theme(legend.position = "none",
                    plot.margin = margin(0, 5, 0, 5))

# bottom row: legend toward top
f2c2 <- f2c + theme(
  legend.position = "right",
  legend.box.margin = margin(t = 0, r = 85, b = 250, l = 0),
  plot.margin = margin(0, 5, 0, 5)
)

f2d2 <- f2d + theme(legend.position = "none",
                    plot.margin = margin(0, 5, 0, 5))

p_out <- ggarrange(
  ggarrange(f2a2, f2b2, ncol = 2, widths = c(2, 1),
            common.legend = T, legend = "right"),
  ggarrange(f2c2, f2d2, ncol = 2, widths = c(2, 1),
            common.legend = T, legend = "right"),
  nrow = 2, heights = c(3, 1.5)
  
)

ggsave(
  "./fig/f3.png",
  p_out,
  width = 12, height = 12
)

