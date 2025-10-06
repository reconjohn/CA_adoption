source("./syntax/Function.R")
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)


remove <- c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv", "education","employment")


### MRP mapping
tt <- CA_t %>% dplyr::select(GEOID, estimate) %>%
  left_join(mrp %>% 
              dplyr::select(-future_PV), by = "GEOID")

tt %>%
  gather(key, value, future_EV_0:future_IC_159) %>%
  mutate(key = factor(key, levels = names(tt)[3:14])) %>% 
  ggplot() +
  geom_sf(fill = "white", color = "gray0") + # US border
  geom_sf(aes(fill = value), color = NA, size = 0.3) +
  facet_wrap(~key, nrow = 3, dir = "v") +

  theme_minimal() +
  # scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis_c(option = "magma") +

  labs(title = "Final adoption by scenario", fill = "") +
  theme(legend.position = "right",
        # legend.text=element_text(size=6),
        # legend.key.size = unit(0.3, 'cm'),
        strip.text = element_text(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title=element_text(family="Franklin Gothic Demi", size=15, hjust = 0))


### scenario 
effect <- list()
for(i in seq_along(ipt)){
  future <- fut[[i]]
  scene <- c("Optimistic","Pessimistic")
  
  for(j in 1:2){
    f_d <- final_ef_peer(data, i, future[j]) %>% 
      mutate(class = paste0(ipt[i],"_",scene[j]))
    
    effect <- append(effect, list(f_d))
    
  }
}

save(effect, mrp, file = "./data/results.Rdata") # for official sharing
load("./data/results.Rdata")


### mapping the final adoption 
for(i in 1:5){
  plot <- CA_t %>% 
    dplyr::select(GEOID) %>% 
    left_join(dplyr::bind_rows(effect[(2*i-1):(2*i)]) %>% 
                mutate(
                  peer_effect = rowSums(dplyr::select(., all_of(names(.)[str_detect(names(.), "^peer")])), na.rm = TRUE),
                  home_age = rowSums(dplyr::select(., all_of(names(.)[str_detect(names(.), "^home_age")])), na.rm = TRUE)
                ) %>%
                dplyr::select(-ends_with(c("New","Older","peer","none"))), 
              by = "GEOID") %>% 
    na.omit() %>% 
    pivot_longer(cols = c(Effect, MRP, Final), names_to = "key", values_to = "value") %>% 
    mutate(key = factor(key, levels = c("MRP","Effect","Final"))) %>% 
    ggplot() +
    geom_sf(fill = "white", color = "gray0") + # US border
    geom_sf(aes(fill = value), color = NA, size = 0.3) +
    facet_grid(class~key) +
    
    theme_minimal() +
    # scale_fill_distiller(palette = "RdBu", direction = -1) +
    scale_fill_viridis_c(option = "magma") +
    
    labs(title = "Final adoption by scenario", fill = "") +
    theme(legend.position = "right",
          # legend.text=element_text(size=6),
          # legend.key.size = unit(0.3, 'cm'),
          strip.text = element_text(size = 14, face = "bold"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.title=element_text(family="Franklin Gothic Demi", size=15, hjust = 0))
  
  ggsave(paste0("./fig/",ipt[i],"_final.png"),
         plot,
         width = 12, height = 8)
  
}



select_var <- list(c("peer_effect","home_age"),
                   c("peer_effect","charging_5mile_f1","rangeanxiety","home_age"),
                   c("peer_effect"),
                   c("peer_effect","home_age"),
                   c("peer_effect","home_age"))

for(i in 1:5){
  plot <- CA_t %>% 
    dplyr::select(GEOID) %>% 
    left_join(dplyr::bind_rows(effect[(2*i-1):(2*i)]) %>% 
                mutate(
                  peer_effect = rowSums(dplyr::select(., all_of(names(.)[str_detect(names(.), "^peer")])), na.rm = TRUE),
                  home_age = rowSums(dplyr::select(., all_of(names(.)[str_detect(names(.), "^home_age")])), na.rm = TRUE)
                ) %>%
                dplyr::select(-ends_with(c("New","Older","peer","none"))), 
              by = "GEOID") %>% 
    na.omit() %>% 
    pivot_longer(cols = select_var[[i]], names_to = "key", values_to = "value") %>% 
    ggplot() +
    geom_sf(fill = "white", color = "gray0") + # US border
    geom_sf(aes(fill = value), color = NA, size = 0.3) +
    facet_grid(class~key) +
    
    theme_minimal() +
    # scale_fill_distiller(palette = "RdBu", direction = -1) +
    scale_fill_viridis_c(option = "magma") +
    
    labs(title = "Final adoption by scenario", fill = "") +
    theme(legend.position = "right",
          # legend.text=element_text(size=6),
          # legend.key.size = unit(0.3, 'cm'),
          strip.text = element_text(size = 14, face = "bold"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.title=element_text(family="Franklin Gothic Demi", size=15, hjust = 0))
  
  ggsave(paste0("./fig/",ipt[i],"_effect.png"),
         plot,
         width = 12, height = 8)
  
}


### combined burden
### adoption change (final future adoption including all scenario optimistics - future adoption)
# PV: -4 MWh/HHyr
# EV: 2 MWh/HHyr
# HP: 2 MWh/HHyr
burden <- effect[[9]] %>% # PS optimistic
  left_join(mrp %>% 
              dplyr::select(GEOID, future_PS_0), by = "GEOID") %>% 
  mutate(PV_increase = (Final - future_PS_0)*-4) %>% 
  dplyr::select(GEOID, PV_increase) %>% 
  left_join(effect[[3]] %>% # EV optimistic
              left_join(mrp %>% 
                          dplyr::select(GEOID, future_EV_0), by = "GEOID") %>% 
              mutate(EV_increase = (Final - future_EV_0)*2) %>% 
              dplyr::select(GEOID, EV_increase),
            by = "GEOID") %>% 
  left_join(effect[[5]] %>% # HP optimistic
              left_join(mrp %>% 
                          dplyr::select(GEOID, future_HP_0), by = "GEOID") %>% 
              mutate(HP_increase = (Final - future_HP_0)*2) %>% 
              dplyr::select(GEOID, HP_increase),
            by = "GEOID") %>% 
  mutate(increase = PV_increase+EV_increase+HP_increase) %>% 
  left_join(CA_t %>% 
              st_drop_geometry() %>% 
              dplyr::select(GEOID, estimate)) %>% 
  mutate(demand = increase*estimate/1000) # in GWh


CA_t %>% 
  dplyr::select(GEOID) %>% 
  left_join(burden, by = "GEOID") %>% 
  ggplot() +
  geom_sf(fill = "white", color = "gray0") + # US border
  geom_sf(aes(fill = demand), color = NA, size = 0.3) +
  
  theme_minimal() +
  # scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis_c(option = "magma") +
  
  labs(title = "Adoption impact", fill = "Grid demand\nincrease(GWh)") +
  theme(legend.position = "right",
        # legend.text=element_text(size=6),
        # legend.key.size = unit(0.3, 'cm'),
        strip.text = element_text(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title=element_text(family="Franklin Gothic Demi", size=15, hjust = 0))

burden_mean <- effect[[9]] %>% # PS optimistic
  left_join(mrp %>% 
              dplyr::select(GEOID, future_PS_0), by = "GEOID") %>% 
  mutate(PV_increase = (Final - future_PS_0)*-4) %>% 
  dplyr::select(GEOID, PV_increase) %>% 
  left_join(effect[[3]] %>% # EV optimistic
              left_join(mrp %>% 
                          dplyr::select(GEOID, future_EV_0), by = "GEOID") %>% 
              mutate(EV_increase = (Final - future_EV_0)*2) %>% 
              dplyr::select(GEOID, EV_increase),
            by = "GEOID") %>% 
  left_join(effect[[5]] %>% # HP optimistic
              left_join(mrp %>% 
                          dplyr::select(GEOID, future_HP_0), by = "GEOID") %>% 
              mutate(HP_increase = (Final - future_HP_0)*2) %>% 
              dplyr::select(GEOID, HP_increase),
            by = "GEOID") %>% 
  mutate(increase = PV_increase+EV_increase+HP_increase) # in MWh


CA_t %>% 
  dplyr::select(GEOID) %>% 
  left_join(burden_mean, by = "GEOID") %>% 
  ggplot() +
  geom_sf(fill = "white", color = "gray0") + # US border
  geom_sf(aes(fill = increase), color = NA, size = 0.3) +
  
  theme_minimal() +
  # scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis_c(option = "magma") +
  
  labs(title = "Intervention impact", fill = "Grid demand\nincrease(MWh/HH)") +
  theme(legend.position = "right",
        # legend.text=element_text(size=6),
        # legend.key.size = unit(0.3, 'cm'),
        strip.text = element_text(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title=element_text(family="Franklin Gothic Demi", size=15, hjust = 0))




### average adoption 
d1 <- data %>% 
  summarise(across(
    .cols = all_of(names(.)[str_detect(names(.), "PV|EV|HP|IC|PS")& !str_detect(names(.), "peer")]),
    .fns = ~ weighted.mean(.x, wt_ca, na.rm = TRUE)
  )) %>% 
  gather(key, adoption) %>% 
  mutate(
    tech = str_extract(key, "(PV|PS|EV|HP|IC)"),
    scenario = case_when(
      !str_detect(key, "^future") ~ "current",
      str_detect(key, "(_\\d{1}$)|(PV$)") ~ "Future",
      str_detect(key, "_\\d{2}$") ~ "Lsubsidy",
      str_detect(key, "_\\d{3}$") ~ "Hsubsidy",
      TRUE ~ "future"  # fallback for keys like "future_PV"
    )
  ) %>% 
  dplyr::select(-key) %>%
  pivot_wider(names_from = scenario, values_from = adoption) %>% 
  mutate(
    future = Future - current,
    low.subsidy = Lsubsidy - future,
    high.subsidy = Hsubsidy - Lsubsidy
  ) %>% 
  dplyr::select(-Future,-Lsubsidy,-Hsubsidy)




# mrp %>% 
#   left_join(CA_t %>% 
#               st_drop_geometry() %>% 
#               dplyr::select(GEOID, estimate)) %>% 
#   summarise(across(
#     .cols = all_of(names(.)[str_detect(names(.), "PV|EV|HP|IC|PS")& !str_detect(names(.), "peer")]),
#     .fns = ~ weighted.mean(.x, estimate, na.rm = TRUE)
#   )) %>% 
#   gather(key, adoption) %>% 
#   separate(key, into = c("scenario", "tech", "WTP"), sep = "_", convert = TRUE)

  

### optimistic scenarios
select_var <- list(c("peer","home_age"),
                   c("peer","charging_5mile_f1","rangeanxiety","home_age"),
                   c("peer"),
                   c("peer","home_age"),
                   c("peer","home_age"))

rates <- list(c(0,0.2,-0.14,-0.23),
              c(0,0.2,-2,0.32,-0.04,-0.26),
              c(0,-0.25),
              c(0,0.2,0,-0.17),
              c(0,0.2,-0.14,-0.23))

result <- data.frame()
for(k in 1:5){
  
  b_ev <- mreg(read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
                 data_process(ev = c("Fully electric")) %>% 
                 data_clean(1), 
               remove = c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv",
                          "education","employment"),
               i = k,
               future = fut[[k]][1])

  pattern <- paste(select_var[[k]], collapse = "|")
  
  df <- b_ev[[4]] %>% 
    filter(str_detect(var, pattern)) %>%
    dplyr::select(var, Estimate) %>% 
    cbind(tibble(rate = rates[[k]])) %>% 
    mutate(effect = Estimate*rate) 
  
  rd <- df %>%
    summarise(
      peer            = sum(effect[str_detect(var, "peer")], na.rm = TRUE),
      home_age            = sum(effect[str_detect(var, "home_age")], na.rm = TRUE),
      rangeanxiety    = sum(effect[str_detect(var, "rangeanxiety")], na.rm = TRUE),
      charging_5mile  = sum(effect[str_detect(var, "charging_5mile_f1")], na.rm = TRUE)
    ) %>% 
    mutate(tech = ipt[k])
  
  result <- rbind(rd, result)
}
  

df <- d1 %>% 
  left_join(result, by = "tech") 

stage_order <- c("current", "future", "low.subsidy", "high.subsidy", 
                 "peer", "home_age", "rangeanxiety", "charging_5mile")


# df_long <- df %>%
#   pivot_longer(cols = -tech, names_to = "stage", values_to = "value") %>%
#   mutate(stage = factor(stage, levels = stage_order)) %>% 
#   group_by(tech) %>%
#   mutate(
#     value = replace_na(value, 0),
#     offset = cumsum(lag(value, default = 0))  # cumulative offset
#   ) %>% 
#   mutate(tech = factor(tech, levels = c("PS","PV","EV","HP","IC")))
# 
# # Plot
# ggplot(df_long, aes(x = tech, y = value, fill = stage)) +
#   geom_bar(stat = "identity", position = position_stack(vjust = 1), aes(y = value, ymin = offset, ymax = offset + value)) +
#   geom_rect(aes(xmin = as.numeric(factor(tech)) - 0.4,
#                 xmax = as.numeric(factor(tech)) + 0.4,
#                 ymin = offset,
#                 ymax = offset + value,
#                 fill = stage),
#             color = "black") +
#   labs(title = "Step-wise Adoption Contributions by Tech",
#        x = "Technology",
#        y = "Adoption Contribution",
#        fill = "Stage") +
#   theme_minimal()


df_long <- df %>%
  pivot_longer(cols = -tech, names_to = "stage", values_to = "value") %>%
  mutate(stage = factor(stage, levels = stage_order)) %>% 
  
  group_by(tech) %>%
  mutate(
    value = replace_na(value, 0),
    start = cumsum(lag(value, default = 0)),  # cumulative start
    end = start + value
  ) %>% 
  mutate(tech = factor(tech, levels = c("PS","PV","EV","HP","IC"))) %>% 
  mutate(scene = "Optimistic")





dff <- df_long %>%
  mutate(across(where(is.numeric), ~ .x * 100)) %>% 
  group_by(tech) %>%
  mutate(id = row_number(),
         label_val = if_else(value != 0, as.character(round(value, 0)), "")) %>%
  ungroup() %>% # Ungrouping is good practice after mutations
  mutate(class = ifelse(stage %in% c("current","future"), "Natural increase",
                        ifelse(stage %in% c("low.subsidy","high.subsidy"), "Policy intervention",
                               "Others")),
         class = factor(class, levels = c("Natural increase","Policy intervention","Others")))

# *** FIX IS HERE ***
# 3. Create a separate, filtered data frame for the connector lines.
# This data frame excludes the last bar of each 'tech' group.
segment_data <- dff %>%
  group_by(tech) %>%
  filter(id < max(id))

hlines <- tibble::tribble(
  ~tech, ~y_intercept, ~line_label,
  "PS", 20, "Base: 0.20",
  "EV", 42, "Base: 0.42",
  "HP", 46,   "Base: 46" # This line will not appear as there is no "HP" data
) %>% 
  mutate(tech = factor(tech, levels = c("PS","EV","HP")))

# 4. Create the cascade chart
ggplot(dff, aes(x = stage)) +
  
  # Draw the cascade bars using geom_rect
  geom_rect(aes(xmin = id - 0.45, xmax = id + 0.45, ymin = start, ymax = end, fill = class)) +
  
  geom_hline(data = hlines, aes(yintercept = y_intercept), 
             color = "darkred", linetype = "dashed", linewidth = 1) +
  
  # Add labels inside the bars
  geom_text(aes(x = id, y = end + 4, label = label_val),
            color = "black",
            fontface = "bold",
            size = 4) +
  
  # Add connector lines between bars
  # *** FIX IS HERE ***
  # We now explicitly tell this layer to use our pre-filtered 'segment_data'
  geom_segment(data = segment_data,
               aes(x = id + 0.45, y = end, xend = id + 1 - 0.45, yend = end),
               color = "gray40",
               linewidth = 0.75) +
  
  # Facet by 'tech' to create separate charts for PV and PS
  facet_wrap(~ tech, scales = "free_x", nrow = 1) +
  
  # Format labels and titles
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "",
    x = "Stage",
    y = "Cumulative Adoption (%)",
    fill = ""
  ) +
  
  # Customize the theme for better readability
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


### DAC


  
### pessimistic scenarios
tech <- c("PV","EV","HP","IC","PS")

select_var <- list(c("peer","home_age"),
                   c("peer","charging_5mile_f1","rangeanxiety", "home_age"),
                   c("peer"),
                   c("peer","home_age"),
                   c("peer","home_age"))


rates_p <- list(c(0,0.09,0,-0.18), #new, newer, peer, none
              c(0, 0.09, -1,0.16,0,-0.15), #range, charging, peer, none
              c(0,-0.11),
              c(0, 0.09, 0,-0.07),
              c(0,0.09,0,-0.18))

result_p <- data.frame()
for(k in 1:5){
  
  b_ev <- mreg(read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
                 data_process(ev = c("Fully electric")) %>% 
                 data_clean(1), 
               remove = c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv",
                          "education","employment"),
               i = k,
               future = fut[[k]][2])
  
  pattern <- paste(select_var[[k]], collapse = "|")
  
  df <- b_ev[[4]] %>% 
    filter(str_detect(var, pattern)) %>%
    dplyr::select(var, Estimate) %>% 
    cbind(tibble(rate = rates_p[[k]])) %>% 
    mutate(effect = Estimate*rate) 
  
  rd <- df %>%
    summarise(
      peer            = sum(effect[str_detect(var, "peer")], na.rm = TRUE),
      home_age            = sum(effect[str_detect(var, "home_age")], na.rm = TRUE),
      rangeanxiety    = sum(effect[str_detect(var, "rangeanxiety")], na.rm = TRUE),
      charging_5mile  = sum(effect[str_detect(var, "charging_5mile_f1")], na.rm = TRUE)
    ) %>% 
    mutate(tech = tech[k])
  
  result_p <- rbind(rd, result_p)
}


df_p <- d1 %>% 
  dplyr::select(-high.subsidy) %>% 
  left_join(result_p, by = "tech") 

stage_order_p <- c("current", "future", "low.subsidy", 
                 "peer", "home_age", "rangeanxiety", "charging_5mile")


# df_long <- df %>%
#   pivot_longer(cols = -tech, names_to = "stage", values_to = "value") %>%
#   mutate(stage = factor(stage, levels = stage_order)) %>% 
#   group_by(tech) %>%
#   mutate(
#     value = replace_na(value, 0),
#     offset = cumsum(lag(value, default = 0))  # cumulative offset
#   ) %>% 
#   mutate(tech = factor(tech, levels = c("PS","PV","EV","HP","IC")))
# 
# # Plot
# ggplot(df_long, aes(x = tech, y = value, fill = stage)) +
#   geom_bar(stat = "identity", position = position_stack(vjust = 1), aes(y = value, ymin = offset, ymax = offset + value)) +
#   geom_rect(aes(xmin = as.numeric(factor(tech)) - 0.4,
#                 xmax = as.numeric(factor(tech)) + 0.4,
#                 ymin = offset,
#                 ymax = offset + value,
#                 fill = stage),
#             color = "black") +
#   labs(title = "Step-wise Adoption Contributions by Tech",
#        x = "Technology",
#        y = "Adoption Contribution",
#        fill = "Stage") +
#   theme_minimal()


df_long_p <- df_p %>%
  pivot_longer(cols = -tech, names_to = "stage", values_to = "value") %>%
  mutate(stage = factor(stage, levels = stage_order_p)) %>% 
  
  group_by(tech) %>%
  mutate(
    value = replace_na(value, 0),
    start = cumsum(lag(value, default = 0)),  # cumulative start
    end = start + value
  ) %>% 
  mutate(tech = factor(tech, levels = c("PS","PV","EV","HP","IC"))) %>% 
  mutate(scene = "Pessimistic")

### opt vs. pessi plots
df_long %>% 
  rbind(df_long_p) %>% 
  mutate(scene = factor(scene, levels = rev(c("Optimistic","Pessimistic")))) %>% 
  ggplot(aes(y = scene, xmin = start, xmax = end, fill = stage)) +
  geom_rect(aes(xmin = start, xmax = end, ymin = as.numeric(factor(scene)) - 0.4,
                ymax = as.numeric(factor(scene)) + 0.4),
            color = "black") +
  facet_wrap(~tech, switch = "y", nrow = 5) +
  labs(title = "Step-wise Adoption Contributions by Tech",
       x = "Cumulative Adoption",
       y = "",
       fill = "Stage") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 14),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "right",
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


