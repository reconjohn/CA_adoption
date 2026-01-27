source("./syntax/Function.R")
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

remove <- c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv")

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

### final adoption by tract
final <- data.frame()
for(i in 1:5){
  tp <- dplyr::bind_rows(effect[(2*i-1):(2*i)]) %>% 
                mutate(
                  peer_effect = rowSums(dplyr::select(., all_of(names(.)[str_detect(names(.), "^peer")])), na.rm = TRUE),
                  home_age = rowSums(dplyr::select(., all_of(names(.)[str_detect(names(.), "^home_age")])), na.rm = TRUE)
                ) %>%
                dplyr::select(-ends_with(c("New","Newer","peer","none"))) %>% 
    pivot_longer(cols = c(Effect, MRP, Final), names_to = "key", values_to = "value") %>% 
    mutate(key = factor(key, levels = c("MRP","Effect","Final"))) %>% 
    filter(key == "Final") %>% 
    dplyr::select(GEOID, class, value)
  
  final <- rbind(tp, final)
}

# for mapping 
CA_c <- CA_t %>% 
  mutate(county = gsub("\\d{6}$", "", GEOID)) %>% 
  group_by(county) %>% 
  summarise(geometry = st_union(geometry))


### mapping
map_dac <- CA_t %>% 
  dplyr::select(GEOID) %>% 
  left_join(final %>% 
              separate(class, into = c("tech", "scen"), sep = "_"), by = "GEOID") %>% 
  
  filter(tech != "PV") %>% 
  filter(scen == "Optimistic") %>% 
  mutate(
    tech = recode(tech,
                  "PS" = "PV + Storage",
                  "IC" = "Induction stoves",
                  "HP" = "Heat pumps",
                  "EV" = "Electric vehicles",
                  "PV" = "Photovoltaics"),
    tech = factor(tech, levels = c("Photovoltaics","PV + Storage", "Electric vehicles", "Heat pumps", "Induction stoves"))) %>%
  na.omit() %>% 
  
  st_intersection(dac_sf %>% 
                    filter(sample == 1)) %>% 
  st_collection_extract("POLYGON") %>%  # Remove geometry collections
  filter(st_area(.) > units::set_units(1, "m^2")) %>%  # Remove tiny fragments
  mutate(DAC = "DAC") %>% 
  rbind(
    CA_t %>% 
      dplyr::select(GEOID) %>% 
      left_join(final %>% 
                  separate(class, into = c("tech", "scen"), sep = "_"), by = "GEOID") %>% 
      
      filter(tech != "PV") %>% 
      filter(scen == "Optimistic") %>% 
      mutate(
        tech = recode(tech,
                      "PS" = "PV + Storage",
                      "IC" = "Induction stoves",
                      "HP" = "Heat pumps",
                      "EV" = "Electric vehicles",
                      "PV" = "Photovoltaics"),
        tech = factor(tech, levels = c("Photovoltaics","PV + Storage", "Electric vehicles", "Heat pumps", "Induction stoves"))) %>%
      na.omit() %>% 
      
      st_intersection(dac_sf %>% 
                        filter(sample == 0)) %>% 
      st_collection_extract("POLYGON") %>%  # Remove geometry collections
      filter(st_area(.) > units::set_units(1, "m^2")) %>%  # Remove tiny fragments
      mutate(DAC = "Non-DAC")
  )

tech_name <- c("PV + Storage", "Electric vehicles", "Heat pumps", "Induction stoves")
dac_map <- list()
for(i in 1:4){
  dac_map[[i]] <- map_dac %>% 
    filter(tech == tech_name[i]) %>% 
    
    ggplot() +
    geom_sf(fill = "white", color = "gray0") + # US border
    geom_sf(aes(fill = value), color = NA, size = 0.3) +
    scale_fill_viridis_c(option = "magma", name = "Adoption") +
    # new_scale_fill() +
    
    geom_sf(data = CA_c, fill = NA, color = "gray50", linewidth = 0.05) +
    # geom_sf(data = dac_sf %>% 
    #           filter(sample == 1) %>% 
    #           mutate(sample = "DAC"), aes(fill = sample), color = NA, alpha = 0.3) +
    # scale_fill_manual(values = c("DAC" = "gray80"),
    #                   name = "") +
    
    facet_wrap(~DAC, nrow = 2) +
    
    theme_minimal() +
    # scale_fill_distiller(palette = "RdBu", direction = -1) +
    # scale_fill_viridis_c(option = "magma") +
    
    labs(title = tech_name[i], fill = "Adoption") +
    
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      plot.title = element_text(face = "bold", size = 16),
      
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      legend.text = element_text(size = 10),
      legend.position = "bottom",
      
      strip.placement = "outside", # Keep labels on the outside
      # strip.background =element_rect(fill="gray22",color="gray22"),
      strip.text = element_text(color = 'black',family="Franklin Gothic Book",size=12, face = "bold"),
      strip.text.y.left = element_text(angle = 0), # Ensure domain labels are horizontal
      
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()  )
  
}

f3b <- ggarrange(plotlist = dac_map, nrow = 1)


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

###################################################################### HO
## average adoption
d1 <- data %>% # only homeowners 
      filter(home_own == 1) %>% 
      summarise(across(
        .cols = all_of(names(.)[str_detect(names(.), "PV|EV|HP|IC|PS")& !str_detect(names(.), "peer")]),
        .fns = ~ weighted.mean(.x, wt_ca, na.rm = TRUE)
      )) %>%
      gather(key, adoption) %>% 
  # mutate(
  #   tech = str_extract(key, "(PV|PS|EV|HP|IC)"),
  #   scenario = case_when(
  #     !str_detect(key, "^future") ~ "current",
  #     str_detect(key, "(_\\d{1}$)|(PV$)") ~ "Future",
  #     str_detect(key, "_\\d{2}$") ~ "Lsubsidy",
  #     str_detect(key, "_\\d{3}$") ~ "Hsubsidy",
  #     TRUE ~ "future"  # fallback for keys like "future_PV"
  #   )
  # ) %>%
  mutate(
    tech = str_extract(key, "(PV|PS|EV|HP|IC)"),
    scenario = case_when(
      !str_detect(key, "^future") ~ "current",
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
    high.subsidy = Hsubsidy - Lsubsidy
  ) %>%
  dplyr::select(-Future,-Lsubsidy,-Hsubsidy)


# ### scenario waterfall
# d1 <- mrp_mean[-c(18,19),] %>% # using MRP mean
#           pivot_wider(names_from = name, values_from = V1) %>% 
#   gather(key, adoption) %>% 
#   mutate(
#     tech = str_extract(key, "(PV|PS|EV|HP|IC)"),
#     scenario = case_when(
#       !str_detect(key, "^future") ~ "current",
#       str_detect(key, "(_\\d{1}$)|(PV$)") ~ "Future",
#       str_detect(key, "_\\d{2}$") ~ "Lsubsidy",
#       str_detect(key, "_\\d{3}$") ~ "Hsubsidy",
#       TRUE ~ "future"  # fallback for keys like "future_PV"
#     )
#   ) %>% 
#   dplyr::select(-key) %>%
#   pivot_wider(names_from = scenario, values_from = adoption) %>% 
#   mutate(
#     future = Future - current,
#     low.subsidy = Lsubsidy - Future,
#     high.subsidy = Hsubsidy - Lsubsidy
#   ) %>% 
#   dplyr::select(-Future,-Lsubsidy,-Hsubsidy) %>% 
#   filter(tech != "PV")


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
                   c("peer"),
                   c("peer","home_age"))

rates <- list(c(0,0.2,-0.14,-0.23), 
              c(0,0.2,-2, 0.32,-0.04,-0.26), # brandnew, newer, range, charge, peer, none, 
              c(0,-0.25),
              c(0,-0.17),
              c(0,0.2,-0.14,-0.23))

result <- data.frame()
for(k in 2:5){
  
  ### without sum contrasts for scenario variables
  b_ev <- mreg_scene(data = data %>% 
                       filter(home_own == 1) %>% 
                 dplyr::select(VAR[[k-1]], matches("home_own", ignore.case = FALSE),
                               climatezone, dac, matches("PV|PS|EV|HP|IC", ignore.case = FALSE),wt_ca),
                 remove = "home_own",
               i = k,
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
      home_age            = sum(effect[str_detect(var, "home_age")], na.rm = TRUE),
      charging_5mile  = sum(effect[str_detect(var, "charging_5mile_f1")], na.rm = TRUE),
      rangeanxiety    = sum(effect[str_detect(var, "rangeanxiety")], na.rm = TRUE)
    ) %>% 
    mutate(tech = ipt[k])
  
  result <- rbind(rd, result)
}
  

stage_order <- c("Current", "Future", "Low", "High", 
                 "Peer effects", "Home built after 2020", "Fast charger", "Range")

df_long <- d1 %>% 
  left_join(result, by = "tech")  %>%
  pivot_longer(cols = -tech, names_to = "stage", values_to = "value") %>%
  mutate(stage = recode(stage,
                        "current" = "Current",
                        "future" = "Future",
                        "low.subsidy" = "Low",
                        "high.subsidy" = "High",
                        
                        "peer" = "Peer effects",
                        "home_age" = "Home built after 2020",
                        "charging_5mile" = "Fast charger",
                        "rangeanxiety" = "Range")) %>% 
  
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
  filter(value >0) %>% 
  mutate(across(where(is.numeric), ~ .x * 100)) %>% 
  group_by(tech) %>%
  mutate(id = row_number(),
         label_val = if_else(value != 0, as.character(round(value, 0)), "")) %>%
  ungroup() %>% # Ungrouping is good practice after mutations
  mutate(class = ifelse(stage %in% c("Current","Future"), "No subsidy",
                        ifelse(stage %in% c("Low","High"), "Policy intervention",
                               "Time variant")),
         class = factor(class, levels = c("No subsidy","Policy intervention","Time variant")))


segment_data <- dff %>%
  group_by(tech) %>%
  filter(id < max(id)) %>% 
  filter(tech != "PV") %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "EV" = "Electric vehicles",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"))) 

hlines <- tibble::tribble(
  ~tech, ~y_intercept, ~line_label,
  "PV + Storage", 20, "Base: 0.20",
  "Electric vehicles", 42, "Base: 0.42",
  "Heat pumps", 46,   "Base: 46" # This line will not appear as there is no "HP" data
) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps")))



# error_data_opt <- data.frame(tech = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"), 
#                               x = c(7,9,6,7), 
#                               ymin = c(3.5,12,11.5,10.5), 
#                               ymax = c(15,34.8,36.5,25.3),
#                               text = c(7,23,25,15),
#                               xmin = c(4.5,6,4,4.7)) %>% 
#   mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves")))


opt_result <- dff %>% 
  filter(tech != "PV") %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "EV" = "Electric vehicles",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"))) 

# 
#   ggplot(aes(x = stage)) +
#   
#   # Draw the cascade bars using geom_rect
#   geom_rect(aes(xmin = id - 0.45, xmax = id + 0.45, ymin = start, ymax = end, fill = class)) +
#   
#   geom_hline(data = hlines %>% 
#                filter(tech != "PV"), aes(yintercept = y_intercept), 
#              color = "darkred", linetype = "dashed", linewidth = 1) +
#   
#   # Add labels inside the bars
#   geom_text(aes(x = id, y = end + 2, label = label_val),
#             color = "black",
#             fontface = "bold",
#             size = 4) +
#   
#   geom_segment(data = segment_data %>% 
#                  filter(tech != "PV"),
#                aes(x = id + 0.45, y = end, xend = id + 1 - 0.45, yend = end),
#                color = "gray40",
#                linewidth = 0.75) +
#   
#   # geom_errorbar(data = error_data_opt,
#   #               aes(x = x, ymin = ymin, ymax = ymax),
#   #               width = 0.2,
#   #               color = "red",
#   #               linewidth = 1) +  # dynamically inserted arrow
#   # 
#   # geom_text(data = error_data_opt,
#   #           aes(x = xmin, y = ymin, label = paste0("Impact: ", text)),
#   #           size = 4,
#   #           hjust = 0,
#   #           fontface = "bold") +
# 
#   # Facet by 'tech' to create separate charts for PV and PS
#   facet_wrap(~ tech, scales = "free_x",  nrow = 1) +
#   
#   # Format labels and titles
#   scale_y_continuous(labels = scales::comma) +
#   labs(
#     title = "Optimistic scenario",
#     x = "",
#     y = "Cumulative Adoption (%)",
#     fill = ""
#   ) +
#   
#   # Customize the theme for better readability
#   theme_minimal(base_size = 12) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
#     plot.title = element_text(face = "bold", size = 16),
#     legend.position = "bottom",
#     strip.text = element_text(size = 14, face = "bold"),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank()
#   )


### pessimistic scenarios
tech <- c("PV","EV","HP","IC","PS")

select_var <- list(c("peer","home_age"),
                   c("peer","charging_5mile_f1","rangeanxiety", "home_age"),
                   c("peer"),
                   c("peer"),
                   c("peer","home_age"))


rates_p <- list(c(0,0.09,0,-0.18), #new, newer, peer, none
                c(0, 0.09, -1, 0.16,0,-0.15), #range, charging, peer, none, 
                c(0,-0.11),
                c(0,-0.07),
                c(0,0.09,0,-0.18))

result_p <- data.frame()
for(k in 2:5){
  
  b_ev <- mreg_scene(data = data %>% 
                       filter(home_own == 1) %>% 
                       dplyr::select(VAR[[k-1]], matches("home_own", ignore.case = FALSE),
                                     climatezone, dac, matches("PV|PS|EV|HP|IC", ignore.case = FALSE),wt_ca),
                     remove = "home_own",
               i = k,
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
      peer            = sum(effect[str_detect(var, "peer")], na.rm = TRUE),
      home_age            = sum(effect[str_detect(var, "home_age")], na.rm = TRUE),
      charging_5mile  = sum(effect[str_detect(var, "charging_5mile_f1")], na.rm = TRUE),
      rangeanxiety    = sum(effect[str_detect(var, "rangeanxiety")], na.rm = TRUE)
    ) %>% 
    mutate(tech = tech[k])
  
  result_p <- rbind(rd, result_p)
}


df_long_p <- d1 %>% 
  dplyr::select(-high.subsidy) %>% 
  left_join(result_p, by = "tech")  %>%
  pivot_longer(cols = -tech, names_to = "stage", values_to = "value") %>%
  mutate(stage = recode(stage,
                        "current" = "Current",
                        "future" = "Future",
                        "low.subsidy" = "Low",
                        "high.subsidy" = "High",
                      
                        "peer" = "Peer effects",
                        "home_age" = "Home built after 2020",
                        "charging_5mile" = "Fast charger",
                        "rangeanxiety" = "Range")) %>% 
  mutate(stage = factor(stage, levels = stage_order)) %>% 
  
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
  mutate(class = ifelse(stage %in% c("Current","Future"), "No subsidy",
                        ifelse(stage %in% c("Low"), "Policy intervention",
                               "Time variant")),
         class = factor(class, levels = c("No subsidy","Policy intervention","Time variant")))


segment_data <- dff %>%
  group_by(tech) %>%
  filter(id < max(id)) %>% 
  filter(tech != "PV") %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "EV" = "Electric vehicles",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"))) 

hlines <- tibble::tribble(
  ~tech, ~y_intercept, ~line_label,
  "PV + Storage", 20, "Base: 0.20",
  "Electric vehicles", 42, "Base: 0.42",
  "Heat pumps", 46,   "Base: 46" # This line will not appear as there is no "HP" data
) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps")))

# error_data_pess <- data.frame(tech = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"), 
#                               x = c(6,8,5,6), 
#                               ymin = c(3.8,12.2,11.8,10.5), 
#                               ymax = c(11,24,19.1,13.4),
#                               text = c(7,12,7,3),
#                               xmin = c(4.2,5.5,3.5,4.2)) %>% 
#   mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves")))

pess_result <- dff %>% 
  filter(tech != "PV") %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "EV" = "Electric vehicles",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"))) 


d <- opt_result %>% 
  mutate(
    id = ifelse(class == "Time variant", 4, id)   # stack on top of More subsidy
  ) %>% 
  rbind(
    pess_result %>%
      filter(class == "Time variant") %>% 
      mutate(
        id = 3
      )
  ) 

# Identify the time-variant stages
time_variant_stages <- unique(d$stage[d$class == "Time variant"])


fill_vals <- c(
  setNames(RColorBrewer::brewer.pal(length(time_variant_stages), "Set2"),
           time_variant_stages),
  "Other" = "azure3" 
)

pattern_vals <- c(
  setNames(rep("stripe", length(time_variant_stages)), time_variant_stages),
  "Other" = "none"
)

d %>%
  mutate(sta = as.character(stage),
         ST = ifelse(class == "Time variant", sta, "Other"),
         ST = factor(ST, levels = c("Peer effects","Home built after 2020","Fast charger","Range", "Other"))) %>% 
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
    pattern_fill = "black",
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
  
  facet_wrap(~ tech, scales = "free_x", nrow = 1) +
  
  scale_x_discrete(limits = c("Current", "Future", "Low", "High")) +
  
  scale_y_continuous(labels = scales::comma) +
  
  scale_fill_manual(values = fill_vals, name = "",
                    breaks = time_variant_stages) +
  scale_pattern_manual(values = pattern_vals, name = "",
                       breaks = time_variant_stages) +
  
  labs(
    title = "",
    x = "",
    y = "Cumulative Adoption (%)"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "right",
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )



# pattern_vals <- c(
#   "Current"       = "none",
#   "Future"        = "none",
#   "Subsidy"       = "none",
#   "More subsidy"  = "none",
#   "Fast charger"  = "stripe",   # hatch only this one
#   "Peer effects" = "stripe",
#   "Home built after 2020" = "stripe",
#   "Range" = "stripe"
# )
# 
# opt_result %>% 
#   mutate(
#     id = ifelse(class == "Time variant", 4, id)   # stack on top of More subsidy
#   ) %>% 
#   rbind(
#     pess_result %>%
#           filter(class == "Time variant") %>% 
#           mutate(
#             id = 3
#           )
#     ) %>% 
#   # mutate(
#   #   value = ifelse(stage != "Current", value*0.56, value),
#   #   start = ifelse(stage != "Future", start*0.56, start),
#   #   end = ifelse(stage != "Current", end*0.56, end),
#   #   label_val = if_else(value != 0, as.character(round(value, 0)), "")) %>% 
#   
#   # filter(tech != "Electric vehicles") %>% 
#   # filter(start > end | is.na(start) | is.na(end))
# 
#   ggplot(aes(x = stage)) +
#   
#   # Draw the cascade bars using geom_rect
#   # geom_rect(aes(xmin = id - 0.45, xmax = id + 0.45, ymin = start, ymax = end, fill = stage)) +
#   
#   geom_rect_pattern(
#     aes(
#       xmin = id - 0.45,
#       xmax = id + 0.45,
#       ymin = start,
#       ymax = end,
#       fill = stage,
#       pattern = stage
#     ),
#     pattern_fill = "black",
#     pattern_angle = 45,
#     pattern_density = 0.15,
#     pattern_spacing = 0.02
#   ) +
#   
#   geom_hline(data = hlines %>% 
#                filter(tech != "PV"), aes(yintercept = y_intercept), 
#              color = "darkred", linetype = "dashed", linewidth = 1) +
#   
#   # # Add labels inside the bars
#   # geom_text(aes(x = id, y = end + 2, label = label_val),
#   #           color = "black",
#   #           fontface = "bold",
#   #           size = 4) +
#   
#   # geom_segment(data = segment_data %>% 
#   #                filter(tech != "PV"),
#   #              aes(x = id + 0.45, y = end, xend = id + 1 - 0.45, yend = end),
#   #              color = "gray40",
#   #              linewidth = 0.75) +
#   
#   
#   # geom_errorbar(data = error_data_pess,
#   #               aes(x = x, ymin = ymin, ymax = ymax),
#   #               width = 0.2,
#   #               color = "red",
#   #               linewidth = 1) +  # dynamically inserted arrow
#   
#   # geom_text(data = error_data_pess,
#   #           aes(x = xmin, y = ymin, label = paste0("Impact: ", text)),
#   #           size = 4,
#   #           hjust = 0,
#   #           fontface = "bold") +
#   
#   # Facet by 'tech' to create separate charts for PV and PS
#   facet_wrap(~ tech, scales = "free_x", nrow = 1) +
#   
#   scale_x_discrete(limits = c(
#     "Current",
#     "Future",
#     "Subsidy",
#     "More subsidy"
#   )) +
# 
#   
#   # Format labels and titles
#   scale_y_continuous(labels = scales::comma) +
#   scale_pattern_manual(values = pattern_vals) +
#   
#   labs(
#     title = "Homeowners",
#     x = "",
#     y = "Cumulative Adoption (%)",
#     fill = ""
#   ) +
#   
#   # Customize the theme for better readability
#   theme_minimal(base_size = 12) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
#     plot.title = element_text(face = "bold", size = 16),
#     legend.position = "right",
#     strip.text = element_text(size = 14, face = "bold"),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank()
#   )


###################################################################### Renters
## average adoption
d1 <- data %>% # only homeowners 
  filter(home_own == 0) %>% 
  summarise(across(
    .cols = all_of(names(.)[str_detect(names(.), "PV|EV|HP|IC|PS")& !str_detect(names(.), "peer")]),
    .fns = ~ weighted.mean(.x, wt_ca, na.rm = TRUE)
  )) %>%
  gather(key, adoption) %>% 
  mutate(
    tech = str_extract(key, "(PV|PS|EV|HP|IC)"),
    scenario = case_when(
      !str_detect(key, "^future") ~ "current",
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
    high.subsidy = Hsubsidy - Lsubsidy
  ) %>%
  dplyr::select(-Future,-Lsubsidy,-Hsubsidy) %>% 
  mutate(future = ifelse(tech %in% c("HP","IC"), 0, future)) # convert all values to zero for renters



### optimistic scenarios
select_var <- list(c("peer","home_age"),
                   c("peer","charging_5mile_f1","rangeanxiety","home_age"),
                   c("peer"),
                   c("peer"),
                   c("peer","home_age"))

rates <- list(c(0,0.2,-0.14,-0.23), 
              c(0,0.2,-2, 0.32,-0.04,-0.26), # brandnew, newer, range, charge, peer, none, 
              c(0,-0.25),
              c(0,-0.17),
              c(0,0.2,-0.14,-0.23))

result <- data.frame()
for(k in 2:5){
  
  ### without sum contrasts for scenario variables
  b_ev <- mreg_scene(data = data %>% 
                       filter(home_own == 0) %>% 
                       mutate(across(matches("future_IC|future_HP"), ~ replace(., is.na(.), 0))) %>% 
                       dplyr::select(VAR[[k-1]], matches("home_own", ignore.case = FALSE),
                                     climatezone, dac, matches("PV|PS|EV|HP|IC", ignore.case = FALSE),wt_ca),
                     remove = "home_own",
                     i = k,
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
      home_age            = sum(effect[str_detect(var, "home_age")], na.rm = TRUE),
      charging_5mile  = sum(effect[str_detect(var, "charging_5mile_f1")], na.rm = TRUE),
      rangeanxiety    = sum(effect[str_detect(var, "rangeanxiety")], na.rm = TRUE)
    ) %>% 
    mutate(tech = ipt[k])
  
  result <- rbind(rd, result)
}


stage_order <- c("Current", "Future", "Low", "High", 
                 "Peer effects", "Home built after 2020", "Fast charger", "Range")

df_long <- d1 %>% 
  left_join(result, by = "tech")  %>%
  pivot_longer(cols = -tech, names_to = "stage", values_to = "value") %>%
  mutate(stage = recode(stage,
                        "current" = "Current",
                        "future" = "Future",
                        "low.subsidy" = "Low",
                        "high.subsidy" = "High",
                        
                        "peer" = "Peer effects",
                        "home_age" = "Home built after 2020",
                        "charging_5mile" = "Fast charger",
                        "rangeanxiety" = "Range")) %>% 
  
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
  filter(value >0) %>% 
  mutate(across(where(is.numeric), ~ .x * 100)) %>% 
  group_by(tech) %>%
  mutate(id = row_number(),
         label_val = if_else(value != 0, as.character(round(value, 0)), "")) %>%
  ungroup() %>% # Ungrouping is good practice after mutations
  mutate(class = ifelse(stage %in% c("Current","Future"), "No subsidy",
                        ifelse(stage %in% c("Low","High"), "Policy intervention",
                               "Time variant")),
         class = factor(class, levels = c("No subsidy","Policy intervention","Time variant")))


segment_data <- dff %>%
  group_by(tech) %>%
  filter(id < max(id)) %>% 
  filter(tech != "PV") %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "EV" = "Electric vehicles",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"))) 

hlines <- tibble::tribble(
  ~tech, ~y_intercept, ~line_label,
  "PV + Storage", 20, "Base: 0.20",
  "Electric vehicles", 42, "Base: 0.42",
  "Heat pumps", 46,   "Base: 46" # This line will not appear as there is no "HP" data
) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps")))


opt_result <- dff %>% 
  filter(tech != "PV") %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "EV" = "Electric vehicles",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"))) 


### pessimistic scenarios
tech <- c("PV","EV","HP","IC","PS")

select_var <- list(c("peer","home_age"),
                   c("peer","charging_5mile_f1","rangeanxiety", "home_age"),
                   c("peer"),
                   c("peer"),
                   c("peer","home_age"))


rates_p <- list(c(0,0.09,0,-0.18), #new, newer, peer, none
                c(0, 0.09, -1, 0.16,0,-0.15), #range, charging, peer, none, 
                c(0,-0.11),
                c(0,-0.07),
                c(0,0.09,0,-0.18))

result_p <- data.frame()
for(k in 2:5){
  
  b_ev <- mreg_scene(data = data %>% 
                       filter(home_own == 0) %>% 
                       mutate(across(matches("future_IC|future_HP"), ~ replace(., is.na(.), 0))) %>% 
                       dplyr::select(VAR[[k-1]], matches("home_own", ignore.case = FALSE),
                                     climatezone, dac, matches("PV|PS|EV|HP|IC", ignore.case = FALSE),wt_ca),
                     remove = "home_own",
                     i = k,
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
      peer            = sum(effect[str_detect(var, "peer")], na.rm = TRUE),
      home_age            = sum(effect[str_detect(var, "home_age")], na.rm = TRUE),
      charging_5mile  = sum(effect[str_detect(var, "charging_5mile_f1")], na.rm = TRUE),
      rangeanxiety    = sum(effect[str_detect(var, "rangeanxiety")], na.rm = TRUE)
    ) %>% 
    mutate(tech = tech[k])
  
  result_p <- rbind(rd, result_p)
}


df_long_p <- d1 %>% 
  dplyr::select(-high.subsidy) %>% 
  left_join(result_p, by = "tech")  %>%
  pivot_longer(cols = -tech, names_to = "stage", values_to = "value") %>%
  mutate(stage = recode(stage,
                        "current" = "Current",
                        "future" = "Future",
                        "low.subsidy" = "Low",
                        "high.subsidy" = "High",
                        
                        "peer" = "Peer effects",
                        "home_age" = "Home built after 2020",
                        "charging_5mile" = "Fast charger",
                        "rangeanxiety" = "Range")) %>% 
  mutate(stage = factor(stage, levels = stage_order)) %>% 
  
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
  mutate(class = ifelse(stage %in% c("Current","Future"), "No subsidy",
                        ifelse(stage %in% c("Low"), "Policy intervention",
                               "Time variant")),
         class = factor(class, levels = c("No subsidy","Policy intervention","Time variant")))


segment_data <- dff %>%
  group_by(tech) %>%
  filter(id < max(id)) %>% 
  filter(tech != "PV") %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "EV" = "Electric vehicles",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"))) 

hlines <- tibble::tribble(
  ~tech, ~y_intercept, ~line_label,
  "PV + Storage", 20, "Base: 0.20",
  "Electric vehicles", 42, "Base: 0.42",
  "Heat pumps", 46,   "Base: 46" # This line will not appear as there is no "HP" data
) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps")))

pess_result <- dff %>% 
  filter(tech != "PV") %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "EV" = "Electric vehicles",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"))) 


da <- opt_result %>% 
  mutate(
    id = ifelse(class == "Time variant", 4, id)   # stack on top of More subsidy
  ) %>% 
  rbind(
    pess_result %>%
      filter(class == "Time variant") %>% 
      mutate(
        id = 3
      )
  ) 

# Identify the time-variant stages
time_variant_stages <- unique(da$stage[da$class == "Time variant"])


fill_vals <- c(
  setNames(RColorBrewer::brewer.pal(length(time_variant_stages), "Set2"),
           time_variant_stages),
  "Other" = "azure3" 
)

pattern_vals <- c(
  setNames(rep("stripe", length(time_variant_stages)), time_variant_stages),
  "Other" = "none"
)

da %>%
  mutate(sta = as.character(stage),
         ST = ifelse(class == "Time variant", sta, "Other"),
         ST = factor(ST, levels = c("Peer effects","Home built after 2020","Fast charger","Range", "Other"))) %>% 
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
    pattern_fill = "black",
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
  
  facet_wrap(~ tech, scales = "free_x", nrow = 1) +
  
  scale_x_discrete(limits = c("Current", "Future", "Low", "High")) +
  
  scale_y_continuous(labels = scales::comma) +
  
  scale_fill_manual(values = fill_vals, name = "",
                    breaks = time_variant_stages) +
  scale_pattern_manual(values = pattern_vals, name = "",
                       breaks = time_variant_stages) +
  
  labs(
    title = "",
    x = "",
    y = "Cumulative Adoption (%)"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "right",
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

### combination 
daa <- d %>% 
  mutate(across(c("value"), ~ .x*0.55)) %>% 
  left_join(da %>% 
              mutate(across(c("value"), ~ .x*0.45)) %>% 
              dplyr::select(tech, stage, value, scene), by = c("tech","stage","scene")) %>% 
  mutate(value.y = ifelse(is.na(value.y), 0, value.y),
         value = value.x + value.y) %>% 
  dplyr::select(-start, -end) %>% 
  filter(scene == "Optimistic") %>% 
  
  group_by(tech) %>%
  mutate(
    start = cumsum(lag(value, default = 0)),  # cumulative start
    end = start + value
  ) %>% 
  
  rbind(
    d %>% 
      mutate(across(c("value"), ~ .x*0.55)) %>% 
      left_join(da %>% 
                  mutate(across(c("value"), ~ .x*0.45)) %>% 
                  dplyr::select(tech, stage, value, scene), by = c("tech","stage","scene")) %>% 
      mutate(value.y = ifelse(is.na(value.y), 0, value.y),
             value = value.x + value.y) %>% 
      dplyr::select(-start, -end) %>% 
      filter(id %in% c(1,2,3)) %>% 
      filter(scene == "Optimistic") %>% 
      rbind(
        d %>% 
          mutate(across(c("value"), ~ .x*0.55)) %>% 
          left_join(da %>% 
                      mutate(across(c("value"), ~ .x*0.45)) %>% 
                      dplyr::select(tech, stage, value, scene), by = c("tech","stage","scene")) %>% 
          mutate(value.y = ifelse(is.na(value.y), 0, value.y),
                 value = value.x + value.y) %>% 
          dplyr::select(-start, -end) %>% 
          filter(scene == "Pessimistic") 
      ) %>% 

      group_by(tech) %>%
      mutate(
        start = cumsum(lag(value, default = 0)),  # cumulative start
        end = start + value
      ) %>% 
      filter(scene == "Pessimistic")
  ) 


# Identify the time-variant stages
time_variant_stages <- unique(daa$stage[daa$class == "Time variant"])

fill_vals <- c(
  setNames(RColorBrewer::brewer.pal(length(time_variant_stages), "Set2"),
           time_variant_stages),
  "Other" = "azure3" 
)

pattern_vals <- c(
  setNames(rep("stripe", length(time_variant_stages)), time_variant_stages),
  "Other" = "none"
)

daa %>%
  mutate(sta = as.character(stage),
         ST = ifelse(class == "Time variant", sta, "Other"),
         ST = factor(ST, levels = c("Peer effects","Home built after 2020","Fast charger","Range", "Other"))) %>% 
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
    pattern_fill = "black",
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
  
  facet_wrap(~ tech, scales = "free_x", nrow = 1) +
  
  scale_x_discrete(limits = c("Current", "Future", "Low", "High")) +
  
  scale_y_continuous(labels = scales::comma) +
  
  scale_fill_manual(values = fill_vals, name = "",
                    breaks = time_variant_stages) +
  scale_pattern_manual(values = pattern_vals, name = "",
                       breaks = time_variant_stages) +
  
  labs(
    title = "",
    x = "",
    y = "Cumulative Adoption (%)"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "right",
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
  

##############################################################################
### opt vs. pessi plots
com_op <- df_long %>% 
  rbind(df_long_p) %>% 
  mutate(scene = factor(scene, levels = rev(c("Optimistic","Pessimistic")))) %>% 
  filter(tech != "PV") %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "EV" = "Electric vehicles",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"))) %>% 
  
  ggplot(aes(y = scene, xmin = start, xmax = end, fill = stage)) +
  geom_rect(aes(xmin = start, xmax = end, ymin = as.numeric(factor(scene)) - 0.4,
                ymax = as.numeric(factor(scene)) + 0.4),
            color = "black") +
  facet_wrap(~tech, switch = "y", nrow = 5) +
  labs(title = "",
       x = "Cumulative Adoption",
       y = "",
       fill = "") +
  theme_minimal() +
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
  ) +
  guides(fill = guide_legend(nrow = 1))


ggsave("./fig/com_op.png",
       com_op,
       width = 12, height = 6)



### DAC 
d1_dac <- data %>%
  group_by(dac) %>% 
  summarise(across(
    .cols = all_of(names(.)[str_detect(names(.), "PV|EV|HP|IC|PS")& !str_detect(names(.), "peer")]),
    .fns = ~ weighted.mean(.x, wt_ca, na.rm = TRUE)
  )) %>%
  gather(key, adoption, -dac) %>%
  mutate(
    tech = str_extract(key, "(PV|PS|EV|HP|IC)"),
    scenario = case_when(
      !str_detect(key, "^future") ~ "current",
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
    high.subsidy = Hsubsidy - Lsubsidy
  ) %>%
  dplyr::select(-Future,-Lsubsidy,-Hsubsidy) %>% 
  mutate(dac = ifelse(dac == "0", "Non_DAC", "DAC")) %>% 
  filter(tech != "PV")


### MRP results
d1_dac <- mrp_mean_dac %>% 
  gather(key, adoption, -sample) %>% 
  mutate(
    tech = str_extract(key, "(PV|PS|EV|HP|IC)"),
    scenario = case_when(
      !str_detect(key, "^future") ~ "current",
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
    high.subsidy = Hsubsidy - Lsubsidy
  ) %>% 
  mutate(dac = ifelse(sample == "0", "Non_DAC", "DAC")) %>% 
  dplyr::select(-Future,-Lsubsidy,-Hsubsidy,-sample) %>% 
  filter(tech != "PV")  


### optimistic scenarios
select_scene <- list(c("home_age","peer_PV"),
                   c("home_age","peer_EV","charging_5mile_f","rangeanxiety"),
                   c("peer_HP"),
                   c("peer_IC"),
                   c("home_age","peer_PV"))

rates <- list(c(0,0.2,-0.14,-0.23),
              c(0,0.2,-0.04,-0.26,0.32,-2), # home, peer, charging, range
              c(0,-0.25),
              c(0,-0.17),
              c(0,0.2,-0.14,-0.23))


result_dac <- data.frame()
for(k in 2:5){
  
  ### scenario variable impact by DAC - random slope model 
  b_ev <- mreg_dac(data %>% 
                     dplyr::select(VAR[[k-1]], climatezone, dac, matches("PV|PS|EV|HP|IC", ignore.case = FALSE),wt_ca), 
        
               i = k,
               scenario = select_scene[[k]],
               future = 0) # variable effects and SE 


  df <- b_ev[[1]] %>% t() %>% 
    as.data.frame() %>% 
    dplyr::slice(-1) %>% 
    mutate(var = row.names(.)) %>% 
    cbind(tibble(rate = rates[[k]])) %>% 
    mutate(non_DAC = `0`*rate,
           DAC = `1`*rate) 
  
  rd <- df %>%
    summarise(
      charging_5mile  = sum(non_DAC[str_detect(var, "charging_5mile_f1")], na.rm = TRUE),
      peer            = sum(non_DAC[str_detect(var, "peer")], na.rm = TRUE),
      home_age            = sum(non_DAC[str_detect(var, "home_age")], na.rm = TRUE),
      rangeanxiety    = sum(non_DAC[str_detect(var, "rangeanxiety")], na.rm = TRUE)
    ) %>% 
    mutate(tech = ipt[k],
           dac = "Non_DAC") %>% 
    rbind(
      df %>%
        summarise(
          charging_5mile  = sum(DAC[str_detect(var, "charging_5mile_f1")], na.rm = TRUE),
          peer            = sum(DAC[str_detect(var, "peer")], na.rm = TRUE),
          home_age            = sum(DAC[str_detect(var, "home_age")], na.rm = TRUE),
          rangeanxiety    = sum(DAC[str_detect(var, "rangeanxiety")], na.rm = TRUE)
        ) %>% 
        mutate(tech = ipt[k],
               dac = "DAC") 
    )
  
  result_dac <- rbind(rd, result_dac)
}


df_dac <- d1_dac %>% 
  left_join(result_dac, by = c("tech", "dac")) 


df_long <- df_dac %>%
  pivot_longer(cols = -c("dac","tech"), names_to = "stage", values_to = "value") %>%
  mutate(stage = recode(stage,
                        "current" = "Current",
                        "future" = "Future",
                        "low.subsidy" = "Subsidy",
                        "high.subsidy" = "More subsidy",
                        "charging_5mile" = "Fast charger",
                        "peer" = "Peer effects",
                        "home_age" = "Home built after 2020",
                        "rangeanxiety" = "Range")) %>% 
  mutate(stage = factor(stage, levels = stage_order)) %>% 
  
  group_by(dac, tech) %>%
  mutate(
    value = replace_na(value, 0),
    start = cumsum(lag(value, default = 0)),  # cumulative start
    end = start + value
  ) %>% 
  mutate(tech = factor(tech, levels = c("PS","PV","EV","HP","IC"))) %>% 
  mutate(scene = "Optimistic")


dff <- df_long %>%
  mutate(across(where(is.numeric), ~ .x * 100)) %>% 
  group_by(dac, tech) %>%
  mutate(id = row_number(),
         label_val = if_else(value != 0, as.character(round(value, 0)), "")) %>%
  ungroup() %>% # Ungrouping is good practice after mutations
  mutate(class = ifelse(stage %in% c("Current","Future"), "No subsidy",
                        ifelse(stage %in% c("Subsidy","More subsidy","Fast charger"), "Policy intervention",
                               "Time variant")),
         class = factor(class, levels = c("No subsidy","Policy intervention","Time variant")))

segment_data <- dff %>%
  group_by(dac, tech) %>%
  filter(id < max(id)) %>% 
  filter(tech != "PV") %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "EV" = "Electric vehicles",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"))) 

hlines <- tibble::tribble(
  ~tech, ~y_intercept, ~line_label,
  "PV + Storage", 20, "Base: 0.20",
  "Electric vehicles", 42, "Base: 0.42",
  "Heat pumps", 46,   "Base: 46" # This line will not appear as there is no "HP" data
) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps")))


# error_data_dac <- data.frame(dac = "Non_DAC",
#                              tech = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"), 
#                                                x = c(8,9,7,8), 
#                                                ymin = c(13.5,25,33.7,26.5), 
#                                                ymax = c(16.5,44.5,38.7,25),
#                                                xmin = c(8.5,8.5,7.5,8.5)) %>% 
#   mutate(text = round(ymax - ymin, 1)) %>% 
#   mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves")))


f3a <- dff %>% 
  filter(tech != "PV") %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "EV" = "Electric vehicles",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"))) %>% 
  ggplot(aes(x = stage)) +
  
  # Draw the cascade bars using geom_rect
  geom_rect(aes(xmin = id - 0.45, xmax = id + 0.45, ymin = start, ymax = end, fill = class)) +
  
  geom_hline(data = hlines, aes(yintercept = y_intercept), 
             color = "darkred", linetype = "dashed", linewidth = 1) +
  
  # geom_hline(data = error_data_dac %>% 
  #              dplyr::select(-dac), aes(yintercept = ymin), 
  #            color = "gray70", linetype = "dashed", linewidth = 1) +
  
  # Add labels inside the bars
  geom_text(aes(x = id, y = end + 2, label = label_val),
            color = "black",
            fontface = "bold",
            size = 4) +

  geom_segment(data = segment_data,
               aes(x = id + 0.45, y = end, xend = id + 1 - 0.45, yend = end),
               color = "gray40",
               linewidth = 0.75) +
  
  # geom_errorbar(data = error_data_dac,
  #               aes(x = x, ymin = ymin, ymax = ymax),
  #               width = 0.5,
  #               color = "red",
  #               linewidth = 1) +  # dynamically inserted arrow
  # 
  # 
  # geom_text(data = error_data_dac,
  #           aes(x = xmin, y = ymin, label = paste0(text)),
  #           size = 4,
  #           hjust = 0,
  #           fontface = "bold") +
  
  # Facet by 'tech' to create separate charts for PV and PS
  facet_grid(dac ~ tech, scales = "free_x", switch = "y") +
  
  coord_flip() +
  
  # Format labels and titles
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "",
    x = "",
    y = "Cumulative Adoption (%)",
    fill = ""
  ) +
  
  # Customize the theme for better readability
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom",
    
    strip.placement = "outside", # Keep labels on the outside
    strip.background =element_rect(fill="gray22",color="gray22"),
    strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
    strip.text.y.left = element_text(angle = 0), # Ensure domain labels are horizontal
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# f3 <- ggarrange(f3a, f3b, nrow = 2,
#                 heights = c(1,1.5),
#                 labels = c("A", "B"),  # Adds labels to plots
#                 label.x = 0,        # Adjust horizontal position of labels
#                 label.y = 1,        # Adjust vertical position of labels
#                 font.label = list(size = 14, face = "bold")
# )


ggsave("./fig/f3.png",
       f3a,
       width = 12, height = 12)

