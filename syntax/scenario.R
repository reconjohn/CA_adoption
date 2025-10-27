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


# ### scenario 
# effect <- list()
# for(i in seq_along(ipt)){
#   future <- fut[[i]]
#   scene <- c("Optimistic","Pessimistic")
#   
#   for(j in 1:2){
#     f_d <- final_ef_peer(data, i, future[j]) %>% 
#       mutate(class = paste0(ipt[i],"_",scene[j]))
#     
#     effect <- append(effect, list(f_d))
#     
#   }
# }
# 
# 
# df_common <- lapply(effect, function(df) df[, c("GEOID","Effect","MRP","Final","class"), drop = FALSE])
# combined_df <- bind_rows(df_common)
# write_csv(combined_df, "./data/result.csv")
# 
# 
# save(effect, mrp, file = "./data/results.Rdata") # for official sharing


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

final <- data.frame()
for(i in 1:5){
  tp <- dplyr::bind_rows(effect[(2*i-1):(2*i)]) %>% 
                mutate(
                  peer_effect = rowSums(dplyr::select(., all_of(names(.)[str_detect(names(.), "^peer")])), na.rm = TRUE),
                  home_age = rowSums(dplyr::select(., all_of(names(.)[str_detect(names(.), "^home_age")])), na.rm = TRUE)
                ) %>%
                dplyr::select(-ends_with(c("New","Older","peer","none"))) %>% 
    pivot_longer(cols = c(Effect, MRP, Final), names_to = "key", values_to = "value") %>% 
    mutate(key = factor(key, levels = c("MRP","Effect","Final"))) %>% 
    filter(key == "Final") %>% 
    dplyr::select(GEOID, class, value)
  
  final <- rbind(tp, final)
}

CA_c <- CA_t %>% 
  mutate(county = gsub("\\d{6}$", "", GEOID)) %>% 
  group_by(county) %>% 
  summarise(geometry = st_union(geometry))


f3b <- CA_t %>% 
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
  ) %>% 

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
  
  facet_grid(DAC~tech, switch = "y") +
  
  theme_minimal() +
  # scale_fill_distiller(palette = "RdBu", direction = -1) +
  # scale_fill_viridis_c(option = "magma") +
  
  labs(title = "", fill = "Adoption") +
  
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 16),
    
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = "right",
    
    strip.placement = "outside", # Keep labels on the outside
    # strip.background =element_rect(fill="gray22",color="gray22"),
    strip.text = element_text(color = 'black',family="Franklin Gothic Book",size=12, face = "bold"),
    strip.text.y.left = element_text(angle = 0), # Ensure domain labels are horizontal
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


### moran's I 
moran_by_class <- CA_t[!st_is_empty(CA_t), ] %>% 
  left_join(final, by = "GEOID") %>%
  group_split(class) %>%
  map_df(function(df) {
    # Remove rows with NA in value
    df_clean <- df %>% filter(!is.na(value))
    
    # Skip if fewer than 2 observations
    if (nrow(df_clean) < 2) {
      return(tibble(
        class = unique(df$class),
        SI = NA_real_,
        p_value = NA_real_,
        note = "Too few observations"
      ))
    }
    

    # Create spatial weights for this subset
    nb <- poly2nb(df_clean, queen = TRUE)
    lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
    
    # Run Moran's I permutation test
    test <- moran.mc(df_clean$value, listw = lw, nsim = 999, zero.policy = TRUE)
    
    # Return results
    tibble(
      class = unique(df$class),
      SI = test$statistic,
      p_value = test$p.value,
      note = NA_character_
    )
  })


### only urban
la_metro_geoids <- c(
  "06037",  # Los Angeles
  "06059"  # Orange
)

bay_area_geoids <- c(
  "06075",  # San Francisco
  "06081",  # San Mateo
  "06085"  # Santa Clara
)



moran_by_la <- CA_t %>% 
  mutate(county = gsub("\\d{6}$", "", GEOID)) %>% 
  filter(county %in% la_metro_geoids) %>% 
  left_join(final, by = "GEOID") %>%
  group_split(class) %>%
  map_df(function(df) {
    # Remove rows with NA in value
    df_clean <- df %>% filter(!is.na(value))
    
    # Skip if fewer than 2 observations
    if (nrow(df_clean) < 2) {
      return(tibble(
        class = unique(df$class),
        SI = NA_real_,
        p_value = NA_real_,
        note = "Too few observations"
      ))
    }
    
    # Create spatial weights for this subset
    nb <- poly2nb(df_clean, queen = TRUE)
    lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
    
    # Run Moran's I permutation test
    test <- moran.mc(df_clean$value, listw = lw, nsim = 999, zero.policy = TRUE)
    
    # Return results
    tibble(
      class = unique(df$class),
      SI = test$statistic,
      p_value = test$p.value,
      note = NA_character_
    )
  })


moran_by_bay <- CA_t %>% 
  mutate(county = gsub("\\d{6}$", "", GEOID)) %>% 
  filter(county %in% bay_area_geoids) %>% 
  left_join(final, by = "GEOID") %>%
  group_split(class) %>%
  map_df(function(df) {
    # Remove rows with NA in value
    df_clean <- df %>% filter(!is.na(value))
    
    # Skip if fewer than 2 observations
    if (nrow(df_clean) < 2) {
      return(tibble(
        class = unique(df$class),
        SI = NA_real_,
        p_value = NA_real_,
        note = "Too few observations"
      ))
    }
    
    # Create spatial weights for this subset
    nb <- poly2nb(df_clean, queen = TRUE)
    lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
    
    # Run Moran's I permutation test
    test <- moran.mc(df_clean$value, listw = lw, nsim = 999, zero.policy = TRUE)
    
    # Return results
    tibble(
      class = unique(df$class),
      SI = test$statistic,
      p_value = test$p.value,
      note = NA_character_
    )
  })


f6b <- moran_by_class %>% 
  mutate(area = "State") %>% 
  rbind(
    moran_by_la %>% 
      mutate(area = "LA"),
    moran_by_bay %>% 
      mutate(area = "Bay")
  ) %>% 
  filter(!is.na(SI)) %>%
  separate(class, into = c("tech", "scenario"), sep = "_", convert = TRUE) %>%
  mutate(tech = fct_reorder(tech, SI)) %>%
  ggplot(aes(x = scenario, y = SI, fill = tech)) +
  geom_col(width = 0.6, position = position_dodge(width = 0.7)) +
  geom_text(aes(label = round(SI, 2)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) +
  facet_wrap(~area, nrow = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Spatial inequality",
    x = "",
    y = "Moran's I",
    fill = "Technology"
  ) +
  theme_minimal(base_size = 13) +
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


f6a <- CA_t %>% 
  mutate(county = gsub("\\d{6}$", "", GEOID)) %>% 
  filter(county %in% c(la_metro_geoids, bay_area_geoids)) %>% 
  ggplot() +
  geom_sf(fill = "white", color = "gray0") + # US border
  geom_sf(aes(fill = estimate/1000), color = NA, size = 0.3) +
  geom_sf(data = CA_c, fill = NA, color = "gray40", linewidth = 0.1) +
  
  theme_minimal() +
  # scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis_c(option = "magma") +
  
  labs(title = "Urban Areas in CA", fill = "Population (k)") +
  
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
    
    strip.background =element_rect(fill="gray22",color="gray22"),
    strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=14, face = "bold"),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())


f6 <- ggarrange(f6a, f6b, nrow = 1)
ggsave("./fig/f6.png",
       f6,
       width = 12, height = 8)


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

dac_tract <- read_csv(file = "../DAC/data/DAC_CA_censustract.csv") %>%
  dplyr::select(GEOID, sample)


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


f7a <- CA_t %>% 
  dplyr::select(GEOID) %>% 
  left_join(burden, by = "GEOID") %>% 
  ggplot() +
  geom_sf(fill = "white", color = "gray0") + # US border
  geom_sf(aes(fill = demand), color = NA, size = 0.3) +
  scale_fill_viridis_c(option = "magma", name = "Grid demand\nincrease (GWh)") +
  new_scale_fill() +
  
  geom_sf(data = CA_c, fill = NA, color = "white", linewidth = 0.05) +
  geom_sf(data = dac_sf %>% 
            filter(sample == 1) %>% 
            mutate(sample = "DAC"), aes(fill = sample), color = NA, alpha = 0.3) +
  scale_fill_manual(values = c("DAC" = "gray80"),
                    name = "") +
  
  theme_minimal() +
  # scale_fill_distiller(palette = "RdBu", direction = -1) +

  labs(title = "Impact on residential demand by tract", fill = "Grid demand\nincrease (GWh)") +
  theme(legend.position = "right",
        # legend.text=element_text(size=6),
        # legend.key.size = unit(0.3, 'cm'),
        strip.text = element_text(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = 0))

f7b <- burden %>% 
  left_join(dac_tract, by = "GEOID") %>% 
  group_by(sample) %>%
  summarise(mean_increase = mean(demand, na.rm = T), .groups = "drop") %>%
  mutate(DAC = ifelse(sample == 0, "Non-DAC", "DAC"),
         DAC = factor(DAC, levels = c("DAC", "Non-DAC"))) %>% 
  ggplot(aes(x = DAC, y = mean_increase, fill = DAC)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(mean_increase, 3)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("DAC" = "#E66101", "Non-DAC" = "#5E9ACF")) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Grid demand increase",
    x = "",
    y = "Mean increase (GWh/tract)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

f7c <- CA_t %>% 
  dplyr::select(GEOID) %>% 
  left_join(burden, by = "GEOID") %>% 
  ggplot() +
  geom_sf(fill = "white", color = "gray0") + # US border
  geom_sf(aes(fill = increase/7*100), color = NA, size = 0.3) +
  scale_fill_viridis_c(option = "magma", name = "Grid demand\nincrease (%)") +
  new_scale_fill() +
  
  geom_sf(data = CA_c, fill = NA, color = "white", linewidth = 0.05) +
  geom_sf(data = dac_sf %>% 
            filter(sample == 1) %>% 
            mutate(sample = "DAC"), aes(fill = sample), color = NA, alpha = 0.3) +
  scale_fill_manual(values = c("DAC" = "gray80"),
                    name = "") +
  
  theme_minimal() +
  
  labs(title = "Impact on demand by household", fill = "Grid demand\nincrease (%)") +
  theme(legend.position = "right",
        # legend.text=element_text(size=6),
        # legend.key.size = unit(0.3, 'cm'),
        strip.text = element_text(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = 0))



f7d <- burden_mean %>% 
  left_join(dac_tract, by = "GEOID") %>% 
  group_by(sample) %>%
  summarise(mean_increase = mean(increase, na.rm = T), .groups = "drop") %>%
  mutate(DAC = ifelse(sample == 0, "Non-DAC", "DAC"),
         DAC = factor(DAC, levels = c("DAC", "Non-DAC"))) %>% 
  ggplot(aes(x = DAC, y = mean_increase/7*100, fill = DAC)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(mean_increase/7*100, 1)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("DAC" = "#E66101", "Non-DAC" = "#5E9ACF")) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "",
    x = "",
    y = "Mean increase (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )


### moran's I
df_clean <- CA_t[!st_is_empty(CA_t), ] %>% 
  left_join(burden, by = "GEOID") %>%
  filter(!is.na(increase))
    
    # Create spatial weights for this subset
    nb <- poly2nb(df_clean, queen = TRUE)
    lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
    
    # Run Moran's I permutation test
    test_a <- moran.mc(df_clean$increase, listw = lw, nsim = 999, zero.policy = TRUE)
    test_b <- moran.mc(df_clean$demand, listw = lw, nsim = 999, zero.policy = TRUE)
    

f7e <- moran_by_class %>% 
  separate(class, into = c("tech", "scenario"), sep = "_", convert = TRUE) %>% 
  filter(scenario == "Optimistic") %>% 
  dplyr::select(-note, -scenario) %>% 
  na.omit() %>% 
  rbind(
    tibble(
      tech = c("Demand/HH", "Demand"),
      SI = c(test_a$statistic, test_b$statistic),
      p_value = c(test_a$p.value, test_b$p.value)
    )
  ) %>% 
  mutate(tech = factor(tech, levels = c("Demand", "Demand/HH", "PS","PV","IC","HP","EV"))) %>% 
  ggplot(aes(x = tech, y = SI)) +
  geom_col(width = 0.6, position = position_dodge(width = 0.7)) +
  geom_text(aes(label = round(SI, 2)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Spatial inequality",
    x = "",
    y = "Moran's I",
    fill = "Technology"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
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


f7 <- ggarrange(f7a, ggarrange(f7b,f7d, nrow = 1), f7c, f7e, nrow = 2, ncol = 2, widths = c(1,0.6))

ggsave("./fig/f7.png",
       f7,
       width = 12, height = 10)
  
  

f6b <- moran_by_class %>% 
  mutate(area = "State") %>% 
  rbind(
    moran_by_la %>% 
      mutate(area = "LA"),
    moran_by_bay %>% 
      mutate(area = "Bay")
  ) %>% 
  filter(!is.na(SI)) %>%
  separate(class, into = c("tech", "scenario"), sep = "_", convert = TRUE) %>%
  mutate(tech = fct_reorder(tech, SI)) %>%
  ggplot(aes(x = scenario, y = SI, fill = tech)) +
  geom_col(width = 0.6, position = position_dodge(width = 0.7)) +
  geom_text(aes(label = round(SI, 2)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) +
  facet_wrap(~area, nrow = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Spatial inequality",
    x = "",
    y = "Moran's I",
    fill = "Technology"
  ) +
  theme_minimal(base_size = 13) +
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
    low.subsidy = Lsubsidy - Future,
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
      charging_5mile  = sum(effect[str_detect(var, "charging_5mile_f1")], na.rm = TRUE),
      peer            = sum(effect[str_detect(var, "peer")], na.rm = TRUE),
      home_age            = sum(effect[str_detect(var, "home_age")], na.rm = TRUE),
      rangeanxiety    = sum(effect[str_detect(var, "rangeanxiety")], na.rm = TRUE)
    ) %>% 
    mutate(tech = ipt[k])
  
  result <- rbind(rd, result)
}
  

stage_order <- c("Current", "Future", "Subsidy", "More subsidy", "Fast charger",
                 "Peer effects", "Home built after 2020", "Range")

df_long <- d1 %>% 
  left_join(result, by = "tech")  %>%
  pivot_longer(cols = -tech, names_to = "stage", values_to = "value") %>%
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
                        ifelse(stage %in% c("Subsidy","More subsidy","Fast charger"), "Policy intervention",
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



error_data_opt <- data.frame(tech = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"), 
                              x = c(7,9,6,7), 
                              ymin = c(7,12,12,13), 
                              ymax = c(13,35,39,28),
                              text = c(7,23,27,15),
                              xmin = c(4.5,6,4,4.7)) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves")))


opt_result <- dff %>% 
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
  
  geom_hline(data = hlines %>% 
               filter(tech != "PV"), aes(yintercept = y_intercept), 
             color = "darkred", linetype = "dashed", linewidth = 1) +
  
  # Add labels inside the bars
  geom_text(aes(x = id, y = end + 2, label = label_val),
            color = "black",
            fontface = "bold",
            size = 4) +
  
  geom_segment(data = segment_data %>% 
                 filter(tech != "PV"),
               aes(x = id + 0.45, y = end, xend = id + 1 - 0.45, yend = end),
               color = "gray40",
               linewidth = 0.75) +
  
  geom_errorbar(data = error_data_opt,
                aes(x = x, ymin = ymin, ymax = ymax),
                width = 0.2,
                color = "red",
                linewidth = 1) +  # dynamically inserted arrow

  geom_text(data = error_data_opt,
            aes(x = xmin, y = ymin, label = paste0("Impact: ", text)),
            size = 4,
            hjust = 0,
            fontface = "bold") +

  # Facet by 'tech' to create separate charts for PV and PS
  facet_wrap(~ tech, scales = "free_x",  nrow = 1) +
  
  # Format labels and titles
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Optimistic scenario",
    x = "",
    y = "Cumulative Adoption (%)",
    fill = ""
  ) +
  
  # Customize the theme for better readability
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


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
      charging_5mile  = sum(effect[str_detect(var, "charging_5mile_f1")], na.rm = TRUE),
      peer            = sum(effect[str_detect(var, "peer")], na.rm = TRUE),
      home_age            = sum(effect[str_detect(var, "home_age")], na.rm = TRUE),
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
                        "low.subsidy" = "Subsidy",
                        "high.subsidy" = "More subsidy",
                        "charging_5mile" = "Fast charger",
                        "peer" = "Peer effects",
                        "home_age" = "Home built after 2020",
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
                        ifelse(stage %in% c("Subsidy","Fast charger"), "Policy intervention",
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

error_data_pess <- data.frame(tech = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"), 
                              x = c(6,8,5,6), 
                              ymin = c(7,12,12,13), 
                              ymax = c(9.7,24,20,16),
                              text = c(3,11,8,3),
                              xmin = c(4.2,5.5,3.5,4.2)) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves")))

pess_result <- dff %>% 
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
  
  geom_hline(data = hlines %>% 
               filter(tech != "PV"), aes(yintercept = y_intercept), 
             color = "darkred", linetype = "dashed", linewidth = 1) +
  
  # Add labels inside the bars
  geom_text(aes(x = id, y = end + 2, label = label_val),
            color = "black",
            fontface = "bold",
            size = 4) +
  
  geom_segment(data = segment_data %>% 
                 filter(tech != "PV"),
               aes(x = id + 0.45, y = end, xend = id + 1 - 0.45, yend = end),
               color = "gray40",
               linewidth = 0.75) +
  
  
  geom_errorbar(data = error_data_pess,
                aes(x = x, ymin = ymin, ymax = ymax),
                width = 0.2,
                color = "red",
                linewidth = 1) +  # dynamically inserted arrow
  
  geom_text(data = error_data_pess,
            aes(x = xmin, y = ymin, label = paste0("Impact: ", text)),
            size = 4,
            hjust = 0,
            fontface = "bold") +
  
  # Facet by 'tech' to create separate charts for PV and PS
  facet_wrap(~ tech, scales = "free_x",  nrow = 1) +
  
  # Format labels and titles
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Pessimistic scenario",
    x = "",
    y = "Cumulative Adoption (%)",
    fill = ""
  ) +
  
  # Customize the theme for better readability
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


f2 <- ggarrange(opt_result, pess_result, nrow = 2,
          labels = c("A", "B"),  # Adds labels to plots
          label.x = 0,        # Adjust horizontal position of labels
          label.y = 1,        # Adjust vertical position of labels
          font.label = list(size = 14, face = "bold")
          )
ggsave("./fig/f2.png",
       f2,
       width = 12, height = 12)




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
    low.subsidy = Lsubsidy - Future,
    high.subsidy = Hsubsidy - Lsubsidy
  ) %>% 
  dplyr::select(-Future,-Lsubsidy,-Hsubsidy) %>% 
  mutate(dac = ifelse(dac == "0", "Non_DAC", "DAC"))


### optimistic scenarios
select_scene <- list(c("home_age","peer_PV"),
                   c("home_age","peer_EV","charging_5mile_f","rangeanxiety"),
                   c("peer_HP"),
                   c("home_age","peer_IC"),
                   c("home_age","peer_PV"))

rates <- list(c(0,0.2,-0.14,-0.23),
              c(0,0.2,-0.04,-0.26,0.32,-2),
              c(0,-0.25),
              c(0,0.2,0,-0.17),
              c(0,0.2,-0.14,-0.23))


result_dac <- data.frame()
for(k in 1:5){
  
  b_ev <- mreg_dac(read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
                 data_process(ev = c("Fully electric")) %>% 
                 data_clean(1), 
                
               remove = c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv",
                          "education","employment"),
               scenario = select_scene[[k]],
               i = k,
               future = fut[[k]][1])

  
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


error_data_dac <- data.frame(dac = "Non_DAC",
                             tech = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"), 
                                               x = c(8,9,7,8), 
                                               ymin = c(11.3,24.3,37.3,25.9), 
                                               ymax = c(14.3,45,40.7,29.5),
                                               xmin = c(7.5,8.5,7.5,7.5)) %>% 
  mutate(text = round(ymax - ymin, 1)) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves")))


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
  
  geom_hline(data = error_data_dac %>% 
               dplyr::select(-dac), aes(yintercept = ymin), 
             color = "gray70", linetype = "dashed", linewidth = 1) +
  
  # Add labels inside the bars
  geom_text(aes(x = id, y = end + 2, label = label_val),
            color = "black",
            fontface = "bold",
            size = 4) +

  geom_segment(data = segment_data,
               aes(x = id + 0.45, y = end, xend = id + 1 - 0.45, yend = end),
               color = "gray40",
               linewidth = 0.75) +
  
  geom_errorbar(data = error_data_dac,
                aes(x = x, ymin = ymin, ymax = ymax),
                width = 0.5,
                color = "red",
                linewidth = 1) +  # dynamically inserted arrow


  geom_text(data = error_data_dac,
            aes(x = xmin, y = ymin, label = paste0(text)),
            size = 4,
            hjust = 0,
            fontface = "bold") +
  
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

f3 <- ggarrange(f3a, f3b, nrow = 2,
                heights = c(1,1.5),
                labels = c("A", "B"),  # Adds labels to plots
                label.x = 0,        # Adjust horizontal position of labels
                label.y = 1,        # Adjust vertical position of labels
                font.label = list(size = 14, face = "bold")
)


ggsave("./fig/f3.png",
       f3,
       width = 12, height = 12)

