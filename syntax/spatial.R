
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


