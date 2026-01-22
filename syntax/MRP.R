
### survey data
data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1)

### mrp data
# mrp <- read_csv("./data/raw/mrp_scenariovars_tract_nov7.csv") %>% 
#   mutate(GEOID = str_sub(geoid_tract2020, 10)) %>%
#   dplyr::select(-geoid_tract2020,-future_EV10) 


# CA_t <- get_acs("tract", state="CA", year = 2023, geometry = TRUE,
#                                 variables= "B25026_001") # population


### comparison of adoption vs. MRP
mrp_mean <- mrp %>%
  left_join(CA_t %>% st_drop_geometry(), by = "GEOID") %>%
  summarise(across(where(is.numeric), ~ weighted.mean(.x, w = estimate, na.rm = TRUE))) %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "name") 



tt <- mrp_mean %>%
  left_join(
    data %>%
      summarise(across(
        .cols = all_of(names(.)[str_detect(names(.), "EV|PS|HP|IC")& !str_detect(names(.), "peer")]),
        .fns = ~ weighted.mean(.x, wt_ca, na.rm = TRUE)
      )) %>%
      t() %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "name"),
    by = "name"
  ) %>%
  filter(!is.na(V1.y))  %>% # Remove rows with NA in V1.y
  filter(name != "future_PV") 

tt %>%
  ggplot(aes(x = V1.x, y = V1.y, label = name)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed") +
  geom_abline() +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  labs(
    x = "MRP",
    y = "Survey",
    title = "Scatter Plot"
  ) +
  theme_minimal()


### MRP mapping
CA_t %>% 
  dplyr::select(GEOID, estimate) %>%
  left_join(mrp %>%
              dplyr::select(-future_PV), by = "GEOID") %>%
  gather(key, value, future_EV_0:IC) %>%
  # mutate(key = factor(key, levels = names(tt)[3:14])) %>%
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


### error check
ttt <- mrp %>% 
  mutate(larger = ifelse(future_PS_0 > future_PS_low, 1, 0)) 

ttt$larger %>% sum(na.rm = T)
