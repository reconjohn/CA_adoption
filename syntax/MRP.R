
### survey data
data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1)

### mrp data
mrp <- read_csv("./data/raw/mrp_scenariovars_tract.csv") %>% 
  mutate(GEOID = str_sub(geoid_tract2020, 10)) %>%
  dplyr::select(-geoid_tract2020,-future_EV10) %>% 
  mutate(future_PS_0 = ifelse(future_PS_0 > future_PS_72, future_PS_72, future_PS_0)) 


# CA_t <- get_acs("tract", state="CA", year = 2023, geometry = TRUE,
#                                 variables= "B25026_001") # population


### comparison of adoption vs. MRP
mrp_mean <- mrp %>%
  left_join(CA_t %>% st_drop_geometry(), by = "GEOID") %>%
  summarise(across(where(is.numeric), ~ weighted.mean(.x, w = estimate, na.rm = TRUE))) %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "name") 



mrp_mean %>%
  left_join(
    data %>%
      summarise(across(
        .cols = all_of(names(.)[str_detect(names(.), "PV|EV|HP|IC|PS")& !str_detect(names(.), "peer")]),
        .fns = ~ weighted.mean(.x, wt_ca, na.rm = TRUE)
      )) %>%
      t() %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "name"),
    by = "name"
  ) %>%
  filter(!is.na(V1.y))  %>% # Remove rows with NA in V1.y
  filter(name != "future_PV") %>%
  
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
