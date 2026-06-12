
### homeownership portion by DAC
ca_homeown <- get_acs(
  geography = "tract",
  variables = c(
    total_occ = "B25003_001",
    owner_occ = "B25003_002"
  ),
  state = "CA",
  year = 2022,       # or 2023 if available
  geometry = TRUE,
  cache_table = TRUE
) %>%
  select(GEOID, variable, estimate, geometry) %>%
  st_drop_geometry() %>%
  tidyr::pivot_wider(
    names_from = variable,
    values_from = estimate
  ) %>%
  mutate(
    homeownership_rate = owner_occ / total_occ
  )

dac <- read_csv(file = "../DAC/data/DAC_CA_censustract.csv") %>%
  dplyr::select(GEOID, sample)

# homeownership portion by DAC
ca_homeown %>% 
  left_join(dac, by = "GEOID") %>% 
  group_by(sample) %>% 
  summarise(rate = weighted.mean(homeownership_rate, w = total_occ, na.rm = T))


### EV real adoption comparison 
evs <- read_csv("./data/raw/EVs_2024.csv") %>% 
  group_by(County,`Fuel Technology`) %>% 
  summarise(count = sum(`Vehicle Population`)) %>% 
  dplyr::rename(fuel = `Fuel Technology`) %>% 
  pivot_wider(names_from = fuel, values_from = count) %>% 
  mutate(across(everything(), ~ replace_na(.x, 0))) %>% 
  mutate(total = BEV + FCEV + ICE + PHEV,
         EV_2024 = BEV/ total)

ca_counties <- counties(state = "CA", cb = TRUE) %>%
  st_drop_geometry() %>%
  dplyr::select(NAME, GEOID) %>%
  mutate(NAME = toupper(NAME))


mrp %>% 
  left_join(CA_t %>% 
              dplyr::select(GEOID, estimate), by = "GEOID") %>% 
  dplyr::select(EV, estimate) %>% 
  mutate(mean_EV = weighted.mean(EV, w = estimate))

mrp_EV <- mrp %>% 
  left_join(CA_t %>% 
              dplyr::select(GEOID, estimate), by = "GEOID") %>% 
  mutate(GEOID = substr(GEOID, 1, 5)) %>%
  group_by(GEOID) %>% 
  summarise(mrp_EV = weighted.mean(EV, w = estimate)) %>% 
  dplyr::select(GEOID, mrp_EV)

# ev <- read_csv("./data/raw/census_tract_ev_penetration.csv") %>%
#   dplyr::rename(GEOID = `Census Tract`) %>%
#   left_join(mrp %>%
#               dplyr::select(GEOID, EV, future_EV_0, future_EV_low, future_EV_high),
#             by = "GEOID") %>%
#   mutate(True_EV = BEV/Total)

ev_2022 <- read_csv("./data/raw/census_tract_ev_penetration.csv") %>%
  dplyr::rename(GEOID = `Census Tract`) %>%
  left_join(CA_t %>% 
              dplyr::select(GEOID, estimate), by = "GEOID") %>% 

  mutate(GEOID = substr(GEOID, 1, 5)) %>%
  group_by(GEOID) %>% 
  summarise(BEV = sum(BEV),
            Total = sum(Total),
            EV_2022 = BEV/Total) 


# Join EV data to full county list
ev <- ca_counties %>%
  left_join(evs, by = c("NAME" = "County")) %>%
  rename(County = NAME) %>% 
  left_join(mrp_EV, by = "GEOID") %>% 
  left_join(ev_2022 %>% 
              dplyr::select(GEOID, EV_2022), by = "GEOID")

sum(ev$PHEV)/sum(ev$total)


ev %>% 
  ggplot(aes(x = EV_2024, y = EV_2022)) +
  geom_point(alpha = 0.2, size = 0.4) + 
  geom_abline(intercept = 0, slope = 1)+
  geom_smooth(method = "lm") +
  labs(x = "EV adoption (BEV/Total) 2024", y = "EV adoption (BEV/Total) 2022",
       title = "CA County level EV adoption")


ev %>% 
  mutate(mrp_EV = mrp_EV/1.7) %>% 
  gather(key, value, mrp_EV, EV_2024) %>% 
  ggplot(aes(x = EV_2022, y = value, color = key)) +
  geom_point(alpha = 0.2, size = 0.4) + 
  geom_abline(intercept = 0, slope = 1)+
  geom_smooth(method = "lm") +
  labs(x = "EV adoption (BEV/Total) 2022", y = "EV estimations",
       title = "CA County level EV adoption (converting factor of 1.7)",
       color = "")


ev %>% 
  mutate(mrp_EV = mrp_EV/1.4) %>% 
  gather(key, value, mrp_EV, EV_2024) %>% 
  ggplot(aes(x = EV_2022, y = value, color = key)) +
  geom_point(alpha = 0.2, size = 0.4) + 
  geom_abline(intercept = 0, slope = 1)+
  geom_smooth(method = "lm") +
  labs(x = "EV adoption (BEV/Total) 2022", y = "EV estimations",
       title = "CA County level EV adoption (converting factor of 1.3)",
       color = "")


ev %>% 
  gather(key, value, mrp_EV, EV_2024) %>% 
  ggplot(aes(x = EV_2022, y = value, color = key)) +
  geom_point(alpha = 0.2, size = 0.4) + 
  geom_abline(intercept = 0, slope = 1)+
  geom_smooth(method = "lm") +
  labs(x = "EV adoption (BEV/Total) 2022", y = "EV estimations",
       title = "CA County level EV adoption",
       color = "")


CA_t %>% 
  left_join(ev %>% 
              gather(key, value, EV, True_EV), by = "GEOID") %>% 
  filter(!is.na(key)) %>% 
  ggplot() +
  geom_sf(aes(fill = value), color = NA) +
  facet_wrap(~key) +
  scale_fill_viridis()

