### other spatial effects

### low scenario
effect <- list()
for(i in 2:5){
  future <- "low"
  
  f_d <- final_ef_peer(data %>%
                         dplyr::select(VAR[[i-1]], climatezone, dac, matches("PV|PS|EV|HP|IC"),wt_ca),
                       i, future) %>%
    mutate(class = paste0(ipt[i],"_",future))
  
  effect <- append(effect, list(f_d))
  
}


### final adoption by tract
final <- data.frame()
for(i in 1:4){
  tp <- effect[[i]] %>% 
    mutate(
      peer_effect = rowSums(dplyr::select(., all_of(names(.)[str_detect(names(.), "^peer")])), na.rm = TRUE)
      # home_age = rowSums(dplyr::select(., all_of(names(.)[str_detect(names(.), "^home_age")])), na.rm = TRUE)
    ) %>%
    dplyr::select(-ends_with(c("New","Newer","peer","none"))) %>% 
    pivot_longer(cols = c(Effect, MRP, Final), names_to = "key", values_to = "value") %>% 
    mutate(key = factor(key, levels = c("MRP","Effect","Final"))) %>% 
    filter(key == "Final") %>% 
    dplyr::select(GEOID, class, value)
  
  final <- rbind(tp, final)
}


mmp <- CA_t %>% 
  dplyr::select(GEOID) %>% 
  left_join(final %>% 
              separate(class, into = c("tech", "scen"), sep = "_"), by = "GEOID") %>% 
  
  filter(tech != "PV") %>% 
  filter(scen == "low") %>% 
  mutate(
    tech = recode(tech,
                  "PS" = "PV + Storage",
                  "IC" = "Induction stoves",
                  "HP" = "Heat pumps",
                  "EV" = "Electric vehicles",
                  "PV" = "Photovoltaics"),
    tech = factor(tech, levels = c("Photovoltaics","PV + Storage", "Electric vehicles", "Heat pumps", "Induction stoves"))) %>%
  na.omit()


mmp %>%
  st_drop_geometry() %>%
  pivot_wider(names_from = tech, values_from = value) %>%
  write_csv("./data/low_spatial.csv")


### no subsidy
mrp %>% 
  dplyr::select(GEOID, which(str_detect(names(.), paste0("_0")))) %>% 
  write_csv("./data/0_spatial.csv")
  



