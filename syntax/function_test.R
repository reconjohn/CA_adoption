source("./syntax/Function.R")
############################################################################### EV
ps <- mreg(read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
               data_process(ev = c("Fully electric")) %>% 
               data_clean(1), 
             remove = c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv",
                        "education","employment"),
             i = 5,
           future = 127)

reg_plot(ps[[1]], "PS")



# base
tech <- c("PV","EV","HP","IC","PS")
for(k in 1:5){
  
  b_ev <- mreg(read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
                 data_process(ev = c("Fully electric")) %>% 
                 data_clean(1), 
               remove = c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv",
                          "education","employment"),
               i = k)
  
  a <- reg_plot(b_ev[[1]], tech[k])
  b <- zone_plot(b_ev[[2]], "Climate")
  c <- zone_plot(b_ev[[3]], "DAC")
  d <- map_pl(dat_pr(b_ev[[2]], b_ev[[3]]), tech[k])
  
  fig <- ggarrange(a, 
                   ggarrange(b,c,d, nrow = 1),
                   nrow = 2,
                   heights = c(2,1))
  
  ggsave(paste0("./fig/fig/",tech[k],".png"),
         fig,
         width = 12, height = 8)
}







# EV type comparison 
full_ev <- mreg(read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
                  data_process(ev = c("A plug-in hybrid","Fully electric")) %>% 
                  data_clean(1), 
                remove = c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv"),
                i = 2)

ph_ev <- mreg(read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
                data_process(ev = c("A plug-in hybrid")) %>% 
                data_clean(1), 
              remove = c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv"),
              i = 2)

b_ev <- mreg(read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
               data_process(ev = c("Fully electric")) %>% 
               data_clean(1), 
             remove = c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv"),
             i = 2)

full_ev[[1]] %>% 
  mutate(class = "All") %>% 
  rbind(
    ph_ev[[1]] %>% 
      mutate(class = "PHEV"),
    b_ev[[1]] %>% 
      mutate(class = "BEV")
  ) %>% 
  com_reg_plot("EV")


# income comparison 
low_ev <- mreg(read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
                 data_process(ev = c("Fully electric")) %>% 
                 data_clean(1) %>% 
                 filter(income < mean(income, na.rm = T)), 
               remove = c("education","employment"),
               i = 2)

high_ev <- mreg(read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
                  data_process(ev = c("Fully electric")) %>% 
                  data_clean(1) %>% 
                  filter(income > mean(income, na.rm = T)), 
                remove = c("education","employment"),
                i = 2)

low_ev[[1]] %>% 
  mutate(class = "Low income") %>% 
  rbind(
    high_ev[[1]] %>% 
      mutate(class = "High income")
  ) %>% 
  com_reg_plot("EV")


### scenario 
da <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
  data_process(ev = c("Fully electric")) %>% 
  data_clean(1)
tt <- mreg(da, 
           remove = c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv",
                      "education","employment"),
           i = 2, scenario = c("peer_EV","charging_5mile_f"))

map_plot(dat_prep(tt[[2]], tt[[3]]), "EV", c("RE","peer_EVneighbor","charging_5mile_f1"))
sep_map(tt[[2]],"Climate zone", c("RE","peer_EVneighbor","charging_5mile_f1"))
sep_map(tt[[3]],"DAC", c("RE","peer_EVneighbor","charging_5mile_f1"))
plot_adoption_effects(dat_prep(tt[[2]], tt[[3]]), 2,
                      c("RE","peer_EVneighbor","charging_5mile_f1"),
                      tt[[4]])

# MRP results
EV_da <- read_csv("./data/raw/ev_mrp_tract_est_2.csv") %>% 
  dplyr::rename(GEOID = GEOID_tract,
                EV = current_dcl1l2) %>% 
  dplyr::select(GEOID, EV) %>% 
  left_join(crosswalk %>% 
              dplyr::select(BZone,sample,GEOID, sum), by = "GEOID") %>%
  mutate(group = interaction(BZone, sample, sep = "")) %>% 
  group_by(group) %>% 
  summarise(Base = weighted.mean(EV, w = sum)) 


### logistic push three results (estimates, combined random effects, summary)
ltt <- mreg_logit(da, 
                  remove = c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv",
                             "education","employment"),
                  i = 2, scenario = c("peer_EV","charging_5mile_f"))


tt[[1]] %>% 
  mutate(class = "LPM") %>% 
  rbind(
    ltt[[1]] %>% 
      mutate(class = "Logistic")
  ) %>% 
  com_reg_plot("EV")


map_plot(ltt[[2]], "EV", c("peer_EVneighbor","charging_5mile_f1"))

### get the LMP effects based on MRP baseline
lpm_mrp <- mrp_effects(dat_prep(tt[[2]], tt[[3]]), 
                       c("peer_EVneighbor","charging_5mile_f1"),
                       tt[[4]], EV_da)

### get the logit effects based on MRP baseline
logit_mrp <- mrp_logit(ltt[[2]], c("peer_EVneighbor","charging_5mile_f1"),
                       EV_da)

lpm_mrp %>% 
  dplyr::rename(LPM = R_effect) %>% 
  left_join(logit_mrp %>% 
              st_drop_geometry() %>% 
              dplyr::rename(Logit = R_effect) , by = c("group","key")) %>% 
  gather(class, value, LPM,Logit) %>% 
  mutate(class = factor(class, levels = c("LPM","Logit"))) %>% 
  mutate(key = ifelse(key == "peer_EVneighbor", "Neighbor",
                      ifelse(key == "charging_5mile_f1", "Fast charger", "Base")),
         key = factor(key, levels = c("Base","Neighbor","Fast charger"))) %>% 
  
  ggplot() +
  geom_sf(fill = "white", color = "gray0") + # US border
  geom_sf(aes(fill = value), color = NA, size = 0.3) +
  facet_grid(class ~ key, switch = "y") +
  
  theme_minimal() +
  scale_fill_viridis_c(trans = "sqrt") +
  # scale_fill_distiller(palette = "RdBu", direction = -1, trans = "sqrt") +
  
  labs(title = "Final adoption results", fill = "") +
  theme(legend.position = "right",
        # legend.text=element_text(size=6),
        # legend.key.size = unit(0.3, 'cm'),
        strip.text = element_text(size = 14), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title=element_text(family="Franklin Gothic Demi", size=20))




### future adoption
current_ev <- mreg(read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
                     data_process(ev = c("Fully electric")) %>% 
                     data_clean(1), 
                   remove = c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv",
                              "education","employment"),
                   i = 2)

future_ev <- mreg(read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
                    data_process(ev = c("Fully electric")) %>% 
                    data_clean(1), 
                  remove = c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv",
                             "education","employment"),
                  i = 2,
                  future = 120) # wtp subsidies of 12000

# model comparison
current_ev[[1]] %>% 
  mutate(class = "Current adoption") %>% 
  rbind(
    future_ev[[1]] %>% 
      mutate(class = "Futre adoption")
  ) %>% 
  com_reg_plot("EV")

### scenario
c_da <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
  data_process(ev = c("Fully electric")) %>% 
  data_clean(1)
current_ev <- mreg(c_da, 
                   remove = c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv",
                              "education","employment"),
                   i = 2,
                   scenario = c("peer_EV","charging_5mile_f"))

future_ev <- mreg(c_da, 
                  remove = c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv",
                             "education","employment"),
                  i = 2,
                  scenario = c("peer_EV","charging_5mile_f"),
                  future = 120)


weighted.mean(c_da$EV, w = c_da$wt_ca, na.rm = T)
weighted.mean(f_da$EV, w = f_da$wt_ca, na.rm = T)


mpr_c <- read_csv("./data/raw/ev_mrp_tract_est_2.csv") %>% 
  dplyr::rename(GEOID = GEOID_tract,
                EV = current_dcl1l2) %>% 
  dplyr::select(GEOID, EV) %>% 
  left_join(crosswalk %>% 
              dplyr::select(BZone,sample,GEOID, sum), by = "GEOID") %>%
  mutate(group = interaction(BZone, sample, sep = "")) %>% 
  group_by(group) %>% 
  summarise(Base = weighted.mean(EV, w = sum)) 

mpr_f <- read_csv("./data/raw/ev_mrp_tract_est_2.csv") %>% 
  dplyr::rename(GEOID = GEOID_tract,
                EV = currentandfuture5_dcl1l2) %>% 
  dplyr::select(GEOID, EV) %>% 
  left_join(crosswalk %>% 
              dplyr::select(BZone,sample,GEOID, sum), by = "GEOID") %>%
  mutate(group = interaction(BZone, sample, sep = "")) %>% 
  group_by(group) %>% 
  summarise(Base = weighted.mean(EV, w = sum)) 

current_mpr <- mrp_effects(dat_prep(current_ev[[2]], current_ev[[3]]), 
                           c("peer_EVneighbor","charging_5mile_f1"),
                           current_ev[[4]], mpr_c)

future_mpr <- mrp_effects(dat_prep(future_ev[[2]], future_ev[[3]]), 
                          c("peer_EVneighbor","charging_5mile_f1"),
                          future_ev[[4]], mpr_f)


future_mpr %>% 
  dplyr::rename(Future = R_effect) %>% 
  left_join(current_mpr %>% 
              st_drop_geometry() %>% 
              dplyr::rename(Current = R_effect) , by = c("group","key")) %>% 
  mutate(Diff = Future - Current) %>% 
  gather(class, value, Future:Diff) %>% 
  mutate(class = factor(class, levels = c("Current","Future","Diff"))) %>% 
  mutate(key = ifelse(key == "peer_EVneighbor", "Neighbor",
                      ifelse(key == "charging_5mile_f1", "Fast charger", "Base")),
         key = factor(key, levels = c("Base","Neighbor","Fast charger"))) %>% 
  
  ggplot() +
  geom_sf(fill = "white", color = "gray0") + # US border
  geom_sf(aes(fill = value), color = NA, size = 0.3) +
  facet_grid(key ~ class, switch = "y") +
  
  theme_minimal() +
  scale_fill_viridis_c(trans = "sqrt") +
  # scale_fill_distiller(palette = "RdBu", direction = -1, trans = "sqrt") +
  
  labs(title = "Final adoption results", fill = "") +
  theme(legend.position = "right",
        # legend.text=element_text(size=6),
        # legend.key.size = unit(0.3, 'cm'),
        strip.text = element_text(size = 14), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title=element_text(family="Franklin Gothic Demi", size=20))


### WTP impact
c_da <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
  data_process(ev = c("Fully electric")) %>% 
  data_clean(1)
current_ev <- mreg(c_da, 
                   remove = c("solstor_wtp_dv","heatpump_wtp_pc","induction_dv",
                              "education","employment"),
                   i = 2,
                   scenario = c("ev_wtp_pc"))

future_ev <- mreg(c_da, 
                  remove = c("solstor_wtp_dv","heatpump_wtp_pc","induction_dv",
                             "education","employment"),
                  i = 2,
                  scenario = c("ev_wtp_pc"),
                  future = 1)


current_ev[[1]] %>% 
  mutate(class = "Current") %>% 
  rbind(
    future_ev[[1]] %>% 
      mutate(class = "Futre")
  ) %>% 
  com_reg_plot("EV")


mpr_c <- read_csv("./data/raw/ev_mrp_tract_est_2.csv") %>% 
  dplyr::rename(GEOID = GEOID_tract,
                EV = current_dcl1l2) %>% 
  dplyr::select(GEOID, EV) %>% 
  left_join(crosswalk %>% 
              dplyr::select(BZone,sample,GEOID, sum), by = "GEOID") %>%
  mutate(group = interaction(BZone, sample, sep = "")) %>% 
  group_by(group) %>% 
  summarise(Base = weighted.mean(EV, w = sum)) 

mpr_f <- read_csv("./data/raw/ev_mrp_tract_est_2.csv") %>% 
  dplyr::rename(GEOID = GEOID_tract,
                EV = currentandfuture5_dcl1l2) %>% 
  dplyr::select(GEOID, EV) %>% 
  left_join(crosswalk %>% 
              dplyr::select(BZone,sample,GEOID, sum), by = "GEOID") %>%
  mutate(group = interaction(BZone, sample, sep = "")) %>% 
  group_by(group) %>% 
  summarise(Base = weighted.mean(EV, w = sum)) 

current_mpr <- mrp_effects(dat_prep(current_ev[[2]], current_ev[[3]]), 
                           c("ev_wtp_pc"),
                           current_ev[[4]], mpr_c)

future_mpr <- mrp_effects(dat_prep(future_ev[[2]], future_ev[[3]]), 
                          c("ev_wtp_pc"),
                          future_ev[[4]], mpr_f)


future_mpr %>% 
  dplyr::rename(Future = R_effect) %>% 
  left_join(current_mpr %>% 
              st_drop_geometry() %>% 
              dplyr::rename(Current = R_effect) , by = c("group","key")) %>% 
  mutate(Diff = Future - Current) %>% 
  gather(class, value, Future:Diff) %>% 
  mutate(class = factor(class, levels = c("Current","Future","Diff"))) %>% 
  
  mutate(key = ifelse(key == "ev_wtp_pc", "WTP", "Base"),
         key = factor(key, levels = c("Base","WTP"))) %>% 
  
  ggplot() +
  geom_sf(fill = "white", color = "gray0") + # US border
  geom_sf(aes(fill = value), color = NA, size = 0.3) +
  facet_grid(key ~ class, switch = "y") +
  
  theme_minimal() +
  scale_fill_viridis_c(trans = "sqrt") +
  # scale_fill_distiller(palette = "RdBu", direction = -1, trans = "sqrt") +
  
  labs(title = "Final adoption results", fill = "") +
  theme(legend.position = "right",
        # legend.text=element_text(size=6),
        # legend.key.size = unit(0.3, 'cm'),
        strip.text = element_text(size = 14), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title=element_text(family="Franklin Gothic Demi", size=20))

