source("./syntax/Function.R")
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
  data_process(ev = c("Fully electric")) %>% 
  data_cl() %>% 
  mutate(LMI = ifelse(income < 60000, "LMI", "Non-LMI"))


sub_ps <- data.frame()
for(i in seq(from = 100, to = 15000, by = 100)){
  pt <- data %>% 
    mutate(future = ifelse((storage_plans %in% c("Yes", "Maybe") & solstor_wtp_dv < i) | 
                             (solar_pv_plans %in% c("Yes", "Maybe") & solstor_wtp_dv < i) |
                             PS == 1, 1, 0)) %>% 
    
    group_by(LMI) %>% 
    summarise(mean = weighted.mean(future, wt_ca, na.rm = T)) %>% 
    mutate(subsidy = i)
  
  sub_ps <- rbind(pt, sub_ps)
}

sub_ev <- data.frame()
for(i in seq(from = 100, to = 15000, by = 100)){
  pt <- data %>% 
    mutate(future = ifelse((vehicle_next_when == 1 & vehicle_next_fuel == 1 &
                                 ev_wtp_pc < i)| EV == 1, 1, 0)) %>% 
    group_by(LMI) %>% 
    summarise(mean = weighted.mean(future, wt_ca, na.rm = T)) %>% 
    mutate(subsidy = i)
  
  sub_ev <- rbind(pt, sub_ev)
}


sub_hp <- data.frame()
for(i in seq(from = 100, to = 13000, by = 100)){
  pt <- data %>% 
    mutate(future = ifelse(heatpump_direct == 1 & 
                             heatpump_wtp_pc < i| HP == 1, 1, 0)) %>% 
    group_by(LMI) %>% 
    summarise(mean = weighted.mean(future, wt_ca, na.rm = T)) %>% 
    mutate(subsidy = i)
  
  sub_hp <- rbind(pt, sub_hp)
}

sub_ic <- data.frame()
for(i in seq(from = 100, to = 1300, by = 100)){
  pt <- data %>% 
    mutate(future = ifelse(induction_direct == 1 &
                             induction_dv < i| IC == 1, 1, 0)) %>% 
    group_by(LMI) %>% 
    summarise(mean = weighted.mean(future, wt_ca, na.rm = T)) %>% 
    mutate(subsidy = i)
  
  sub_ic <- rbind(pt, sub_ic)
}

curv <- rbind(
  sub_ev %>% 
    mutate(tech = "EV"),
  
  sub_ps %>% 
    mutate(tech = "PS"),
  
  sub_hp %>% 
    mutate(tech = "HP"),
  
  sub_ic %>% 
    mutate(tech = "IC")
)


pess <- data.frame(subsidy = c(5000, 8000, 4000, 300),
                   tech = c("PV + Storage", "Electric vehicles", "Heat pumps","Induction stoves")) %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "EV" = "Electric vehicles",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves")))

opt <- data.frame(subsidy = c(12500, 12500, 8000, 800),
                   tech = c("PV + Storage", "Electric vehicles", "Heat pumps","Induction stoves")) %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "EV" = "Electric vehicles",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves")))

f5a <- curv %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "EV" = "Electric vehicles",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"))) %>% 
  filter(!is.na(LMI)) %>% 
  # mutate(dac = recode(dac,
  #                     '0' = "Non-DAC",
  #                     '1' = "DAC"),
  #        dac = factor(dac, levels = c("DAC","Non-DAC"))) %>% 
  ggplot()+
  geom_smooth(aes(x = subsidy, y = mean, group = LMI, color = LMI)) +
  geom_vline(data = pess, aes(xintercept = subsidy), linetype = "dashed", color = "gray70") +
  geom_vline(data = opt, aes(xintercept = subsidy), linetype = "dotted", color = "gray20") +
  labs(x = "Subsidies (2025$)", y = "Future Adoption", color = "") +
  facet_wrap(~tech, scale = "free", nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        
        strip.placement = "outside", # Keep labels on the outside
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
        strip.text.y.left = element_text(angle = 0), # Ensure domain labels are horizontal
        
        legend.position = "bottom",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=10),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=10),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.03)) 


### extract smooth values
p <- curv %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "EV" = "Electric vehicles",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"))) %>% 
  # mutate(dac = recode(dac,
  #                     '0' = "Non-DAC",
  #                     '1' = "DAC"),
  #        dac = factor(dac, levels = c("DAC","Non-DAC"))) %>% 
  ggplot(aes(x = subsidy, y = mean, group = LMI, color = LMI)) +
  geom_smooth() + # Default method is 'gam' for n > 1000, 'loess' otherwise
  facet_wrap(~tech, scale = "free_x", nrow = 1)

plot_data <- ggplot_build(p)

smooth_values <- plot_data$data[[1]] %>%
  dplyr::select(x, y, group, PANEL) # dplyr::select relevant columns

panel_to_tech <- plot_data$layout$layout %>%
  dplyr::select(PANEL, tech)

group_to_dac <- data.frame(
  dac = c("DAC","Non-DAC"),
  group = c(1,2)
) 


group_to_dac <- data.frame(
  LMI = c("LMI","Non-LMI"),
  group = c(1,2)
) 


# Join everything together to get a clean, final table
final_smooth_data <- smooth_values %>%
  left_join(panel_to_tech, by = "PANEL") %>%
  left_join(group_to_dac, by = c("group")) %>%
  dplyr::select(
    tech,
    LMI,
    subsidy = x,
    mean = y
  ) %>%
  arrange(tech, LMI, subsidy)


final_smooth_data %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "EV" = "Electric vehicles",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"))) %>% 
  # mutate(dac = recode(dac,
  #                     '0' = "Non-DAC",
  #                     '1' = "DAC"),
  #        dac = factor(dac, levels = c("DAC","Non-DAC"))) %>% 
  ggplot()+
  geom_smooth(aes(x = subsidy, y = mean, group = LMI, color = LMI)) +
  geom_vline(data = pess, aes(xintercept = subsidy), linetype = "dashed", color = "gray70") +
  geom_vline(data = opt, aes(xintercept = subsidy), linetype = "dotted", color = "gray20") +
  labs(x = "Subsidies (2025$)", y = "Future Adoption", color = "") +
  facet_wrap(~tech, scale = "free", nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        
        strip.placement = "outside", # Keep labels on the outside
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
        strip.text.y.left = element_text(angle = 0), # Ensure domain labels are horizontal
        
        legend.position = "bottom",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=10),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=10),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.03)) 


f5b <-final_smooth_data %>% 
  arrange(tech, LMI, subsidy) %>%
  group_by(tech, LMI) %>%
  mutate(delta_subsidy = subsidy - lag(subsidy),
    delta_mean = mean - lag(mean),
    change = delta_mean/delta_subsidy
  ) %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "EV" = "Electric vehicles",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"))) %>% 
  filter(!is.na(LMI)) %>% 
  # mutate(dac = recode(dac,
  #                     '0' = "Non-DAC",
  #                     '1' = "DAC"),
  #        dac = factor(dac, levels = c("DAC","Non-DAC"))) %>% 
  ggplot()+
  geom_smooth(aes(x = subsidy, y = change*1000, group = LMI, color = LMI)) +
  geom_vline(data = pess, aes(xintercept = subsidy), linetype = "dashed", color = "gray70") +
  geom_vline(data = opt, aes(xintercept = subsidy), linetype = "dotted", color = "gray20") +
  labs(x = "Subsidies (2025$)", y = "Adoption gradient (per $k)", color = "") +
  facet_wrap(~tech, scale = "free", nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        
        strip.placement = "outside", # Keep labels on the outside
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
        strip.text.y.left = element_text(angle = 0), # Ensure domain labels are horizontal
        
        legend.position = "bottom",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=10),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=10),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.03)) 


f5 <- ggarrange(f5a, f5b, nrow = 2,
                common.legend = T, legend = "bottom",
                heights = c(1,1.1),
                labels = c("A", "B"),  # Adds labels to plots
                label.x = 0,        # Adjust horizontal position of labels
                label.y = 1,        # Adjust vertical position of labels
                font.label = list(size = 14, face = "bold")
)
ggsave("./fig/f5.png",
       f5,
       width = 12, height = 6)

