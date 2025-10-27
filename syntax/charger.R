source("./syntax/Function.R")
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

# data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
#   data_process(ev = c("Fully electric"))

load("../adoption/data/dac_sf.RData") # CA_t, cz, dac_sf, sc_map (DAC, climate zone spatial data)

### add urban variable
data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
  data_process(ev = c("Fully electric")) %>% 
  left_join(CA_t %>% 
              st_drop_geometry() %>% 
              dplyr::select(GEOID, estimate), by = c("tractid" = "GEOID")) %>% 
  mutate(urban = ifelse(estimate > 7000, "Urban", "Rural")) %>% 
  mutate(dac = ifelse(dac == 1 & urban == "Urban", "Urban_DAC",
                      ifelse(dac == 1 & urban == "Rural", "Rural_DAC",
                             ifelse(dac == 0 & urban == "Urban", "Urban_Non_DAC", "Rural_Non_DAC")))) %>% 
  dplyr::select(-estimate, -urban) %>% 
  data_clean(1)


### EV charger
select_scene <- list(c("home_age","peer_PV"),
                     c("home_age","peer_EV","charging_5mile_f","rangeanxiety"),
                     c("peer_HP"),
                     c("home_age","peer_IC"),
                     c("home_age","peer_PV"))

k <- 2
  
  b_ev <- mreg_dac(data, 
                   remove = c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv",
                              "education","employment"),
                   scenario = select_scene[[k]],
                   i = k,
                   future = fut[[k]][1],
                   peer = 1)
  
  num <- length(b_ev[[1]])
  
  df <- b_ev[[1]] %>% 
    dplyr::select(matches("charging")) %>% 
    dplyr::rename(Charging = names(.)[[1]]) %>% 
    
    mutate(dac = row.names(.)) %>% 
    mutate(SE = c(b_ev[[2]][num-1,num-1,]))
  

f6c <- df %>%
  mutate(urban = ifelse(dac %in% c("Urban_DAC","Urban_Non_DAC"), "Urban", "Rural"),
         dac = ifelse(dac %in% c("Urban_DAC","Rural_DAC"), "DAC", "Non-DAC")) %>% 
  ggplot(aes(x = urban, y = Charging, fill = dac)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(
    aes(ymin = Charging - SE, ymax = Charging + SE),
    position = position_dodge(width = 0.7),
    width = 0.2
  ) +

  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "",
    y = "Effect magnitude",
    fill = "",
    title = "Charging density effect"
  ) +
  theme_minimal(base_size = 14) +
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




### heat pump 
tech <- c("PV","EV","HP","IC","PS")
k <- 3
  
  b_ev <- mreg(read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
                 data_process(ev = c("Fully electric")) %>% 
                 data_clean(1), 
               remove = c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv",
                          "education","employment"),
               i = k)
  
  b <- zone_plot(b_ev[[2]], "Climate")
  d <- map_pl(dat_pr(b_ev[[2]], b_ev[[3]]), tech[k])
  




f6c <- df %>%
  mutate(urban = ifelse(dac %in% c("Urban_DAC","Urban_Non_DAC"), "Urban", "Rural"),
         dac = ifelse(dac %in% c("Urban_DAC","Rural_DAC"), "DAC", "Non-DAC")) %>% 
  ggplot(aes(x = urban, y = Charging, fill = dac)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(
    aes(ymin = Charging - SE, ymax = Charging + SE),
    position = position_dodge(width = 0.7),
    width = 0.2
  ) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "",
    y = "Effect magnitude",
    fill = "",
    title = "Charging density effect"
  ) +
  theme_minimal(base_size = 14) +
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





f4 <- ggarrange(
  ggarrange(f4a, f4b, nrow = 2), f4c, nrow = 2,
  heights = c(1,1),
  labels = c("A", "B"),  # Adds labels to plots
  label.x = 0,        # Adjust horizontal position of labels
  label.y = 1,        # Adjust vertical position of labels
  font.label = list(size = 14, face = "bold")
)


ggsave("./fig/f4.png",
       f4,
       width = 12, height = 12)


