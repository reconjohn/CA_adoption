library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(pander)
library(quantmod)
library(stringr)
library(SpatialEpi)
library(maps)
library(shapefiles)
# library(maptools)
library(RColorBrewer)
library(spdep)
# library(INLA)
# library(rgdal)
library(pander)
library(DCluster)
library(geoR)
library(sp)
library(splancs)
# library(GWmodel)
library(psych)
library (cluster)
library(reshape)
library(reshape2)
library(som)
library(GPArotation)
library(corrplot)
library(GGally)
library(faraway)
library(sjPlot)
library(lattice)
library(png)
library(grid)
library(gridExtra)
# library(rgeos)
library(leaflet)
library(tigris)
library(broom)
library("broom.mixed")
library(rmapshaper)
library(ggpubr)
library(lme4)
library(lmerTest)
library(stargazer)
library(arm)
library(pscl)
library(spatialreg)
library(ggstance)
library(viridis)
library(ggpubr)
library(ggrepel)
library(readxl)
library(fastDummies)
library(caret)
library(xgboost)
library(tidytext)
library(ggnewscale)
library(ggpattern)
library(scico)
library(shadowtext)
library(ggtext)
library(marginaleffects)
library(ggnewscale)
library(gridGraphics)
library(patchwork)
library(gtsummary)
library(ggpubr)
library(ineq)
options(tigris_use_cache = TRUE, tigris_class = "sf")

# uploading library 
lapply(c("spatstat","colorRamps","tmap","ggmap","mapview","geoR","knitr","kableExtra","data.table","gdata","tigris","sf","scales","tidycensus","plotly", "tidyverse"), require, character.only = TRUE)
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
sf::sf_use_s2(FALSE)

load("./data/data.RData")

ipt <- c("PV","EV","HP","IC","PS")

### sum contrasts variables 
# binary
bina1 <- c(
  "born_us",
  "employment",
  "home_own",
  "vehicle_next_used",
  "charging_5mile_f",
  "charging_work",
  
  "upfrontpayback",
  "lowincome_sup",
  "outage_generatorown",
  "ccinsure_lost",
  "ccmove_where"
) 

# categorical variables: 
cat1 <- c(
  "race",
  "gender",
  "education",
  "home_type",
  "home_age",
  
  "peer_EV",
  "peer_PV",
  "peer_HP",
  "peer_IC",
  
  "primary_heating_type",
  "previous_heating_type",
  "primary_cooling_type",
  "hotwater_energy",
  
  "kitchen_range_type",
  "electrification"
) 


ftr1 <- c("climatezone",
          "dac",
          "gender",
          "education",
          "race",
          "born_us",
          "home_type",
          "home_own",
          "employment",
          "home_age",
          
          "peer_EV",
          "peer_PV",
          "peer_HP",
          "peer_IC",
          
          "charging_5mile_f",
          "vehicle_next_used",
          "charging_work",
          
          "primary_heating_type",
          "previous_heating_type",
          "primary_cooling_type",
          "hotwater_energy",    
          
          "kitchen_range_type",
          "electrification",
          "upfrontpayback",
          "lowincome_sup",
          "outage_generatorown",
          "ccinsure_lost",
          "ccmove_where"
) 


# ### comparison of adoption vs. MRP
# mrp_mean <- mrp %>%
#   left_join(CA_t %>% st_drop_geometry(), by = "GEOID") %>%
#   summarise(across(where(is.numeric), ~ weighted.mean(.x, w = estimate, na.rm = TRUE))) %>% 
#   t() %>% 
#   as.data.frame() %>% 
#   tibble::rownames_to_column(var = "name") 
# 
# 
# mrp_mean_dac <- mrp %>%
#   left_join(dac, by = "GEOID") %>% 
#   left_join(CA_t %>% st_drop_geometry(), by = "GEOID") %>%
#   group_by(sample) %>% 
#   summarise(across(where(is.numeric), ~ weighted.mean(.x, w = estimate, na.rm = TRUE))) %>% 
#   dplyr::select(-estimate,-moe)


# mrp_mean %>%
#   left_join(
#     data %>%
#       summarise(across(
#         .cols = all_of(names(.)[str_detect(names(.), "PV|EV|HP|IC|PS")& !str_detect(names(.), "peer")]),
#         .fns = ~ weighted.mean(.x, wt_ca, na.rm = TRUE)
#       )) %>%
#       t() %>%
#       as.data.frame() %>%
#       tibble::rownames_to_column(var = "name"),
#     by = "name"
#   ) %>%
#   filter(!is.na(V1.y))  %>% # Remove rows with NA in V1.y
#   filter(name != "future_PV") %>%
# 
#   ggplot(aes(x = V1.x, y = V1.y, label = name)) +
#   geom_point(color = "steelblue", size = 3) +
#   geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed") +
#   geom_abline() +
#   geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
#   labs(
#     x = "MRP",
#     y = "Survey",
#     title = "Scatter Plot"
#   ) +
#   theme_minimal()

# future2 <- c(120, 75) # if i == 2,
# future3 <- c(110, 30) # if i == 3,
# future4 <- c(159, 35) # if i == 4,
# future1 <- future5 <- c(127, 76) # if i == 5,
# fut <- list(future1,future2,future3,future4,future5)


data <- dat %>% 
  dplyr::select(-c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv","solstor_wtp_dv"),
              -c("PS_int","EV_int","HP_int","IC_int"))
### removing variables 
VAR <- list()
ipt <- c("PV","EV","HP","IC","PS")
future <- "high"
for(i in 2:5){
  
  if(i == 1){
    future_var <- sym(paste0("future_", ipt[i]))
  }else{
    future_var <- sym(paste0("future_", ipt[i],"_",future))
  }
  
  
  if(!is.null(future)){
    da_r <- data %>% 
      mutate(!!ipt[i] := .data[[future_var]]) %>% 
      dplyr::select(# remove multicollinear variables
        -starts_with("future")
      ) 
  }
  
  
  da_r <- da_r %>%
    mutate(across(where(is.numeric) & !c("PV","PS","EV","HP","IC","wt_ca"), ~ scale(.) %>% as.numeric()))
  
  if(i %in% c(1,5)){
    # for PV, remove zone effect, tech
    tract <- c("climatezone","dac","ev_wtp_pc","heatpump_wtp_pc","induction_dv","wt_ca",
               "peer_EV","peer_HP","peer_IC","previous_heating_type",
               "PV","PS","EV","HP","IC")
    
  }else if(i == 2){
    # for EV, remove zone effect, tech
    tract <- c("climatezone","dac","solstor_wtp_dv","heatpump_wtp_pc","induction_dv","wt_ca",
               "peer_IC","peer_HP","peer_PV","previous_heating_type",
               "PV","PS","EV","HP","IC")
    
  }else if(i == 3){
    # for HP, remove zone effect, tech, heating/cooling type
    tract <- c("climatezone","dac","ev_wtp_pc","solstor_wtp_dv","induction_dv","wt_ca",
               "peer_EV","peer_IC","peer_PV",
               "PV","PS","EV","HP","IC","primary_heating_type","primary_cooling_type")
    
    
  }else{
    # for IC, remove zone effect, tech, cooking type
    tract <- c("climatezone","dac","ev_wtp_pc","solstor_wtp_dv","heatpump_wtp_pc","wt_ca",
               "peer_EV","peer_HP","peer_PV","previous_heating_type",
               "PV","PS","EV","HP","IC","kitchen_range_type")
    
  }
  model1vars <- setdiff(names(da_r), tract)
  fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), 
                           "+ climatezone + dac"))
  
  da_rr <- da_r %>% 
    dplyr::select(model1vars, climatezone, dac, ipt[i], wt_ca) %>% 
    na.omit()
  
  
  fit <- lm(fvar, weights = wt_ca, data = da_rr)
  fit_step <- step(fit, direction = "both")
  
  VAR[[i-1]] <- attr(terms(fit_step), "term.labels")
  
}

### scenario
effect <- list()
for(i in 2:5){
  future <- "high"
  
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
