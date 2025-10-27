source("./syntax/Function.R")
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

remove <- c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv", "education","employment")


# ML feature importance
tc_kfold <- trainControl(method = "cv", number = 10, returnResamp="all",
                         savePredictions = TRUE, classProbs = TRUE,
                         summaryFunction = twoClassSummary)

result <- data.frame()
for(k in 2:5){
  dt <- mreg_var(data, i = k)
  fvar <- as.formula(paste(ipt[k], "~", paste(dt[[2]], collapse = " + ")))
  
  outcome_var <- as.character(fvar[[2]])
  # Sanitize factor levels
  levels(dt[[1]][[outcome_var]]) <- make.names(levels(dt[[1]][[outcome_var]]))
  
  ### GLM
  glm <- train(form = fvar, data= dt[[1]], method = "glm",
               trControl = tc_kfold,
               na.action=na.omit,
               family = 'binomial',
               weights = dt[[1]]$wt_ca)
  
  varimp_glm <- varImp(glm)
  
  ### lasso
  lambda <- 10^seq(-3, 3, length = 100)
  lasso <- train(form = fvar, data=dt[[1]], method = "glmnet",
                 trControl = tc_kfold,
                 tuneGrid = expand.grid(alpha = 1, lambda = lambda),
                 na.action=na.omit,
                 weights = dt[[1]]$wt_ca)
  
  varimp_lasso <- varImp(lasso)
  
  ### xgbTree
  tune_grid <- expand.grid(
    nrounds = c(50),       # Number of boosting iterations
    max_depth = c(3, 6),           # Maximum depth of trees
    eta = c(0.01, 0.1),         # Learning rate
    gamma = c(1),               # Minimum loss reduction required
    colsample_bytree = c(0.6),# Fraction of features used per tree
    min_child_weight = c(1, 5),
    subsample = c(0.6)        # Fraction of samples used per tree
  )
  xg = train(form = fvar, data=dt[[1]],
             method="xgbTree", trControl = tc_kfold,
             tuneGrid = tune_grid,
             na.action = na.omit,
             weights = dt[[1]]$wt_ca)
  
  varimp_xg <- varImp(xg)
  
  
  imp <- rbind(
    varimp_xg[[1]] %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      mutate(model = "XGBoost"),
    varimp_lasso[[1]] %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      mutate(model = "Lasso"),
    varimp_glm[[1]] %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      mutate(model = "GLM")) %>% 
    mutate(tech = ipt[k])
  
  result <- rbind(imp, result)
}


results <- result %>% 
  mutate(rowname = recode(
    rowname,
    "born_us1" = "born US",
    "upfrontpayback1" = "upfront prefer",
    "therm_winter1" = "thermostat use",
    "outage_generatorown1" = "own generator",
    
    "charging_5mile_f1" = "fast charger",
    # "vehicle_next_fuel1" = "EV to buy",
    "vehicle_next_used1" = "new car",
    # "vehicle_next_when" = "Later to buy car",
    "vehicle_1_miles" = "milage",
    "charging_work1" = "charger at work",
    
    # "home_age" = "newer home",
    "rangeanxiety" = "more range for EV",
    "ccmove_where1" = "leave CA",
    "homevac" = "evacuation"
  
  )) %>% 
  mutate(rowname = case_when(
    str_detect(rowname, "peer_.*none") ~ "neighbor/peer",
    str_detect(rowname, "peer_.*peer") ~ "peer",
    str_detect(rowname, "home_ageNewer") ~ "home after 2020",
    TRUE ~ rowname
  )) %>% 
  mutate(domain = ifelse(str_detect(rowname,"income|education|race|ideology|pid|US|employment|neighbor|peer"), "Social",
                           
                           ifelse(str_detect(rowname, "home|household"), "Housing",
                                  
                                  ifelse(str_detect(rowname,"primary|thermostat"), "Heat/Cool",
                                         
                                         ifelse(str_detect(rowname, "kitchen"), "Cook",
                                                
                                                ifelse(str_detect(rowname,"therm|burden|savemoney|health|safety|upfront|direct|plan|electrification"), "Behavior",
                                                       
                                                       ifelse(str_detect(rowname, "outage_impact|freq|dur|generator|cc|CA|evacuation"), "Resilience", 

                                                                     ifelse(str_detect(rowname, "milage|charger|charger|car|EV|l1l2|dc"), "Vehicle",rowname))))))),
           domain = factor(domain, levels = c("Housing","Heat/Cool","Cook","Vehicle","Social","Behavior","Resilience","Peer Effect"))) %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "EV" = "Electric vehicles",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"))) 


f1 <- results %>% 
  group_by(rowname, tech) %>% 
  mutate(mean = mean(Overall)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_col(aes(y = reorder(rowname, desc(mean)), x = Overall, fill = model), width = 0.7, position = position_dodge(0.8)) +
  geom_point(aes(y = reorder(rowname, desc(mean)), x = mean), color = "red", width = 0.7, position = position_dodge(0.8))+
  labs(y = "", x = "Importance (%)", fill = "", title = "") +
  facet_grid(domain ~ tech, space = "free", scale = "free", switch = "y") +
  theme_bw() +
  scale_fill_brewer(palette = "Paired") +
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


ggsave("./fig/f1.png",
       f1,
       width = 12, height = 12)


### comparison of adoption vs. MRP
mrp %>%
  left_join(CA_t %>% st_drop_geometry(), by = "GEOID") %>%
  summarise(across(where(is.numeric), ~ weighted.mean(.x, w = estimate, na.rm = TRUE))) %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "name") %>% 
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






  




  
  
 

