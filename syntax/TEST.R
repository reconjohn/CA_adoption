source("./syntax/Function.R")
library(caret)        # Main package for model training and preprocessing
library(randomForest) # For Random Forest model
library(gbm)          # For Gradient Boosting Machine
library(glmnet)       # For Elastic Net (Ridge/Lasso)

ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>%
  data_process(ev = c("Fully electric")) %>% 
  data_clean() 

write_csv(data, "./data/data_cleaned.csv")

### NAs 
na_counts <- colSums(is.na(data)) %>% sort(decreasing = TRUE)
# Convert to a data frame for plotting
na_df <- data.frame(variable = names(na_counts), na_count = na_counts) %>% 
  filter(na_count > 1000)

# Create the plot
ggplot(na_df, aes(x = reorder(variable, na_count), y = na_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip axes for better readability
  labs(title = "NA Counts by Variable",
       x = "Variable",
       y = "Number of NA Values") +
  theme_minimal()


### remove those with NAs
data <- data %>% 
  dplyr::select(-na_df$variable,cost_combo_winter_final,cost_combo_summer_final,rangeanxiety,matches("future")) %>% 
  dplyr::select(-ExternalReference,-tractid,-wt_ca,-ev_wtp_pc) %>% 
  dplyr::select(-matches("future"),-PV)

data %>% names()

dv_name <- "PS"
tech <- c("PS","EV","HP","IC")

dat <- data %>% 
  dplyr::select(-matches(setdiff(tech, dv_name)))

names(dat)

predictors_names <- setdiff(colnames(dat), dv_name)
x <- dat[, predictors_names]


na_percent <- colMeans(is.na(x)) * 100
na_threshold <- 30
cols_to_remove_na <- names(na_percent[na_percent > na_threshold])

if (length(cols_to_remove_na) > 0) {
  cat("Removing", length(cols_to_remove_na), "predictors with >", na_threshold, "% NAs:\n")
  cat(paste(cols_to_remove_na, collapse = ", "), "\n")
  x <- x[, !(colnames(x) %in% cols_to_remove_na)]
} else {
  cat("No predictors found with >", na_threshold, "% NAs.\n")
}

categorical_vars <- c(
  "climatezone","gender","race","employment","home_type","home_age","primary_heating_type",
  "primary_cooling_type","kitchen_range_type","charging_access"
)

for (var in categorical_vars) {
  # Check if var is in the dataframe colnames first
  if (var %in% colnames(x)) { # Check in 'x' now, not 'data'
    x[[var]] <- as.factor(x[[var]])
  }
}

temp_dummy_model <- dummyVars(~ ., data = x, fullRank = TRUE)
x_temp_dummied <- predict(temp_dummy_model, newdata = x)

### removing correlation
numeric_cols <- sapply(x, is.numeric)
x_numeric <- x[, numeric_cols]

corr_matrix <- cor(x_numeric, use = "pairwise.complete.obs")
corr_matrix[is.na(corr_matrix)] <- 0

corr_cutoff <- 0.90
highly_corr_indices <- findCorrelation(corr_matrix, cutoff = corr_cutoff, names = FALSE)


remaining_categorical <- intersect(categorical_vars, colnames(x))
for (var in remaining_categorical) {
  x[[var]] <- as.factor(x[[var]])
}

na_indices <- which(apply(x, 1, function(row) any(is.na(row))))
x1 <- x[-na_indices,]
y <- dat[-na_indices,dv_name, drop = TRUE]

dummy_model <- dummyVars(~ ., data = x1, fullRank = TRUE)
x_final <- predict(dummy_model, newdata = x1)

rf <- randomForest(
  x = x_final,
  y = y,
  ntree = 100,
  importance = TRUE,
  keep.inbag = FALSE
)

# Get importance (Type 1 = %IncMSE)
imp <- importance(rf, type = 1, scale = FALSE)
imp_df <- data.frame(Var = rownames(imp), Importance = imp[,1])
imp_df <- imp_df[order(imp_df$Importance, decreasing = TRUE), ]

# Create the plot
ggplot(imp_df, aes(x = reorder(Var, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip axes for better readability
  labs(title = "Feature Importance",
       x = "Variable",
       y = "Importance") +
  theme_minimal()



################################################################################
### simple ML
data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>%
  data_process(ev = c("Fully electric")) %>% 
  data_clean() 

# ML feature importance
tc_kfold <- trainControl(method = "cv", number = 10, returnResamp="all",
                         savePredictions = TRUE, classProbs = TRUE,
                         summaryFunction = twoClassSummary)

result <- data.frame()
for(k in 2:5){
  dt <- mreg_var1(data, i = k, future = 0)
  fvar <- as.formula(paste(ipt[k], "~", paste(dt[[2]], collapse = " + ")))
  
  # "ev_wtp_pc","solstor_wtp_dv","heatpump_wtp_pc","induction_dv"
  # dt[[1]] %>%
  #   dplyr::select(dt[[2]]) %>% 
  #   na.omit() %>% View
  
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
  
  ### rf
  rf <- train(form = fvar, data= dt[[1]], method = "ranger",
              metric = "ROC",
              trControl = tc_kfold,
              na.action=na.omit,
              importance = "impurity",
              weights = dt[[1]]$wt_ca)
  
  varimp_rf <- varImp(rf)
  
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
      mutate(model = "GLM"),
    
    
    varimp_rf[[1]] %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      mutate(model = "RF")
    ) %>% 
    mutate(tech = ipt[k])

  result <- rbind(imp, result)
}


results <- result %>% 
  mutate(rowname = case_when(
    str_detect(rowname, "peer_.*none") ~ "neighbor/peer",
    str_detect(rowname, "peer_.*peer") ~ "peer",
    TRUE ~ rowname
  )) %>%
  mutate(domain = ifelse(str_detect(rowname,"outage|freq|dur|generator|cc|CA|evacuation|yale|avail|vac"), "Resilience", 
                         
                         ifelse(str_detect(rowname, "home|household|home_age"), "Housing",
                                
                                ifelse(str_detect(rowname,"primary|thermostat|combo|hotwater|previous"), "Heat/Cool",
                                       
                                       ifelse(str_detect(rowname, "kitchen"), "Cook",
                                              
                                              ifelse(str_detect(rowname,"therm|burden|savemoney|health|safety|upfront|direct|plan|electrification"), "Behavior",
                                                     
                                                     ifelse(str_detect(rowname, "income|education|race|ideology|pid|born|employment|neighbor|peer|age|gender"), "Social",
                                                            
                                                            ifelse(str_detect(rowname, "milage|charger|charging|car|EV|l1l2|dc|vehicle|range"), "Vehicle",rowname))))))),
         domain = factor(domain, levels = c("Housing","Heat/Cool","Cook","Vehicle","Social","Behavior","Resilience","Peer Effect"))) %>% 
  mutate(tech = recode(tech, 
                       "PS" = "PV + Storage",
                       "EV" = "Electric vehicles",
                       "HP" = "Heat pumps",
                       "IC" = "Induction stoves")) %>% 
  mutate(tech = factor(tech, levels = c("PV + Storage","Electric vehicles","Heat pumps","Induction stoves"))) 



f1a <- results %>% 
  mutate(rowname = gsub("`", "", rowname)) %>% 
  group_by(rowname, tech) %>% 
  mutate(mean = mean(Overall)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_col(aes(y = reorder(rowname, mean), x = Overall, fill = model), width = 0.7, position = position_dodge(0.8)) +
  geom_point(aes(y = reorder(rowname, mean), x = mean), color = "red", width = 0.7, position = position_dodge(0.8))+
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


f1a <- results %>% 

  mutate(rowname = gsub("`", "", rowname)) %>% 
  group_by(rowname, tech) %>% 
  mutate(mean = mean(Overall)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_col(aes(y = reorder(rowname, mean), x = Overall, fill = model), width = 0.7, position = position_dodge(0.8)) +
  geom_point(aes(y = reorder(rowname, mean), x = mean), color = "red", width = 0.7, position = position_dodge(0.8))+
  labs(y = "", x = "Importance (%)", fill = "", title = "") +
  coord_flip() +
  facet_grid(tech ~ domain, space = "free", scale = "free", switch = "y") +
  theme_bw() +
  scale_fill_brewer(palette = "Paired") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        
        strip.placement = "outside", # Keep labels on the outside
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
        strip.text.y.left = element_text(angle = 0), # Ensure domain labels are horizontal
        
        legend.position = "right",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=10, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=10),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.03)) 



ggsave("./fig/f1b.png",
       f1a,
       width = 12, height = 8)


ggsave("./fig/f1.png",
       f1,
       width = 12, height = 12)


### cor plot
na_counts <- colSums(is.na(data)) %>% sort(decreasing = TRUE)
na_df <- data.frame(variable = names(na_counts), na_count = na_counts) %>% 
  filter(na_count > 1000)

da_r <- data %>% 
  dplyr::select(-na_df$variable,rangeanxiety, -ev_wtp_pc) %>% 
  dplyr::select(-ExternalReference,-tractid,-wt_ca,-tractid) %>% 
  dplyr::select(-PV, -matches("future"))

categorical_vars <- c(
  "climatezone","dac","gender","race","education","born_us","employment","home_type","home_age","home_own","charging_work",
  "vehicle_next_used","primary_heating_type","hotwater_energy","previous_heating_type",
  "primary_cooling_type","kitchen_range_type","charging_access",
  "upfrontpayback","lowincome_sup","outage_generatorown",
  "ccinsure_lost","ccmove_where","charging_5mile_r","charging_5mile_f",
  "electrification","peer_EV","peer_PV","peer_HP","peer_IC","ccinsure_both"
)

cat_da <- da_r %>% 
  dplyr::select(categorical_vars) %>% 
  mutate(across(everything(), as.character))

cat_da$peer_EV <- relevel(as.factor(cat_da$peer_EV), ref = "none")
cat_da$peer_PV <- relevel(as.factor(cat_da$peer_PV), ref = "none")
cat_da$peer_HP <- relevel(as.factor(cat_da$peer_HP), ref = "none")
cat_da$peer_IC <- relevel(as.factor(cat_da$peer_IC), ref = "none")

dmy <- dummyVars(" ~ .", cat_da, fullRank = T)
cat_da_transformed <- data.frame(predict(dmy, newdata = cat_da))


png(file="./fig/cor_cat.png",
    units="in", width=17, height=17, res=300)

corrplot::corrplot(cor(cat_da_transformed, use = "pairwise.complete.obs"), 
                   method = 'square', order = 'FPC', type = 'lower', diag = FALSE)

dev.off()


# non-categorical variables
con_da <- da_r %>% 
  dplyr::select(!categorical_vars) 

summary(con_da)

library(corrplot)
png(file="./fig/cor_num.png",
    units="in", width=10, height=10, res=300)

corrplot::corrplot(cor(con_da, use = "pairwise.complete.obs"), 
                   method = 'square', order = 'FPC', type = 'lower', diag = FALSE)

dev.off()


# combine
d <- con_da %>% 
  cbind(cat_da_transformed)

names(d)
summary(d)

png(file="./fig/cor_tot.png",
    units="in", width=17, height=17, res=300)

corrplot::corrplot(cor(d, use = "pairwise.complete.obs"), 
                   method = 'square', order = 'FPC', type = 'lower', diag = FALSE)

dev.off()
### remove yale, ideology, charging access, hvac avail due to collinearity 


################################################################################
### variable selection
### comparison for WTP 
wtp_var <- c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv","solstor_wtp_dv")
data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
  data_process(ev = c("Fully electric")) %>% 
  data_clean(1) # remove variables with substantial NAs and collinearity

### PS modiviation version 
    data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
      data_process(ev = c("Fully electric")) %>% 
      data_cl() %>% 
      mutate(
        # future adoption
        # future_EV10 = ifelse((vehicle_next_when > 1 & vehicle_next_fuel == 1)| EV == 1, 1, 0),
        future_EV_0 = ifelse((vehicle_next_when == 1 & vehicle_next_fuel == 1 &
                                ev_wtp_pc == 0)| EV == 1, 1, 0),
        future_EV_low = ifelse((vehicle_next_when == 1 & vehicle_next_fuel == 1 &
                                  ev_wtp_pc <= 8000)| EV == 1, 1, 0),
        future_EV_high = ifelse((vehicle_next_when == 1 & vehicle_next_fuel == 1 &
                                   ev_wtp_pc <= 12500)| EV == 1, 1, 0),
        
        future_PV = ifelse(solar_pv_plans %in% c("Yes", "Maybe") | PV == 1, 1, 0),
        future_PS_0 = ifelse((storage_plans %in% c("Yes", "Maybe")) | 
                               (solar_pv_plans %in% c("Yes", "Maybe")) |
                               PS == 1, 1, 0),
        future_PS_low = ifelse((storage_plans %in% c("Yes", "Maybe") & solstor_wtp_dv <= 5000) |
                                 (solar_pv_plans %in% c("Yes", "Maybe") & solstor_wtp_dv <= 5000) |
                                 PS == 1, 1, 0),
        future_PS_high = ifelse((storage_plans %in% c("Yes", "Maybe") & solstor_wtp_dv <= 12500) |
                                  (solar_pv_plans %in% c("Yes", "Maybe") & solstor_wtp_dv <= 12500) |
                                  PS == 1, 1, 0),
        
        future_HP_0 = ifelse(heatpump_direct == 1 & 
                               heatpump_wtp_pc == 0| HP == 1, 1, 0),
        future_HP_low = ifelse(heatpump_direct == 1 & 
                                 heatpump_wtp_pc <= 4000| HP == 1, 1, 0),
        future_HP_high = ifelse(heatpump_direct == 1 & 
                                  heatpump_wtp_pc <= 8000| HP == 1, 1, 0),
        
        future_IC_0 = ifelse(induction_direct == 1 &
                               induction_dv == 0| IC == 1, 1, 0),
        future_IC_low = ifelse(induction_direct == 1 &
                                 induction_dv <= 300| IC == 1, 1, 0),
        future_IC_high = ifelse(induction_direct == 1 &
                                  induction_dv <= 800| IC == 1, 1, 0)
      ) %>% 
      # mutate(across(matches("future"), ~ ifelse(is.na(.), 0, .))) %>% 
      
      
      # remove variables
      dplyr::select(-pid,-pid_dem,-pid_ind,-pid_rep,
                    -solar_install_owner,-solar_install_renter,-solar_install,-solar_date_owner,-solar_pv_plans,-storage_plans,-storage_own,
                    -vehicle_1_miles,-vehicle_2_miles,-workfromhome,-vehicle_charging_own:-charging_install,-fastcharge_likely,
                    -charging_5mile_non_1:-charging_5mile_own_2,
                    -vehicle_wherecharge_1,-vehicle_wherecharge_2,-vehicle_wherecharge_3,-vehicle_whencharge,
                    -vehicle_next_fuel,
                    
                    -replan_heat_owner:-replan_nocool_renter,-elec_savemoney_rent,-elec_health_renter,
                    -natgas_ypccc,
                    
                    -elec_home_cost_1:-elec_home_cost_4,-elec_home_others_1:-elec_home_who_4,
                    -heatpump_motiv,-induction_appeal,-induction_downsides,
                    
                    -cost_fuel_winter,-cost_electric_winter,-cost_combo_winter_summed,-cost_combo_winter,
                    -cost_combo_summer,-cost_electric_summer,
                    -ccinsure_owner,-ccinsure_renter,
                    
                    -elec_home_cost_PV,
                    -elec_home_cost_EV,-vehicle_next_when,-vehicle_1_class,
                    -elec_home_cost_HP,-elec_home_cost_IC,
                    -cooling_plan,-heating_plan,
                    
                    -solstor_wtp_payback,-heatpump_wtp_payback,-ss_upfr_text,-ssupfront_num,-hpupfront_num,-evupfront_num,
                    -primary_cooling_type_25_TEXT,-primary_heating_type_64_TEXT,-heatenergy_furnace,-heatenergy_boiler,-hotwater_energy_6_TEXT,
                    -heatpump_adopter
                    # -future_EV10
      ) %>%
      relocate(wt_ca, .after = last_col()) %>%
      relocate(tractid, .after = last_col()) %>% 
      dplyr::select(-ExternalReference, -tractid,
                    # remove NAs
                    -induction_direct, -heatpump_direct, -ac_unit_number,-hotwater_nextelec,
                    # remove not significant or collinear variables
                    -charging_access,-charging_5mile_r, -ccinsure_both,-yale_worried,-ideology,-hvac_avail
                    
                    # -charging_work, -vehicle_num,
                    # -born_us, -employment, -education, -race, -gender, -lowincome_sup,
                    # -elec_health, -upfrontpayback, -elec_safety, -ccinsure_lost,
                    # -ccpastmove, -elec_avail, -ccmove_where, -homevac
      ) %>% 
      relocate(wt_ca, .after = last_col())



### without Home own
data <- data %>% 
  dplyr::select(-matches("future"), matches("_0"),-home_own,
                -matches("combo")) # collinear with wtp

categorical_vars <- c(
  "climatezone","dac","gender","race","education","born_us","employment","home_type","home_age","home_own","charging_work",
  "vehicle_next_used","primary_heating_type","hotwater_energy","previous_heating_type",
  "primary_cooling_type","kitchen_range_type",
  "upfrontpayback","lowincome_sup","outage_generatorown",
  "ccinsure_lost","ccmove_where","charging_5mile_f",
  "electrification","peer_EV","peer_PV","peer_HP","peer_IC"
)

for (var in categorical_vars) {
  # Check if var is in the dataframe colnames first
  if (var %in% colnames(data)) { # Check in 'x' now, not 'data'
    data[[var]] <- as.factor(data[[var]])
  }
}


tech <- c("PV","EV","HP","IC","PS")
plot <- list()
tp <- data.frame()
for(k in 2:5){
  
  # without WTP
  tot <- mreg(data, 
               remove = wtp_var,
               i = k,
              future = 0)
  
  # with WTP with subset 
  wtp_w <- mreg(data, 
               i = k,
               future = 0)
  
  # without WTP with subset 
  wtp_wo <- mreg(data %>% 
                   filter(!is.na(!!sym(wtp_var[k]))), 
               remove = wtp_var,
               i = k,
               future = 0)
  
  plot[[k-1]] <- tot[[1]] %>% 
    mutate(class = "Total") %>% 
    rbind(
      wtp_w[[1]] %>% 
        mutate(class = "WTP subset (+WTP)"),
      wtp_wo[[1]] %>% 
        mutate(class = "WTP subset") 
    ) %>% 
    mutate(domain = as.character(domain),
           domain = ifelse(str_detect(var, "peer|Older home|BrandNew|EV range|Newer home|Fast"), "Scenario", domain),
           domain = factor(domain, levels = c("Housing","Heat/Cool","Cook","Vehicle","Resilience","Social","Behavior","Scenario"))) %>% 
    com_reg_plot(tech[[k]])
  
  
  tt <- tot[[1]] %>% 
    mutate(class = "Total") %>% 
    rbind(
      wtp_w[[1]] %>% 
        mutate(class = "WTP subset (+WTP)"),
      wtp_wo[[1]] %>% 
        mutate(class = "WTP subset") 
    ) %>% 
    mutate(domain = as.character(domain),
           domain = ifelse(str_detect(var, "peer|Older home|BrandNew|EV range|Newer home|Fast"), "Scenario", domain),
           domain = factor(domain, levels = c("Housing","Heat/Cool","Cook","Vehicle","Resilience","Social","Behavior","Scenario"))) %>% 
    mutate(tech = ipt[k])
  
  tp <- rbind(tt, tp)

  # reg_plot(wtp_w[[1]], tech[k])

}

# Total, WTP subset + WTP, WTP subset
tp %>% 
  filter(domain == "Scenario") %>% 
  ggplot(aes(x = pe, y = reorder(var, pe), xmin=lwr, xmax=upr)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  geom_pointrangeh(aes(fill = color, color = class), 
                   position = position_dodge2v(height = 0.4), fatten = 4, size = 0.5,
                   pch=21) +
  
  facet_wrap(~ tech, scale = "free", nrow = 2) + 
  theme_bw() +
  
  coord_flip() +
  
  labs(x = "", y ="", fill = "Significant", color = "",
       title = "") +
  
  scale_fill_manual(values=c("black", "white")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=17),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black",family="Franklin Gothic Book",size=9),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20))
  

plot[[1]]


### variable selection
### comparison for WTP 
wtp_var <- c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv","solstor_wtp_dv")
data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
  data_process(ev = c("Fully electric")) %>% 
  data_clean(1) # remove variables with substantial NAs and collinearity


### PS modiviation version 
    data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
      data_process(ev = c("Fully electric")) %>% 
      data_cl() %>% 
      mutate(
        # future adoption
        # future_EV10 = ifelse((vehicle_next_when > 1 & vehicle_next_fuel == 1)| EV == 1, 1, 0),
        future_EV_0 = ifelse((vehicle_next_when == 1 & vehicle_next_fuel == 1 &
                                ev_wtp_pc == 0)| EV == 1, 1, 0),
        future_EV_low = ifelse((vehicle_next_when == 1 & vehicle_next_fuel == 1 &
                                  ev_wtp_pc <= 8000)| EV == 1, 1, 0),
        future_EV_high = ifelse((vehicle_next_when == 1 & vehicle_next_fuel == 1 &
                                   ev_wtp_pc <= 12500)| EV == 1, 1, 0),
        
        future_PV = ifelse(solar_pv_plans %in% c("Yes", "Maybe") | PV == 1, 1, 0),
        future_PS_0 = ifelse((storage_plans %in% c("Yes", "Maybe")) | 
                               (solar_pv_plans %in% c("Yes", "Maybe")) |
                               PS == 1, 1, 0),
        future_PS_low = ifelse((storage_plans %in% c("Yes", "Maybe") & solstor_wtp_dv <= 5000) |
                                 (solar_pv_plans %in% c("Yes", "Maybe") & solstor_wtp_dv <= 5000) |
                                 PS == 1, 1, 0),
        future_PS_high = ifelse((storage_plans %in% c("Yes", "Maybe") & solstor_wtp_dv <= 12500) |
                                  (solar_pv_plans %in% c("Yes", "Maybe") & solstor_wtp_dv <= 12500) |
                                  PS == 1, 1, 0),
        
        future_HP_0 = ifelse(heatpump_direct == 1 & 
                               heatpump_wtp_pc == 0| HP == 1, 1, 0),
        future_HP_low = ifelse(heatpump_direct == 1 & 
                                 heatpump_wtp_pc <= 4000| HP == 1, 1, 0),
        future_HP_high = ifelse(heatpump_direct == 1 & 
                                  heatpump_wtp_pc <= 8000| HP == 1, 1, 0),
        
        future_IC_0 = ifelse(induction_direct == 1 &
                               induction_dv == 0| IC == 1, 1, 0),
        future_IC_low = ifelse(induction_direct == 1 &
                                 induction_dv <= 300| IC == 1, 1, 0),
        future_IC_high = ifelse(induction_direct == 1 &
                                  induction_dv <= 800| IC == 1, 1, 0)
      ) %>% 
      # mutate(across(matches("future"), ~ ifelse(is.na(.), 0, .))) %>% 
      
      
      # remove variables
      dplyr::select(-pid,-pid_dem,-pid_ind,-pid_rep,
                    -solar_install_owner,-solar_install_renter,-solar_install,-solar_date_owner,-solar_pv_plans,-storage_plans,-storage_own,
                    -vehicle_1_miles,-vehicle_2_miles,-workfromhome,-vehicle_charging_own:-charging_install,-fastcharge_likely,
                    -charging_5mile_non_1:-charging_5mile_own_2,
                    -vehicle_wherecharge_1,-vehicle_wherecharge_2,-vehicle_wherecharge_3,-vehicle_whencharge,
                    -vehicle_next_fuel,
                    
                    -replan_heat_owner:-replan_nocool_renter,-elec_savemoney_rent,-elec_health_renter,
                    -natgas_ypccc,
                    
                    -elec_home_cost_1:-elec_home_cost_4,-elec_home_others_1:-elec_home_who_4,
                    -heatpump_motiv,-induction_appeal,-induction_downsides,
                    
                    -cost_fuel_winter,-cost_electric_winter,-cost_combo_winter_summed,-cost_combo_winter,
                    -cost_combo_summer,-cost_electric_summer,
                    -ccinsure_owner,-ccinsure_renter,
                    
                    -elec_home_cost_PV,
                    -elec_home_cost_EV,-vehicle_next_when,-vehicle_1_class,
                    -elec_home_cost_HP,-elec_home_cost_IC,
                    -cooling_plan,-heating_plan,
                    
                    -solstor_wtp_payback,-heatpump_wtp_payback,-ss_upfr_text,-ssupfront_num,-hpupfront_num,-evupfront_num,
                    -primary_cooling_type_25_TEXT,-primary_heating_type_64_TEXT,-heatenergy_furnace,-heatenergy_boiler,-hotwater_energy_6_TEXT,
                    -heatpump_adopter
                    # -future_EV10
      ) %>%
      relocate(wt_ca, .after = last_col()) %>%
      relocate(tractid, .after = last_col()) %>% 
      dplyr::select(-ExternalReference, -tractid,
                    # remove NAs
                    -induction_direct, -heatpump_direct, -ac_unit_number,-hotwater_nextelec,
                    # remove not significant or collinear variables
                    -charging_access,-charging_5mile_r, -ccinsure_both,-yale_worried,-ideology,-hvac_avail
                    
                    # -charging_work, -vehicle_num,
                    # -born_us, -employment, -education, -race, -gender, -lowincome_sup,
                    # -elec_health, -upfrontpayback, -elec_safety, -ccinsure_lost,
                    # -ccpastmove, -elec_avail, -ccmove_where, -homevac
      ) %>% 
      relocate(wt_ca, .after = last_col())


### with Home own
data <- data %>% 
  dplyr::select(-matches("future"), matches("_0"),
                -matches("combo")) # collinear with wtp

categorical_vars <- c(
  "climatezone","dac","gender","race","education","born_us","employment","home_type","home_age","home_own","charging_work",
  "vehicle_next_used","primary_heating_type","hotwater_energy","previous_heating_type",
  "primary_cooling_type","kitchen_range_type",
  "upfrontpayback","lowincome_sup","outage_generatorown",
  "ccinsure_lost","ccmove_where","charging_5mile_f",
  "electrification","peer_EV","peer_PV","peer_HP","peer_IC","home_own"
)

for (var in categorical_vars) {
  # Check if var is in the dataframe colnames first
  if (var %in% colnames(data)) { # Check in 'x' now, not 'data'
    data[[var]] <- as.factor(data[[var]])
  }
}


tech <- c("PV","EV","HP","IC","PS")
plot <- list()
tp1 <- data.frame()
for(k in 2:5){
  
  # with HW
  tot <- mreg(data, 
              remove = wtp_var,
              i = k,
              future = 0)
  
  # without WH and subset 
  wtp_w <- mreg(data %>% 
                  filter(!is.na(!!sym(wtp_var[k]))), 
                remove = c(wtp_var, "home_own"),
                i = k,
                future = 0)
  
  # with HW and subset 
  wtp_wo <- mreg(data %>% 
                   filter(home_own == 1), 
                 remove = c(wtp_var, "home_own"),
                 i = k,
                 future = 0)
  
  plot[[k-1]] <- tot[[1]] %>% 
    mutate(class = "Total (+HO)") %>% 
    rbind(
      wtp_w[[1]] %>% 
        mutate(class = "WTP subset"),
      wtp_wo[[1]] %>% 
        mutate(class = "Homeown subset") 
    ) %>% 
    mutate(domain = as.character(domain),
           domain = ifelse(str_detect(var, "peer|Older home|BrandNew|EV range|Newer home|Fast"), "Scenario", domain),
           domain = factor(domain, levels = c("Housing","Heat/Cool","Cook","Vehicle","Resilience","Social","Behavior","Scenario"))) %>% 
    com_reg_plot(tech[[k]])
  
  
  tt <- tot[[1]] %>% 
    mutate(class = "Total (+HO)") %>% 
    rbind(
      wtp_w[[1]] %>% 
        mutate(class = "WTP subset"),
      wtp_wo[[1]] %>% 
        mutate(class = "Homeown subset") 
    ) %>% 
    mutate(domain = as.character(domain),
           domain = ifelse(str_detect(var, "peer|Older home|BrandNew|EV range|Newer home|Fast"), "Scenario", domain),
           domain = factor(domain, levels = c("Housing","Heat/Cool","Cook","Vehicle","Resilience","Social","Behavior","Scenario"))) %>% 
    mutate(tech = ipt[k])
  
  tp1 <- rbind(tt, tp1)
  
  # reg_plot(wtp_w[[1]], tech[k])
  
}

# Total +HO, homeown subset
tp1 %>% 
  mutate(class = factor(class, levels = c("Total (+HO)", "Homeown subset", "WTP subset"))) %>% 
  filter(domain == "Scenario") %>% 

  ggplot(aes(x = pe, y = reorder(var, pe), xmin=lwr, xmax=upr)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  geom_pointrangeh(aes(fill = color, color = class), 
                   position = position_dodge2v(height = 0.4), fatten = 4, size = 0.5,
                   pch=21) +
  
  facet_wrap(~ tech, scale = "free", nrow = 2) + 
  theme_bw() +
  
  coord_flip() +
  
  labs(x = "", y ="", fill = "Significant", color = "",
       title = "") +
  
  scale_fill_manual(values=c("black", "white")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=17),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black",family="Franklin Gothic Book",size=9),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20))


plot[[1]]

### combine
rbind(
  tp %>% 
    filter(domain == "Scenario"),
  tp1 %>% 
    filter(domain == "Scenario") %>% 
    filter(class != "WTP subset")
) %>% 
  mutate(class = factor(class, levels = c("Total", "Total (+HO)", "Homeown subset", "WTP subset", "WTP subset (+WTP)"))) %>% 
  
  ggplot(aes(x = pe, y = reorder(var, pe), xmin=lwr, xmax=upr)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  geom_pointrangeh(aes(fill = color, color = class), 
                   position = position_dodge2v(height = 0.4), fatten = 4, size = 0.5,
                   pch=21) +
  
  facet_wrap(~ tech, scale = "free", nrow = 2) + 
  theme_bw() +
  
  coord_flip() +
  
  labs(x = "", y ="", fill = "Significant", color = "",
       title = "") +
  
  scale_fill_manual(values=c("black", "white")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=17),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black",family="Franklin Gothic Book",size=9),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20))


# HP
rbind(
  tp %>% 
    filter(domain == "Scenario"),
  tp1 %>% 
    filter(domain == "Scenario") %>% 
    filter(class != "WTP subset")
) %>% 
  mutate(class = factor(class, levels = c("Total", "Total (+HO)", "Homeown subset", "WTP subset", "WTP subset (+WTP)"))) %>% 
  filter(tech == "HP",
         !var %in% c("More EV range","Fast charger",
                     "Newer home","BrandNew home","Older home")) %>% 
  
  ggplot(aes(x = pe, y = reorder(var, pe), xmin=lwr, xmax=upr)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  geom_pointrangeh(aes(fill = color, color = class), 
                   position = position_dodge2v(height = 0.4), fatten = 4, size = 0.5,
                   pch=21) +
  
  facet_wrap(~ tech, scale = "free", nrow = 2) + 
  theme_bw() +
  
  coord_flip() +
  
  labs(x = "", y ="", fill = "Significant", color = "",
       title = "") +
  
  scale_fill_manual(values=c("black", "white")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=17),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black",family="Franklin Gothic Book",size=9),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20))

# IC
rbind(
  tp %>% 
    filter(domain == "Scenario"),
  tp1 %>% 
    filter(domain == "Scenario") %>% 
    filter(class != "WTP subset")
) %>% 
  mutate(class = factor(class, levels = c("Total", "Total (+HO)", "Homeown subset", "WTP subset", "WTP subset (+WTP)"))) %>% 
  filter(tech == "IC",
         !var %in% c("More EV range","Fast charger",
                     "Newer home","BrandNew home","Older home")) %>% 
  
  ggplot(aes(x = pe, y = reorder(var, pe), xmin=lwr, xmax=upr)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  geom_pointrangeh(aes(fill = color, color = class), 
                   position = position_dodge2v(height = 0.4), fatten = 4, size = 0.5,
                   pch=21) +
  
  facet_wrap(~ tech, scale = "free", nrow = 2) + 
  theme_bw() +
  
  coord_flip() +
  
  labs(x = "", y ="", fill = "Significant", color = "",
       title = "") +
  
  scale_fill_manual(values=c("black", "white")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=17),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black",family="Franklin Gothic Book",size=9),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20))

# PS
rbind(
  tp %>% 
    filter(domain == "Scenario"),
  tp1 %>% 
    filter(domain == "Scenario") %>% 
    filter(class != "WTP subset")
) %>% 
  mutate(class = factor(class, levels = c("Total", "Total (+HO)", "Homeown subset", "WTP subset", "WTP subset (+WTP)"))) %>% 
  filter(tech == "PS",
         !var %in% c("More EV range","Fast charger") ) %>% 
  
  ggplot(aes(x = pe, y = reorder(var, pe), xmin=lwr, xmax=upr)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  geom_pointrangeh(aes(fill = color, color = class), 
                   position = position_dodge2v(height = 0.4), fatten = 4, size = 0.5,
                   pch=21) +
  
  facet_wrap(~ tech, scale = "free", nrow = 2) + 
  theme_bw() +
  
  coord_flip() +
  
  labs(x = "", y ="", fill = "Significant", color = "",
       title = "") +
  
  scale_fill_manual(values=c("black", "white")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=17),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black",family="Franklin Gothic Book",size=9),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20))


# EV
rbind(
  tp %>% 
    filter(domain == "Scenario"),
  tp1 %>% 
    filter(domain == "Scenario") %>% 
    filter(class != "WTP subset")
) %>% 
  mutate(class = factor(class, levels = c("Total", "Total (+HO)", "Homeown subset", "WTP subset", "WTP subset (+WTP)"))) %>% 
  filter(tech == "EV") %>% 
  
  ggplot(aes(x = pe, y = reorder(var, pe), xmin=lwr, xmax=upr)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  geom_pointrangeh(aes(fill = color, color = class), 
                   position = position_dodge2v(height = 0.4), fatten = 4, size = 0.5,
                   pch=21) +
  
  facet_wrap(~ tech, scale = "free", nrow = 2) + 
  theme_bw() +
  
  coord_flip() +
  
  labs(x = "", y ="", fill = "Significant", color = "",
       title = "") +
  
  scale_fill_manual(values=c("black", "white")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=17),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black",family="Franklin Gothic Book",size=9),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20))


###################################################################################
### variable selection
### comparison for cost variables
wtp_var <- c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv","solstor_wtp_dv")
data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% 
  data_process(ev = c("Fully electric")) %>% 
  data_clean(1) # remove variables with substantial NAs and collinearity

data <- data %>% 
  dplyr::select(-matches("future"), matches("_0"),-home_own,
                -wtp_var) # collinear with wtp

categorical_vars <- c(
  "climatezone","dac","gender","race","education","born_us","employment","home_type","home_age","home_own","charging_work",
  "vehicle_next_used","primary_heating_type","hotwater_energy","previous_heating_type",
  "primary_cooling_type","kitchen_range_type",
  "upfrontpayback","lowincome_sup","outage_generatorown",
  "ccinsure_lost","ccmove_where","charging_5mile_f",
  "electrification","peer_EV","peer_PV","peer_HP","peer_IC"
)

for (var in categorical_vars) {
  # Check if var is in the dataframe colnames first
  if (var %in% colnames(data)) { # Check in 'x' now, not 'data'
    data[[var]] <- as.factor(data[[var]])
  }
}

cost_rm <- c("cost_combo_winter_final","cost_combo_summer_final")
tech <- c("PV","EV","HP","IC","PS")
plot <- list()
tp <- data.frame()
for(k in 2:5){
  
  # without cost
  tot <- mreg(data, 
              remove = cost_rm,
              i = k,
              future = 0)
  
  # with cost with subset 
  wtp_w <- mreg(data, 
                i = k,
                future = 0)
  
  # without cost with subset 
  wtp_wo <- mreg(data %>% 
                   filter(!if_any(all_of(cost_rm), is.na)),
                 remove = cost_rm,
                 i = k,
                 future = 0)
  
  plot[[k-1]] <- tot[[1]] %>% 
    mutate(class = "Overall") %>% 
    rbind(
      wtp_w[[1]] %>% 
        mutate(class = "WTP with"),
      wtp_wo[[1]] %>% 
        mutate(class = "WTP without") 
    ) %>% 
    mutate(domain = as.character(domain),
           domain = ifelse(str_detect(var, "peer|Older home|BrandNew|EV range|Newer home|Fast"), "Scenario", domain),
           domain = factor(domain, levels = c("Housing","Heat/Cool","Cook","Vehicle","Resilience","Social","Behavior","Scenario"))) %>% 
    com_reg_plot(tech[[k]])
  
  
  tt <- tot[[1]] %>% 
    mutate(class = "Overall") %>% 
    rbind(
      wtp_w[[1]] %>% 
        mutate(class = "Var_cost with"),
      wtp_wo[[1]] %>% 
        mutate(class = "Var_cost without") 
    ) %>% 
    mutate(domain = as.character(domain),
           domain = ifelse(str_detect(var, "peer|Older home|BrandNew|EV range|Newer home|Fast"), "Scenario", domain),
           domain = factor(domain, levels = c("Housing","Heat/Cool","Cook","Vehicle","Resilience","Social","Behavior","Scenario"))) %>% 
    mutate(tech = ipt[k])
  
  tp <- rbind(tt, tp)
  
  # reg_plot(wtp_w[[1]], tech[k])
  
}

tp %>% 
  filter(domain == "Scenario") %>% 
  ggplot(aes(x = pe, y = reorder(var, pe), xmin=lwr, xmax=upr)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  geom_pointrangeh(aes(fill = color, color = class), 
                   position = position_dodge2v(height = 0.4), fatten = 4, size = 0.5,
                   pch=21) +
  
  facet_wrap(~ tech, scale = "free", nrow = 2) + 
  theme_bw() +
  
  coord_flip() +
  
  labs(x = "", y ="", fill = "Significant", color = "",
       title = "") +
  
  scale_fill_manual(values=c("black", "white")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=17),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black",family="Franklin Gothic Book",size=9),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20))


### removing variables 
VAR <- list()
ipt <- c("PV","EV","HP","IC","PS")
future <- 0
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

### final model 
data <- read_csv("./data/raw/cca_15jul2025_weighted.csv") %>% data_process(ev = c("Fully electric")) %>% data_clean(1) %>% 
  dplyr::select(-c("cost_combo_winter_final","cost_combo_summer_final"),
                -c("solstor_wtp_dv","ev_wtp_pc","heatpump_wtp_pc","induction_dv","solstor_wtp_dv"))

tech <- c("PV","EV","HP","IC","PS")
for(k in 2:5){
  
  b_ev <- mreg(data %>% 
                 dplyr::select(VAR[[k-1]], climatezone, dac, matches("PV|PS|EV|HP|IC", ignore.case = FALSE),wt_ca), 
               i = k,
               future = 0)
  
  a <- reg_plot(b_ev[[1]], tech[k])
  b <- zone_plot(b_ev[[2]], "Climate")
  c <- zone_plot(b_ev[[3]], "DAC")
  d <- map_pl(dat_pr(b_ev[[2]], b_ev[[3]]), tech[k])
  
  fig <- ggarrange(a, 
                   ggarrange(b,c,d, nrow = 1),
                   nrow = 2,
                   heights = c(2,1))
  
  ggsave(paste0("./fig/",tech[k],".png"),
         fig,
         width = 12, height = 8)
}


### scenario
effect <- list()
for(i in 2:5){
  future <- c("high","low")
  scene <- c("High","Low")
  
  for(j in 1:2){
    f_d <- final_ef_peer(data %>%
                           dplyr::select(VAR[[i-1]], climatezone, dac, matches("PV|PS|EV|HP|IC"),wt_ca),
                         i, future[j]) %>%
      mutate(class = paste0(ipt[i],"_",scene[j]))
    
    effect <- append(effect, list(f_d))
    
  }
}

df_common <- lapply(effect, function(df) df[, c("GEOID","Effect","Final","class"), drop = FALSE])
combined_df <- bind_rows(df_common)

combined_df_wide <- combined_df %>%
  dplyr::select(-Effect) %>%
  pivot_wider(names_from = class, values_from = Final) %>%
  left_join(mrp %>%
              dplyr::select(GEOID, which(str_detect(names(.), "_0"))), by = "GEOID")

write_csv(combined_df_wide, "./data/result.csv")
save(effect, file = "./data/results.Rdata") # for official sharing
