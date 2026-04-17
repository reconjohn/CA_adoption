source("./syntax/Function.R")
source("./syntax/Data.R")

df <- final %>% 
  separate(class, into = c("tech", "scen"), sep = "_") %>% 
  dplyr::select(-scen) %>% 
  pivot_wider(names_from = tech, values_from = value) %>% 
  mutate(across(where(is.numeric), ~ scale(.) %>% as.numeric())) 


df_long <- df %>%
  pivot_longer(cols = c(PS, IC, HP, EV),
               names_to = "tech",
               values_to = "value")

f5m <- df_long %>%
  filter(!is.na(value)) %>% 
  arrange(desc(value)) %>%              # order by highest value
  mutate(GEOID = factor(GEOID, levels = unique(GEOID)),
         tech = factor(tech, levels = c("PS","EV","HP","IC"))) %>%   # apply ordering
  
  ggplot(aes(x = GEOID, y = tech, fill = value)) +
  geom_tile() +
  scale_fill_scico(palette = "roma") +
  labs(x = NULL, y = "", fill = "Standardized\nAdoption", title = "Technology adoption across census tracts") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),      # remove GEOID labels
    axis.text.y = element_text(size = 13),
    axis.ticks.x = element_blank(),     # remove ticks
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )


f5n <- function() {
  par(mar = c(1, 1, 1, 1)) # Small, uniform margins
  corrplot::corrplot(
    cor(df %>% dplyr::select(PS, IC, HP, EV), use = "complete.obs"),
    method = "color",
    type = "upper",
    addCoef.col = "black",
    tl.col = "black",
    tl.srt = 0,
    diag = FALSE,
    # col = scico::scico(200, palette = "roma")
    col = colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                             "cyan", "#007FFF", "blue", "#00007F"))(200)
  )
}


# Wrap the plots and assign titles through plot_annotation or subtitle tags
p_combined <- (f5m + labs(title = "Technology adoption across census tracts")) + 
  (wrap_elements(panel = ~f5n()) + labs(title = "Correlation among technologies"))

f5y <- p_combined + 
  plot_layout(widths = c(2, 1)) & 
  theme(
    # This ensures both titles sit at the exact same vertical 'ceiling'
    plot.title = element_text(family = "Franklin Gothic Demi", size = 16, hjust = 0),
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5)
  )



# pca <- prcomp(df %>% dplyr::select(PS, IC, HP, EV) %>% 
#                 na.omit(), scale. = TRUE)
# 
# # Scores for each GEOID
# scores <- as.data.frame(pca$x) 
# 
# # Loadings (for arrows)
# loadings <- as.data.frame(pca$rotation) %>%
#   rownames_to_column("tech")
# 
# var_exp <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
# 
# # --- Plot ---
# f5k <- ggplot() +
#   # Points
#   geom_point(
#     data = scores,
#     aes(PC1, PC2),
#     size = 0.1,
#     color = "gold4",
#     alpha = 0.2
#   ) +
# 
#   # Loadings arrows
#   geom_segment(
#     data = loadings,
#     aes(x = 0, y = 0, xend = PC1 * 3, yend = PC2 * 3),
#     arrow = arrow(length = unit(0.25, "cm")),
#     linewidth = 1,
#     color = "black"
#   ) +
# 
#   # Loading labels
#   geom_text_repel(
#     data = loadings,
#     aes(x = PC1 * 3, y = PC2 * 3, label = tech),
#     size = 5,
#     fontface = "bold"
#   ) +
# 
#   # Axis labels with variance explained
#   labs(
#     x = paste0("PC1 (", var_exp[1], "%)"),
#     y = paste0("PC2 (", var_exp[2], "%)"),
#     title = "PCA Biplot"
#   ) +
# 
#   coord_equal() +
#   theme_minimal(base_size = 12) +
#   theme(
#     panel.grid = element_line(color = "gray85"),
#     plot.title = element_text(face = "bold", size = 14)
#   )


# for mapping 
CA_c <- CA_t %>% 
  mutate(county = gsub("\\d{6}$", "", GEOID)) %>% 
  group_by(county) %>% 
  summarise(geometry = st_union(geometry))


### mapping
mmp <- CA_t %>% 
  dplyr::select(GEOID) %>% 
  left_join(final %>% 
              separate(class, into = c("tech", "scen"), sep = "_"), by = "GEOID") %>% 
  
  filter(tech != "PV") %>% 
  filter(scen == "high") %>% 
  mutate(
    tech = recode(tech,
                  "PS" = "PV + Storage",
                  "IC" = "Induction stoves",
                  "HP" = "Heat pumps",
                  "EV" = "Electric vehicles",
                  "PV" = "Photovoltaics"),
    tech = factor(tech, levels = c("Photovoltaics","PV + Storage", "Electric vehicles", "Heat pumps", "Induction stoves"))) %>%
  na.omit()


### moran's I 
moran_by_class <- mmp %>%
  group_split(tech) %>%
  map_df(function(df) {
    # Remove rows with NA in value
    df_clean <- df %>% filter(!is.na(value))
    
    # Skip if fewer than 2 observations
    if (nrow(df_clean) < 2) {
      return(tibble(
        tech = unique(df$tech),
        SI = NA_real_,
        p_value = NA_real_,
        note = "Too few observations"
      ))
    }
    
    
    # Create spatial weights for this subset
    nb <- poly2nb(df_clean, queen = TRUE)
    lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
    
    # Run Moran's I permutation test
    test <- moran.mc(df_clean$value, listw = lw, nsim = 999, zero.policy = TRUE)
    
    # Return results
    tibble(
      tech = unique(df$tech),
      SI = test$statistic,
      p_value = test$p.value,
      note = NA_character_
    )
  })

tech_name <- c("PV + Storage", "Electric vehicles", "Heat pumps", "Induction stoves")
mmap <- list()
for(i in 1:4){
  mmap[[i]] <- mmp %>% 
    filter(tech == tech_name[i]) %>% 
    
    ggplot() +
    geom_sf(fill = "white", color = "gray0",) + # US border
    geom_sf(aes(fill = value), color = NA, size = 0.3) +
    scale_fill_scico(palette = "lajolla", name = "Adoption") +
    
    geom_sf(data = CA_c, fill = NA, color = "gray90", linewidth = 0.1) +
    annotate("text", x = -120, y = 41, 
             label = paste0("SC: ",moran_by_class$SI[[i]] %>% 
                              round(2)),
             hjust = -0.2, vjust = 1.2, size = 5) +
  
    theme_minimal() +
    labs(title = tech_name[i], fill = "Adoption", x = "", y = "") +
    
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      plot.title = element_text(face = "bold", size = 16),
      
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      legend.text = element_text(size = 10),
      legend.position = "bottom",
      
      strip.placement = "outside", # Keep labels on the outside
      # strip.background =element_rect(fill="gray22",color="gray22"),
      strip.text = element_text(color = 'black',family="Franklin Gothic Book",size=12, face = "bold"),
      strip.text.y.left = element_text(angle = 0), # Ensure domain labels are horizontal
      
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()  )
  
}

f5a <- ggarrange(plotlist = mmap, nrow = 1)


# f5b <- moran_by_class %>%
#   dplyr::select(-note) %>% 
#   mutate(
#     sig = p_value < 0.05
#   ) %>% 
#   ggplot(aes(x = tech, y = SI, fill = sig)) +
#   geom_col() +
#   scale_fill_manual(values = c("FALSE" = "grey70", "TRUE" = "tomato")) +
#   labs(
#     title = "Spatial Clustering",
#     x = "",
#     y = "Moran's I",
#     fill = "Significant (p < 0.05)"
#   ) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.major.x = element_blank(),
#         legend.position = "bottom")


map_dac <- CA_t %>% 
  dplyr::select(GEOID) %>% 
  left_join(final %>% 
              separate(class, into = c("tech", "scen"), sep = "_"), by = "GEOID") %>% 
  
  filter(tech != "PV") %>% 
  filter(scen == "high") %>% 
  mutate(
    tech = recode(tech,
                  "PS" = "PV + Storage",
                  "IC" = "Induction stoves",
                  "HP" = "Heat pumps",
                  "EV" = "Electric vehicles",
                  "PV" = "Photovoltaics"),
    tech = factor(tech, levels = c("Photovoltaics","PV + Storage", "Electric vehicles", "Heat pumps", "Induction stoves"))) %>%
  na.omit() %>% 
  
  st_intersection(dac_sf %>% 
                    filter(sample == 1)) %>% 
  st_collection_extract("POLYGON") %>%  # Remove geometry collections
  filter(st_area(.) > units::set_units(1, "m^2")) %>%  # Remove tiny fragments
  mutate(DAC = "DAC") %>% 
  rbind(
    CA_t %>% 
      dplyr::select(GEOID) %>% 
      left_join(final %>% 
                  separate(class, into = c("tech", "scen"), sep = "_"), by = "GEOID") %>% 
      
      filter(tech != "PV") %>% 
      filter(scen == "high") %>% 
      mutate(
        tech = recode(tech,
                      "PS" = "PV + Storage",
                      "IC" = "Induction stoves",
                      "HP" = "Heat pumps",
                      "EV" = "Electric vehicles",
                      "PV" = "Photovoltaics"),
        tech = factor(tech, levels = c("Photovoltaics","PV + Storage", "Electric vehicles", "Heat pumps", "Induction stoves"))) %>%
      na.omit() %>% 
      
      st_intersection(dac_sf %>% 
                        filter(sample == 0)) %>% 
      st_collection_extract("POLYGON") %>%  # Remove geometry collections
      filter(st_area(.) > units::set_units(1, "m^2")) %>%  # Remove tiny fragments
      mutate(DAC = "Non-DAC")
  )


tech_name <- c("PV + Storage", "Electric vehicles", "Heat pumps", "Induction stoves")
dac_map <- list()
for(i in 1:4){
  dac_map[[i]] <- map_dac %>% 
    filter(tech == tech_name[i]) %>% 
    mutate(value_centered = value - median(value, na.rm = TRUE)) %>% 
  
    ggplot() +
    geom_sf(fill = "white", color = "gray0") + # US border
    geom_sf(aes(fill = value_centered), color = NA, size = 0.3) +
    # scale_fill_viridis_c(option = "magma", name = "Adoption") +
    
    scale_fill_scico(
      palette = "roma",
      name = "Deviation\nfrom Median",
      midpoint = 0
    ) +
      # scale_fill_gradient2(
      #   low = "#D73027",
      #   mid = "white",
      #   high = "#4575B4",
      #   
      #   midpoint = 0,
      #   name = "Deviation\nfrom Mean"
      # ) +
      
    # new_scale_fill() +
    
    geom_sf(data = CA_c, fill = NA, color = "black", linewidth = 0.05) +
    # geom_sf(data = dac_sf %>% 
    #           filter(sample == 1) %>% 
    #           mutate(sample = "DAC"), aes(fill = sample), color = NA, alpha = 0.3) +
    # scale_fill_manual(values = c("DAC" = "gray80"),
    #                   name = "") +
    
    # 
    # annotate("text", data = G, x = -120, y = 41, 
    #          label = Gini,
    #          hjust = -0.2, vjust = 1.2, size = 5) +
    
    facet_wrap(~DAC, nrow = 2) +
    
    theme_minimal() +
    # scale_fill_distiller(palette = "RdBu", direction = -1) +
    # scale_fill_viridis_c(option = "magma") +
    
    labs(title = tech_name[i], fill = "Adoption") +
    
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      plot.title = element_text(face = "bold", size = 16),
      
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      legend.text = element_text(size = 10),
      legend.position = "bottom",
      
      strip.placement = "outside", # Keep labels on the outside
      # strip.background =element_rect(fill="gray22",color="gray22"),
      strip.text = element_text(color = 'black',family="Franklin Gothic Book",size=12, face = "bold"),
      strip.text.y.left = element_text(angle = 0), # Ensure domain labels are horizontal
      
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()  )
  
}

f5c <- ggarrange(plotlist = dac_map, nrow = 1)

ggsave("./fig/f5.png",
       ggarrange(f5a,
                 f5c,
                 f5y,
                 nrow = 3, 
                 heights = c(1.2,2,1.2),
                 labels = c("A", "B", "C"),  # Adds labels to plots
                 label.x = 0,        # Adjust horizontal position of labels
                 label.y = 1,        # Adjust vertical position of labels
                 vjust = 1,
                 hjust = -1,
                 font.label = list(size = 14, face = "bold")),
       width = 12, height = 14)



s14 <- map_dac %>% 
  st_drop_geometry() %>% 
  group_by(tech, DAC) %>% 
  summarise(SD = sd(value),
            Gini = ineq(value, type = "Gini")) %>% 
  mutate(
    tech = factor(tech, levels = unique(tech)),
    DAC = factor(DAC, levels = c("DAC", "Non-DAC"))
  ) %>% 
  gather(key, value, SD,Gini) %>% 
  ggplot(aes(x = tech, y = value, fill = DAC, group = DAC)) +
  geom_col(position = "dodge") +
  facet_wrap(~key, scales = "free_y", nrow = 1) +
  labs(
    title = "Disparity index",
    x = "",
    y = "Disparity",
    fill = ""
  ) +
  scale_fill_manual(values = c("#C2A385","#4A6273")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        strip.background =element_rect(fill="gray22",color="gray22"),
        axis.text.x = element_text(size = 12, angle = 30, hjust = 1),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=14, face = "bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())

ggsave("./fig/s14.png",
       s14,
       width = 12, height = 6)


select_var <- list(c("peer_effect"),
                   c("peer_effect","rangeanxiety"),
                   c("peer_effect"),
                   c("peer_effect"),
                   c("peer_effect"))
plot <- list()
for(i in 1:4){
  plot[[i]] <- CA_t %>% 
    dplyr::select(GEOID) %>% 
    left_join(effect[[i]] %>% 
                mutate(
                  peer_effect = rowSums(dplyr::select(., all_of(names(.)[str_detect(names(.), "^peer")])), na.rm = TRUE)
                  # home_age = rowSums(dplyr::select(., all_of(names(.)[str_detect(names(.), "^home_age")])), na.rm = TRUE)
                ) %>%
                dplyr::select(-ends_with(c("New","Older","peer","none"))), 
              by = "GEOID") %>% 
    na.omit() %>% 
    pivot_longer(cols = select_var[[i+1]], names_to = "key", values_to = "value") %>% 
    ggplot() +
    geom_sf(fill = "white", color = "gray0") + # US border
    geom_sf(aes(fill = value), color = NA, size = 0.3) +
    facet_grid(class~key, switch = "y") +
    
    theme_minimal() +
    # scale_fill_distiller(palette = "RdBu", direction = -1) +
    scale_fill_viridis_c(option = "magma") +
    
    labs(title = paste0("Margianl effect on ", tech[[i+1]]), fill = "") +
    theme(legend.position = "right",
          # legend.text=element_text(size=6),
          # legend.key.size = unit(0.3, 'cm'),
          strip.text = element_text(size = 14, face = "bold"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.title=element_text(family="Franklin Gothic Demi", size=15, hjust = 0))

}

ggsave("./fig/s13.png",
       ggarrange(plot[[4]], plot[[1]],plot[[2]],plot[[3]]),
       width = 12, height = 6)
