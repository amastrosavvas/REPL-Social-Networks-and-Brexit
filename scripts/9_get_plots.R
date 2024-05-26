# LOAD PACKAGES  ---------------------------------------------------------------

packages <-
  c(
    "dplyr",
    "tidyr",
    "sf",
    "ggplot2",
    "ggrepel",
    "gridExtra",
    "reshape2",
    "gtable",
    "expss"
  )

for (p in packages){ 
  if (! (p %in% installed.packages())){
    install.packages(p)
  }
}

library(dplyr)   
library(ggplot2)  
library(ggrepel)
library(gridExtra)
library(reshape2)
library(gtable)
library(sf)   


itl321x_sf <- readRDS(file = "./output/geometries/itl321x_sf.RDS")

regdat <- 
  readRDS(file = "./output/datasets/main/regdat.RDS") %>%
  mutate(
    across(c(ITL321XCD, ITL321XNM), ~as.character(.)),
    across(everything(), ~expss::unlab(.))
  )

nndat <- 
  readRDS(file = "./output/datasets/nndat.RDS") %>%
  mutate(
    across(everything(), ~expss::unlab(.))
  ) 
  

colfunc <- colorRampPalette(c("#800000", "#faf2f2")) # Palette  


# LCD PLOTS  -------------------------------------------------------------------

# Prepare underlying data for fig a/b/c LCD plots
nndat_LCD <-
  nndat %>%
  select(ITL321XCD_O, ITL321XCD_D,
         n_LCDi, n_SCI, n_SCI_c,
         LCDi, LCD, SCI, SCI_c
  ) %>% 
  mutate(
    LCDi_c = ifelse(is.na(SCI_c), NA, LCDi)
  ) %>%
  group_by(ITL321XCD_O) %>%
  mutate(
    n_LCDi_c = rank(-LCDi_c, ties.method = "first"),
    n_LCDi_c = ifelse(is.na(n_LCDi_c), 0, n_LCDi_c),
    n_SCI_c = ifelse(is.na(n_SCI_c), 0, n_SCI_c),
    same_sci_LCD = ifelse(n_SCI == n_LCDi, 1, 0),
    same_sci_LCD_c = ifelse(n_SCI_c == n_LCDi_c, 1, 0)
  ) %>%
  ungroup() %>%
  mutate(
    across(-contains("ITL321X"), ~as.numeric(.))
  )

# Prepare  data for fig a/b LCD plots
nndat_LCD_figab <-
  reshape2::melt(nndat_LCD, id.vars = 1:2) %>% # Arrange by nn-region-pair-measure indicator
  filter(grepl("^n_", variable)) %>%
  mutate(
    variable = gsub("n_", "", variable)
  ) %>% 
  rename(
    nn = value
  )  %>%
  left_join( # Append values for each measure
    melt(nndat_LCD, id.vars = 1:2),
    by = c("ITL321XCD_O", "ITL321XCD_D", "variable")
  ) %>%
  filter(nn >= 1 & nn <=10) %>% # Restrict to 10 nearest neighbours
  left_join( # Append values for geographic distance variable in km
    nndat_LCD[, c("ITL321XCD_O", "ITL321XCD_D", "LCD")]
  ) %>%
  rename(LCD_values = LCD, measure = variable) %>%
  mutate(LCD_values = LCD_values/1000) %>%
  group_by(ITL321XCD_O, measure) %>% # Get relative to max values in each region-measure
  mutate(
    max_values = max(value)
  ) %>%
  ungroup() %>%
  mutate(
    rel_to_max_values = value/max_values
  )

nndat_LCD_figab_means <- # Get means/CIs by nn-ind
  nndat_LCD_figab %>%
  group_by(nn, measure) %>%
  summarise(
    mean_LCD_values = mean(LCD_values),
    se_LCD_values = 1.96*sd(LCD_values)/sqrt(142),
    mean_rel_to_max_values = mean(rel_to_max_values),
    se_rel_to_max_values =  1.96*sd(rel_to_max_values)/sqrt(142)
  ) %>%
  ungroup()


nndat_LCD_figab_means <- # Rearrange for plotting
  nndat_LCD_figab_means %>%
  tidyr::pivot_longer(cols =contains("mean_"), names_to = "values.ind", values_to = "mean.values") %>%
  tidyr::pivot_longer(cols =contains("se_"), names_to = "values.ind2", values_to = "se.values") %>%
  mutate(
    values.ind = gsub("mean_", "", values.ind),
    values.ind2 = gsub("se_", "", values.ind2),
  ) %>%
  filter(values.ind == values.ind2) %>%
  select(-values.ind2) %>%
  mutate(
    ciu.values = mean.values + se.values,
    cil.values = mean.values - se.values,
    values.ind = factor(
      ifelse(
        grepl("LCD", values.ind), 
        "Least-cost geographic distance (km)\n", 
        "Weight relative to 1st neighbour\n")
      , 
      levels = c("Least-cost geographic distance (km)\n", "Weight relative to 1st neighbour\n")),
    measure = factor(measure, levels = c("SCI_c", "LCDi_c", "SCI", "LCDi"))
  )


ggplot(nndat_LCD_figab_means, aes(x = nn, y = mean.values, ymin = cil.values, ymax = ciu.values, linetype = measure, colour = measure)) +
  geom_line(linewidth = 0.56) +
  facet_wrap(~values.ind, nrow = 2, strip.position = "left", scales = "free_y")  +
  geom_ribbon(alpha = 0.11, linetype = 0) +
  theme_bw() +
  xlab("\nNeighbour rank") +
  ylab(NULL) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    panel.spacing.y = unit(0.5, "lines"),
    strip.text.y = element_text(size = 10, colour = "black"),
    axis.title.x =element_text(size = 10, colour = "black"),
    panel.grid.minor = element_blank(),
    legend.title=element_blank(),
    legend.position = "bottom"
  )  +
  scale_x_continuous(limits = c(1, 10), breaks = seq(1, 10, 1)) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(7), limits = c(0, NA), expand = expansion(mult = c(0.05, 0.1)),
    labels = function(x) sub("$.0$", "", x)
  ) +
  scale_color_manual(
    values = c("#800000", "#800000", "black", "black"),
    labels = c(expression("SCI"["(-c)"]), expression("1/GEO"["(-c)"]), "SCI", "1/GEO"),
    guide = guide_legend(label.hjust = 0, title = "Measure")
  ) +
  scale_linetype_manual(
    values = c("solid", "dashed", "solid", "dashed"),
    labels = c(expression("SCI"["(-c)"]), expression("1/GEO"["(-c)"]), "SCI", "1/GEO"),
    guide = guide_legend(label.hjust = 0, title = "Measure")
  )

ggsave(filename = "./output/plots/lcdplot.pdf", width = 100, height = 160, dpi = 600, units = "mm", device='pdf')

# Prepare data for fig c LCD plot
LCD_figc_df <- list()

LCD_figc_df[[1]] <-
  nndat_LCD %>%
  select(ITL321XCD_O, ITL321XCD_D, same_sci_LCD, n_SCI) %>%
  rename(nn = n_SCI) %>%
  group_by(nn) %>%
  summarise(
    same_sci_LCD = sum(same_sci_LCD)/142
  ) %>%
  ungroup() 

LCD_figc_df[[2]] <-
  nndat_LCD %>%
  select(ITL321XCD_O, ITL321XCD_D, same_sci_LCD_c, n_SCI_c) %>%
  rename(nn = n_SCI_c) %>%
  group_by(nn) %>%
  summarise(
    same_sci_LCD_c = sum(same_sci_LCD_c)/142
  ) %>%
  ungroup() 

LCD_figc_df <- 
  left_join(LCD_figc_df[[1]], LCD_figc_df[[2]]) 

LCD_figc_df <-
  data.frame(LCD_figc_df[1:1], stack(LCD_figc_df[2:3])) %>%
  filter(!is.na(values), nn %in% 1:10)

ggplot(
  data = LCD_figc_df,
  aes_string(x = "nn", y = "values", group = "ind",  fill = "ind")
) +
  geom_area(alpha = 0.5, position = "identity") +
  scale_x_continuous(limits = c(1, 10), breaks = seq(1, 10, 1), minor_breaks = seq(1, 10, 1))  +
  scale_y_continuous(
    limits = c(0, 0.7),
    breaks = seq(0, 1, 0.1),
    labels = as.character(seq(0, 1, 0.1)),
    minor_breaks = NULL
  ) +
  scale_fill_manual(
    values = c("lightgray", "#800000"),
    labels = c("[SCI, 1/GEO]", expression(paste("[SCI"["(-c)"], ", ", "1/GEO"["(-c)"], "]")))
  ) +
  theme_bw() + 
  labs(y = "Share of overlapping neighbours\n", x = "\nNeighbour rank")  +
  theme( 
    legend.position = "bottom",
    legend.justification = "top",
    legend.text.align = 0,
    legend.title = element_blank(), 
    axis.title = element_text(size=10, colour = "black")
  ) 

ggsave(filename = "./output/plots/ovlplot.pdf", width = 100, height = 100, dpi = 600, units = "mm", device='pdf')

# LAGGED SHOCK - LEAVE PLOT  ---------------------------------------------------

relplot <- 
  regdat %>%
  select(ITL321XCD, ITL321XNM, pc_leave, rshk_GBR, lgrshk_GBR_w1_5_SCI_c, zones_ha) %>%
  arrange(zones_ha) %>%
  mutate(
    zones_ha = factor(zones_ha, levels = paste0("Z", 1:18)),
    rshk_GBR = rshk_GBR/1000,
    lgrshk_GBR_w1_5_SCI_c = lgrshk_GBR_w1_5_SCI_c/1000
  ) %>%
  tidyr::pivot_longer(names_to = "variable", cols = c("pc_leave", "rshk_GBR"))


ggplot(relplot, aes(x = lgrshk_GBR_w1_5_SCI_c, y = value)) +
  geom_point(data = relplot) +
  facet_wrap(~variable, nrow = 2, strip.position = "left", scales = "free_y", labeller = as_labeller(c(pc_leave = "Leave vote share (%) \n", rshk_GBR = "\n Res. import shock: Within-region \n (£ thousands per worker) \n") ) ) +
  geom_smooth(method = "lm", se = F, color = "#990000") +
  theme_bw() +
  theme(
   panel.grid.minor = element_blank(),
   panel.grid.major = element_blank(),
   axis.title.x = element_text(size = 9),
   axis.title.y = element_text(size = 9),
   strip.text = element_text(size = 9),
   strip.background = element_blank(),
   strip.placement = "outside"
  ) +
  labs(
    y = "",
    x = "\n Res. import shock: Social neighbours \n (£ thousands per worker)"
  )


# ggsave(filename = "./output/plots/relplot.pdf", width = 130, height = 200, dpi = 600, units = "mm", device='pdf')

ggsave(filename = "./output/plots/relplot.pdf", width = 100, height = 150, dpi = 600, units = "mm", device='pdf')


# COMMUTING ZONE MAP -----------------------------------------------------------

czmap_sf <-
  itl321x_sf %>%
  left_join(regdat[, c("ITL321XCD", "zones_ha")], by = "ITL321XCD") %>%
  group_by(zones_ha) %>%
  summarise() %>%
  ungroup()

itl21x_outline_sf <-
  itl321x_sf %>%
  summarise()

ggplot() +
  geom_sf(
    data = itl321x_sf,
    color = "white",
    size =.2
  ) +
  theme_void() +
  geom_sf(
    data= czmap_sf,
    color="#800000",
    alpha = 0,
    size=.18
  ) +
  geom_sf(
    data= itl21x_outline_sf,
    color="lightgray",
    alpha = 0,
    size=.18
  ) +
  geom_sf_text(
    data = czmap_sf, 
    aes(label=zones_ha),
    color="#800000", 
    size=2,
    fontface = "bold",
    family="sans"
  ) 

ggsave(filename = "./output/plots/zonemap.pdf", width = 120, height = 130, dpi = 600, units = "mm", device='pdf')


# COMMUTING ZONE PLOTS ---------------------------------------------------------

zonedat <-
  regdat %>%
  select(ITL321XCD, zones_ha, Electorate) %>%
  group_by(zones_ha) %>%
  summarise(
    zonesn = n(),
    zoneselec = sum(Electorate/10^6)
  ) 

zonedat <- 
  data.frame(zonedat[1], stack(zonedat[3:2])) %>%
  mutate(
    ind = case_when(
      ind == "zonesn" ~ "\nHITL3 regions (count)",
      ind == "zoneselec" ~ "\nElectorate, 2016 (millions)"
    )
  )

ggplot(zonedat) +
  geom_bar(aes(x = values, y = factor(zones_ha, levels = rev(paste0("Z", 1:18)))), stat = "identity") +
  facet_wrap(~ind, ncol = 2, strip.position = "bottom", scales = "free_x") +
  ylab("Commuting zone\n") +
  xlab(NULL) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size= 10, colour = "black"),
    axis.title.y =element_text(size = 10 , colour = "black"),
    panel.spacing.x = unit(0.5, "lines"),
    panel.grid.minor = element_blank()
  )  +
  scale_x_continuous(breaks = scales::pretty_breaks(7), limits = c(0, NA), expand = expansion(mult = c(0.05, 0.1)))


ggsave(filename = "./output/plots/zonebars.pdf", width = 110, height = 105, dpi = 600, units = "mm", device='pdf')

# SCI MAPS ---------------------------------------------------------------------

# maxmin_shocked <- list()

# maxmin_shocked[["max"]] <-
#  regdat %>%
#  slice_max(order_by = rshk_GBR, n = 1) %>%
#  pull(ITL321XNM)

# maxmin_shocked[["md"]] <-
#  regdat %>%
#  filter(rshk_GBR == median(rshk_GBR)) %>%
#  pull(ITL321XNM)

# maxmin_shocked[["min"]] <-
#  regdat %>%
#  slice_min(order_by = rshk_GBR, n = 1) %>%
#  pull(ITL321XNM)

#maxmin_shocked <- unname(unlist(maxmin_shocked))

nn5dat <-
  nndat %>%
  select(
    ITL321XCD_O, ITL321XNM_O, ITL321XNM_D, ITL321XCD_D, 
    n_SCI, n_SCI_c
  ) %>% 
  filter(
    ITL321XNM_O  %in% c("Bridgend and Neath Port Talbot", "Chorley and West Lancashire", "Kensington & Chelsea and Hammersmith & Fulham"), #maxmin_shocked
    n_SCI <= 5 | n_SCI_c <= 5 
  )  %>% 
  tidyr::gather(ind, nn, contains("n_")) %>%
  filter(nn <= 5) %>%
  select(-nn)

nndat2 <-
  nn5dat %>%
  left_join(
    itl321x_sf[, c("ITL321XCD", "geometry")],
    by = c("ITL321XCD_D" = "ITL321XCD")
  ) %>%
  st_as_sf %>%
  mutate(
    ind = factor(ind, levels = c("n_SCI", "n_SCI_c")),
    ITL321XNM_O = case_when(
      ITL321XNM_O == "Bridgend and Neath Port Talbot" ~ "'Bridgend and\nNeath Port Talbot'",
      ITL321XNM_O == "Chorley and West Lancashire" ~ "'Chorley and\nWest Lancashire'",
      ITL321XNM_O == "Kensington & Chelsea and Hammersmith & Fulham" ~ "'Kensington & Chelsea and\nHammersmith & Fulham'",
      TRUE ~ ITL321XNM_O
    )
  ) 

nndat3 <-
  nn5dat %>%
  select(-ITL321XCD_D, -ITL321XNM_D) %>%
  left_join(
    itl321x_sf[, c("ITL321XCD", "geometry")],
    by = c("ITL321XCD_O" = "ITL321XCD")
  ) %>%
  st_as_sf %>%
  mutate(
    ind = factor(ind, levels = c("n_SCI", "n_SCI_c")),
    ITL321XNM_O = case_when(
      ITL321XNM_O == "Bridgend and Neath Port Talbot" ~ "'Bridgend and\nNeath Port Talbot'",
      ITL321XNM_O == "Chorley and West Lancashire" ~ "'Chorley and\nWest Lancashire'",
      ITL321XNM_O == "Kensington & Chelsea and Hammersmith & Fulham" ~ "'Kensington & Chelsea and\nHammersmith & Fulham'",
      TRUE ~ ITL321XNM_O
    )
  )

levels(nndat2$ind) <- c("SCI", expression("SCI"["(-c)"]))
levels(nndat3$ind) <- c("SCI", expression("SCI"["(-c)"]))

ggplot() +
  geom_sf(
    data=itl321x_sf,
    color= "white"
  ) +
  theme_void() +
  geom_sf(
    data=nndat2,
    color= NA,
    fill="black",
    alpha = 1
  ) +
  geom_sf(
    data = nndat3,
    color= NA,
    fill="#800000", 
    alpha = 1
  ) +
  facet_grid(
    ITL321XNM_O~ind,
    switch = "y",
    labeller = label_parsed
  ) +
  theme(
    strip.text.y = element_text(size = 10, color = "black", margin = margin(0, 0, 0, 0.35, "cm"), angle = 90),
    strip.text.x = element_text(size = 10, color = "black"),
    plot.margin=unit(c(0,-10,0,-10), "cm"),
    panel.spacing.y = unit(-1, "lines")
  )

ggsave(filename = "./output/plots/neighbourmap.pdf", width = 140, height = 190, dpi = 600, units = "mm", device='pdf')


# WORKPLACE VS RESIDENTIAL SHOCK SCATTERPLOT  ----------------------------------

regdat <-
  regdat %>%
  mutate(
    rshk_GBR_outliers = abs(shk_GBR - rshk_GBR),
    rshk_GBR_outliers = ntile(rshk_GBR_outliers, 100)
  )

ggplot(regdat, aes(y = shk_GBR/1000, x = rshk_GBR/1000)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, lty = 2) + 
  scale_y_continuous(
    name = "Import shock (£ thousands per worker) \n", 
    limits = c(0, 3.5),
    breaks = seq(0, 3.5, by = 0.5),
    labels = gsub(".0", "", as.character(seq(0, 3.5, by = 0.5)))
  ) + 
  scale_x_continuous(
    name = "\nRes. import shock (£ thousands per worker)", 
    limits = c(0, 3.5),
    breaks = seq(0, 3.5, by = 0.5),
    labels = gsub(".0", "", as.character(seq(0, 3.5, by = 0.5)))
  ) +
  geom_text_repel(
    aes(label = ITL321XNM), 
    data = regdat[regdat$rshk_GBR_outliers >= 95,],
    size = 3,
    min.segment.length = 0,
    segment.color = "NA",
    color = "grey40",
    nudge_y = case_when(
      regdat[regdat$rshk_GBR_outliers >= 95,]$ITL321XNM == "Bridgend and Neath Port Talbot" ~ 0,
      regdat[regdat$rshk_GBR_outliers >= 95,]$ITL321XNM == "South Nottinghamshire" ~ -0,
      regdat[regdat$rshk_GBR_outliers >= 95,]$ITL321XNM == "South Hampshire" ~ 0.05,
      regdat[regdat$rshk_GBR_outliers >= 95,]$ITL321XNM == "Central Valleys" ~ 0,
      regdat[regdat$rshk_GBR_outliers >= 95,]$ITL321XNM == "Enfield" ~ 0,
      regdat[regdat$rshk_GBR_outliers >= 95,]$ITL321XNM == "Walsall" ~ 0,
      TRUE ~ 0
    ),
    nudge_x = case_when(
      regdat[regdat$rshk_GBR_outliers >= 95,]$ITL321XNM == "Bridgend and Neath Port Talbot" ~ -0.62,
      regdat[regdat$rshk_GBR_outliers >= 95,]$ITL321XNM == "South Nottinghamshire" ~ 0.35,
      regdat[regdat$rshk_GBR_outliers >= 95,]$ITL321XNM == "South Hampshire" ~ -0.45,
      regdat[regdat$rshk_GBR_outliers >= 95,]$ITL321XNM == "Central Valleys" ~ -0.3,
      regdat[regdat$rshk_GBR_outliers >= 95,]$ITL321XNM == "Enfield" ~ -0.3,
      regdat[regdat$rshk_GBR_outliers >= 95,]$ITL321XNM == "Walsall" ~ -0.15,
      TRUE ~ 0
    )
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size=10, colour = "black"),
    panel.border = element_rect(colour = "black", fill=NA)
  ) 

ggsave(filename = "./output/plots/adjshockplot.pdf", width = 100, height = 85, dpi = 600, units = "mm", device='pdf')


# LEAVE - SHOCK - LAGGED SHOCK MAPS --------------------------------------------

# Join voting and shock data with NUTS3X boundaries
trimap_sf <-
  itl321x_sf %>%
  left_join(
    regdat[, 
      c(
        "ITL321XCD", 
        "pc_leave", 
        "rshk_GBR", 
        "lgrshk_GBR_w1_5_SCI_cZ"
      )
    ], 
    by = "ITL321XCD"
  )

trimap_sf <- # Create deciles for fill variables
  trimap_sf %>%
  mutate(
    across(
      c( "pc_leave", "rshk_GBR", "lgrshk_GBR_w1_5_SCI_cZ"), 
      ~ntile(., 10),
      .names = "dc_{.col}"
    )
  ) 

trimap_sf <- # Gather, rename, and reorder fill variables
  trimap_sf %>%
  tidyr::gather(ind, decile, contains("dc_")) %>%
  mutate(
    ind = case_when(
      ind == "dc_pc_leave" ~ "Leave vote share",
      ind == "dc_rshk_GBR" ~ "Res. import shock:\nWithin-region\n",
      ind == "dc_lgrshk_GBR_w1_5_SCI_cZ" ~ "Res. import shock:\nSocial neighbours\n",
      TRUE ~ ind
    )
  ) %>%
  select(-pc_leave, -rshk_GBR, -lgrshk_GBR_w1_5_SCI_cZ)
  
trimap_sf$ind <-
  factor(
    trimap_sf$ind,
    levels = c(
      "Res. import shock:\nWithin-region\n",
      "Res. import shock:\nSocial neighbours\n",
      "Leave vote share"
    )
  ) 

ggplot(filter(trimap_sf), aes(fill = as.character(decile))) +
  geom_sf(
    size=.01,
    color = NA
  ) +
  coord_sf() +
  facet_wrap(~ind, ncol = 1, strip.position = "left") +
  theme_void() +
  scale_fill_manual(
    name = "Decile",
    values = c("#600000", colfunc(9)),
    breaks = rev(1:10),
    guide = guide_legend( 
      direction = "vertical",
      keyheight = unit(4, units = "mm"),
      keywidth = unit(1, units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 0,
      byrow = F,
      label.position = "right"
    )
  ) + theme(
    legend.position = "right", 
    legend.text=element_text(size=9),
    legend.title = element_text(size = 10),
    legend.box.margin=margin(5,5,5,30),
    strip.text.y = element_text(size = 10, angle = 90),
    panel.spacing.y = unit(-1, "lines")
  )

ggsave(filename = "./output/plots/trimap.pdf", width = 120, height = 190, dpi = 600, units = "mm", device='pdf')


# ENDOGENOUS PLOT --------------------------------------------

tabe2 <- data.frame(
  value = c(-0.00882, -0.0170, -0.0253, -0.0321, -0.0346),
  se = c(0.00395, 0.00612, 0.00719, 0.00851, 0.00999),
  label = c("[1,5]", "[1,10]", "[1,15]", "[1,20]", "[1,25]")
)

tabe2 <-
  tabe2 %>%
  mutate(
    l_ci95 = value - 1.96*se,
    u_ci95 = value + 1.96*se,
    label = factor(label, levels = c("[1,5]", "[1,10]", "[1,15]", "[1,20]", "[1,25]"))
  ) 

ggplot(tabe2, aes(x=label, y = value)) + 
  geom_point(colour = "darkred") + 
  geom_errorbar(aes(ymin = l_ci95, ymax = u_ci95), width = .1, colour = "darkred") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    axis.text.x = element_text(size = unit(8, "pt")),
    axis.text.y = element_text(size = unit(8, "pt")),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = unit(9, "pt")),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size = unit(9, "pt"))
  ) +
  scale_y_continuous(breaks = seq(0, -0.05, -0.01)) +
  ylab("Effect on probability of being in range") +
  xlab("Neighbour rank (range)") 

ggsave(filename = "./output/plots/endplot.pdf", width = 100, height = 85, dpi = 600, units = "mm", device='pdf')

# CLEAN UP ENVIRONMENT ---------------------------------------------------------

rm(list = ls()) 
gc()