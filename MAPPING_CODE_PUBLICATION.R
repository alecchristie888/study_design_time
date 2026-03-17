################################################
######## MAPPING
################################################

# Load libraries
library(dplyr)
library(tidyr)
library(readr)
library(countrycode)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(patchwork)
library(scales)
library(cowplot)
library(stringr)

#--------------------------
# Data prep
#--------------------------
# Load datasets
df <- readxl::read_xlsx("CE_Database_Publication.xlsx")
df_ne <- readxl::read_xlsx("NonEnglish_Database_Publication.xlsx")

str(df)
str(df_ne)

# Convert categorical variables to factors
df$synopsis <- as.factor(df$synopsis)

df$study.design <- as.factor(df$study.design)
df_ne$study.design <- as.factor(df_ne$Study_design)

df$country <- as.character(df$country)
df_ne$country <- as.character(df_ne$Country)

df$continent <- as.factor(df$continent)
df_ne$continent <- as.factor(df_ne$Continent)

unique(df$country)
unique(df_ne$country)
unique(df$continent)
unique(df_ne$continent)

# Ensure year and lat/long is numeric
df$year <- as.numeric(as.character(df$year))
df_ne$year <- as.numeric(as.character(df_ne$Year))

df$lat <- as.numeric(as.character(df$lat))
df$long <- as.numeric(as.character(df$long))

df_ne$lat <- as.numeric(as.character(df_ne$Lat))
df_ne$long <- as.numeric(as.character(df_ne$Long))

length(unique(df$study.ID))
df_ne <- df_ne %>% rename(study.ID = `Paper ID`)
length(unique(df_ne$study.ID))


df -> df_analysis
df_ne -> df_ne_analysis

# recode study designs
df_analysis$study.design.grouped <- dplyr::recode(df_analysis$study.design,
                                                  'After' = 'After',
                                                  'Before-After' = 'BA',
                                                  'BACI' = 'BACI',
                                                  'R-BACI' = 'Rand.exp.',
                                                  'Paired BACI' = 'BACI',
                                                  'Paired R-BACI' = 'Rand.exp.',
                                                  'Control-Impact' = 'CI',
                                                  'Paired Control-Impact' = 'CI',
                                                  'RCT' = 'Rand.exp.',
                                                  'Paired RCT' = 'Rand.exp.'
)

df_analysis <- droplevels(df_analysis)
unique(df_analysis$study.design.grouped)

unique(df_ne_analysis$study.design)
df_ne_analysis$study.design.grouped <- dplyr::recode(df_ne_analysis$study.design,
                                                     'After' = 'After',
                                                     'Before-After' = 'BA',
                                                     'Control-Impact' = 'CI',
                                                     'BACI' = 'BACI',
                                                     'RCT' = 'Rand.exp.'
)
df_ne_analysis <- droplevels(df_ne_analysis)

unique(df_ne_analysis$study.design.grouped)

df_analysis$study.design.grouped <- factor(df_analysis$study.design.grouped,
                                           levels = c("After", "BA", "CI", "BACI", "Rand.exp."))

df_analysis <- droplevels(df_analysis)

df_ne_analysis$study.design.grouped <- factor(df_ne_analysis$study.design.grouped,
                                              levels = c("After", "BA", "CI", "BACI", "Rand.exp."))

df_ne_analysis <- droplevels(df_ne_analysis)

unique(df_ne_analysis$study.design.grouped)
unique(df_analysis$study.design.grouped)

length(unique(df_analysis$study.ID))
length(unique(df_ne_analysis$study.ID))

#check non-E languages
unique(df_ne_analysis$Language)


df_analysis_short <- df_analysis %>% select(study.ID, synopsis, year, study.design.grouped, continent, country, lat, long)
df_ne_analysis$synopsis <- "Non-English language"
df_ne_analysis_short <- df_ne_analysis %>% select(study.ID, year, synopsis, study.design.grouped, continent, country, lat, long)
str(df_analysis_short)
str(df_ne_analysis_short)


#combine into one dataset
df_analysis_all <- rbind(df_analysis_short, df_ne_analysis_short)
unique(df_analysis_all$study.design.grouped)
str(df_analysis_all)

#################################################################################
################### Standardize country names using countrycode #################
#################################################################################
# Create a standardized country code column for your dataset
df_analysis_all <- df_analysis_all %>%
  mutate(iso3c = countrycode(country, origin = 'country.name', destination = 'iso3c',
                             # Expect warnings for names that won't be matched (like 'NA' or 'Goudier Island')
                             warn = TRUE))

# Handle specific mismatches with a custom dictionary
# Some names might not be standard. Create a mapping for them.
custom_map <- c('China, P.R.' = 'CHN',
                'The Netherlands' = 'NLD',
                'Trinidad & Tobago' = 'TTO',
                'Czech Republic' = 'CZE',
                'Cote dIvoire' = 'CIV', # Handle character encoding issues
                'CÙte d?Ivoire' = 'CIV', # Another potential encoding issue
                'C?te d?Ivoire' = 'CIV',
                'NA' = NA) # Explicitly map 'NA' string to NA value

# Apply the custom mapping
df_analysis_all <- df_analysis_all %>%
  mutate(iso3c = ifelse(is.na(iso3c), countrycode(country, origin = 'country.name', destination = 'iso3c', custom_match = custom_map, warn = FALSE), iso3c))

# Check which countries still could not be matched
df_analysis_all %>% filter(is.na(iso3c) & !is.na(country)) %>% distinct(country) # these are regions or islands

####################### BII data ##########################

bii_long <- read_csv("resource_biidata.csv")

# 1) Filter to BII and just the two scenarios you care about, up to 2023
bii_hist_proj <- bii_long %>%
  filter(
    variable == "bii",
    scenario %in% c("historical", "ssp2rcp4p5messageglobiom"),
    year <= 2023
  )

# 2) Build a clean country-year table
bii_country <- bii_hist_proj %>%
  select(area_code, year, scenario, bii = value) %>%
  filter(!is.na(bii))

bii_country_iso <- bii_country %>%
  mutate(
    iso3c = str_sub(area_code, start = -3L)
  ) %>%
  filter(!is.na(iso3c))

# 3) Historical part only (for interpolation)
bii_hist_only <- bii_country_iso %>%
  filter(scenario == "historical")

min_hist_year <- min(bii_hist_only$year)
max_hist_year <- min(2014, max(bii_hist_only$year))  # cap at 2014

# 4) Interpolate ONLY within historical years up to 2014
bii_interp_hist <- bii_hist_only %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  complete(year = seq(min_hist_year, max_hist_year, 1)) %>%
  arrange(iso3c, year) %>%
  mutate(
    bii_int = if (sum(!is.na(bii)) >= 2) {
      approx(
        x    = year[!is.na(bii)],
        y    = bii[!is.na(bii)],
        xout = year,
        rule = 1   # NA beyond range; we are within min/max_hist_year anyway
      )$y
    } else {
      NA_real_
    }
  ) %>%
  ungroup() %>%
  mutate(scenario = "historical")

# 5) Keep SSP2-RCP4.5 projections for 2015–2023 as-is
bii_proj <- bii_country_iso %>%
  filter(
    scenario == "ssp2rcp4p5messageglobiom",
    year >= 2015, year <= 2023
  ) %>%
  select(iso3c, year, bii_proj = bii)  # rename to make clear these are projections

# 6) Combine into one dataset
bii_alldata <- bii_interp_hist %>%
  select(iso3c, year, bii = bii_int, scenario) %>%
  bind_rows(
    bii_proj %>%
      mutate(scenario = "ssp2rcp4p5messageglobiom",
             bii = bii_proj) %>%
      select(iso3c, year, bii, scenario)
  )


# Merge BII data with the merged_df_clean
merged_df_final <- left_join(df_analysis_all, bii_alldata, by = c("iso3c", "year"))

#remove NAs
nrow(merged_df_final %>%
       filter(!is.na(bii)))
nrow(merged_df_final)



############# summarise by country
library(dplyr)
library(sf)
library(ggplot2)
library(scales)
library(patchwork)
library(cowplot)

country_summary <- merged_df_final %>%
  filter(!is.na(iso3c)) %>%
  group_by(iso3c) %>%
  summarise(
    n_total   = n_distinct(study.ID),
    n_after   = n_distinct(study.ID[study.design.grouped == "After"]),
    n_ba      = n_distinct(study.ID[study.design.grouped == "BA"]),
    n_ci      = n_distinct(study.ID[study.design.grouped == "CI"]),
    n_baci    = n_distinct(study.ID[study.design.grouped == "BACI"]),
    n_randexp = n_distinct(study.ID[study.design.grouped == "Rand.exp."]),
    prop_after    = n_after / n_total,
    prop_rigorous = (n_baci + n_randexp) / n_total,
    bii_mean = mean(bii, na.rm = TRUE),
    .groups = "drop"
  )

world <- ne_countries(scale = "medium", returnclass = "sf") |>
  st_transform(4326) |>
  mutate(
    iso_a3 = dplyr::case_when(
      name == "France" ~ "FRA",
      name == "Norway" ~ "NOR",
      TRUE ~ iso_a3
    )
  )

#----------------------------------
# 2. Precompute global ranges for need and evidence
#    (based on ALL designs using n_total)
#----------------------------------
cs_global <- country_summary %>%
  mutate(evidence_raw = n_total)

world_global <- world |>
  left_join(cs_global, by = c("iso_a3" = "iso3c")) |>
  mutate(
    need_raw = bii_mean,
    ev_log   = log1p(evidence_raw),
    ev_positive = !is.na(evidence_raw) & evidence_raw >= 0,
    need_ok     = !is.na(need_raw)
  )

need_range_global <- range(world_global$need_raw[world_global$need_ok & world_global$ev_positive],
                           na.rm = TRUE)
ev_log_range_global <- range(world_global$ev_log[world_global$need_ok & world_global$ev_positive],
                             na.rm = TRUE)

# Fixed breaks used in ALL legends
evidence_breaks_orig <- c(0, 3, 10, 50, 200, 1000)
need_breaks_orig     <- pretty(need_range_global, n = 3)

need_breaks_scaled <- rescale(
  need_breaks_orig,
  to   = c(0, 1),
  from = need_range_global
)

ev_breaks_scaled <- rescale(
  log1p(evidence_breaks_orig),
  to   = c(0, 1),
  from = ev_log_range_global
)

#----------------------------------
# 3. Bivariate palette & interpolation
#----------------------------------
pal_mat <- matrix(
  c(
    "#cb181d", "#fcae91", "#fee5d9",
    "#f16913", "#fdae6b", "#fdd0a2",
    "#08519c", "#9ecae1", "#deebf7"
  ),
  nrow = 3, byrow = TRUE
)

interp_bi_color <- function(need_s, ev_s) {
  need_s <- pmin(pmax(need_s, 0), 1)
  ev_s   <- pmin(pmax(ev_s,   0), 1)
  
  rn <- need_s * 2 + 1
  cn <- ev_s   * 2 + 1
  
  r0 <- floor(rn); c0 <- floor(cn)
  r1 <- pmin(r0 + 1, 3); c1 <- pmin(c0 + 1, 3)
  fr <- rn - r0; fc <- cn - c0
  
  c00 <- col2rgb(pal_mat[r0, c0])
  c10 <- col2rgb(pal_mat[r1, c0])
  c01 <- col2rgb(pal_mat[r0, c1])
  c11 <- col2rgb(pal_mat[r1, c1])
  
  mix <- (1 - fr) * (1 - fc) * c00 +
    fr       * (1 - fc) * c10 +
    (1 - fr) * fc       * c01 +
    fr       * fc       * c11
  
  rgb(mix[1, ]/255, mix[2, ]/255, mix[3, ]/255)
}

col_need_only    <- "white"
col_missing_both <- "black"

#----------------------------------
# 4. Function to build ONE map
#----------------------------------
make_bi_map <- function(design = c("baci","rand_exp","all","after","ba","ci"),
                        world_sf = world,
                        country_sum = country_summary) {
  
  design <- match.arg(design)
  
  if (design == "baci") {
    evidence_label <- "Studies: BACI"
    cs <- country_sum %>% mutate(evidence_raw = n_baci)
  } else if (design == "rand_exp") {
    evidence_label <- "Studies: Rand.exp."
    cs <- country_sum %>% mutate(evidence_raw = n_randexp)
  } else if (design == "all") {
    evidence_label <- "Studies: All designs"
    cs <- country_sum %>% mutate(evidence_raw = n_total)
  } else if (design == "after") {
    evidence_label <- "Studies: After"
    cs <- country_sum %>% mutate(evidence_raw = n_after)
  } else if (design == "ba") {
    evidence_label <- "Studies: BA"
    cs <- country_sum %>% mutate(evidence_raw = n_ba)
  } else if (design == "ci") {
    evidence_label <- "Studies: CI"
    cs <- country_sum %>% mutate(evidence_raw = n_ci)
  }
  
  world_bi <- world_sf |>
    left_join(cs, by = c("iso_a3" = "iso3c")) |>
    mutate(
      need_raw = bii_mean,
      ev_log   = log1p(evidence_raw),
      zero_studies = (is.na(evidence_raw) | evidence_raw == 0) & !is.na(need_raw),
      missing_need = is.na(need_raw) & evidence_raw > 0,
      missing_both = (is.na(evidence_raw) | evidence_raw == 0) & is.na(need_raw),
      ev_positive  = !is.na(evidence_raw) & evidence_raw >= 0,
      need_ok      = !is.na(need_raw),
      
      need_scaled = rescale(
        need_raw,
        to   = c(0, 1),
        from = need_range_global
      ),
      ev_scaled = rescale(
        ev_log,
        to   = c(0, 1),
        from = ev_log_range_global
      ),
      
      need_scaled_clean = ifelse(is.na(need_scaled), 0.5, need_scaled),
      ev_scaled_clean   = ifelse(is.na(ev_scaled),   0.5, ev_scaled),
      
      bi_color_cont = mapply(
        interp_bi_color,
        need_scaled_clean,
        ev_scaled_clean
      ),
      bi_color_cont = ifelse(
        missing_need,
        col_need_only,
        bi_color_cont
      ),
      bi_color_cont = ifelse(
        missing_both,
        col_missing_both,
        bi_color_cont
      )
    )
  
  map_cont <- ggplot(world_bi) +
    geom_sf(aes(fill = bi_color_cont), color = "grey30", size = 0.1) +
    scale_fill_identity() +
    ggtitle(evidence_label) +
    theme_minimal() +
    theme(
      axis.text  = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 10)
    )
  
  map_cont
}

#----------------------------------
# 5. Build ONE shared legend (for “Studies: All designs” label)
#----------------------------------
make_bi_legend <- function(evidence_label = "Studies") {
  
  grid_n <- 80
  legend_grid <- expand.grid(
    need_scaled = seq(0, 1, length.out = grid_n),
    ev_scaled   = seq(0, 1, length.out = grid_n)
  )
  
  legend_grid$bi_color_cont <- mapply(
    interp_bi_color,
    legend_grid$need_scaled,
    legend_grid$ev_scaled
  )
  
  legend_cont <- ggplot(legend_grid,
                        aes(x = ev_scaled, y = need_scaled, fill = bi_color_cont)) +
    geom_raster() +
    scale_fill_identity() +
    scale_x_continuous(
      name   = evidence_label,
      breaks = ev_breaks_scaled,
      labels = evidence_breaks_orig
    ) +
    scale_y_continuous(
      name   = "Mean BII",
      breaks = need_breaks_scaled,
      labels = round(need_breaks_orig, 3)
    ) +
    coord_equal(expand = FALSE) +
    theme_cowplot() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size=9),
          axis.title = element_text(size=12))
  
  legend_cont
}

legend_extra_df <- tibble::tibble(
  status = factor(
    c("No BII only", "No BII & no studies"),
    levels = c("No BII only", "No BII & no studies")
  ),
  x = 1,
  y = c(1, 2)
)

legend_extra <- ggplot(legend_extra_df,
                       aes(x = x, y = y, fill = status)) +
  geom_tile(color = "grey30") +
  scale_fill_manual(
    name   = NULL,
    values = c(
      "No BII only"         = col_need_only,
      "No BII & no studies" = col_missing_both
    )
  ) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  scale_y_continuous(expand = expansion(mult = 0),
                     breaks = legend_extra_df$y,
                     labels = legend_extra_df$status) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )

#----------------------------------
# 6. main 3‑panel figure with ONE legend
#----------------------------------
# 1) build maps
fig_all     <- make_bi_map("all")
fig_randexp <- make_bi_map("rand_exp")
fig_baci    <- make_bi_map("baci")

# 2) build shared legend (small)
legend_main <- make_bi_legend()

# 3) inset legend onto the big map
fig_all_with_legend <- fig_all +
  inset_element(
    legend_main,
    left = -0.2,
    bottom = -0.65,
    right = 0.65,
    top = 0.0,
    align_to = "panel"
  ) +
  inset_element(
    legend_extra,
    left = 0.4,
    bottom = -0.25,
    right = 0.7,
    top = -0.05,
    align_to = "panel"
  )

# 4) combine: big map on left, two smaller maps stacked on right
maps_main <- fig_all_with_legend | (fig_randexp / fig_baci)

maps_main +
  plot_layout(widths = c(1.25, 1))

# ggsave(
#   "Figure_6_bii_studies.png",
#   width = 10, height = 7, dpi = 600
# )

#----------------------------------
# 7. Example: supplementary 3‑panel (After, BA, CI) with ONE legend
#----------------------------------
fig_after <- make_bi_map("after")
fig_ba    <- make_bi_map("ba")
fig_ci    <- make_bi_map("ci")

legend_main <- make_bi_legend()

fig_ci_with_legend <- fig_ci +
  inset_element(
    legend_main,
    left = -0.2,
    bottom = -0.65,
    right = 0.65,
    top = 0.0,
    align_to = "panel"
  ) +
  inset_element(
    legend_extra,
    left = 0.4,
    bottom = -0.25,
    right = 0.7,
    top = -0.05,
    align_to = "panel"
  )

maps_supp <- fig_ci_with_legend | (fig_after / fig_ba)

maps_supp +
  plot_layout(widths = c(1.25, 1))

# ggsave(
#   "Figure_S3_bii_studies.png",
#   width = 10, height = 7, dpi = 600
# )

#### basic map of each design

world_bi_basic <- world_global %>% select(iso_a3, name_long, n_after, n_ba, n_ci, n_baci, n_randexp, n_total)
#replace NAs with zeroes for plotting
world_bi_basic <- world_bi_basic %>%
  mutate(
    n_after    = if_else(is.na(n_after),    0L, n_after),
    n_ba       = if_else(is.na(n_ba),       0L, n_ba),
    n_ci       = if_else(is.na(n_ci),       0L, n_ci),
    n_baci     = if_else(is.na(n_baci),     0L, n_baci),
    n_randexp  = if_else(is.na(n_randexp),  0L, n_randexp),
    n_total    = if_else(is.na(n_total),    0L, n_total)
  )

world_long_basic <- world_bi_basic %>%
  mutate(
    across(
      c(n_after, n_ba, n_ci, n_baci, n_randexp, n_total),
      ~ if_else(is.na(.x), 0L, .x)
    )
  ) %>%
  pivot_longer(
    cols = c(n_after, n_ba, n_ci, n_baci, n_randexp, n_total),
    names_to  = "design",
    values_to = "n"
  ) %>%
  mutate(
    zero   = (n == 0),
    log_n  = if_else(n > 0, log1p(n), NA_real_),
    design = factor(
      design,
      levels = c( "n_total", "n_after", "n_ba", "n_ci", "n_baci", "n_randexp")
    )
  )

max_df <- world_long_basic %>%
  group_by(design) %>%
  summarise(max_n = max(n, na.rm = TRUE), .groups = "drop")

design_labs <- c(
  n_after   = "After",
  n_ba      = "Before-After",
  n_ci      = "Control-Impact",
  n_baci    = "BACI",
  n_randexp = "Randomised exp.",
  n_total   = "Total"
)

map_all <- ggplot() +
  geom_sf(
    data = dplyr::filter(world_long_basic, zero),
    fill  = "grey90", color = "grey30", size = 0.1
  ) +
  geom_sf(
    data = dplyr::filter(world_long_basic, !zero),
    aes(fill = log_n),
    color = "grey30", size = 0.1
  ) +
  scale_fill_gradient(
    name = "Studies",
    low  = "#fee5d9",
    high = "#cb181d",
    breaks=log1p(c(1,3,10,30,100,300,1000,2000)),
    labels=c(1,3,10,30,100,300,1000,2000)
  ) +
  facet_wrap(
    ~ design,
    ncol     = 2,
    labeller = as_labeller(design_labs)
  ) +
  geom_text(
    data = max_df,
    aes(x = -Inf, y = Inf, label = paste0("Max = ", max_n)),
    hjust = -0.1,  # nudge inside from left
    vjust =  1.1,  # nudge down from top
    size  = 3
  ) +
  theme_minimal() +
  theme(
    axis.text  = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

map_all


# ggsave(
#   filename = "Figure_4_map_designs.png",
#   plot     = map_all,
#   width    = 12,
#   height   = 8,
#   dpi      = 600
# )

# numbers of countries per study design
country_summary %>%
  summarise(
    n_countries_total      = sum(n_total      > 0, na.rm = TRUE),
    n_countries_after      = sum(n_after      > 0, na.rm = TRUE),
    n_countries_ba      = sum(n_ba      > 0, na.rm = TRUE),
    n_countries_ci      = sum(n_ci      > 0, na.rm = TRUE),
    n_countries_baci    = sum(n_baci    > 0, na.rm = TRUE),
    n_countries_randexp = sum(n_randexp > 0, na.rm = TRUE)
  )/160

