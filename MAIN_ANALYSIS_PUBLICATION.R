########## Load libraries
library(dplyr)
library(tidyr)
library(countrycode)
library(WDI)
library(readr)
library(stringr)

library(ggplot2)
library(cowplot)
library(patchwork)
library(performance)

library(rstan)
library(brms)
library(tidybayes)
library(ggh4x)


################ LOAD DATA AND DO INITIAL STATS
# Load datasets
df <- readxl::read_xlsx("CE_Database_Publication.xlsx")
df_ne <- readxl::read_xlsx("Non_English_Database_Publication.xlsx")

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


#cumulative % study designs in 1970 vs 2023
# total unique studies per period (any design)
period_totals <- df_analysis_all %>%
  filter(year <= 2023) %>%
  mutate(period = if_else(year <= 1970, "≤1970", "≤2023")) %>%
  group_by(period) %>%
  summarise(
    total_studies = n_distinct(study.ID),
    .groups = "drop"
  )

# unique studies per design × period, with cumulative % of studies
cum_endpoints <- df_analysis_all %>%
  filter(year <= 2023) %>%
  mutate(period = if_else(year <= 1970, "≤1970", "≤2023")) %>%
  group_by(period, study.design.grouped) %>%
  summarise(
    n_studies = n_distinct(study.ID),
    .groups = "drop"
  ) %>%
  left_join(period_totals, by = "period") %>%
  mutate(
    prop = n_studies / total_studies
  )

cum_endpoints


#recode language
df_analysis_all <- df_analysis_all |>
  dplyr::mutate(
    language = ifelse(synopsis == "Non-English language", 
                      "non_English", "English"),
    language = factor(language)
  )

head(df_analysis_all)
unique(df_analysis_all$language)


# total unique studies per language (any design)
lang_totals <- df_analysis_all %>%
  group_by(language) %>%
  summarise(
    total_lang_studies = n_distinct(study.ID),
    .groups = "drop"
  )

# unique studies per design × language, with % of all studies in that language
design_lang_stats <- df_analysis_all %>%
  group_by(language, study.design.grouped) %>%
  summarise(
    n_studies = n_distinct(study.ID),
    .groups = "drop"
  ) %>%
  left_join(lang_totals, by = "language") %>%
  mutate(
    prop_within_lang = n_studies / total_lang_studies
  )

design_lang_stats

# total unique studies overall
total_studies_all <- df_analysis_all %>%
  summarise(
    total_all_studies = n_distinct(study.ID)
  ) %>%
  pull(total_all_studies)

design_overall <- df_analysis_all %>%
  group_by(study.design.grouped) %>%
  summarise(
    n_studies = n_distinct(study.ID),
    .groups = "drop"
  ) %>%
  mutate(
    total_all   = total_studies_all,
    prop_overall = n_studies / total_all
  )

design_overall


# write.csv(design_lang_stats, "table_design_lang_stats.csv", row.names = TRUE)
# write.csv(design_overall,    "table_design_overall.csv",   row.names = TRUE)

#-- Load World Bank Data ---
# Download GDP per capita (constant 2015 US$) for all countries, 1960–2023
gdp_pc <- WDI(
  country   = "all",
  indicator = c(gdp_pc_const2015 = "NY.GDP.PCAP.KD"),
  start     = 1960,
  end       = 2023,
  extra = TRUE
)

# Inspect
head(gdp_pc)
# keep just income group and gdp per capita plus country name and code
gdp_pc <- gdp_pc %>%
  select(iso3c, country, year, gdp_pc_const2015, income)

gdp_pc <- gdp_pc %>%
  mutate(
    income_group = dplyr::case_when(
      income %in% c("Low income") ~ "Low",
      income %in% c("Lower middle income", "Upper middle income") ~ "Middle",
      income %in% c("High income") ~ "High",
      TRUE ~ NA_character_
    )
  )


#fill in Puerto Rico and Venezuela
gdp_pc <- gdp_pc %>%
  mutate(
    income_group = case_when(
      iso3c == "PRI" ~ "High", 
      iso3c == "VEN" ~ "Middle",  
      TRUE ~ income_group
    )
  )

head(gdp_pc)
unique(gdp_pc$income_group)
#gdp_pc %>% filter(iso3c %in% c("PRI", "VEN")) %>% distinct(iso3c, income_group)

#################################################################################
################### Standardize country names using countrycode #################
#################################################################################
# Create a standardized country code column for your dataset
df_analysis_all <- df_analysis_all %>%
  mutate(iso3c = countrycode(country, origin = 'country.name', destination = 'iso3c',
                             # Expect warnings for names that won't be matched (like 'NA' or 'Goudier Island')
                             warn = TRUE))

# Create a standardized country code column for the World Bank dataset
df_world_bank <- gdp_pc %>%
  rename(country_wb = country)


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

setdiff(df_analysis_all$iso3c, df_world_bank$iso3c) # check which iso3c codes in your data are not in the world bank data
#these are countries or islands that tend to be small and lack data needed

setdiff(df_world_bank$iso3c, df_analysis_all$iso3c) # check which iso3c codes in the world bank data are not in your data
#expect a fair few to be where no studies have been conducted


# Merge the two datasets
merged_df <- left_join(df_analysis_all, df_world_bank, by = c("iso3c", "year"))

merged_df %>% filter(is.na(gdp_pc_const2015) ) %>%
  distinct(country, country_wb, iso3c, gdp_pc_const2015,year) #check which countries have missing data

merged_df %>% filter(is.na(gdp_pc_const2015) ) %>%
  distinct(country, year)  %>% #check which countries have missing data
  print(n=350)

merged_df %>% filter(is.na(gdp_pc_const2015) ) %>%
  distinct(year) %>% #check which countries have missing data
  print(n=100)

#remove NAs
merged_df_clean <- merged_df %>%
  filter(!is.na(gdp_pc_const2015))

nrow(merged_df_clean) #check how many rows remain after removing NAs
nrow(merged_df)

length(unique(merged_df_clean$study.ID)) #check how many unique studies remain after removing NAs
length(unique(merged_df$study.ID)) 
unique(merged_df_clean$iso3c)


####################### BII data ##########################

bii_long <- read_csv("resource_biidata.csv")

# 1) Filter to BII and the two scenarios up to 2023
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
merged_df_final <- left_join(merged_df_clean, bii_alldata, by = c("iso3c", "year"))

#remove NAs
merged_df_final_clean <- merged_df_final %>%
  filter(!is.na(bii))

nrow(merged_df_final_clean) #check how many rows remain after removing NAs
nrow(merged_df_final)

length(unique(merged_df_final_clean$study.ID)) #check how many unique studies remain after removing NAs
length(unique(merged_df_final$study.ID)) 

unique(merged_df_final %>% select(country,bii,year) %>% filter(is.na(bii))) %>% print(n=850)

#export dataframe with all data in necessary for analysis 
#write.csv(merged_df_final,"df_all_data_publication.csv",row.names=FALSE)

####################### modelling ##########################

df <- read.csv("df_all_data_publication.csv")
str(df)

#remove synopses that do not have all five designs
df_filter <- df %>% filter(synopsis!="Sustainable Aquaculture" & synopsis!="Bee Conservation" & synopsis!="Grassland Conservation" & synopsis!="Natural Pest Control" & synopsis!="Primate Conservation")
sort(unique(df$synopsis))
sort(unique(df_filter$synopsis))
str(df_filter)

# make sure factors are set
df_filter$study.design.grouped <- factor(
  df_filter$study.design.grouped,
  levels = c("After", "BA", "CI", "BACI", "Rand.exp.")
)

df_model <- df_filter |>
  mutate(
    study_design_grouped = relevel(study.design.grouped, ref = "After"),
    year_sc    = scale(year, center = TRUE, scale = TRUE)[,1],
    log_gdp_sc = scale(log(gdp_pc_const2015), center = TRUE, scale = TRUE)[,1],
    bii_sc     = scale(bii, center = TRUE, scale = TRUE)[,1]
  ) %>%
  drop_na()


#### check for collinearity
check_collinearity(lm(
  as.numeric(study_design_grouped) ~ year_sc + log_gdp_sc + bii_sc + language + synopsis,
  data = df_model
))


# Fit the Bayesian Multilevel Multinomial Model 
# Define the set of priors
# Three classes of parameters:
# 1. 'b': The fixed-effect regression coefficients (for each synopsis)
# 2. 'sd': The standard deviation of the random effects (for study.ID)
# 3. 'Intercept': The standard deviation for the intercept terms

#Gelman (2008) logistic reg. prior(normal(0, 2.5), class = b): This sets a Normal distribution centered at 0 with a standard deviation of 2.5 as the prior for all coefficients (b). This prior suggests that a log-odds of ±5 is plausible but a log-odds of ±10 is very unlikely.
#https://mc-stan.org/learn-stan/case-studies/weakly_informative_shapes.html prior(exponential(1), class = sd): This sets an Exponential distribution with a rate of 1 for random effect standard deviations. This prior gently encourages smaller standard deviations, which helps regularize the model.

recommended_priors <- c(
  # --- Priors for the 'muBA' outcome ---
  set_prior("normal(0, 2.5)", class = "b", dpar = "muBA"),
  set_prior("exponential(1)", class = "sd", dpar = "muBA"),
  set_prior("normal(0, 5)", class = "Intercept", dpar = "muBA"),

  # --- Priors for the 'muBACI' outcome ---
  set_prior("normal(0, 2.5)", class = "b", dpar = "muBACI"),
  set_prior("exponential(1)", class = "sd", dpar = "muBACI"),
  set_prior("normal(0, 5)", class = "Intercept", dpar = "muBACI"),

  # --- Priors for the 'muCI' outcome ---
  set_prior("normal(0, 2.5)", class = "b", dpar = "muCI"),
  set_prior("exponential(1)", class = "sd", dpar = "muCI"),
  set_prior("normal(0, 5)", class = "Intercept", dpar = "muCI"),

  # --- Priors for the 'muRandexp' outcome ---
  set_prior("normal(0, 2.5)", class = "b", dpar = "muRandexp"),
  set_prior("exponential(1)", class = "sd", dpar = "muRandexp"),
  set_prior("normal(0, 5)", class = "Intercept", dpar = "muRandexp")
)

options(mc.cores = 12)  # or 12; brms only uses up to chains*threads anyway

library(cmdstanr)

mod_brms <- brm(
  study.design.grouped ~ year_sc + log_gdp_sc + bii_sc + language + synopsis +
    (1 | study.ID),
  data   = df_model,
  family = categorical(link = "logit"),
  chains = 4,
  cores  = 4,
  iter = 4000,
  warmup = 2000,
  prior = recommended_priors,
  threads = threading(threads = 3),
  refresh = 50,
  backend = "cmdstanr" ,
  file = "model_linear_time_publication_4000.rds" # Saves the fit to a file
)

# #read model .rds file after model has run to avoid rerunning time-consuming model
# mod_brms <- readRDS("model_linear_time_publication_4000.rds")

summary(mod_brms)      # check Rhat, Bulk_ESS, Tail_ESS

# diagnostic checks - also time-consuming to run
# pp_check(mod_brms, type = "bars", ndraws=100) # observed vs replicated category frequencies
# pp_check(mod_brms, type = "stat_grouped",
#          group = "language", stat = "mean", ndraws=100)
# pp_check(mod_brms, type = "stat_grouped",
#          group = "synopsis", stat = "mean", ndraws=100)
#  
# plot(mod_brms, pars = "^b_")             # fixed effects
# plot(mod_brms, pars = "^sd_")            # random-effect SDs

# full summary object
s_brms <- summary(mod_brms)

# random-effect SDs (study.ID)
re_sd <- as.data.frame(round(s_brms$random$study.ID,digits=3))

# fixed-effect coefficients
fe <- as.data.frame(round(s_brms$fixed,digits=3))

# inspect
head(re_sd)
head(fe)

#export model summaries
# write.csv(re_sd, "tableS3_random_effect_sd.csv", row.names = TRUE)
# write.csv(fe,    "tableS2_fixed_effects.csv",   row.names = TRUE)


#### generate predicted probabilities

########## by language 
lang_draws <- df_model %>%
  group_by(language) %>%
  add_epred_draws(
    mod_brms,
    ndraws=500,
    re_formula = NA
  )

lang_summ <- lang_draws %>%
  group_by(language, .category) %>%
  mean_qi(.epred, .width = 0.95)

lang_summ

ref_lang <- "English"

lang_draws_mean <- lang_draws %>%
  group_by(.draw, language, .category) %>%
  summarise(
    .epred = mean(.epred),
    .groups = "drop"
  )

lang_contrasts <- lang_draws_mean %>%
  group_by(.draw, .category) %>%
  mutate(
    .epred_ref = .epred[language == ref_lang],
    diff       = .epred - .epred_ref
  ) %>%
  ungroup() %>%
  group_by(language, .category) %>%
  mean_qi(diff, .width = 0.95)

lang_contrasts


########## by synopsis
syn_draws <- df_model %>%
  group_by(synopsis) %>%
  add_epred_draws(mod_brms, re_formula = NA, ndraws = 500)

syn_summ <- syn_draws %>%
  group_by(synopsis, .category) %>%
  mean_qi(.epred, .width = 0.95)

syn_summ

########## by BII
df_bii <- df_model %>%
  mutate(bii_bin = cut_number(bii_sc, 4))

bii_draws <- df_bii %>%
  group_by(bii_bin) %>%
  add_epred_draws(mod_brms, re_formula = NA, ndraws = 500)

bii_summ <- bii_draws %>%
  group_by(bii_bin, .category) %>%
  mean_qi(.epred, .width = 0.95)

bii_summ
########## by GDP
df_gdp <- df_model %>%
  mutate(gdp_bin = cut_number(log_gdp_sc, 4))

gdp_draws <- df_gdp %>%
  group_by(gdp_bin) %>%
  add_epred_draws(mod_brms, re_formula = NA, ndraws = 500)

gdp_summ <- gdp_draws %>%
  group_by(gdp_bin, .category) %>%
  mean_qi(.epred, .width = 0.95)

gdp_summ


###################################################################################
######################## Plot cumulative studies over time ########################
###################################################################################

#### Figure 2 multipanel

# --- Data Preparation for Cumulative Proportions ---
design_annual_counts <- df_analysis_all %>%
  group_by(year, study.design.grouped) %>%
  summarise(n = n_distinct(study.ID), .groups = "drop") %>%
  complete(year, study.design.grouped, fill = list(n = 0))


# 2. Calculate the cumulative count for EACH study design over time.
design_cum_counts <- design_annual_counts %>%
  arrange(year) %>%
  group_by(study.design.grouped) %>%
  mutate(cum_design_n = cumsum(n)) %>%
  ungroup()

# 3. Calculate the TOTAL cumulative count of ALL studies over time.
total_cum_counts <- design_annual_counts %>%
  group_by(year) %>%
  summarise(total_n = sum(n)) %>%
  arrange(year) %>%
  mutate(total_cum_n = cumsum(total_n))

# 4. Join the datasets and calculate the cumulative proportion.
plot_data <- design_cum_counts %>%
  left_join(total_cum_counts, by = "year") %>%
  # Avoid division by zero for years before any studies were published
  filter(total_cum_n > 0) %>%
  mutate(cum_prop = cum_design_n / total_cum_n)

# --- Create the Stacked Area Chart ---
plot_data_cut <- plot_data %>%
  filter(year >= 1970)  # Focus on years from 1970 onwards as per model

# Get first 5 colours from viridis D
cols <- viridisLite::viridis(5, option = "D")
# Change the 5th colour to a darker yellow / orange
cols[5] <- "orange"  


cum_prop_plot <- ggplot(plot_data_cut, aes(x = year, y = cum_prop, fill = study.design.grouped)) +
  # Use position = "stack" to stack the areas on top of each other
  geom_area(position = "stack", alpha = 0.8) +
  scale_x_continuous(breaks=c(seq(1970,2023,5),2023))+
  scale_y_continuous(labels = scales::percent_format()) + # Format y-axis as percentage
  labs(
    x = "Year",
    y = "Cumulative raw proportion of studies",
    fill = "Study design"
  ) +
  scale_fill_manual(values = cols) +
  theme_cowplot() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.key.width = unit(2, "cm"),
    legend.justification = "center",
    legend.position = "bottom"
  )

# Display the plot
print(cum_prop_plot)


################### Figure S1
ggplot(plot_data, aes(x = year, y = cum_prop, fill = study.design.grouped)) +
  # Use position = "stack" to stack the areas on top of each other
  geom_area(position = "stack", alpha = 0.8) +
  scale_x_continuous(breaks=c(seq(1910,2023,20),2023))+
  scale_y_continuous(labels = scales::percent_format()) + # Format y-axis as percentage
  labs(
    x = "Year",
    y = "Cumulative raw proportion of studies",
    fill = "Study design"
  ) +
  scale_fill_manual(values = cols) +
  theme_cowplot() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.key.width = unit(2, "cm"),
    legend.justification = "center",
    legend.position = "bottom"
  )

#ggsave("FigureS1_cumul_raw_allyears.png", width = 12, height = 10, dpi = 600)


############# binned yearly proportions

year_breaks  <- c(seq(1970, 2018, by = 4), 2023)
year_labels  <- paste0(year_breaks[-length(year_breaks)], "-")

binned_proportions <- df_analysis_all %>%
  mutate(
    year_bin = cut(
      year,
      breaks = year_breaks,
      labels = year_labels,
      right  = FALSE,
      include.lowest = TRUE
    )
  ) %>%
  filter(!is.na(year_bin)) %>%
  group_by(year_bin, study.design.grouped) %>%
  summarise(n = n_distinct(study.ID), .groups = "drop") %>% 
  group_by(year_bin) %>%
  mutate(proportion = n / sum(n), 
         total=sum(n)) %>%
  ungroup()

plot_raw <- ggplot(binned_proportions,
                   aes(x = year_bin, y = proportion,
                       color = study.design.grouped,
                       group = study.design.grouped)) +
  geom_text(aes(label = total, y=0), size = 3.5, color = "black", show.legend = FALSE) +
  geom_point(alpha = 0.5, size = 2.5) +
  geom_line(alpha = 0.5, linewidth = 0.5) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) +
  scale_color_manual(values = cols) +
  scale_y_continuous(breaks = seq(0, 0.7, 0.1), limits = c(0, 0.65)) +
  guides(color = "none") +
  labs(x = "Year of publication",
       y = "Raw proportion of studies")

plot_raw



########## model plot

base_data <- df_model  
year_seq_sc <- seq(min(base_data$year_sc), max(base_data$year_sc), length.out = 10)

#Expand over year_sc but keep the joint distribution of other vars
newdata <- base_data %>%
  select(log_gdp_sc, bii_sc, language, synopsis, study.design.grouped) %>%  # all predictors except year_sc
  #sample_n(5000) %>%                       # sample_n(...) to avoid memory overload if needed
  crossing(year_sc = year_seq_sc)      # Cartesian product to vary year_sc = ., re_formula = NA)

year_draws <- newdata %>%
  add_epred_draws(
    mod_brms,
    newdata   = .,
    re_formula = NA,
    ndraws    = 500        
  ) %>%
  select(.draw, year_sc, study.design.grouped, .epred)

predicted_probs <- year_draws %>%
  group_by(year_sc, .category, .draw) %>%
  summarise(.epred = mean(.epred), .groups = "drop") %>%
  group_by(year_sc, .category) %>%
  mean_qi(.epred, .width = 0.95) %>%
  ungroup() %>%
  mutate(
    year = year_sc * sd(df_filter$year, na.rm = TRUE) +
      mean(df_filter$year, na.rm = TRUE),
    study.design.grouped = .category
  )

head(predicted_probs)
unique(predicted_probs$year)
predicted_probs %>% filter(year == 1970 | year == 2023)

plot_model <- ggplot(predicted_probs,
                     aes(x = year, y = .epred,
                         color = study.design.grouped )) +
  geom_ribbon(
    aes(x = year, ymin = .lower, ymax = .upper, 
        color=study.design.grouped , 
        fill = study.design.grouped ),
    alpha = 0.25, show.legend = FALSE
  ) +
  geom_line(linewidth = 1.2) +
  theme_cowplot() +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) +
  guides(color = "none", fill="none") +
  scale_y_continuous(breaks = seq(0, 0.7, 0.1), limits = c(0, 0.7)) +
  labs(x = "Year of publication",
       y = "Predicted proportion of studies")


cum_prop_plot + (plot_raw + plot_model) + plot_layout(ncol = 1)

#ggsave("Figure2_cumul_raw_and_model_bayes.png", width = 12, height = 10, dpi = 600)



##### by continent

year_breaks  <- c(seq(1970, 2018, by = 5), 2023)
year_labels  <- paste0(year_breaks[-length(year_breaks)], "-")

binned_by_continent <- df_analysis_all %>%
  filter(!is.na(continent) & continent !="NA" & continent !="Antarctic") %>%
  mutate(
    year_bin = cut(
      year,
      breaks = year_breaks,
      labels = year_labels,
      right  = FALSE,
      include.lowest = TRUE
    )
  ) %>%
  filter(!is.na(year_bin)) %>%
  group_by(continent, year_bin, study.design.grouped) %>%
  summarise(n = n_distinct(study.ID), .groups = "drop") %>%
  group_by(continent, year_bin,) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

max_cont <- binned_by_continent %>% group_by(continent) %>% summarise(total_n=sum(n))

plot_continent <- ggplot(binned_by_continent,
                         aes(x = year_bin, y = proportion,
                             color = study.design.grouped,
                             group = study.design.grouped)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_line(alpha = 0.5, linewidth = 0.5) +
  facet_wrap(~ continent, ncol=3, ,scales="fixed") +
  geom_text(
    data = max_cont,
    inherit.aes = FALSE,
    aes(x = -Inf, y = Inf, label = paste0("Total = ", total_n)),
    hjust = -0.1,  # nudge inside from left
    vjust =  1.1,  # nudge down from top
    size  = 4
  ) +
  theme_cowplot() +
  scale_color_manual(name="Study Design",values = cols) +
  guides(color = guide_legend(override.aes = list(size = 4, linewidth=1.5))) +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1),
        legend.position = "bottom", legend.justification = "center",
        legend.key.width = unit(1.5,"cm")) +
  scale_y_continuous(breaks = seq(0, 1.0, 0.2), limits = c(0, 1.0)) +
  labs(x = "Year of publication",
       y = "Raw proportion of studies")
plot_continent

################# income group
year_breaks  <- c(seq(1970, 2018, by = 5), 2023)
year_labels  <- paste0(year_breaks[-length(year_breaks)], "-")

binned_by_income <- df %>%
  mutate(
    year_bin = cut(
      year,
      breaks = year_breaks,
      labels = year_labels,
      right  = FALSE,
      include.lowest = TRUE
    )
  ) %>%
  filter(!is.na(year_bin)) %>%
  group_by(income_group, year_bin, study.design.grouped) %>%
  summarise(n =  n_distinct(study.ID), .groups = "drop") %>%
  group_by(income_group, year_bin) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

binned_by_income$income_group <- factor(paste0(binned_by_income$income_group, " Income"), levels=c("Low Income","Middle Income","High Income"))

binned_by_income %>% filter(income_group=="Low Income")

max_inc <- binned_by_income %>% group_by(income_group) %>% summarise(total_n=sum(n))

plot_income <- ggplot(binned_by_income,
                      aes(x = year_bin, y = proportion,
                          color = study.design.grouped,
                          group = study.design.grouped)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_line(alpha = 0.5, linewidth = 0.5) +
  facet_wrap(~ income_group, ncol=3,scales="fixed") +
  geom_text(
    data = max_inc,
    inherit.aes = FALSE,
    aes(x = -Inf, y = Inf, label = paste0("Total = ", total_n)),
    hjust = -0.1,  # nudge inside from left
    vjust =  1.1,  # nudge down from top
    size  = 4
  ) +
  theme_cowplot() +
  scale_color_manual(values = cols) +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, 1.0, 0.2)) +
  guides(color = "none") +
  labs(x = "Year of publication",
       y = "Raw proportion of studies")
plot_income


plot_continent + plot_income + plot_layout(ncol = 1, heights = c(1.4,1))
#ggsave("Figure5_continent_income.png", width = 12, height = 15, dpi = 600)



##### Figure 3 - subject areas part 1

syn_df <- syn_draws %>%
  group_by(synopsis, .category) %>%
  mean_qi(.epred, .width = 0.95) %>%
  ungroup() %>%
  rename(
    design = .category,
    prob   = .epred,
    lower  = .lower,
    upper  = .upper
  )

# Grand mean probability for each design across synopses
overall_draws <- df_model %>%
  add_epred_draws(
    mod_brms,
    re_formula = NA,
    ndraws     = 500
  )

grand_df <- overall_draws %>%
  # 1) For each draw and category, average over all studies
  group_by(.draw, .category) %>%
  summarise(mean_epred = mean(.epred), .groups = "drop") %>%
  # 2) Then summarise across draws to get posterior mean & CrI
  group_by(.category) %>%
  mean_qi(mean_epred, .width = 0.95) %>%
  ungroup() %>%
  rename(
    design      = .category,
    grand_prob  = mean_epred,
    grand_lower = .lower,
    grand_upper = .upper
  ) %>%
  select(design, grand_prob, grand_lower, grand_upper)

syn_df <- syn_df %>%
  left_join(grand_df, by = "design") %>%
  mutate(
    diff      = prob - grand_prob,
    direction = ifelse(diff > 0, "higher", "lower")
  )

#remove words after 'conservation' (including the word conservation) in synopsis for plotting
syn_df$synopsis <- gsub(" Conservation.*", "", syn_df$synopsis)

#shorten some long synopsis names
syn_df$synopsis <- gsub("Management of ", "", syn_df$synopsis)
syn_df$synopsis <- gsub(" and ", " & ", syn_df$synopsis)
syn_df$synopsis <- gsub("Freshwater", "FW", syn_df$synopsis)
syn_df$synopsis <- gsub("Invasive Species", "Inv.Sp.", syn_df$synopsis)
syn_df$synopsis <- gsub("Biodiversity of ", "", syn_df$synopsis)
syn_df$synopsis <- gsub("Invertebrate", "Invert.", syn_df$synopsis)
syn_df$synopsis <- gsub("Mediterranean", "Med.", syn_df$synopsis)
syn_df$synopsis <- gsub("Artificial", "Art.", syn_df$synopsis)
syn_df$synopsis <- gsub("Sustainable ", "Sust.", syn_df$synopsis)

# Order synopses by prob for After, for consistent y-axis
order_syn <- syn_df %>%
  filter(design == "After") %>%
  arrange(prob) %>%
  pull(synopsis)

syn_df$synopsis <- factor(syn_df$synopsis, levels = order_syn)


syn_model_plot <- ggplot(syn_df,
                         aes(x = prob, y = synopsis, color = diff)) +
  # Add the grand mean 95% CIs  as a shaded rectangle
  geom_rect(aes(xmin = grand_lower, xmax = grand_upper, ymin = -Inf, ymax = Inf),
            fill = "gray80", alpha = 0.5, inherit.aes = FALSE
  ) +
  geom_vline(aes(xintercept = grand_prob),
             linetype = "dashed", color = "grey50") +
  
  geom_pointrange(aes(xmin = lower, xmax = upper), size = 0.5) +
  facet_wrap2(~ design, ncol = 5, scales = "fixed",
              strip = strip_themed(
                background_x = elem_list_rect(fill = cols)
              ))+
  scale_color_gradient2(
    name  = "Deviation from\ngrand mean",
    low   = "red",
    mid   = "grey40",
    high  = "royalblue",
    midpoint = 0,
    breaks=c(-0.4,-0.2, 0, 0.2,0.4,0.6)
  ) +
  theme_cowplot() +
  labs(
    x = "Predicted proportion of studies (averaged across years)",
    y = "Subject area (synopsis)"
  )+
  scale_x_continuous(breaks=seq(0,0.8,0.2)) +
  theme(
    strip.background = element_rect(fill=viridisLite::viridis(5, option = "D")),
    strip.text= element_text(color="white"),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11),
    legend.key.width = unit(1,"cm"),
    legend.position = "bottom",
    legend.justification = "center",
    panel.spacing = unit(1, "lines") # Add some space between facets
  )

syn_model_plot

#ggsave("Figure_3_synopsisp1_bayes.png", width = 12, height = 8, dpi = 600)




#### Figure 3 - subject areas - part 2

# calculate the proportion of each study design within each synopsis,
# and also gets the total number of studies for each synopsis.
plot_data <- df_analysis_all %>%
  # Count UNIQUE studies per synopsis × study design
  group_by(synopsis, study.design.grouped) %>%
  summarise(n_studies = n_distinct(study.ID), .groups = "drop") %>% 
  # Group by synopsis to calculate proportions and totals
  group_by(synopsis) %>%
  mutate(
    proportion    = n_studies / sum(n_studies),
    total_studies = sum(n_studies)
  ) %>%
  ungroup()

#remove words after 'conservation' (including the word conservation) in synopsis for plotting
plot_data$synopsis <- gsub(" Conservation.*", "", plot_data$synopsis)

#shorten some long synopsis names
plot_data$synopsis <- gsub("Management of ", "", plot_data$synopsis)
plot_data$synopsis <- gsub(" and ", " & ", plot_data$synopsis)
plot_data$synopsis <- gsub("Freshwater", "FW", plot_data$synopsis)
plot_data$synopsis <- gsub("Invasive Species", "Inv.Sp.", plot_data$synopsis)
plot_data$synopsis <- gsub("Biodiversity of ", "", plot_data$synopsis)
plot_data$synopsis <- gsub("Invertebrate", "Invert.", plot_data$synopsis)
plot_data$synopsis <- gsub("Mediterranean", "Med.", plot_data$synopsis)
plot_data$synopsis <- gsub("Artificial", "Art.", plot_data$synopsis)
plot_data$synopsis <- gsub("Sustainable ", "Sust.", plot_data$synopsis)

unique(plot_data$synopsis)

# --- Determine the Order of Synopses ---

# sort names by the proportion of the "After" study design in descending order.
synopsis_order <- plot_data %>%
  filter(study.design.grouped == "After") %>%
  arrange(desc(proportion)) %>%
  pull(synopsis)


# --- Create the Plot ---

syn_raw_plot <- ggplot(plot_data, aes(x = factor(synopsis, levels = synopsis_order), y = proportion, fill = study.design.grouped)) +
  
  # Create the stacked bars
  geom_bar(stat = "identity", position = "stack") +
  
  # Add the text labels for the total counts at the top of each bar
  geom_text(
    # separate, distinct dataset for the labels to avoid duplicates
    data = distinct(plot_data, synopsis, total_studies),
    aes(x=synopsis,label = total_studies, y = 1.05), # Position the labels just above the bars
    size = 4, # Adjust size as needed
    inherit.aes = FALSE # prevent mapping conflicts
  ) +
  scale_fill_manual(values = cols)+
  
  # Adjust y-axis to make space for the text labels
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks=seq(0,1,0.2)) +
  
  # Add labels and title
  labs(
    x = "Subject area (synopsis)",
    y = "Raw proportion of studies"
  ) +
  
  guides(fill="none")+
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=12),
        plot.title = element_text(face = "bold")
  ) 

syn_raw_plot
#ggsave("Figure_3_synopsisp2.png", width = 12, height = 8, dpi = 600)

syn_both_plots <- egg::ggarrange(syn_raw_plot, syn_model_plot, ncol = 1, heights=c(1,1.2))

# ggsave(
#   filename = "Figure_3_synopsis_bothparts_bayes.png",
#   plot     = syn_both_plots,
#   width    = 12,
#   height   = 10,
#   dpi      = 600
# )

######################################################
############# Suppl. figure S2 #######################
######################################################

sort(unique(df_analysis_all$year))

year_breaks  <- c(seq(1970, 2018, by = 4), 2023)
year_labels  <- paste0(year_breaks[-length(year_breaks)], "-")

synopsis_time <- df_analysis_all %>%
  filter(synopsis!="Non-English language") %>%
  mutate(
    year_bin = cut(
      year,
      breaks = year_breaks,
      labels = year_labels,
      right  = FALSE,
      include.lowest = TRUE
    )
  ) %>%
  filter(!is.na(year_bin)) %>%
  group_by(year_bin, synopsis) %>%
  summarise(n = n_distinct(study.ID), .groups = "drop") %>%
  group_by(year_bin) %>%
  mutate(prop_syn = n / sum(n)) %>%
  ungroup()

#remove words after 'conservation' (including the word conservation) in synopsis for plotting
synopsis_time$synopsis <- gsub(" Conservation.*", "", synopsis_time$synopsis)

#shorten some long synopsis names
synopsis_time$synopsis <- gsub("Management of ", "", synopsis_time$synopsis)
synopsis_time$synopsis <- gsub(" and ", " & ", synopsis_time$synopsis)
synopsis_time$synopsis <- gsub("Freshwater", "FW", synopsis_time$synopsis)
synopsis_time$synopsis <- gsub("Invasive Species", "Inv.Sp.", synopsis_time$synopsis)
synopsis_time$synopsis <- gsub("Biodiversity of ", "", synopsis_time$synopsis)
synopsis_time$synopsis <- gsub("Invertebrate", "Invert.", synopsis_time$synopsis)
synopsis_time$synopsis <- gsub("Mediterranean", "Med.", synopsis_time$synopsis)
synopsis_time$synopsis <- gsub("Artificial", "Art.", synopsis_time$synopsis)
synopsis_time$synopsis <- gsub("Sustainable ", "Sust.", synopsis_time$synopsis)

synopsis_order <- synopsis_time %>%
  group_by(synopsis) %>%
  summarise(mean_prop_syn = mean(prop_syn)) %>%
  arrange(desc(mean_prop_syn)) %>%
  pull(synopsis)

synopsis_time$synopsis <- factor(synopsis_time$synopsis, levels = synopsis_order)

ggplot(synopsis_time, aes(x = year_bin, y = prop_syn, color = synopsis, group = synopsis)) +
  geom_line() +
  geom_point() +
  theme_cowplot() +
  facet_wrap(~synopsis,ncol=3)+
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) +
  labs(x = "Year of publication",
       y = "Raw proportion of studies")+
  guides(color = "none")

#ggsave("Figure_S2_synopsis_time.png", width = 12, height = 14, dpi = 600)


