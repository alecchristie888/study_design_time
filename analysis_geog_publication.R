################################################################################################
##################################### Geographical analysis of study designs ###################
################################################################################################

# Load libraries
library(lme4)   
library(glmmTMB)
library(emmeans)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(brms)
library(rstan)
library(cmdstanr)
#cmdstanr::install_cmdstan()
#CmdStan path set to: C:/Users/alecc/.cmdstan/cmdstan-2.36.0
library(loo)
library(performance)
library(tidyr)
library(countrycode)
library(broom.mixed)
library(bayesplot)
library(maps)
library(ggspatial)
library(cowplot)
library(rnaturalearth)
library(viridis)
library(forcats)
library(patchwork)


# load data
df_analysis_all <- read.csv("df_geog_publication.csv")

#percentage of studies in each continent
df_analysis_all %>% filter(!is.na(continent)) %>%
  group_by(continent) %>%
  summarise(n = n()) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  arrange(desc(percentage))

#percentage of study designs in each continent
df_analysis_all %>% filter(!is.na(continent)) %>%
  group_by(continent, study.design.grouped) %>%
  summarise(n = n()) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  arrange(continent, desc(percentage))

#percentage of study designs globally
df_analysis_all %>% filter(!is.na(continent)) %>%
  group_by(study.design.grouped) %>%
  summarise(n = n()) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  arrange(desc(percentage))


#data last updated 1st July 2025
ge_data <- read.csv("API_GE.EST_DS2_en_csv_v2_450167.csv")
gdp_per_capita <- read.csv("API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_496395.csv")
rd_exp_pct <- read.csv("API_GB.XPD.RSDV.GD.ZS_DS2_en_csv_v2_496977.csv")

sort(unique(ge_data$Country.Name))
sort(unique(ge_data$Country.Code))
sort(unique(gdp_per_capita$Country.Name))
sort(unique(gdp_per_capita$Country.Code))
sort(unique(rd_exp_pct$Country.Name))
sort(unique(rd_exp_pct$Country.Code))

######################################################################################################################
############### multiply gdp data by percentage research expenditure to get research expenditure per capita ##########
######################################################################################################################

str(rd_exp_pct)
str(gdp_per_capita)

# ---  Reshape the Data to a Long Format ---

# Pivot the R&D data
rd_long <- rd_exp_pct %>%
  pivot_longer(
    cols = `X1960`:`X2024`,
    names_to = "year",
    values_to = "rd_percent_gdp"
  )

# Pivot the GDP data
gdp_long <- gdp_per_capita %>%
  pivot_longer(
    cols = `X1960`:`X2024`,
    names_to = "year",
    values_to = "gdp_per_capita"
  )


# --- Join the Datasets ---
# select only the key columns and join the two long datasets together.
# inner_join to keep rows with data for both R&D and GDP for a given country and year.
combined_data <- rd_long %>%
  select(Country.Name, Country.Code, year, rd_percent_gdp) %>%
  inner_join(
    gdp_long %>% select(Country.Code, year, gdp_per_capita),
    by = c("Country.Code", "year")
  )

combined_data$year <- gsub("X", "", combined_data$year)
unique(combined_data$rd_percent_gdp)
unique(combined_data$year)

# --- Calculate R&D Expenditure per Capita ---
# The formula is: (R&D % of GDP / 100) * GDP per Capita
# We also remove rows where data is missing (NA) for either variable.
final_data <- combined_data %>%
  filter(!is.na(rd_percent_gdp) & !is.na(gdp_per_capita)) %>%
  mutate(
    rd_expenditure_per_capita = (rd_percent_gdp / 100) * gdp_per_capita,
    # The 'year' column is currently character data, so let's make it numeric
    year = as.numeric(year)
  )

# View the first few rows of your new, analysis-ready dataset
print(head(final_data))


##################################################
########### GE Data processing ###################
##################################################

# Pivot the GE data
ge_long <- ge_data %>%
  pivot_longer(
    cols = `X1960`:`X2024`,
    names_to = "year",
    values_to = "government_effectiveness"
  )

ge_long$year <- gsub("X", "", ge_long$year)
ge_long$year <- as.numeric(ge_long$year)
unique(ge_long$government_effectiveness)
unique(ge_long$year)

##################################################
# join the ge data to the final data
df_geo <- final_data %>%
  select(Country.Name, Country.Code, year, rd_expenditure_per_capita) %>%
  inner_join(
    ge_long %>% select(Country.Name, year, government_effectiveness),
    by = c("Country.Name", "year")
  )


#################################################################################
################### Standardize country names using countrycode #################
#################################################################################
# Create a standardized country code column for your dataset
df_analysis_all <- df_analysis_all %>%
  mutate(iso3c = countrycode(country, origin = 'country.name', destination = 'iso3c',
                             # Expect warnings for names that won't be matched (like 'NA' or 'Goudier Island')
                             warn = TRUE))

# Create a standardized country code column for the World Bank dataset
df_world_bank <- df_geo %>%
  rename(country_wb = `Country.Name`) %>% # Rename for easier handling
  mutate(iso3c = Country.Code)


# Handle specific mismatches with a custom dictionary
# Some names might not be standard. Let's create a mapping for them.
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

#remove NAs
merged_df_clean <- merged_df %>%
  filter(!is.na(rd_expenditure_per_capita) & !is.na(government_effectiveness))

nrow(merged_df_clean) #check how many rows remain after removing NAs
nrow(merged_df)
length(unique(merged_df_clean$study.ID)) #check how many unique studies remain after removing NAs))
length(unique(merged_df$study.ID)) 
unique(merged_df_clean$iso3c)

# Model including country-level predictors and a random effect for country

# Ensure country-level variables (GDP, etc.) are scaled so they are on a 
# similar numeric range, which helps the model converge.
merged_df_clean$rd_expenditure_per_capita_scaled <- scale(merged_df_clean$rd_expenditure_per_capita)
merged_df_clean$government_effectiveness_scaled <- scale(merged_df_clean$government_effectiveness)

merged_df_clean_short <- merged_df_clean %>%
  select(study.ID, synopsis, year, study.design.grouped, iso3c, continent, lat, long, rd_expenditure_per_capita_scaled, government_effectiveness_scaled)

# Group by country and count the number of distinct study designs
diversity_by_country <- merged_df_clean_short %>%
  group_by(iso3c) %>%
  summarise(
    number_of_distinct_designs = n_distinct(study.design.grouped)
  )

# set weakly informative priors
get_prior(formula = study.design.grouped ~
            rd_expenditure_per_capita_scaled +
            government_effectiveness_scaled +
            # Include continent and 'iso3c' as nested random effects
            (1 | continent/iso3c) +
            # Keep 'study.ID' to account for multiple observations of the same study
            (1 | study.ID),
          data = merged_df_clean_short,
          family = categorical(link = "logit"))

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

# fit first linear model 
model_geo_1 <- brm(
  formula = study.design.grouped ~
    rd_expenditure_per_capita_scaled +
    government_effectiveness_scaled +
    # Include continent and 'iso3c' as nested random effects
    (1 | continent/iso3c) +
    # Keep 'study.ID' to account for multiple observations of the same study
    (1 | study.ID),
  data = merged_df_clean_short,
  family = categorical(link = "logit"),
  iter = 2000,
  warmup = 1000,
  # --- Add the weakly informative priors ---
  prior = recommended_priors,
  # Address the divergent transitions
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  refresh = 10,
  chains = 4,
  cores = 4,
  threads = threading(3),
  backend = "cmdstanr" , file = "model_geog_linear_publication.rds" # Saves the fit to a file
)

#reload model data if saved
#model_geo_1 <- readRDS("model_geog_linear_publication.rds")
summary(model_geo_1)

tidy_summary <- tidy(model_geo_1, effects = c("fixed", "ran_pars"), rhat=TRUE, ess=TRUE)

##################################################################################################
########################### check linearity of logit assumption ##################################
##################################################################################################

# --- Create the empirical logit plot ---
merged_df_clean_short %>%
  # Create bins based on the quantiles of the predictor
  #    create 10 bins of equal size.
  mutate(
    expenditure_bin = cut_number(rd_expenditure_per_capita_scaled, n = 10)
  ) %>%
  
  # Count observations in each bin for each study design
  count(expenditure_bin, study.design.grouped, name = "n") %>%
  
  # Pivot to get counts for each design in columns
  # The 'values_fill = 1' is a trick to avoid log(0) errors for empty cells
  pivot_wider(names_from = study.design.grouped, values_from = n, values_fill = 1) %>%
  
  # Calculate the logit comparisons against the 'After' category
  mutate(
    `BA vs. After` = log(BA / After),
    `BACI vs. After` = log(BACI / After),
    `CI vs. After` = log(CI / After),
    `Rand.exp. vs. After` = log(`Rand.exp.` / After)
  ) %>%
  
  # Pivot back to a long format for plotting with facets
  pivot_longer(
    cols = c(`BA vs. After`, `BACI vs. After`, `CI vs. After`, `Rand.exp. vs. After`),
    names_to = "comparison",
    values_to = "logit_value"
  ) %>%
  
  # Create the faceted plot
  ggplot(aes(x = expenditure_bin, y = logit_value, group = 1)) +
  geom_point(alpha = 0.6, color = "midnightblue") +
  # Add a LOESS smoother to help visualize the trend
  geom_smooth(method = "loess", se = FALSE, color = "firebrick") +
  facet_wrap(~ comparison, scales = "free_y") +
  labs(
    title = "Empirical Logit Plots for R&D Expenditure",
    subtitle = "Compares each study design against the 'After' category",
    x = "R&D Expenditure (Binned)",
    y = "Empirical Log-Odds"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#some non-linearity

################################################################################
# same for GE score ############################################################
################################################################################

merged_df_clean_short %>%
  # Create bins based on the quantiles of the predictor
  # create 10 bins of equal size.
  mutate(
    ge_bin = cut_number(government_effectiveness_scaled, n = 10)
  ) %>%
  
  # Count observations in each bin for each study design
  count(ge_bin, study.design.grouped, name = "n") %>%
  
  # Pivot to get counts for each design in columns
  # The 'values_fill = 1' is a trick to avoid log(0) errors for empty cells
  pivot_wider(names_from = study.design.grouped, values_from = n, values_fill = 1) %>%
  
  # Calculate the logit comparisons against the 'After' category
  mutate(
    `BA vs. After` = log(BA / After),
    `BACI vs. After` = log(BACI / After),
    `CI vs. After` = log(CI / After),
    `Rand.exp. vs. After` = log(`Rand.exp.` / After)
  ) %>%
  
  # Pivot back to a long format for plotting with facets
  pivot_longer(
    cols = c(`BA vs. After`, `BACI vs. After`, `CI vs. After`, `Rand.exp. vs. After`),
    names_to = "comparison",
    values_to = "logit_value"
  ) %>%
  
  # Create the faceted plot
  ggplot(aes(x = ge_bin, y = logit_value, group = 1)) +
  geom_point(alpha = 0.6, color = "midnightblue") +
  # Add a LOESS smoother to help visualize the trend
  geom_smooth(method = "loess", se = FALSE, color = "firebrick") +
  facet_wrap(~ comparison, scales = "free_y") +
  labs(
    title = "Empirical Logit Plots for GE score",
    subtitle = "Compares each study design against the 'After' category",
    x = "GE score (Binned)",
    y = "Empirical Log-Odds"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#also some non-linearity here

###########################################################################################
################ fit non-linear model to compare against linear ###########################
###########################################################################################

model_geo_1_spline <- brm(
  formula = study.design.grouped ~
    s(rd_expenditure_per_capita_scaled, k=3) +
    s(government_effectiveness_scaled, k=3) +
    # Include continent and 'iso3c' as nested random effects
    (1 | continent/iso3c) +
    # Keep 'study.ID' to account for multiple observations of the same study
    (1 | study.ID),
  data = merged_df_clean_short,
  family = categorical(link = "logit"),
  iter = 2000,
  warmup = 1000,
  # --- Add the weakly informative priors ---
  prior = recommended_priors,
  # Address the divergent transitions
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  refresh = 10,
  chains = 4,
  cores = 4,
  threads = threading(3),
  backend = "cmdstanr" , file = "model_geog_nonlinear_publication.rds" # Saves the fit to a file
)

#reload model if saved
#model_geo_1_spline <- readRDS("model_geog_nonlinear_publication.rds")
summary(model_geo_1_spline)

# calculate the LOOIC for each model
loo_linear <- loo(model_geo_1)
loo_spline <- loo(model_geo_1_spline)

# Now, formally compare them
model_comparison_loo <- loo_compare(loo_linear, loo_spline)

# Print the comparison tables
print(model_comparison_loo)
#se_diff: This is the standard error of the difference. A common rule of thumb is that if the absolute value of elpd_diff is more than twice the se_diff, the difference is meaningful. 
#elpd_diff: This column shows the difference in elpd relative to the best model. A negative value means a model is worse.
#Best Model on Top: The function automatically places the best-fitting model (the one with the highest expected log predictive density, elpd) at the top.


#####################################################################################################
########### linear is just as good as spline model so use this from now on as simpler is better #####
#####################################################################################################

#more model diagnostics
plot(model_geo_1) 

# Default check: overlay the density of data (y) 
# with densities from the model's predictions (y_rep)
pp_check(model_geo_1)

# categorical outcome plot
pp_check(model_geo_1, type = "bars_grouped", group = "study.design.grouped")

# Run the collinearity check
check_collinearity(model_geo_1)


####################################################################################################
########################################### PLOT MODEL DATA ########################################
####################################################################################################

# --- Generate Conditional Effects Data ---

# Calculate the conditional effects for R&D expenditure
eff_expenditure <- conditional_effects(
  model_geo_1, 
  effects = "rd_expenditure_per_capita_scaled", 
  categorical = TRUE
)

# Calculate the conditional effects for government effectiveness
eff_effectiveness <- conditional_effects(
  model_geo_1, 
  effects = "government_effectiveness_scaled",
  categorical = TRUE
)


# --- Create the Plots ---

# Plot for R&D Expenditure
plot_expenditure <- plot(eff_expenditure, plot = FALSE)[[1]] +
  labs(
    x = "R&D Expenditure per Capita (Scaled)",
    y = "Predicted Probability"
  ) +
  scale_color_viridis_d(name = "Study Design") +
  scale_fill_viridis_d(name = "Study Design") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

# Plot for Government Effectiveness
plot_effectiveness <- plot(eff_effectiveness, plot = FALSE)[[1]] +
  labs(
    x = "Government Effectiveness (Scaled)",
    y = "Predicted Probability"
  ) +
  scale_color_viridis_d(name = "Study Design") +
  scale_fill_viridis_d(name = "Study Design") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

# --- Get the original plot data and color assignments ---

# Create the initial plot object
original_plot <- plot(eff_effectiveness, plot = FALSE)[[1]]

# Extract the data frame
original_data <- original_plot$data

# Determine the number of levels and get the default viridis colors
num_levels <- n_distinct(original_data$cats__)
original_colors <- viridis_pal()(num_levels)

# Create a named vector to map each study design to its original color
color_mapping <- setNames(original_colors, levels(original_data$cats__))
alpha_mapping <- setNames(c(0.25,0.25,0.25,0.25,0.25), levels(original_data$cats__))

# --- Reorder the factor levels in the data ---

# Now, reorder the factor to move 'BACI' to the end
data_reordered <- original_data %>%
  mutate(response = fct_relevel(cats__, "BACI", after = Inf))


# --- Create the final plot with manual color application ---

ggplot(data_reordered, aes(x = effect1__, y = estimate__, color = response, fill = response)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__,alpha=response), linetype = 0) +
  geom_line(linewidth = 1) +
  labs(
    x = "Government Effectiveness (Scaled)",
    y = "Predicted Probability"
  ) +
  # --- Manually apply the original colors to the reordered levels ---
  scale_color_manual(name = "Study Design", values = color_mapping) +
  scale_fill_manual(name = "Study Design", values = color_mapping) +
  scale_alpha_manual(values=alpha_mapping)+
  guides(alpha="none",color="none")+
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")



########################################################################################
###################################### basic plotting ##################################
########################################################################################

# Prepare world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Remove rows with NA lat/long values
df_analysis_all_clean <- df_analysis_all %>%
  filter(!is.na(lat) & !is.na(long) & continent!="NA")
str(df_analysis_all_clean)
length(unique(df_analysis_all_clean$study.ID))
df_analysis_all_clean %>% summarise(n_distinct(study.ID))

df_analysis_all_clean %>%
  group_by(continent) %>%
  summarise(nstud=n_distinct(study.ID), perc=nstud/length(unique(df_analysis_all_clean$study.ID)))

df_analysis_all_clean %>%
  group_by(study.design.grouped) %>%
  summarise(nstud=n_distinct(study.ID), perc=nstud/length(unique(df_analysis_all_clean$study.ID)))


# Calculate the number and proportion of each study design within each continent
design_by_continent_prop <- df_analysis_all_clean %>%
  # Group by both continent and study design
  group_by(continent, study.design.grouped) %>%
  # Count the number of unique studies for each combination
  summarise(n_studies = n_distinct(study.ID), .groups = 'drop') %>%
  # group only by continent to calculate proportions
  group_by(continent) %>%
  # count for each design divided by the total for that continent
  mutate(proportion = n_studies / sum(n_studies)) %>%
  arrange(continent, desc(proportion))

design_by_continent_prop %>% filter(continent=="Europe")


#create a map where the points are grouped into grid cells and coloured by the number of studies in each cell
# Define the grid size (in degrees)
grid_size <- 6
# Create grid cells by rounding lat/long to the nearest grid size
df_analysis_all_clean <- df_analysis_all_clean %>%
  mutate(
    lat_bin = round(lat / grid_size) * grid_size,
    long_bin = round(long / grid_size) * grid_size
  )

# Count the number of studies of each design in each grid cell
grid_data <- df_analysis_all_clean %>%
  group_by(lat_bin, long_bin, study.design.grouped) %>%
  summarise(n_studies = n(), .groups = 'drop')
str(grid_data)

#calculate the sum of all study designs per grid cell
grid_data_total <- grid_data %>%
  group_by(lat_bin, long_bin) %>%
  summarise(n_studies = sum(n_studies)) %>%
  mutate(study.design.grouped = "All designs")

grid_data_plotting <- rbind(grid_data, grid_data_total)

# Create the base map
map_plot <- ggplot(data = world) +
  geom_sf(fill = "white") +
  theme_cowplot() +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude")

# Add grid cells to the map with a scale from grey to dark orange
map_plot <- map_plot +
  geom_tile(data = grid_data_plotting, aes(x = long_bin, y = lat_bin, fill = n_studies), color = "black", alpha = 0.9) +
  scale_fill_continuous(name = "Number of studies", low = "grey90", high = "red", breaks=seq(0,700,100)) +
  facet_wrap(~study.design.grouped, nrow=3) +
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(1, "cm"),
        axis.title = element_text(size=18),
        strip.text = element_text(size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=12))


# Display map
print(map_plot)



##################################################################################################
####### plot for cumulative number of countries and continents over time #########################
##################################################################################################

# --- Cumulative Unique Countries and Continents ---

df_analysis_all_clean <- df_analysis_all %>%
  filter(!is.na(continent) & continent!="NA")

# Get all unique study designs and sorted years
unique_study_designs <- unique(df_analysis_all_clean$study.design.grouped)
unique_years <- sort(unique(df_analysis_all_clean$year))

# Initialize an empty list to store results
results_list <- list()

# Loop through each study design
for (design in unique_study_designs) {
  # Keep track of unique countries and continents found so far for this design
  found_countries <- c()
  found_continents <- c()
  
  # Loop through each year
  for (current_year in unique_years) {
    # Get the new data from the current year for the current design
    new_data <- df_analysis_all_clean %>%
      filter(study.design.grouped == design, year == current_year)
    
    # If there is new data, update the sets of found countries and continents
    if (nrow(new_data) > 0) {
      found_countries <- unique(c(found_countries, new_data$iso3c))
      found_continents <- unique(c(found_continents, new_data$continent))
    }
    
    # If any countries have been found for this design up to this year, record the cumulative count
    if (length(found_countries) > 0) {
      results_list[[length(results_list) + 1]] <- tibble(
        study.design.grouped = design,
        year = current_year,
        cum_countries = length(found_countries),
        cum_continents = length(found_continents)
      )
    }
  }
}

# Combine the list of results into a single data frame
plot1_data <- bind_rows(results_list)

max(plot1_data$cum_continents)
max(plot1_data$cum_countries)

plot1_data$study.design.grouped <- factor(plot1_data$study.design.grouped, levels=c("After","BA","CI","BACI","Rand.exp."))


# --- Create the Cumulative Plot ---
cum_plot_country <- ggplot(plot1_data, aes(x = year, group = study.design.grouped)) +
  geom_line(aes(y = cum_countries, color = study.design.grouped), linewidth = 1) +
  geom_point(aes(y = cum_countries, color = study.design.grouped), size = 2.5) +
  labs(
    x = "Year",
    y = "Cumulative number of countries"
  ) +
  scale_color_viridis_d(name = "Study Design", option = "D") +
  scale_y_continuous(breaks=seq(0,150,25), limits=c(0,150)) +
  scale_x_continuous(breaks=seq(1925,2025,25), limits=c(1900,2025)) +
  theme_cowplot() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

print(cum_plot_country)


##################################################################################################
################################# Test differences in study design proportion between ############
################################# developed and developing economies #############################
##################################################################################################

#classify all european and north american countries as developed economies
df_analysis_all_clean <- df_analysis_all_clean %>%
  mutate(
    region = ifelse(continent == "Europe", "Developed economies", "Developing economies")
  )

#check these
df_analysis_all_clean %>% 
  filter(region=="Developing economies") %>% 
  distinct(iso3c,country,continent)

#now add in the other developed economies
df_analysis_all_clean <- df_analysis_all_clean %>% 
  mutate(region = ifelse(iso3c=="ISR"|iso3c=="AUS"|iso3c=="NZL"|iso3c=="JPN"|
                           iso3c=="KOR"|
                           iso3c=="CAN"|iso3c=="USA"|
                           iso3c=="BMU"|
                           iso3c=="GRL", "Developed economies", region))

#check developed economies
df_analysis_all_clean %>% 
  filter(region=="Developed economies") %>% 
  distinct(iso3c,country,continent)

#check developing economies
df_analysis_all_clean %>% 
  filter(region=="Developing economies") %>% 
  distinct(iso3c,country,continent)

# remove data without iso3c
df_analysis_all_clean <- df_analysis_all_clean %>% filter(!is.na(iso3c))


# --- Chi-Squared Analysis and Plot Preparation ---

# Calculate observed counts of studies for each design in each region
observed_counts <- df_analysis_all_clean %>%
  count(study.design.grouped, region, name = "observed")

# Calculate the overall proportion of studies in each region (the "bias")
total_studies <- nrow(df_analysis_all_clean)
region_proportions <- df_analysis_all_clean %>%
  count(region) %>%
  mutate(prop = n / total_studies)

# Calculate the total number of studies for each design
design_totals <- df_analysis_all_clean %>%
  count(study.design.grouped, name = "design_total")

# Join these together to calculate the expected counts
expected_counts <- observed_counts %>%
  left_join(design_totals, by = "study.design.grouped") %>%
  left_join(region_proportions, by = "region") %>%
  mutate(expected = design_total * prop)

# Calculate the difference (Residuals) and a standardized value (Pearson Residuals)
# A positive residual means more studies were observed than expected.
# A negative residual means fewer studies were observed than expected.
plot_data <- expected_counts %>%
  mutate(
    residual = observed - expected,
    pearson_residual = residual / sqrt(expected),
    representation = case_when(
      pearson_residual > 2 ~ "Over-represented",
      pearson_residual < -2 ~ "Under-represented",
      TRUE ~ "As Expected"
    )
  )

plot_data$study.design.grouped <- factor(plot_data$study.design.grouped, levels=c("After","BA","CI","BACI","Rand.exp."))

# --- Create the Proportional Representation Plot ---
proportional_plot <- ggplot(plot_data, aes(x = study.design.grouped, y = pearson_residual, fill = region)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 1.96, linetype = "dashed", linewidth=1, color = "cornflowerblue") +
  geom_hline(yintercept = -1.96, linetype = "dashed",linewidth=1, color = "darkorange") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  annotate("text", x = 2.5, y = 2.3, label = "Over-represented", hjust = 0, color = "cornflowerblue") +
  annotate("text", x = 2.5, y = -2.3, label = "Under-represented", hjust = 0, color = "darkorange") +
  labs(
    x = "Study Design",
    y = "Standardized Residual (Observed - Expected)",
    fill = "Region"
  ) +
  scale_fill_viridis_d(option = "A", begin=0.05, end=0.5) +
  theme_cowplot() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    legend.position = "bottom"
  )

# Display plot
print(proportional_plot)

#combine with previous plot
combined_plot <- cum_plot_country + proportional_plot + plot_layout(ncol=2, guides = 'collect') & theme(legend.position = 'bottom')
print(combined_plot)

# Create a contingency table (matrix) of study designs versus region
contingency_table <- table(df_analysis_all_clean$study.design.grouped, df_analysis_all_clean$region)

# Display the contingency table to see the raw counts
print(contingency_table)

# Perform the chi-squared test of independence on the table
chi_test <- chisq.test(contingency_table)

# Print the results
print(chi_test)

# The pearson_residual in the plot is calculated from the values in this contingency table.
print(chi_test$residuals)


