###############################################################################################
#################################### Temporal study design analysis ###########################
###############################################################################################

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
library(broom.mixed)
library(tidybayes)
library(tidyr) # For the 'crossing' function
library(viridis)
library(cowplot)
library(marginaleffects)
library(data.table)

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


##### proportions of study designs overall, English language, and  non-English language
df_analysis_all %>%
  group_by(study.design.grouped) %>%
  # Create a summary table with a count of unique designs per study
  summarise(n_stud = n_distinct(study.ID),
            perc_stud = n_stud/length(unique(df_analysis_all$study.ID)))

df_analysis_all %>%
  filter(synopsis != "Non-English language") %>%
  group_by(study.design.grouped) %>%
  # Create a summary table with a count of unique designs per study
  summarise(n_stud = n_distinct(study.ID),
            perc_stud = n_stud/length(unique(df_analysis_all$study.ID[df_analysis_all$synopsis!="Non-English language"])))

df_analysis_all %>%
  filter(synopsis == "Non-English language") %>%
  group_by(study.design.grouped) %>%
  # Create a summary table with a count of unique designs per study
  summarise(n_stud = n_distinct(study.ID),
            perc_stud = n_stud/length(unique(df_analysis_all$study.ID[df_analysis_all$synopsis=="Non-English language"])))


###### check studies with more than one design
studies_multiple_des <- df_analysis_all %>%
  group_by(study.ID) %>%
  # Create a summary table with a count of unique designs per study
  summarise(n_des = n_distinct(study.design.grouped)) %>%
  # Keep only those that have 2 or more
  filter(n_des > 1) %>%
  # Pull out just the vector of study IDs
  pull(study.ID)
length(studies_multiple_des)

##### check studies in more than one synopsis
# Group by study and count the number of distinct synopses for each
synopsis_counts_per_study <- df_analysis_all %>%
  group_by(study.ID) %>%
  summarise(
    distinct_synopses = n_distinct(synopsis)
  )

# Filter this summary to find studies that appear in 2 or more synopses
studies_in_multiple_synopses <- synopsis_counts_per_study %>%
  filter(distinct_synopses >= 2)

# Get the final count
number_of_studies <- nrow(studies_in_multiple_synopses)

# Print the result
number_of_studies

head(studies_in_multiple_synopses)




# --- From that list, count how many have a single, consistent study design ---

# Filter the original data to only include the studies identified in Step 1
# Then, for this subset, count the number of unique designs per study
consistent_design_count <- df_analysis_all %>%
  filter(study.ID %in% studies_in_multiple_synopses) %>%
  group_by(study.ID) %>%
  summarise(
    n_distinct_designs = n_distinct(study.design.grouped)
  ) %>%
  # Now, keep only the studies where the number of unique designs is exactly 1
  filter(n_distinct_designs == 1) %>%
  # Get the final count by counting the rows of the result
  nrow()


# Number of studies in multiple synopses that DO NOT differ by study design:
consistent_design_count

############## check numbers of study designs per year and by synopsis to avoid data sparsity issues

# Group by year and count the number of distinct study designs
diversity_by_year <- df_analysis_all %>%
  group_by(year) %>%
  summarise(
    number_of_distinct_designs = n_distinct(study.design.grouped)
  )

# View the result
print(diversity_by_year, n = 100) # Print all rows if you have ~50 years

#remove years prior to 1981 and post-2020 (continuously has 5 study designs per year) and synopses with <5 study designs (bees, grassland, natural pest, primate and sust.aqua.)
# years after 2021 only have 5 or 6 studies due to cut-off in synopses
df_analysis_all <- df_analysis_all %>%
  filter(year >= 1981 & year <= 2020)

unique(df_analysis_all$year)

#now check synopses have all 5 designs

# Group by synopsis and count the number of distinct study designs
diversity_by_syn <- df_analysis_all %>%
  group_by(synopsis) %>%
  summarise(
    number_of_distinct_designs = n_distinct(study.design.grouped)
  )

# View the result
print(diversity_by_syn[order(diversity_by_syn$number_of_distinct_designs),], n = 50) # Print all rows if you have ~50 years

syn_to_remove <- diversity_by_syn %>% filter(number_of_distinct_designs < 5) %>% pull(synopsis)

#remove 6 synopses with 3 or 4 designs for that time period
df_analysis_all <- df_analysis_all %>%
  filter(!synopsis %in% syn_to_remove)
unique(df_analysis_all$synopsis)
unique(syn_to_remove)

#remove old levels
df_analysis_all <- droplevels(df_analysis_all)
unique(df_analysis_all$synopsis)

# Check the structure
head(df_analysis_all)
summary(df_analysis_all)

# Fit the Bayesian Multilevel Multinomial Model ---

# Define the set of priors
# We target three classes of parameters:
# 1. 'b': The fixed-effect regression coefficients (for each synopsis)
# 2. 'sd': The standard deviation of the random effects (for study.ID)
# 3. 'sds': The standard deviation for the spline smoothing term

#Gelman (2008) logistic reg. prior(normal(0, 2.5), class = b): This sets a Normal distribution centered at 0 with a standard deviation of 2.5 as the prior for all your synopsis coefficients (b). This prior suggests that a log-odds of ±5 is plausible but a log-odds of ±10 is very unlikely. This is a standard, safe choice for logit models.
#https://mc-stan.org/learn-stan/case-studies/weakly_informative_shapes.html prior(exponential(1), class = sd) and class = sds: This sets an Exponential distribution with a rate of 1 for all random effect and spline standard deviations. This prior gently encourages smaller standard deviations, which helps regularize the model and is a well-established best practice.

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

model_brms <- brm(
  formula = study.design.grouped ~ year + synopsis + (1 | study.ID),
  data = df_analysis_all,
  family = categorical(link = "logit"),
  iter = 2000,
  warmup = 1000,
  # --- Add the weakly informative priors ---
  prior = recommended_priors,
  # Address the divergent transitions
  control = list(adapt_delta = 0.95, max_treedepth = 15),
  refresh = 10,
  chains = 4,
  cores = 4,
  threads = threading(3),
  backend = "cmdstanr" , file = "model_linear_time_publication.rds" # Saves the fit to a file
)

#reload model if saved
#model_brms <- readRDS("model_linear_time_publication.rds")

# --- View the Results ---
# The summary gives you the coefficients (estimates) and their 95% credible intervals.
summary(model_brms)

tidy_summary <- tidy(model_brms, effects = c("fixed", "ran_pars"), rhat=TRUE, ess=TRUE)
#write.csv(tidy_summary,"summary_linear_model.csv")

##################################################
########### check linearity of logit #############
##################################################


year_breaks <- seq(1980, 2020, by = 10)

df_analysis_all %>%
  # 1. Create year bins and count observations
  mutate(year_bin = cut(year, breaks = year_breaks, right = FALSE, 
                        labels = seq(1980, 2010, by = 10))) %>%
  filter(!is.na(year_bin)) %>%
  group_by(year_bin, study.design.grouped) %>%
  summarise(n = n(), .groups = 'drop') %>%
  # 2. Pivot to get counts for each design in columns
  pivot_wider(names_from = study.design.grouped, values_from = n, values_fill = 1) %>%
  # 3. Calculate all the logit comparisons at once
  mutate(
    `BA vs. After` = log(BA / After),
    `BACI vs. After` = log(BACI / After),
    `CI vs. After` = log(CI / After),
    `Rand.exp. vs. After` = log(`Rand.exp.` / After)
  ) %>%
  # 4. **THE KEY STEP:** Pivot this data into a long format for plotting
  pivot_longer(
    cols = c(`BA vs. After`, `BACI vs. After`, `CI vs. After`, `Rand.exp. vs. After`),
    names_to = "comparison",
    values_to = "logit_value"
  ) %>%
  # 5. Now, create the faceted plot
  ggplot(aes(x = year_bin, y = logit_value, group = 1)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  # This automatically creates a separate plot for each "comparison"
  facet_wrap(~ comparison, scales = "free_y") +
  labs(
    title = "Empirical Logit Plots for Study Designs vs. 'After'",
    x = "Year (Start of Decade)",
    y = "Empirical Log-Odds"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### some non-linearity going on, test whether non-linear term for year may improve model

#compare different models

# --- Fit the Non-Linear (Spline) Model ---
# The only change is replacing 'year' with 's(year)'.
# 's(year)' tells brms to fit a penalized regression spline, which is a flexible
# way to capture a non-linear trend.

model_spline <- brm(
  formula = study.design.grouped ~ s(year, k=3) + synopsis + (1 | study.ID),
  data = df_analysis_all,
  family = categorical(link = "logit"),
  iter = 2000,
  warmup = 1000,
  # --- Add the weakly informative priors ---
  prior = recommended_priors,
  # Address the divergent transitions
  control = list(adapt_delta = 0.95, max_treedepth = 15),
  refresh = 10,
  chains = 4,
  cores = 4,
  threads = threading(3),
  backend = "cmdstanr" , file = "model_nonlinear_time_publication.rds" # Saves the fit to a file
)

#reload model if saved
model_spline <- readRDS("model_nonlinear_time_publication.rds")
summary(model_spline)


# --- Compare the Two Models using LOOIC ---
loo_linear <- loo(model_brms)
loo_spline <- loo(model_spline)

# Now, formally compare them
model_comparison_loo <- loo_compare(loo_linear, loo_spline)

# Print the comparison tables
print(model_comparison_loo)
#se_diff: This is the standard error of the difference. A common rule of thumb is that if the absolute value of elpd_diff is more than twice the se_diff, the difference is meaningful. 
#elpd_diff: This column shows the difference in elpd relative to the best model. A negative value means a model is worse.
#Best Model on Top: The function automatically places the best-fitting model (the one with the highest expected log predictive density, elpd) at the top.

########################################################
##################### validation #######################
########################################################

#R-hat (Potential Scale Reduction Factor): All values should be very close to 1.0 (e.g., < 1.01). This is the most important convergence metric.
#Effective Sample Size (ESS): Check that the ESS for all parameters is sufficiently large (e.g., > 400 is often a good rule of thumb, but more is better). This indicates how many independent samples you have from the posterior.
#Trace Plots: Visually inspect the trace plots for your main parameters. They should look like "fuzzy caterpillars" with no obvious trends or patterns, indicating the chains are well-mixed and exploring the same space.
# Get a summary of diagnostics
summary(model_brms) 

# Create diagnostic plots
plot(model_brms) 

#"Does my fitted model generate data that looks like my real data?" 
# Compare the observed proportions to the model-predicted proportions
pp_check(model_brms, type = "bars_grouped", group = "study.design.grouped")


############################################################

# --- Post-Hoc Tests  ---

# --- Get predictions using conditional_effects ---
# This function is the brms equivalent of emmeans for plotting.
# It calculates the predicted probabilities for each category across the range of 'year'.
# 'categorical = TRUE' is essential for multinomial models.
ce_data <- conditional_effects(model_brms, 
                               effects = "year", 
                               categorical = TRUE)

# The output is a list, and the plotting data is the first element
predicted_probs_ci <- ce_data[[1]]


# --- Create the plot of study designs over time ---
plot_model <- ggplot() +
  # Credible Interval Ribbons from brms model
  geom_ribbon(
    data = predicted_probs_ci,
    aes(x = year, ymin = lower__, ymax = upper__, fill = cats__),
    alpha = 0.25, show.legend = FALSE
  ) +
  
  # Model Prediction Lines
  geom_line(
    data = predicted_probs_ci,
    aes(x = year, y = estimate__, color = cats__),
    linewidth = 1.2
  ) +
  theme_cowplot()+
  # aesthetics should use the same scale and have the same title.
  scale_color_viridis_d(name = "Study Design", option = "D") +
  scale_fill_viridis_d(name = "Study Design", option = "D") +
  
  guides(fill = "none") + # Turn off the fill legend to avoid duplication
  labs(
    x = "Year of publication",
    y = "Predicted proportion of studies")

# Bin data for plotting
# --- 1. Define the Bins ---
# Create breaks for every 5 years from 1980 to 2025
year_breaks <- seq(1980, 2020, by = 5)

# Create labels for these bins (e.g., "1980-1984")
year_labels <- paste0(year_breaks[-length(year_breaks)], "-")


# --- Calculate Proportions within Bins ---
binned_proportions <- df_analysis_all %>%
  # Create the 'year_bin' variable
  mutate(year_bin = cut(year, 
                        breaks = year_breaks, 
                        labels = year_labels, 
                        right = FALSE, 
                        include.lowest = TRUE)) %>%
  # Remove any data that might fall outside the bins
  filter(!is.na(year_bin)) %>%
  
  # Now, group by the bin variable instead of the original 'year'
  group_by(year_bin, study.design.grouped) %>%
  summarise(n = n(), .groups = 'drop') %>%
  
  # The rest of the pipeline is the same
  group_by(year_bin) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()


plot_raw <-  ggplot() +  # Raw data points
  geom_point(
    data = binned_proportions,
    aes(x = year_bin, y = proportion, color = study.design.grouped),
    alpha = 0.5, size = 2.5
  ) +
  
  # Lines connecting raw data points
  geom_line(
    data = binned_proportions,
    aes(x = year_bin, y = proportion, color = study.design.grouped, group = study.design.grouped),
    linewidth = 0.5,
    alpha = 0.5
  ) +
  theme_cowplot()+
  theme(axis.text.x=element_text(size=13))+
  scale_color_viridis_d(name = "Study Design", option = "D") +
  scale_fill_viridis_d(name = "Study Design", option = "D") +
  
  guides(fill = "none",color = "none") + # Turn off the fill legend to avoid duplication
  labs(
    x = "Year of publication (5 year bins)",
    y = "Raw proportion of studies")

plot_raw + plot_model 

#############################################################################################
####### effect of year on average predicted probabilities averaged across synopsis ##########
#############################################################################################

# Calculate the marginal effect of a one-unit change in 'year'
# on the probability of each study design averaged across synopses.
mfx_year <- slopes(
  model_brms,
  variables = "year"
)

# View the summary table
summary(mfx_year)

##############################################################
####### average predicted probabilities by synopsis ##########
##############################################################

# Create a data grid for all combinations of synopsis and year.
# 'crossing' from tidyr creates a dataframe with every combination
# of the unique values from 'synopsis' and 'year'.
newdata <- crossing(
  synopsis = unique(df_analysis_all$synopsis),
  year = unique(df_analysis_all$year)
)

# Get predicted probabilities for the full grid.
# This calculates the predicted probability for each category at each
# combination of synopsis and year for every posterior draw.
predicted_probs_full <- newdata %>%
  add_epred_draws(model_brms,
                  re_formula = NA,
                  category = "study.design.grouped")

# Average the predicted probabilities across 'year' for each draw.
# For each posterior draw (.draw) and each synopsis, we calculate the
# mean predicted probability (.epred) across all years.
averaged_probs <- predicted_probs_full %>%
  group_by(synopsis, .draw, study.design.grouped) %>%
  summarise(.epred = mean(.epred), .groups = "drop")

# Calculate the grand mean probability for each study design across all synopses.
grand_mean_probs <- averaged_probs %>%
  group_by(.draw, study.design.grouped) %>%
  summarise(grand_mean_epred = mean(.epred), .groups = "drop")

# Determine the 95% credible interval for the grand mean probabilities.
grand_mean_intervals <- grand_mean_probs %>%
  group_by(study.design.grouped) %>%
  median_qi(grand_mean_epred, .width = 0.95)

# For each synopsis, determine if its probability is significantly different from the grand mean.
significance_data <- averaged_probs %>%
  group_by(synopsis, study.design.grouped) %>%
  median_qi(.epred, .width = 0.95) %>% # Get CIs for each synopsis
  left_join(grand_mean_intervals, by = "study.design.grouped", suffix = c("_synopsis", "_grand_mean")) %>%
  mutate(
    is_significant = .lower_synopsis > .upper_grand_mean | .upper_synopsis < .lower_grand_mean,
    direction = case_when(
      .lower_synopsis > .upper_grand_mean ~ "Higher than average",
      .upper_synopsis < .lower_grand_mean ~ "Lower than average",
      TRUE ~ "Not significant"
    )
  )

#remove words after 'conservation' (including the word conservation) in synopsis for plotting
significance_data$synopsis <- gsub(" Conservation.*", "", significance_data$synopsis)

#shorten some long synopsis names
significance_data$synopsis <- gsub("Management of ", "", significance_data$synopsis)
significance_data$synopsis <- gsub(" and ", " & ", significance_data$synopsis)
significance_data$synopsis <- gsub("Freshwater", "FW", significance_data$synopsis)
significance_data$synopsis <- gsub("Invasive Species", "Inv.Sp.", significance_data$synopsis)
significance_data$synopsis <- gsub("Biodiversity of ", "", significance_data$synopsis)
significance_data$synopsis <- gsub("Invertebrate", "Invert.", significance_data$synopsis)
significance_data$synopsis <- gsub("Mediterranean", "Med.", significance_data$synopsis)
significance_data$synopsis <- gsub("Artificial", "Art.", significance_data$synopsis)
significance_data$synopsis <- gsub("Non-English ", "NE-", significance_data$synopsis)
significance_data$synopsis <- gsub("Sustainable ", "Sust.", significance_data$synopsis)

significance_data$synopsis <- factor(significance_data$synopsis,levels=rev(sort(unique(significance_data$synopsis))))

# Create the plot with highlighting.
# The `is_significant` column is now used to control the color aesthetic.
synopsis_model_plot <- ggplot(significance_data, aes(y = synopsis)) +
  # Add the grand mean credible interval as a shaded rectangle
  geom_rect(
    data = grand_mean_intervals,
    aes(xmin = .lower, xmax = .upper, ymin = -Inf, ymax = Inf),
    fill = "gray80", alpha = 0.5, inherit.aes = FALSE
  ) +
  # Add the grand mean median as a dashed line
  geom_vline(
    data = grand_mean_intervals,
    aes(xintercept = grand_mean_epred),
    linetype = "dashed", color = "black"
  ) +
  # Add the synopsis-specific probabilities and their CIs
  geom_pointinterval(aes(x = .epred, xmin = .lower_synopsis, xmax = .upper_synopsis, color = direction), size = 1) +
  facet_wrap(~ study.design.grouped, scales = "free_x") +
  scale_color_manual(values = c(
    "Higher than average" = "blue",
    "Lower than average" = "red",
    "Not significant" = "black" # Changed for better visibility against the grey rect
  )) +
  labs(x = "Predicted Probability (averaged across years)",
    y = "Synopsis",
    color = "Probability vs. Grand Mean"
  ) +
  theme_cowplot() +
  theme(
    strip.background = element_rect(fill="white"),
    strip.text= element_text(color="black"),
    axis.text.y = element_text(size = 8),
    legend.key.width = unit(0.1,"cm"),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines") # Add some space between facets
  ) +
  scale_x_continuous(limits = c(0, 1))

print(synopsis_model_plot)