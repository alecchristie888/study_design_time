#######################################################################################################
############################# Code for creating plots of basic analyses ###############################
#######################################################################################################

# Load libraries
library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(viridis)
library(cowplot)
library(tidyr)

df <- read_xlsx("CE_Database_Publication.xlsx")  # Load your dataset
df_ne <- read_xlsx("NonEnglish_Database_Publication.xlsx")

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

# recode study design names
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

#relevel factors
df_analysis$study.design.grouped <- factor(df_analysis$study.design.grouped,
                                           levels = c("After", "BA", "CI", "BACI", "Rand.exp."))

df_analysis <- droplevels(df_analysis)

df_ne_analysis$study.design.grouped <- factor(df_ne_analysis$study.design.grouped,
                                              levels = c("After", "BA", "CI", "BACI", "Rand.exp."))

df_ne_analysis <- droplevels(df_ne_analysis)

unique(df_ne_analysis$study.design.grouped)
unique(df_analysis$study.design.grouped)

#check non-E languages
unique(df_ne_analysis$Language)

#select key data
df_analysis_short <- df_analysis %>% select(study.ID, synopsis, year, study.design.grouped, continent, country, lat, long)
df_ne_analysis$synopsis <- "Non-English language"
df_ne_analysis_short <- df_ne_analysis %>% select(study.ID, year, synopsis, study.design.grouped, continent, country, lat, long)
str(df_analysis_short)
str(df_ne_analysis_short)

#combine into one dataset
df_analysis_all <- rbind(df_analysis_short, df_ne_analysis_short)
unique(df_analysis_all$study.design.grouped)
str(df_analysis_all)


###################################################################################################
############################## BUILD PLOTS ########################################################
###################################################################################################


# calculate the proportion of each study design within each synopsis,
# and also gets the total number of studies for each synopsis.
plot_data <- df_analysis_all %>%
  # Count the occurrences of each study design within each synopsis
  count(synopsis, study.design.grouped, name = "n_studies") %>%
  
  # Group by synopsis to calculate proportions and totals
  group_by(synopsis) %>%
  mutate(
    proportion = n_studies / sum(n_studies),
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
plot_data$synopsis <- gsub("Non-English ", "NE-", plot_data$synopsis)
plot_data$synopsis <- gsub("Sustainable ", "Sust.", plot_data$synopsis)

unique(plot_data$synopsis)
# --- Determine the Order of Synopses ---

# sort names by the proportion of the "After" study design in descending order.
synopsis_order <- plot_data %>%
  filter(study.design.grouped == "After") %>%
  arrange(desc(proportion)) %>%
  pull(synopsis)


# --- Create the Plot ---

ggplot(plot_data, aes(x = factor(synopsis, levels = synopsis_order), y = proportion, fill = study.design.grouped)) +
  
  # Create the stacked bars
  geom_bar(stat = "identity", position = "stack") +
  
  # Add the text labels for the total counts at the top of each bar
  geom_text(
    # separate, distinct dataset for the labels to avoid duplicates
    data = distinct(plot_data, synopsis, total_studies),
    aes(x=synopsis,label = total_studies, y = 1.02), # Position the labels just above the bars
    size = 4, # Adjust size as needed
    inherit.aes = FALSE # prevent mapping conflicts
  ) +
  scale_fill_viridis_d(name = "Study Design", option = "D") +
  
  # Adjust y-axis to make space for the text labels
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks=seq(0,1,0.1)) +
  
  # Add labels and title
  labs(
    x = "Synopsis (subfield)",
    y = "Proportion of studies"
  ) +
  
  # move the legend to the bottom
  theme_cowplot() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1,size=16),
    axis.text.y = element_text(size=16),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )


###################################################################################
######################## Plot cumulative studies over time ########################
###################################################################################

# --- Data Preparation for Cumulative Proportions ---

# 1. Get annual counts for each study design, ensuring all combinations exist.
design_annual_counts <- df_analysis_all %>%
  count(year, study.design.grouped) %>%
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

cum_prop_plot <- ggplot(plot_data, aes(x = year, y = cum_prop, fill = study.design.grouped)) +
  # Use position = "stack" to stack the areas on top of each other
  geom_area(position = "stack", alpha = 0.8) +
  scale_y_continuous(labels = scales::percent_format()) + # Format y-axis as percentage
  labs(
    x = "Year",
    y = "Cumulative proportion of all studies",
    fill = "Study design"
  ) +
  scale_fill_viridis_d(option = "D") + # Use a colorblind-friendly palette
  theme_cowplot() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  )

# Display the plot
print(cum_prop_plot)


