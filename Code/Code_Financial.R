###############################################################################
# FINANCIAL ECONOMETRICS PROJECT
# Author : Yassine Mandengue
###############################################################################


###############################################################################
# PACKAGES
###############################################################################

library(tidyverse)
library(scales)
library(ggtext)
library(lubridate)
library(fixest)
library(treemapify)


###############################################################################
# PART I – MOTIVATIONAL DATA VISUALIZATION
# "Who emits like a country?"
###############################################################################

# ---------------------------------------------------------------------------
# 1. Data Import
# ---------------------------------------------------------------------------

emissions_low_granularity <- read_csv("Downloads/emissions_low_granularity.csv")

annual_co2_emissions_per_country <- read_csv(
  "Desktop/M2 FIRE/Classes/Financial Econometrics and dataviz/Project/
   annual-co2-emissions-per-country.filtered/
   annual-co2-emissions-per-country.csv"
)


# ---------------------------------------------------------------------------
# 2. Country-Level Data Preparation
# ---------------------------------------------------------------------------
# - Select a subset of representative countries
# - Restrict the period to 2013–2024
# - Convert emissions from tonnes to million tonnes (MtCO2)

df_countries <- annual_co2_emissions_per_country %>%
  filter(
    Entity %in% c("United States", "China", "Brazil",
                  "France", "Netherlands", "Belgium"),
    Year >= 2013, Year <= 2024
  ) %>%
  transmute(
    Year = Year,
    Entity = Entity,
    Emissions_Mt = round(`Annual CO₂ emissions` / 1e6, 2)
  )


# ---------------------------------------------------------------------------
# 3. Company-Level Data Preparation
# ---------------------------------------------------------------------------
# - Focus on major oil & gas companies
# - Emissions already expressed in MtCO2e
# - Include Scope 1, 2 and 3 emissions

df_companies <- emissions_low_granularity %>%
  filter(
    parent_entity %in% c("ExxonMobil", "Shell", "TotalEnergies"),
    year >= 2013, year <= 2024
  ) %>%
  transmute(
    Year = year,
    Entity = parent_entity,
    Emissions_Mt = round(total_emissions_MtCO2e, 2)
  )


# ---------------------------------------------------------------------------
# 4. Merge Countries and Companies
# ---------------------------------------------------------------------------
# - Create a unified long-format dataset
# - Define entity type
# - Set factor order for stacked visualization

data_long <- bind_rows(df_countries, df_companies) %>%
  mutate(
    Type = case_when(
      Entity %in% c("United States", "China", "Brazil",
                    "France", "Netherlands", "Belgium") ~ "Country",
      TRUE ~ "Company"
    ),
    Entity = factor(
      Entity,
      levels = c(
        "China", "United States",
        "ExxonMobil", "Shell", "TotalEnergies",
        "Brazil", "France",
        "Netherlands", "Belgium"
      )
    )
  )


# ---------------------------------------------------------------------------
# Treemap 5 – DataViz F4A261
# ---------------------------------------------------------------------------
# Last year
latest_year <- max(data_long$Year)

treemap_data <- data_long %>%
  filter(Year == latest_year)

# --- Colors ---
colors_entities <- c(
  "China"          = "#E63946",
  "United States"  = "#E8A0BF",
  "ExxonMobil"     = "#2A9D8F",
  "Shell"          = "#E9C46A",
  "TotalEnergies"  = "#F4A261",
  "Brazil"         = "#264653",
  "France"         = "#6A4C93",
  "Netherlands"    = "#E76F51",
  "Belgium"        = "#457B9D"
)

colors_entities2 <- c(
  "China"          = "#E63946",
  "United States"  = "#E8A0BF",
  "ExxonMobil"     = "#E9C46A",
  "Shell"          = "#E9C46A",
  "TotalEnergies"  = "#E9C46A",
  "Brazil"         = "#264653",
  "France"         = "#6A4C93",
  "Netherlands"    = "#E76F51",
  "Belgium"        = "#457B9D"
)

p_tree_all <- ggplot(treemap_data, aes(
  area = Emissions_Mt,
  fill = Entity,
  label = paste0(Entity, "\n", round(Emissions_Mt), " Mt")
)) +
  geom_treemap(color = "white", size = 2) +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    size = 12,
    grow = FALSE,
    reflow = TRUE
  ) +
  scale_fill_manual(values = colors_entities2) +
  labs(
    title = "Who emits like a country?",
    subtitle = paste0("CO₂ emissions in ", latest_year, ": countries vs. oil & gas companies"),
    caption = "Sources: Global Carbon Budget (2025), Carbon Majors Database, CDP\nNote: Company emissions include Scope 1, 2, and 3"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 20, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, color = "gray30", margin = margin(b = 10)),
    plot.caption  = element_text(size = 8, color = "gray50", hjust = 0, margin = margin(t = 15)),
    legend.position = "none",
    plot.margin = margin(20, 20, 15, 15)
  )

p_tree_all

# ---------------------------------------------------------------------------
# Treemap 5.1 – Without US and China
# ---------------------------------------------------------------------------

treemap_data_excl <- treemap_data %>%
  filter(!Entity %in% c("United States", "China"))

p_tree_excl <- ggplot(treemap_data_excl, aes(
  area = Emissions_Mt,
  fill = Entity,
  label = paste0(Entity, "\n", round(Emissions_Mt), " Mt")
)) +
  geom_treemap(color = "white", size = 2) +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    size = 12,
    grow = FALSE,
    reflow = TRUE
  ) +
  scale_fill_manual(values = colors_entities) +
  labs(
    title = "Who emits like a country? (excl. US & China)",
    subtitle = paste0("CO₂ emissions in ", latest_year),
    caption = "Sources: Global Carbon Budget (2025), Carbon Majors Database, CDP"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 20, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, color = "gray30", margin = margin(b = 10)),
    plot.caption  = element_text(size = 8, color = "gray50", hjust = 0, margin = margin(t = 15)),
    legend.position = "none",
    plot.margin = margin(20, 20, 15, 15)
  )

p_tree_excl


###############################################################################
# PART II – STOCK RETURNS AND PARALLEL TRENDS
###############################################################################

# ---------------------------------------------------------------------------
# 1. Stock Prices and Metadata
# ---------------------------------------------------------------------------

prices <- read_csv("stock_prices.csv") %>%
  mutate(Date = as.Date(Date))

metadata <- read_csv("companies_metadata.csv") %>%
  mutate(Treatment = ifelse(Sector == "Green", 1, 0))


# ---------------------------------------------------------------------------
# 2. Long Format and Cleaning
# ---------------------------------------------------------------------------

prices_long <- prices %>%
  pivot_longer(cols = -Date, names_to = "Ticker", values_to = "Price") %>%
  filter(!is.na(Price)) %>%
  arrange(Ticker, Date) %>%
  left_join(metadata, by = "Ticker") %>%
  filter(!is.na(Sector))


# ---------------------------------------------------------------------------
# 3. Return Computation
# ---------------------------------------------------------------------------
# Returns are computed over the full sample to ensure precision.

returns_data <- prices_long %>%
  group_by(Ticker) %>%
  arrange(Date) %>%
  mutate(
    Return = (Price / lag(Price) - 1) * 100,
    Log_Return = log(Price / lag(Price)) * 100
  ) %>%
  ungroup() %>%
  filter(!is.na(Return))


# ---------------------------------------------------------------------------
# 4. Event Window Around COP21
# ---------------------------------------------------------------------------

returns_data_filtered <- returns_data %>%
  filter(Date >= "2015-07-01" & Date <= "2017-06-30")

cop21_date <- as.Date("2015-12-12")


# ---------------------------------------------------------------------------
# 5. Parallel Trends – Quarterly Aggregation
# ---------------------------------------------------------------------------

quarterly_returns <- returns_data_filtered %>%
  mutate(Quarter = floor_date(Date, "quarter")) %>%
  group_by(Quarter, Sector) %>%
  summarise(Mean_Return = mean(Return, na.rm = TRUE), .groups = "drop")

quarterly_cumulative <- quarterly_returns %>%
  arrange(Sector, Quarter) %>%
  group_by(Sector) %>%
  mutate(Cumulative_Return = cumsum(Mean_Return)) %>%
  ungroup()

# Normalization at Q4 2015 (just before COP21)
baseline_quarter <- as.Date("2015-10-01")

baseline_values <- quarterly_cumulative %>%
  filter(Quarter == baseline_quarter) %>%
  select(Sector, Baseline = Cumulative_Return)

quarterly_normalized <- quarterly_cumulative %>%
  left_join(baseline_values, by = "Sector") %>%
  mutate(Normalized_Return = Cumulative_Return - Baseline)


# ---------------------------------------------------------------------------
# 6. Parallel Trends Visualization
# ---------------------------------------------------------------------------

plot_parallel_trends <- ggplot(
  quarterly_normalized,
  aes(x = Quarter, y = Normalized_Return, color = Sector)
) +
  annotate("rect",
           xmin = min(quarterly_normalized$Quarter),
           xmax = baseline_quarter,
           ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.4) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
  geom_vline(xintercept = cop21_date, linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("Green" = "#1B9E77", "Oil_Gas" = "#D95F02"),
    labels = c("Green" = "Green firms", "Oil_Gas" = "Oil & Gas firms")
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(
    title = "Parallel Trends Assumption",
    subtitle = "Cumulative returns normalized to zero before COP21 (Q4 2015)",
    y = "Normalized cumulative return (%)",
    x = NULL,
    caption = "Parallel pre-treatment trends support the validity of the DiD design"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot_parallel_trends


###############################################################################
# PART III – DIFFERENCE-IN-DIFFERENCES ESTIMATION
###############################################################################

did_data <- returns_data %>%
  mutate(
    Post = ifelse(Date >= cop21_date, 1, 0),
    DiD = Post * Treatment,
    YearMonth = floor_date(Date, "month")
  )


# ---------------------------------------------------------------------------
# DiD Models
# ---------------------------------------------------------------------------

# Model 1: Simple DiD (OLS)
model1 <- feols(Return ~ Treatment + Post + DiD, data = did_data)
summary(model1)

# Model 2: DiD with firm fixed effects
model2 <- feols(Return ~ Post + DiD | Ticker, data = did_data)
summary(model2)

# Model 3: DiD with firm and time fixed effects
model3 <- feols(Return ~ DiD | Ticker + YearMonth, data = did_data)
summary(model3)

# Model 4: Heterogeneous effects by country
model4 <- feols(Return ~ DiD * Country | Ticker + YearMonth, data = did_data)
summary(model4)



# =============================================================================
# EXPORT PLOTS
# =============================================================================

ggsave("ptree_all.png", p_tree_all, width = 12, height = 7, dpi = 300)
ggsave("ptree.png", p_tree_excl, width = 12, height = 7, dpi = 300)
ggsave("parallel_trend.png", plot_parallel_trends, width = 12, height = 7, dpi = 300)



# =============================================================================
# EXPORT DiD RESULTS TO LATEX TABLE
# =============================================================================

etable(
  model1, model2, model3, model4,
  tex = TRUE,
  file = "did_results.tex",
  title = "Difference-in-Differences Estimation Results",
  dict = c(
    Treatment = "Green firm",
    Post = "Post-COP21",
    DiD = "Treatment $\\times$ Post"
  ),
  se.below = TRUE,
  digits = 3,
  fitstat = ~ n + r2,
  replace = TRUE
)
