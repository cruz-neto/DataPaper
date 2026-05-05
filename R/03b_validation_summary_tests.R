# integridade cnpjs

library(dplyr)
library(stringr)
library(sf)

cnpj_stats <- database %>%
  st_drop_geometry() %>%
  mutate(year = year(infraction_date)) %>%
  filter(year >= 1981 & year <= 2025) %>% 
  mutate(
    taxpayer_digits = str_remove_all(as.character(taxpayer_id), "\\D+"),
    n_digits = nchar(taxpayer_digits),
    taxpayer_pad14 = str_pad(taxpayer_digits, 14, side = "left", pad = "0"),
    is_14 = nchar(taxpayer_pad14) == 14
  ) %>%
  summarise(
    total_rows = n(),
    rows_with_14_digits = sum(is_14),
    prop_with_14_digits = mean(is_14),
    unique_cnpjs = n_distinct(taxpayer_pad14)
  )

cnpj_stats

## match cnae

library(lubridate)

match_stats <- database %>%
  st_drop_geometry() %>%
  mutate(year = year(infraction_date)) %>%
  filter(year >= 1981 & year <= 2025) %>% 
  mutate(
    has_primary_cnae = !is.na(primary_cnae_code) & primary_cnae_code != ""
  ) %>%
  summarise(
    total_rows = n(),
    matched_rows = sum(has_primary_cnae),
    prop_matched_rows = mean(has_primary_cnae)
  )

match_stats

# cnpjs unicos
unique_match_stats <- database %>%
  st_drop_geometry() %>%
  mutate(year = year(infraction_date)) %>%
  filter(year >= 1981 & year <= 2025) %>% 
  mutate(has_primary_cnae = !is.na(primary_cnae_code) & primary_cnae_code != "") %>%
  distinct(taxpayer_id, has_primary_cnae) %>%
  summarise(
    total_unique = n(),
    matched_unique = sum(has_primary_cnae),
    prop_unique = mean(has_primary_cnae)
  )

unique_match_stats

##### cobertura geografica 

library(sf)
library(dplyr)
library(lubridate)

geo_stats <- database %>%
  st_drop_geometry() %>%  # <- ESSENCIAL
  mutate(year = year(infraction_date)) %>%
  filter(year >= 1981 & year <= 2025) %>%
  summarise(
    total = n(),
    missing_municipality_code = sum(is.na(municipality_code) | municipality_code == ""),
    prop_missing_municipality_code = mean(is.na(municipality_code) | municipality_code == "")
  )

geo_stats

## unicidade da chave key

df <- st_drop_geometry(database) %>% 
  mutate(year = year(infraction_date)) %>%
  filter(year >= 1981 & year <= 2025)

total <- nrow(df)
unique_keys <- length(unique(df$infraction_notice_number))
duplicates <- total - unique_keys

c(total = total, unique_keys = unique_keys, duplicates = duplicates)

# tabela de variaveis 

library(tibble)

data_dictionary <- tibble(
  variable = names(database),
  class = sapply(database, class)
)

data_dictionary

dict_core <- tibble(
  Variable = c(
    "infraction_notice_number",
    "infraction_date",
    "fine_amount",
    "company_name",
    "taxpayer_id",
    "primary_cnae_code",
    "secondary_cnae_codes",
    "municipality_code",
    "state_abbr"
  ),
  Type = c(
    "character",
    "Date",
    "numeric",
    "character",
    "character",
    "character",
    "character",
    "character",
    "character"
  ),
  Description = c(
    "Unique identifier of the infraction notice",
    "Date of infraction record",
    "Monetary value of applied fine",
    "Legal name of corporate offender",
    "Corporate taxpayer identifier (CNPJ)",
    "Primary CNAE 2.0 code (7 digits)",
    "Comma-separated secondary CNAE codes",
    "Official IBGE municipality code",
    "State abbreviation (UF)"
  )
)

dict_core  # Table 1. Core variables in database.csv

dict_core <- add_row(
  dict_core,
  Variable = "record_id",
  Type = "character",
  Description = "Deterministic row-level identifier created to ensure uniqueness of each record"
)


## cobertura temporal 
library(ggplot2)
library(lubridate)

fig_temporal <- database %>%
  st_drop_geometry() %>%
  mutate(year = year(infraction_date)) %>%
  filter(year >= 1981 & year <= 2025) %>% 
  count(year) %>%
  ggplot(aes(year, n)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(
    x = "Year",
    y = "Number of corporate infraction records",
    title = "Annual number of corporate environmental infraction notices (1981–2025)"
  ) +
  theme_minimal()

fig_temporal

# cobertura do cnae ao longo do tempo
fig_cnae_cov <- database %>%
  st_drop_geometry() %>%
  mutate(
    year = year(infraction_date),
    has_cnae = !is.na(primary_cnae_code) & primary_cnae_code != ""
  ) %>%
  filter(year >= 1981 & year <= 2025) %>% 
  group_by(year) %>%
  summarise(prop = mean(has_cnae)) %>%
  ggplot(aes(year, prop)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Year",
    y = "Proportion with primary CNAE",
    title = "Temporal coverage of CNAE linkage"
  ) +
  theme_minimal()

fig_cnae_cov

# distribuição por seçao de cnae
fig_cnae_section <- database %>%
  st_drop_geometry() %>%
  mutate(year = year(infraction_date)) %>%
  filter(year >= 1981 & year <= 2025) %>% 
  filter(!is.na(cnae_section_code)) %>%
  count(cnae_section_description, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(reorder(cnae_section_description, n), n)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "CNAE Section",
    y = "Number of infraction records",
    title = "Top CNAE sections among corporate infraction records"
  ) +
  theme_minimal()

fig_cnae_section

######
dict_core
cnpj_stats
match_stats
unique_match_stats
geo_stats
c(total = total, unique_keys = unique_keys, duplicates = duplicates)
data_dictionary
dict_core
