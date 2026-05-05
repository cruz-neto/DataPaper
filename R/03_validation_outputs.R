# Functions: standardize and validate CNPJ (DV)
library(dplyr)
library(stringr)

pad_cnpj14 <- function(x) {
  x %>%
    as.character() %>%
    str_remove_all("\\D+") %>%
    str_pad(14, side = "left", pad = "0")
}

cnpj_is_valid <- function(cnpj14) {
  if (is.na(cnpj14) || nchar(cnpj14) != 14 || !grepl("^[0-9]{14}$", cnpj14)) return(FALSE)
  if (grepl("^([0-9])\\1{13}$", cnpj14)) return(FALSE)
  d <- as.integer(strsplit(cnpj14, "")[[1]])
  w1 <- c(5,4,3,2,9,8,7,6,5,4,3,2)
  w2 <- c(6,5,4,3,2,9,8,7,6,5,4,3,2)
  
  dv1 <- { r <- sum(d[1:12]*w1) %% 11; if (r < 2) 0 else 11 - r }
  dv2 <- { r <- sum(c(d[1:12], dv1)*w2) %% 11; if (r < 2) 0 else 11 - r }
  (d[13] == dv1) && (d[14] == dv2)
}

# 6.2 — diag_valid_cnpj.csv (diagnosis of DV/Covering)
diag_valid_cnpj <- database %>%
  st_drop_geometry() %>% 
  transmute(
    taxpayer_id_raw = taxpayer_id,
    taxpayer_digits = str_remove_all(as.character(taxpayer_id), "\\D+"),
    n_raw = nchar(taxpayer_digits),
    taxpayer_pad14 = if_else(n_raw == 13, str_pad(taxpayer_digits, 14, "left", "0"), taxpayer_digits),
    n_pad = nchar(taxpayer_pad14),
    valid_pad = if_else(n_pad == 14, vapply(taxpayer_pad14, cnpj_is_valid, logical(1)), FALSE)
  ) %>%
  count(n_raw, n_pad, valid_pad, name = "n") %>%
  arrange(desc(n))

readr::write_csv(diag_valid_cnpj, "diag_valid_cnpj.csv")

# 6.3 — qualidade_geral.csv (main CNAE coverage)
qualidade_geral <- database %>%
  st_drop_geometry() %>% 
  summarise(
    total_rows = n(),
    rows_with_primary_cnae = sum(!is.na(primary_cnae_code) & primary_cnae_code != ""),
    rows_without_primary_cnae = sum(is.na(primary_cnae_code) | primary_cnae_code == ""),
    prop_with_primary_cnae = mean(!is.na(primary_cnae_code) & primary_cnae_code != "")
  )

readr::write_csv(qualidade_geral, "qualidade_geral.csv")


# 6.4 — qualidade_ano_uf.csv (CNAE coverage by year and state "UF".)
library(lubridate)

qualidade_ano_uf <- database %>%
  st_drop_geometry() %>% 
  mutate(
    year = year(infraction_date),
    has_primary_cnae = !is.na(primary_cnae_code) & primary_cnae_code != ""
  ) %>%
  filter(year >= 1980, year <= 2026) %>% # ajuste para sua janela final
  group_by(year, state_abbr) %>%
  summarise(
    n = n(),
    n_with_primary_cnae = sum(has_primary_cnae),
    prop_with_primary_cnae = n_with_primary_cnae / n,
    .groups = "drop"
  )

readr::write_csv(qualidade_ano_uf, "qualidade_ano_uf.csv")

## saving final database
database <- database %>% st_drop_geometry()

library(readr)

write_csv(database, gzfile("database.csv.gz"))
