library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

padroniza_cnpj <- function(x) {
  x %>%
    as.character() %>%
    str_remove_all("[^0-9]") %>%
    str_pad(width = 14, side = "left", pad = "0")
}

#========================================
# 1) Base IBAMA: apenas empresas
#========================================
ibama <- Ibamam::get_dataset_ibamam("distribuidas") 

ibama %>% 
  mutate(
    ano = year(dataAuto)) %>%
  filter(ano > 1980, ano < 2026) %>% 
count()

ibama_cnpj <- ibama %>%
  mutate(
    ano = year(dataAuto)) %>%
  filter(ano > 1980, ano < 2026) %>% 
  filter(enquadramentoJuridico == "CNPJ")

ibama_cnpj <- ibama_cnpj %>%
  mutate(
    cnpj_original = cpfCnpj,
    cnpj_pad = padroniza_cnpj(cpfCnpj)
  )

#========================================
# 2) Base full enxuta: só chave + CNAEs
#========================================

library(readr)
# carregando dados a receita federal
base_full <- read_csv("./data/bq-results-20260330-171136-1774890745668.csv", 
                      col_types = cols(cnae_fiscal_secundaria = col_character()))

base_full2 <- base_full %>%
  mutate(
    cnpj_receita = padroniza_cnpj(cnpj_receita),
    cnae_fiscal_principal = as.character(cnae_fiscal_principal),
    cnae_fiscal_secundaria = as.character(cnae_fiscal_secundaria)
  ) %>%
  select(
    cnpj_receita,
    cnae_fiscal_principal,
    cnae_fiscal_secundaria
  ) %>%
  distinct(cnpj_receita, .keep_all = TRUE)

#========================================
# 3) Diagnóstico de match
#========================================
diag_linhas <- ibama_cnpj %>%
  mutate(match_base_full = cnpj_pad %in% base_full2$cnpj_receita)

diag_unicos <- ibama_cnpj %>%
  distinct(cnpj_pad) %>%
  mutate(match_base_full = cnpj_pad %in% base_full2$cnpj_receita)

cat("\n===== DIAGNÓSTICO EM LINHAS =====\n")
diag_linhas %>%
  count(match_base_full) %>%
  mutate(prop = n / sum(n)) %>%
  print()

cat("\n===== DIAGNÓSTICO EM CNPJS ÚNICOS =====\n")
diag_unicos %>%
  count(match_base_full) %>%
  mutate(prop = n / sum(n)) %>%
  print()

#========================================
# 4) Enriquecer a base IBAMA
#========================================
ibama_enriquecida <- ibama_cnpj %>%
  left_join(
    base_full2,
    by = c("cnpj_pad" = "cnpj_receita")
  )

#========================================
# 5) Cobertura final
#========================================
cat("\n===== COBERTURA FINAL NA BASE DO IBAMA (LINHAS) =====\n")
ibama_enriquecida %>%
  summarise(
    total_linhas = n(),
    linhas_com_cnae_principal = sum(!is.na(cnae_fiscal_principal) & cnae_fiscal_principal != ""),
    linhas_sem_cnae_principal = sum(is.na(cnae_fiscal_principal) | cnae_fiscal_principal == ""),
    prop_linhas_com_cnae_principal = mean(!is.na(cnae_fiscal_principal) & cnae_fiscal_principal != "")
  ) %>%
  print()

cat("\n===== COBERTURA FINAL NA BASE DO IBAMA (CNPJS ÚNICOS) =====\n")
ibama_enriquecida %>%
  distinct(cnpj_pad, cnae_fiscal_principal) %>%
  summarise(
    total_cnpjs_unicos = n(),
    cnpjs_com_cnae_principal = sum(!is.na(cnae_fiscal_principal) & cnae_fiscal_principal != ""),
    cnpjs_sem_cnae_principal = sum(is.na(cnae_fiscal_principal) | cnae_fiscal_principal == ""),
    prop_cnpjs_com_cnae_principal = mean(!is.na(cnae_fiscal_principal) & cnae_fiscal_principal != "")
  ) %>%
  print()

#========================================
# 6) Exemplos sem match
#========================================
cat("\n===== 10 EXEMPLOS SEM CNAE PRINCIPAL =====\n")
ibama_enriquecida %>%
  filter(is.na(cnae_fiscal_principal) | cnae_fiscal_principal == "") %>%
  select(cnpj_original, cnpj_pad) %>%
  distinct() %>%
  slice_head(n = 10) %>%
  print()

#========================================
# 7) Cobertura anual
#========================================
ibama_enriquecida %>%
  mutate(
    ano = year(dataAuto),
    tem_cnae = !is.na(cnae_fiscal_principal) & cnae_fiscal_principal != ""
  ) %>%
  filter(ano >= 1980, ano <= 2026) %>%
  count(ano, tem_cnae) %>%
  group_by(ano) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  filter(tem_cnae) %>%
  ggplot(aes(x = ano, y = prop)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Ano",
    y = "Proporção com CNAE principal",
    title = "Cobertura anual do pareamento CNAE na base do IBAMA"
  ) +
  theme_minimal()


#========================================
# 8) Inserindo coordenadas geograficas - municipios
#========================================

library(geobr)
library(dplyr)
library(sf)

# Baixa todos os municípios do Brasil (ano mais recente disponível)
municipios_geo <- read_municipality(year = 2022, showProgress = FALSE)

# Padroniza o código do município para join
# geobr usa código numérico de 7 dígitos (sem o dígito verificador)
ibama_enriquecida <- ibama_enriquecida %>%
  left_join(
    municipios_geo %>%
      select(code_muni) %>%
      mutate(codigoMunicipio = as.character(code_muni)),
    by = "codigoMunicipio"
  ) %>%
  st_as_sf() 

var_cortadas <- c("nomeMunicipio", "codigoMunicipio", "ultimaAtualizacaoRelatorio", 
                  "enquadramentoJuridico", "cpfCnpj", "cnpj_pad")

# excluindo variaveis - desnecessárias
base_final <- ibama_enriquecida %>% select(-any_of(var_cortadas))

#  library(readr)
tab_cnae <- read_delim("data/tab_cnae.csv", 
                       delim = "#", escape_double = FALSE, trim_ws = TRUE)

library(dplyr)
library(stringr)

# 1) preparar dicionário CNAE
cnae_dict <- tab_cnae %>%
  transmute(
    cod_cnae = str_pad(as.character(cod_cnae), width = 7, side = "left", pad = "0"),
    nm_cnae,
    cod_classe,
    nm_classe,
    cod_grupo,
    nm_grupo,
    cod_divisao,
    nm_divisao,
    cod_secao,
    nm_secao
  ) %>%
  distinct(cod_cnae, .keep_all = TRUE)

# 2) compatibilizar e juntar na base enriquecida
base_enriquecida2 <- ibama_enriquecida %>%
  mutate(
    cnae_fiscal_principal = str_pad(as.character(cnae_fiscal_principal),
                                    width = 7, side = "left", pad = "0")
  ) %>%
  left_join(
    cnae_dict,
    by = c("cnae_fiscal_principal" = "cod_cnae")
  )

library(dplyr)

base_english <- base_enriquecida2 %>%
  rename(
    infraction_date = dataAuto,
    municipality_name = nomeMunicipio,
    municipality_name_geobr = nomeMunicipio_geobr,
    municipality_code = codigoMunicipio,
    infraction_notice_number = numAI,
    infraction_type = tipoInfracao,
    report_last_update = ultimaAtualizacaoRelatorio,
    state_abbr = uf,
    debt_status = situacaoDebito,
    notice_type = tipoAuto,
    legal_framework = enquadramentoLegal,
    company_name = nomeRazaoSocial,
    taxpayer_id = cpfCnpj,
    fine_amount = valorAuto,
    legal_entity_type = enquadramentoJuridico,
    original_cnpj = cnpj_original,
    cnpj = cnpj_pad,
    primary_cnae_code = cnae_fiscal_principal,
    secondary_cnae_codes = cnae_fiscal_secundaria,
    geobr_municipality_code = code_muni,
    
    primary_cnae_description = nm_cnae,
    cnae_class_code = cod_classe,
    cnae_class_description = nm_classe,
    cnae_group_code = cod_grupo,
    cnae_group_description = nm_grupo,
    cnae_division_code = cod_divisao,
    cnae_division_description = nm_divisao,
    cnae_section_code = cod_secao,
    cnae_section_description = nm_secao
  )

cut_var <- c("municipality_name_geobr", "report_last_update", 
             "legal_entity_type", "original_cnpj" , "cnpj", 
             "geobr_municipality_code")

database <- base_english %>% select(-any_of(cut_var))

database <- database %>%
  mutate(record_id = paste0("BRINF_", sprintf("%06d", row_number())))


database <- database %>% 
  st_drop_geometry() %>%
  mutate(ano = as.integer(ano)) %>%
  filter(ano >= 1980, ano < 2026)

