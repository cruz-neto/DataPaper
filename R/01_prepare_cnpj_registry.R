library(data.table)

reparar_csv_cnpj_multilinha <- function(arquivo_entrada,
                                        arquivo_saida,
                                        encoding = "UTF-8") {
  
  con_in  <- file(arquivo_entrada, open = "r", encoding = encoding)
  con_out <- file(arquivo_saida,   open = "w", encoding = encoding)
  
  on.exit({
    close(con_in)
    close(con_out)
  }, add = TRUE)
  
  # cabeçalho original
  header <- readLines(con_in, n = 1, warn = FALSE)
  cabecalho <- strsplit(header, ",", fixed = TRUE)[[1]]
  n_campos <- length(cabecalho)
  
  # escrever cabeçalho novo
  writeLines(paste(cabecalho, collapse = ";"), con_out, useBytes = TRUE)
  
  # cada registro "completo" termina com ,NA repetido 29 vezes
  padrao_fim <- paste0("(,NA){", n_campos - 1, "}$")
  
  buffer <- character(0)
  n_reg <- 0L
  
  repeat {
    linha <- readLines(con_in, n = 1, warn = FALSE)
    if (length(linha) == 0) break
    
    if (length(buffer) == 0) {
      buffer <- linha
    } else {
      buffer <- paste0(buffer, "\n", linha)
    }
    
    # só processa quando o registro lógico terminou
    if (grepl(padrao_fim, buffer, perl = TRUE)) {
      
      reg <- buffer
      
      # remover sufixo artificial
      reg <- sub(padrao_fim, "", reg, perl = TRUE)
      
      # remover aspas externas do registro inteiro
      reg <- sub('^"', "", reg)
      reg <- sub('"$', "", reg)
      
      # normalizar aspas duplicadas
      reg <- gsub('""', '"', reg, fixed = TRUE)
      
      writeLines(reg, con_out, useBytes = TRUE)
      
      buffer <- character(0)
      n_reg <- n_reg + 1L
    }
  }
  
  if (length(buffer) > 0) {
    warning("Sobrou um registro incompleto no final do arquivo.")
  }
  
  invisible(list(
    arquivo_saida = arquivo_saida,
    n_colunas = n_campos,
    n_registros = n_reg
  ))
}

entrada <- "D:/DADOS CNPJ/bd-tratado/CNPJ_ESTABELE.csv"
saida2  <- "D:/DADOS CNPJ/bd-tratado/CNPJ_ESTABELE_REPARADO2.csv"

reparar_csv_cnpj_multilinha(entrada, saida2)




vars_interesse <- c(
  "cnpj_basico",
  "cnpj_ordem",
  "cnpj_dv",
  "matriz_filial",
  "situacao_cadastral",
  "data_situacao_cadastral",
  "data_inicio_atividade",
  "cnae_fiscal_principal",
  "cnae_fiscal_secundaria",
  "cep",
  "uf",
  "municipio"
)

estabele <- fread(
  saida2,
  sep = ";",
  quote = '"',
  na.strings = c("", "NA"),
  encoding = "UTF-8",
  select = vars_interesse
)

dim(estabele)
ncol(estabele)
names(estabele)
estabele[1:3]

estabele[, cnpj := paste0(cnpj_basico, cnpj_ordem, cnpj_dv)]

library(dplyr)
library(stringr)

estabele <- estabele %>%
  mutate(
    cnpj = paste0(
      str_pad(as.character(cnpj_basico), 8, pad = "0"),
      str_pad(as.character(cnpj_ordem), 4, pad = "0"),
      str_pad(as.character(cnpj_dv), 2, pad = "0")
    )
  )

################################################
## BAIXANDO DADOS IBAMA

dados_ibama <- Ibamam::get_dataset_ibamam("distribuidas")

dados_ibama %>% nrow() # numero de multas aplicadas

dados_ibama %>% 
  filter(enquadramentoJuridico == "CNPJ") %>%
  nrow() # numero de multas aplicadas por Empresas

dados_ibama_cnpj <- dados_ibama %>% 
  filter(enquadramentoJuridico == "CNPJ")

dados_ibama_cnpj %>% 
  select(-enquadramentoLegal) %>% 
   head(1) %>% 
  dput()


#######################################
### realizando junçao das bases 
library(dplyr)
library(stringr)

ibama_join <- dados_ibama_cnpj %>%
  mutate(
    cnpj = str_remove_all(cpfCnpj, "[^0-9]")
  )

estabele_join <- estabele %>%
  transmute(
    cnpj,
    matriz_filial,
    situacao_cadastral_estab = situacao_cadastral,
    data_situacao_cadastral_estab = data_situacao_cadastral,
    data_inicio_atividade_estab = data_inicio_atividade,
    cnae_fiscal_principal_estab = cnae_fiscal_principal,
    cnae_fiscal_secundaria_estab = cnae_fiscal_secundaria,
    cep_estab = cep,
    uf_estab = uf,
    municipio_estab = municipio
  ) %>%
  distinct(cnpj, .keep_all = TRUE)

ibama_estabele <- ibama_join %>%
  left_join(estabele_join, by = "cnpj")

sum(ibama_join$cnpj %in% estabele_join$cnpj, na.rm = TRUE)

nrow(dados_ibama_cnpj)
nrow(ibama_estabele)

