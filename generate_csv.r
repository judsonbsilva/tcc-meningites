library(read.dbc)
library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tmap)
library(sf)
library(knitr)
library(viridis)
library(stringr)

## Consolidação Banco de Dados
# O estudo utiliza dados do SINAN, coletados no formato DBC (DBF comprimido), com a devida aprovação pelo CEP, na Secretaria de Saúde do Estado do Piauí. O primeiro passo é definir uma área de trabalho e carregar as bases de dados, juntando todas em um único arquivo CSV.
# <http://www2.datasus.gov.br/DATASUS/index.php?area=02>

# Define o ambiente de trabalho e o diretório com os dados do projeto
# DESCOMENTAR ESSA PARTE CASO QUEIRA RODAR (NO GERAL NEM PRECISA)
# -----> setwd(paste(getwd(), '/Nuvem/Cientifico/meningites piaui/conducao', sep=''))

# Função para carregar os DBC (recebe o diretório)
# quando estão separados em vários arquivos
load_dbcs <- function(dirname){
  # Pega arquivos '.dbc' do diretório
  files <- list.files(dirname, pattern='dbc$')
  data <- data.frame()
  # Conta quantas linhas o arquivo final deve conter para
  # garantir que o código está correto
  lines_number <- 0
  for(file in files){
    # Carrega arquivo da lista
    temp <- read.dbc(paste(dirname, file, sep=''))
    # Caso tenha dado tudo certo, printa no console
    print(paste(file, 'ok.', as.character(nrow(temp))))
    # Mescla com o arquivo anterior
    data <- rbind(data, temp)
    # Conta o número de linhas
    lines_number <- lines_number + nrow(temp)
  }
  # Confere se o número de linhas bateu com o esperado
  if(lines_number != nrow(data)){
    rlang::abort("Erro ao carregar dados")
  }
  print("Dados carregados com sucesso.")
  print(paste("Total de", lines_number, "casos"))
  
  return(data)
}
# Função para ler um único arquivo dbc
load_dbc <- function(dirname, filename){
  temp <- read.dbc(paste(dirname, filename, sep=''))
  # Caso tenha dado tudo certo, printa no console
  print(paste(filename, 'ok.', as.character(nrow(temp))))
  return(temp)
}

# Chama a função passando o diretório em que estão os arquivos .dbc
raw_data <- load_dbc('./dados/', 'MENINNET.dbc')

# Exclui as informações sensíveis que possam existir
raw_data = raw_data %>% select(-one_of(c('NM_MAE_PAC', 'FONETICA_N', 'SOUNDEX', 'NM_PACIENT', 'NU_DDD_TEL', 'NU_TELEFON', 'NM_CONTATO', 'TEL_CONTAT', 'END_CONTAT', 'NM_REFEREN', 'ID_GEO1', 'ID_GEO2', 'NM_COMPLEM', 'NU_NUMERO')))

# Divisão por faixa etária
faixa.etaria <- c(
  'Abaixo de 1 ano',
  'De 1 e 4 anos',
  'De 5 a 14 anos',
  'De 15 a 19 anos',
  'De 20 a 39 anos',
  'De 40 a 59 anos',
  '60 anos ou mais'
)

idade.classifier <- function(idade){
  case_when(
    idade < 1 ~ '1',
    idade >= 1 & idade < 5 ~ '2',
    idade >= 5 & idade < 15 ~ '3',
    idade >= 15 & idade < 20 ~ '4',
    idade >= 20 & idade < 40 ~ '5',
    idade >= 40 & idade < 60 ~ '6',
    idade >= 60 ~ '7',
    is.na(idade) ~ '8'
  )
}

raw_data <- raw_data %>%
  # Cria uma coluna com o ano de notificação do caso
  mutate(ANO_NOTIF = format(DT_NOTIFIC, "%Y")) %>%
  # Filtra as notificações que estão entre 2007 e 2019
  filter(ANO_NOTIF >= 2007 & ANO_NOTIF < 2020) %>%
  # Filtra os casos que contém as informações básicas de interesse
  filter(CLASSI_FIN %in% c('1', '2') & EVOLUCAO %in% c('1','2','3') & CS_SEXO %in% c('M', 'F')) %>%
  # Estima a idade do caso no período de adoecimento
  mutate(IDADE = round(as.numeric(DT_SIN_PRI - DT_NASC) / 365.242199, 1)) %>%
  # Distribui os pacientes por faixa etaria
  mutate(FAIXA.ETARIA = factor(idade.classifier(IDADE)))

# Salva uma versão do banco de dados em CSV 
write.csv(raw_data, 'banco_meningites.csv')
