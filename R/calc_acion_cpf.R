# número médio de ligações por contato que conseguiu pgto
require("xlsx")
require("lubridate")
#require("data.table")

# ler planilha com dados de acionamentos
df_acion <- read.xlsx2("./data/Acionamentos out e nov 2015-raw.xlsx", sheetIndex = 1, header = TRUE)
# acertando a coluna data que no excel está numérica
df_acion <-
    df_acion %>%
    mutate (Data.Agendamento = as.Date(as.numeric(paste(Data.Agendamento)), origin="1899-12-30") )

# calcular mediana de acionamentos por contrato
df_acion_mediana <-
    df_acion %>%
    group_by(Contrato) %>%
    summarise(tot_cont = n())

acion_median <- median(df_acion_mediana$tot_cont)

#-----------------------------------------------------------
# obter dados de pgto
#-----------------------------------------------------------
# obs: campos de valor já vêem sem vírgula como separador de milhar
# obs: cuidado para limpar colunas de totais do xlsx
df_pg <- read.xlsx2("./data/PGTO 2015-cass.xls", sheetIndex = 1, header = TRUE)
# acerta valores numericos do Excel para valor de data correta
df_pg <-
    df_pg %>%
    mutate (Recebimento = as.Date(as.numeric(paste(Recebimento)), origin="1899-12-30"))
# seleciona somente pagamentos do mês de outubro em diante 
# (a partir do inicio do acionamento analisado)
# obs: não precisa se nro de contrato é correspondente a cada dívida, ou seja,
# diferentes para o memso CPF

# acerta colunas a usar
df_pg <-
    df_pg %>%
    mutate(Contrato = Nosso.Número) %>%
    mutate(Contrato = paste0(substr(Nosso.Número, 1,5),"-", substr(Nosso.Número, 6,8))) %>%
    select(CPF, Contrato, Valor = Valor.Acordo) # ou usar Valor.Principal?

df_pg <- df_pg %>% mutate (Valor = as.character(Valor)) # para acerto do merge com cliav
df_pg <- df_pg %>% mutate (Valor = as.numeric(Valor)) # para acerto do merge com cliav
df_pg <- df_pg %>% mutate (CPF = as.character(CPF)) # para acerto do merge com cliav

# pegar somente sem duplicidade de contratos em pgto
df_pg <- 
    df_pg %>%
    distinct(Contrato)

# todos de acionamento, somente pgtos que aparecem em acionamento
df_acion_pg <- left_join(df_acion, df_pg,by=c("Contrato"))

# contar nro de linhas de acionamento por contrato com pgto
df_com_pg <-
    df_acion_pg %>%
    filter (!is.na(CPF))
# contar nro de linhas de acionamento por contrato sem pgto
df_sem_pg <-
    df_acion_pg %>%
    filter (is.na(CPF))

# contar nro de linhas de acionamento por contrato pago, sem pgto e total
df_sum_contato_tt <-
    df_acion_pg %>%
    group_by(Contrato) %>%
    summarise(tot_cont = n())
df_sum_contato_pg <-
    df_com_pg %>%
    group_by(Contrato) %>%
    summarise(tot_cont = n())
df_sum_contato_npg <-
    df_sem_pg %>%
    group_by(Contrato) %>%
    summarise(tot_cont = n())

# histogama mostra que distribuição não é normal (melhor usar mediana)
hist(df_sum_contato_tt$tot_cont)
acion_median_tt <- median(df_sum_contato_tt$tot_cont)
hist(df_sum_contato_pg$tot_cont)
acion_median_pg <- median(df_sum_contato_pg$tot_cont)
hist(df_sum_contato_npg$tot_cont)
acion_median_npg <- median(df_sum_contato_npg$tot_cont)
