#----------------------------------------
# ANALISE DE MOVIMENTACAO (TESTE)
#----------------------------------------
require("stringr", quietly = TRUE, warn.conflicts = FALSE)
require("reshape2", quietly = TRUE, warn.conflicts = FALSE)
require("doMC", quietly = TRUE, warn.conflicts = FALSE)
require("xlsx", quietly = TRUE, warn.conflicts = FALSE)
require("dplyr", quietly = TRUE, warn.conflicts = FALSE)

source("./R/f_le_raw_HG.R") # usar esta função para ler os dados novos. 
source("./R/f_tidy_scores_HG.R") # alterada em 07/12/2015 para trazer nome do respondente tb
df_raw_hg <- f_le_raw_HG() # lê toda a amostra de dados HG

df_tidy_hg <- f_tidy_scores_HG(df_raw_hg) # calcula os fatores de acordo com a puntuação


# Obtendo os dados das respostas
# # OBS: precisa fazer um de-para das ocupações para as classes, senão deixa passar coisas como Farmacia e farmaceutica
# 1. obter todas as ocupações e criar tabela sem repeticoes
#profs <-
#    df_tidy_hg %>%
#    distinct(profissao.na.area.de) %>%
#    select(profissao.na.area.de)
#forms <-
#    df_tidy_hg %>%
#    distinct(formacao.em) %>%
#    select(formacao.em)
# 2. ler do excel de para já criado com os dados atuais 
# 3. mudar os nomes da ocupações/formação no dataframe original
#write.xlsx2(profs, "./data/profs.xlsx")
#write.xlsx2(forms, "./data/forms.xlsx")
f_acentos <- function(df_in) {
    df_in <- mutate_each(df_in, funs(tolower)) # forçando minúsculas
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("á", "a", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("é", "e", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("í", "i", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("ó", "o", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("ú", "u", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("ã", "a", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("õ", "o", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("ç", "c", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("â", "a", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("ê", "e", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("ô", "o", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("  ", " ", x))))
    return (df_in)
}

df_change <- f_acentos(df_tidy_hg)
# restaura fatores como numericos
df_change <-
    df_change %>%
    mutate(sensibility = as.numeric(as.vector(sensibility)),
           power = as.numeric(as.vector(power)),
           quality = as.numeric(as.vector(quality)),
           exposure = as.numeric(as.vector(exposure)),
           structure = as.numeric(as.vector(structure)),
           imagination = as.numeric(as.vector(imagination)),
           stability = as.numeric(as.vector(stability)),
           contacts = as.numeric(as.vector(contacts)))

df_profs <- read.xlsx2("./data/profs.de.para-V1.xlsx", sheetIndex = 1, header = TRUE)
df_profs <- f_acentos(df_profs)

df_forms <- read.xlsx2("./data/forms.de.para-V1.xlsx", sheetIndex = 1, header = TRUE)
df_forms <- f_acentos(df_forms)
# para cada linha de formacao.in de df_change, pesquiso em df_forms$DE e retorno o indice. Se acho
# retorno o valor de df_forms$PARA para o memso indice

# eliminando as formações não relevantes para analisar movimentação
#df_change <-
#   df_change %>%
#    filter(!(profissao.na.area.de %in% c("estagiario", "estudante", "dona de casa", "consultoria",
#                                         "comercio", "entre empregos", "do lar", "empresario", "bancario", "aposentado/a",
#                                         "terceiro setor", "indefinido") |
#                 formacao.em %in% c("ensino medio", "ensino tecnico", "ensino fundamental", "indefinido",
#                                    "ensino tecnologico")))


#df_change <- merge(df_change, df_forms,by=c("formacao.em"), all.y = TRUE)

# mudando a coluna formacao.em usando o dataframe df_forms de DE-PARA em relação as classificacoes
# mudando linhas de formacao df_change DE -> PARA
df_forms <-
    df_forms %>%
    rename(formacao.em = DE)

df_change <- left_join(df_change, df_forms, by=c("formacao.em")) # OK!!!!

df_change <-
    df_change %>%
    select(-formacao.em) %>%
    rename(formacao.em = PARA)

# mudando a coluna profissao.na.area.de usando o dataframe df_profs de DE-PARA em relação as classificacoes
df_profs <-
    df_profs %>%
    rename(profissao.na.area.de = DE)

df_change <- left_join(df_change, df_profs, by=c("profissao.na.area.de")) # OK!!!!

df_change <-
    df_change %>%
    select(-profissao.na.area.de) %>%
    rename(profissao.na.area.de = PARA)

# eliminar formação e profissão igual a "eliminar"
df_change <-
    df_change %>%
    filter(formacao.em != "eliminar" & profissao.na.area.de != "eliminar")

# agora usar o processamento para identificar quem movimentou de carreira ou não

#+++++++++++++++++++++++++++

# obtendo os scores previstos a partir de todos os dados
#pca1 = prcomp(df_tidy_hg[,7:14], scale. = TRUE, center = TRUE)
pca1 = prcomp(df_change[,5:12], scale. = TRUE, center = TRUE)
# selecionando apenas amostra para rapidez de processamanto (posteriormente colocar em combobox)
#tam.amostra = 1000
#my.newdata <- 
#    df_change %>%
#    sample_n(tam.amostra)
# fazendo para toda a base
my.newdata <- df_change
tam.amostra = dim(my.newdata)[1]

# calculando os scores
my.prev <- as.data.frame(predict(pca1, newdata=my.newdata))
# criando dataframe por carreira e profissão
my.prev <- cbind(ID = my.newdata$ID, profissao.na.area.de = my.newdata$profissao.na.area.de, 
                 formacao.em = my.newdata$formacao.em,
                 my.prev)

# PROFISSAO x CLASSE
# criando novo dataframe para tratar mais de uma profissão por classe de carreira
#----------------------------------------------------------------------------------
df_class.carr <- read.xlsx2("./data/Classificacao das carreiras-V2.xlsx", sheetIndex = 1, header = TRUE)
# preparando os dados para as pesquisas a frente
df_class.carr <- mutate_each(df_class.carr, funs(tolower)) # forçando minúsculas
# tirando acentuação e espaço em branco em excesso
df_class.carr <- as.data.frame(sapply(df_class.carr, FUN = function(x) as.character(gsub("á", "a", x))))
df_class.carr <- as.data.frame(sapply(df_class.carr, FUN = function(x) as.character(gsub("é", "e", x))))
df_class.carr <- as.data.frame(sapply(df_class.carr, FUN = function(x) as.character(gsub("í", "i", x))))
df_class.carr <- as.data.frame(sapply(df_class.carr, FUN = function(x) as.character(gsub("ó", "o", x))))
df_class.carr <- as.data.frame(sapply(df_class.carr, FUN = function(x) as.character(gsub("ú", "u", x))))
df_class.carr <- as.data.frame(sapply(df_class.carr, FUN = function(x) as.character(gsub("ã", "a", x))))
df_class.carr <- as.data.frame(sapply(df_class.carr, FUN = function(x) as.character(gsub("õ", "o", x))))
df_class.carr <- as.data.frame(sapply(df_class.carr, FUN = function(x) as.character(gsub("ç", "c", x))))
df_class.carr <- as.data.frame(sapply(df_class.carr, FUN = function(x) as.character(gsub("â", "a", x))))
df_class.carr <- as.data.frame(sapply(df_class.carr, FUN = function(x) as.character(gsub("ê", "e", x))))
df_class.carr <- as.data.frame(sapply(df_class.carr, FUN = function(x) as.character(gsub("ô", "o", x))))
df_class.carr <- as.data.frame(sapply(df_class.carr, FUN = function(x) as.character(gsub("  ", " ", x))))


df_carr <- data.frame()

for (j in 1:ncol(df_class.carr)) {
    # percorre todas as linhas da coluna corrente de df_class.carr
    for (i in 1:length(my.prev$profissao.na.area.de)) {
        # somente pega o string com match exato (ex. administração)
        i_aux <- sum(!is.na(str_match(df_class.carr[,j],
                                      paste0("^", as.character(my.prev$profissao.na.area.de[i]), "$"))))
        
        # para cada profissão encontrada em uma determinada classe, marca a posição correspondente
        # com 1, na célula do dataframe correspondente. Caso contrario, coloca NA
        if(i_aux) {
            df_carr[i,j] = i_aux
        } else {
            df_carr[i,j] = NA
        }
    }
}
# colocando os nomes das classes no dataframe gerado
names(df_carr) <- colnames(df_class.carr)

# concatenando a coluna de profissoes ao dataframe gerado
my.prev.carr <- cbind(my.prev, df_carr)

# duplicar colunas que aparecem com mais de uma classe

# transformando colunas em valores por linha, eliminando NAs, de forma a poder ter os scores de
# cada profissão duplicado para cada classe onde a profissão se encontra
# Desta forma, a análise não perde a contribuição do score de cada profissão para cada classe
carrMelt <- melt(my.prev.carr,id=c("ID","profissao.na.area.de",
                                   "PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8"),
                 measure.vars=colnames(df_class.carr), na.rm = TRUE)
# mudando o nome da variavel de classe
colnames(carrMelt)[11] <- "class.carr"
# eliminando coluna desnecessária
my.prev.carr <-
    carrMelt %>%
    select (-value)

knitr::kable(head(my.prev.carr))

#++++
# FORMACAO x CLASSE
# criando novo dataframe para tratar mais de uma formacao por classe de carreira
#----------------------------------------------------------------------------------
df_form <- data.frame()

for (j in 1:ncol(df_class.carr)) {
    # percorre todas as linhas da coluna corrente de df_class.carr
    for (i in 1:length(my.prev$formacao.em)) {
        # somente pega o string com match exato (ex. administração)
        i_aux <- sum(!is.na(str_match(df_class.carr[,j],
                                      paste0("^", as.character(my.prev$formacao.em[i]), "$"))))
        # para cada profissão encontrada em uma determinada classe, marca a posição correspondente
        # com 1, na célula do dataframe correspondente. Caso contrario, coloca NA
        if(i_aux) {
            df_form[i,j] = i_aux
        } else {
            df_form[i,j] = NA
        }
    }
}
# colocando os nomes das classes no dataframe gerado
names(df_form) <- colnames(df_class.carr)

# concatenando a coluna de profissoes ao dataframe gerado
my.prev.form <- cbind(my.prev, df_form)

# duplicar colunas que aparecem com mais de uma classe
# transformando colunas em valores por linha, eliminando NAs, de forma a poder ter os scores de
# cada profissão duplicado para cada classe onde a profissão se encontra
# Desta forma, a análise não perde a contribuição do score de cada profissão para cada classe
formMelt <- melt(my.prev.form,id=c("ID","formacao.em",
                                   "PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8"),
                 measure.vars=colnames(df_class.carr), na.rm = TRUE)
# mudando o nome da variavel de classe
colnames(formMelt)[11] <- "class.carr"
# eliminando coluna desnecessária
my.prev.form <-
    formMelt %>%
    select (-value)

knitr::kable(head(my.prev.form))
#++++

# SEPARANDO SOMENTE RESPONDENTES QUE MUDARAM DE CLASSE de formação -> ocupação

# 1. fazer inner join para identificar se ao menos uma classe de carreira coinncide com uma classe de formacao
# obs: preciso do ID do respondente para a chave do inner join
# Se ID aparece ao menos uma vez no inner_join, significa que a profissão e a carreira
# do respondente ao menos uma vez aparece na mesma catagoria, configurando que não se movimntou
# para outra carreira
df_sem.mov <- inner_join(my.prev.form[,c(1:2,11)], my.prev.carr[,c(1:2,11)],by=c("ID", "class.carr"))
# tirando a duplicidade dos IDs dos respondentes sem movimento, identifico todos os IDs, sem movimentação
# número de IDs da amostra total identificados como não s emovimentando de carreira:
nrow(df_sem.mov)
df_sem.mov <-
    df_sem.mov %>%
    distinct(ID)
# obtendo respondentes com movimento a partir dos dados originais processados, selecionando
# dos dados otais aqueles que não têm o ID encontrado como sem movimento
df_com.mov <-
    my.prev %>%
    filter(!(ID %in% df_sem.mov$ID))

# criando tabela com scores e campo que identifica movimento, para plot
# criando coluna que identifica MOVIMENTO (S/N)
# conctenando horizontalmente
my.prev.mov <-
    my.prev %>%
    mutate(MOVIMENTO = ifelse((ID %in% df_sem.mov$ID),"NÃO MUDOU","MUDOU"))
# plots

# box plot (colocar no mesmo grid)
require("plotly", quietly = TRUE, warn.conflicts = FALSE)
pc1 <- plot_ly(my.prev.mov, x = PC1, color = MOVIMENTO, type = "box") 
pc1 <- layout(pc1, title = "OPPENESS/EXPLORATION") 
pc1

# box plot (colocar no mesmo grid)
pc2 <- plot_ly(my.prev.mov, x = PC2, color = MOVIMENTO, type = "box") 
pc2 <- layout(pc2, title = "AGREEABLENESS/AMABILIDADE x ANALÍTICO E INDEPENDENTE/FOCO EM RESULTADO") 
pc2

# box plot (colocar no mesmo grid)
pc3 <- plot_ly(my.prev.mov, x = PC3, color = MOVIMENTO, type = "box") 
pc3 <- layout(pc3, title = "INVESTIGATIVO/RESEARCH ORIENTED x COMMITMENT/ENGAJAMENTO") 
pc3

# box plot (colocar no mesmo grid)
pc4 <- plot_ly(my.prev.mov, x = PC4, color = MOVIMENTO, type = "box") 
pc4 <- layout(pc4, title = "PEOPLE DEVELOPMENT ORIENTED x TASK ORIENTED/ORIENTAÇÃO PARA A TAREFA") 
pc4

# box plot (colocar no mesmo grid)

pc5 <- plot_ly(my.prev.mov, x = PC5, color = MOVIMENTO, type = "box") 
pc5 <- layout(pc5, title = "EXTROVERSÃO/AUTENTICIDADE x SOBRIEDADE/COMPOSTURA/COMEDIMENTO") 
pc5

# box plot (colocar no mesmo grid)

pc6 <- plot_ly(my.prev.mov, x = PC6, color = MOVIMENTO, type = "box") 
pc6 <- layout(pc6, title = "MANAGEMENT/ASSISTÊNCIA/RECEPTIVIDADE ATIVA") 
pc6

# box plot (colocar no mesmo grid)

pc7 <- plot_ly(my.prev.mov, x = PC7, color = MOVIMENTO, type = "box") 
pc7 <- layout(pc7, title = "AUSTERIDADE/PONDERAÇÃO x ADAPTABILIDADE/AJUSTAMENTO)") 
pc7



