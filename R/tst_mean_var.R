# testes de diferenças de médias e variancias
require("doMC", quietly = TRUE, warn.conflicts = FALSE)
require("xlsx", quietly = TRUE, warn.conflicts = FALSE)
require("plyr", quietly = TRUE, warn.conflicts = FALSE)
require("dplyr", quietly = TRUE, warn.conflicts = FALSE)

source("./R/f_acentos.R") # usar esta função para ler os dados novos. 
source("./R/f_le_raw_HG.R") # usar esta função para ler os dados novos. 
source("./R/f_tidy_scores_HG.R") # alterada em 07/12/2015 para trazer nome do respondente tb

registerDoMC(5) # parallel processing

df_campos <- read.xlsx2("./data/Classificacao das carreiras-V2.xlsx", sheetIndex = 1, header = TRUE)
# preparando os dados para as pesquisas a frente
df_campos <- f_acentos(df_campos)

require("stringr", quietly = TRUE, warn.conflicts = FALSE)
require("reshape2", quietly = TRUE, warn.conflicts = FALSE)


df_raw_hg <- f_le_raw_HG() # lê toda a amostra de dados HG

df_tidy_hg <- f_tidy_scores_HG(df_raw_hg) # calcula os fatores de acordo com a puntuação

# chama funcao que tira acentos e força minúsculos
df_tidy_hg <- f_acentos(df_tidy_hg)

df_tidy_hg <-
    df_tidy_hg %>%
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

pca1 = prcomp(df_tidy_hg[,7:14], scale. = TRUE, center = TRUE)

scores.total <- as.data.frame(pca1$x)
my.scores.total <- as.data.frame(cbind(ID = df_tidy_hg$ID, 
                                       sexo = df_tidy_hg$sexo, 
                                       tipouser = df_tidy_hg$TIPOUSER, 
                                       profissao.na.area.de = df_tidy_hg$profissao.na.area.de, 
                                       formacao.em = df_tidy_hg$formacao.em,
                                       idade = df_tidy_hg$idade,
                                       cep = df_tidy_hg$cep,
                                       scores.total))

my.scores.total <-
    my.scores.total %>%
    mutate(tipouser = ifelse(tipouser == "", "indefinido", as.character(tipouser)),
           sexo = ifelse(sexo == "f", "feminino","masculino"))

df_forms <-
    df_forms %>%
    rename(formacao.em = DE)

df_change <- left_join(my.scores.total, df_forms, by=c("formacao.em")) 
df_change <-
    df_change %>%
    select(-formacao.em) %>%
    rename(formacao.em = PARA)

df_profs <-
    df_profs %>%
    rename(profissao.na.area.de = DE)

df_change <- left_join(df_change, df_profs, by=c("profissao.na.area.de")) 
df_change <-
    df_change %>%
    select(-profissao.na.area.de) %>%
    rename(profissao.na.area.de = PARA)

df_change <-
    df_change %>%
    filter(formacao.em != "eliminar" & profissao.na.area.de != "eliminar")

par(mfrow=c(2,4))
hist(df_change$PC1, main = "Histograma de Scores da Amostra (PC1)", xlab = "PC1")
hist(df_change$PC2, main = "Histograma de Scores da Amostra (PC2)", xlab = "PC2")
hist(df_change$PC3, main = "Histograma de Scores da Amostra (PC3)", xlab = "PC3")
hist(df_change$PC4, main = "Histograma da de Scores Amostra (PC4)", xlab = "PC4")
hist(df_change$PC5, main = "Histograma da de Scores Amostra (PC5)", xlab = "PC5")
hist(df_change$PC6, main = "Histograma da de Scores Amostra (PC6)", xlab = "PC6")
hist(df_change$PC7, main = "Histograma da de Scores Amostra (PC7)", xlab = "PC7")

df_carr <- data.frame()

for (j in 1:ncol(df_campos)) {
    # percorre todas as linhas da coluna corrente de df_campos
    for (i in 1:length(df_change$profissao.na.area.de)) {
        # somente pega o string com match exato (ex. administração)
        i_aux <- sum(!is.na(str_match(df_campos[,j],
                                      paste0("^", as.character(df_change$profissao.na.area.de[i]), "$"))))
        
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
names(df_carr) <- colnames(df_campos)

# AQUI: ANTES DE CONCATENAR PARA CAMPOS, OBTER PLOTS COM DESVIO PADRAO PARA CEP, IDADE E SEXO
# fazer plot abaixo para ver de campos se sobrepoe. depois testar com CEP e idade

#PLOT POR GÊNERO (QUEBRA ABAIXO PAR TODOS OS COMPONENTES NO MESMO GRAFICO). EX. SOMENTE REGIAO 1
# PARA TODOS OS COMPONENTES NO MESMO PLOT (PC x MN)
mg = list(
    l = 100, # quanto maior, maior a distância do eixo da esquerda
    r = 100, # quanto maior, maior a distância do eixo da esquerda
    b = 100, # quanto maior, maior a distância do eixo da parte de baixo
    t = 100, # quanto maior, maior a distância do eixo da parte de baixo
    pad = 4
)
pc1 <- my.scores.total %>% group_by(sexo) %>%
    summarise(mn = mean(PC1), sd = 1.96 * sd(PC1)) %>%
    arrange(desc(mn)) %>%
    plot_ly(y = sexo, x = mn, error_x = list(array = sd),
            mode = "markers", name = "Agrupado por Sexo") %>%
    layout(
        autosize = F,
        width = 500,
        height = 500,
        margin = mg,
        yaxis = list(title = "Sexo"),
        xaxis = list(title = "PC1 score"))
pc1

# DEPOIS CONTINUAR E OBTER O MESMO PARA CAMPOS E VER SE NÃO SE SOBREPOEM NOS COMPONENTES


#
# concatenando a coluna de profissoes ao dataframe gerado
my.prev.carr <- cbind(df_change, df_carr)
# TST: media onde CCF = 1
#x <- 
#    my.prev.carr %>%
#    filter(CFM == 1)
#mean(x$PC1)
#sd(x$PC1)
# CSL == 1
#y <- 
#    my.prev.carr %>%
#    filter(CSL == 1)
#mean(y$PC1)
#sd(y$PC1)


carrMelt <- melt(my.prev.carr,id=c("ID","profissao.na.area.de",
                                   "PC1","PC2","PC3","PC4","PC5","PC6","PC7"),
                 measure.vars=colnames(df_campos), na.rm = TRUE)
# mudando o nome da variavel de classe
colnames(carrMelt)[10] <- "class.carr"
# eliminando coluna desnecessária
my.prev.carr <-
    carrMelt %>%
    select (-value)

# PLOT AQUI MOSTRA QUE DESVIO PADRÃO PARA CAMPOS EM COMPONENTES É MUITO ALTO
# ou com ggplot
#    ggplot(pc1, aes(x=class.carr, y=mn)) + 
#        geom_errorbar(aes(ymin=mn-sd, ymax=mn+sd), width=.2) +
#        geom_line() +
#        geom_point()
    
    
# PLOTLY ABAIXO NÃO MOSTRAVA CORRETAMENTE A BARRA DE ERRO com opcao value !!!!
    # CUIDADO: USAR opcão array ao inves d evalue em x_error!!!!!!

# TESTE DE VARIANCIA DE CAMPO PROFISISONAL, PC1
library(plotly)
pc1.cpo <- my.prev.carr %>% group_by(class.carr) %>%
    summarise(mn = mean(PC1), sd = 1.96 * sd(PC1)) %>%
    arrange(desc(mn)) %>%
    plot_ly(y = class.carr, x = mn, error_x = list(array = sd),
            mode = "markers", name = "Agrupado por Campo") %>%
    layout(
        autosize = F,
        width = 500,
        height = 500,
        margin = mg,
        yaxis = list(title = "PC1 x Campos"),
        xaxis = list(title = "PC1 score"))
pc1.cpo

# TESTE DE VARIANCIA DE REGIAO CEP, PC1

my.scores.total <-
    my.scores.total %>%
    mutate(CEP.regiao = substring(cep,1,1),
           CEP.subregiao = substring(cep,1,2),
           CEP.setor = substring(cep,1,3),
           CEP.subsetor = substring(cep,1,4)
    )

pc1.regiao <- my.scores.total %>% group_by(CEP.regiao) %>%
    summarise(mn = mean(PC1), sd = 1.96 * sd(PC1)) %>%
    arrange(desc(mn)) %>%
    plot_ly(y = CEP.regiao, x = mn, error_x = list(array = sd),
            mode = "markers", name = "Agrupado por Região de CEP") %>%
    layout(
        autosize = F,
        width = 500,
        height = 500,
        margin = mg,
        yaxis = list(title = "PC1 x Região do CEP"),
        xaxis = list(title = "PC1 score"))
pc1.regiao
# por subregiao
pc1.subregiao <- my.scores.total %>% group_by(CEP.subregiao) %>%
    summarise(mn = mean(PC1), sd = 1.96 * sd(PC1)) %>%
    arrange(desc(mn)) %>%
    plot_ly(y = CEP.subregiao, x = mn, error_x = list(array = sd),
            mode = "markers", name = "Agrupado por Sub Região de CEP") %>%
    layout(
        autosize = F,
        width = 500,
        height = 500,
        margin = mg,
        yaxis = list(title = "PC1 x Sub Região do CEP"),
        xaxis = list(title = "PC1 score"))
pc1.subregiao
# TERMINA PLOTLY
# TENTAR VER AS MEDIAS ANTES DOS PCs
df_tidy_hg_mean <-
    df_tidy_hg %>%
    mutate(CEP.regiao = substring(cep,1,1),
           CEP.subregiao = substring(cep,1,2),
           CEP.setor = substring(cep,1,3),
           CEP.subsetor = substring(cep,1,4)
    )

pc1.regiao.sens <- df_tidy_hg_mean %>% group_by(CEP.regiao) %>%
    summarise(mn = mean(sensibility), sd = 1.96 * sd(sensibility)) %>%
    arrange(desc(mn)) %>%
    plot_ly(y = CEP.regiao, x = mn, error_x = list(array = sd),
            mode = "markers", name = "Agrupado por Região de CEP") %>%
    layout(
        autosize = F,
        width = 500,
        height = 500,
        margin = mg,
        yaxis = list(title = "sensibilidade x região do CEP"),
        xaxis = list(title = "sensibility mean"))
pc1.regiao.sens
