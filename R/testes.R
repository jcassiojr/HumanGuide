x <- df_raw_hg %>%
    filter(nomerespondente %in% c("GISELLE"))

x <- df_raw_hg$nomerespondente[df_raw_hg$nomerespondente in c("GISELLE")]

which(apply(df_raw_hg, 2, function(x) any(grepl("GISELLE", x))))

x <- grepl("GISELLE", df_raw_hg$nomerespondente)
table (x)

df_users <- df_raw_hg %>%
    filter(grepl("Giselle Welter|Alex Welter|Marco Sinicco|Beatriz Welter|Eneko Fonseca|Ana Alterio|Almir Cozzolino|Fiama Ester de Oliveira|Valdir Rasche|Laura Welter|Sven Peters|Arlindo Marin", df_raw_hg$nomerespondente))
# Não achei: Fiama Ester de Oliveira (Não está na planilha original), Marco Sinicco, Valdir Rasche, Sven Peters
df_users <- df_raw_hg %>%
    filter(grepl("Monica Negraes|Julia Arduini|Katia Pineschi|Gabi Lipkau|Ana Raia|Altair|Jerome|Rolf Kenmo|Vagner Molina", df_raw_hg$nomerespondente))







# testar abaixo as reshape
#################
# reshaping data
################
library(reshape2)
head(mtcars)
# melting dataframes
mtcars$carname <- rownames(mtcars)
carMelt <- melt(mtcars,id=c("carname","gear","cyl"),measure.vars=c("mpg","hp"))
head(carMelt,n=3)
tail(carMelt,n=3)
# casting data frames
cylData <- dcast(carMelt, cyl ~ variable)
cylData
cylData <- dcast(carMelt, cyl ~ variable,mean)
cylData
# averaging values (para variable count, em relação ao index spray, soma)
head(InsectSprays)
tapply(InsectSprays$count,InsectSprays$spray,sum)
# another way, split
spIns =  split(InsectSprays$count,InsectSprays$spray)
spIns
sprCount = lapply(spIns,sum)
sprCount
unlist (sprCount)  # voltamos a vetor a partir da lista gerada
# or using sapply to not generate a list as lapply
sapply(spIns,sum)
# another way, plyr package
ddply(InsectSprays,.(spray),summarize,sum=sum(count))
# creating a new variable
spraySums <- ddply(InsectSprays,.(spray),summarize,sum=ave(count,FUN=sum))
dim(spraySums)
head(spraySums)


# criar shiny com combo para PCx, PCy e escolha da formação
# para "brincar"mudando os clusters e as componentes sem saber o que é cada
#cor, apenas para ver se aparece algum grupo
# geralmente é muito dificil de descobrir!!!
require(stats)
set.seed(101)
km <- kmeans(iris[,1:4], 3)
plot(iris[,1], iris[,2], col=km$cluster)
points(km$centers[,c(1,2)], col=1:3, pch=19, cex=2)


# usando valores medios por formação
# aplicando kmeans
# usando cores para humanas, biológicas e exatas
# tabela de scores por formaçao

# --- PLOT COLORIDO DE HUMANAS, BIOLOGICAS E EXATAS TERMINA AQUI

my.newdata.form <- df_tidy_hg
# obtendo os scores previstos
pca1 = prcomp(my.newdata.form[,7:14], scale. = TRUE, center = TRUE)
# calculando os scores
my.prev.form <- as.data.frame(predict(pca1, newdata=my.newdata.form))
my.prev.form <- cbind(formacao.em = my.newdata.form$formacao.em, my.prev.form)
# obtendo o score médio por formacao
# OBS: repetir esta análise para agrupar por campo de atividade
my.prev.form <-
    my.prev.form %>%
    group_by(formacao.em) %>%
    summarise(PC1.medio = mean(PC1),
              PC2.medio = mean(PC2),
              PC3.medio = mean(PC3),
              PC4.medio = mean(PC4),
              PC5.medio = mean(PC5),
              PC6.medio = mean(PC6),
              PC7.medio = mean(PC7),
              PC8.medio = mean(PC8))

# classificando sem definição de área
#------------------------
set.seed(101)
km.form <- kmeans(my.prev.form[,2:9], 3) # cria os clusters independente da formção
plot(my.prev.form[,2]$PC1.medio, my.prev.form[,3]$PC2.medio, col=km.form$cluster) # usando cores do kmean
points(km.form$centers[,c(1,2)], col=1:3, pch=19, cex=2)
# agora usando cores de humanas, bio e exatas



#plot(my.prev.form[,3]$PC2.medio, my.prev.form[,4]$PC3.medio, col=km.form$cluster)
#points(km.form$centers[,c(1,2)], col=1:3, pch=19, cex=2)
#matplot(my.prev.form[,2], my.prev.form[,3],type='p', pch = 2, col=km.form$cluster)
#y <- unclass(my.prev.form[,2])
#y$PC1.medio

#-------------------------------------
# criar clusterização manualmente para plotar por área de formação
#------------------------------------
#my.newdata.form <- df_tidy_hg
#pca1 = prcomp(my.newdata.form[,7:14], scale. = TRUE, center = TRUE)
#my.prev.form <- as.data.frame(predict(pca1, newdata=my.newdata.form))
#my.prev.form <- cbind(formacao.em = my.newdata.form$formacao.em, my.prev.form)
# preciso montar vetor de inteiros de cores para: 1 = humanas, 2 = biologicas,
# 3 = exatas e colocar no lugar de km.form$cluster
my.area <- as.data.frame(my.prev.form[,1])
my.area <-
    my.area %>%
    mutate(area.formacao = ifelse(formacao.em %in% c("Música",
                                            "Publicidade e Propaganda",
                                            "Comunicação e Marketing",
                                            "Estilismo e Moda",
                                            "Relações Internacionais",
                                            "Radialismo e Televisão",
                                            "Jornalismo",
                                            "Turismo",
                                            "Administração Hoteleira",
                                            "Administração",
                                            "Arquitetura e Urbanismo",
                                            "Economia",
                                            "Ciências Sociais",
                                            "Administração de Recursos Humanos",
                                            "Ensino Fundamental",
                                            "Direito",
                                            "Contabilidade + Informação",
                                            "Gestão Industrial",
                                            "Logistica",
                                            "Economia Doméstica",
                                            "Ciências Contábeis",
                                            "História",
                                            "Serviço Social",
                                            "Ensino Técnico",
                                            "Secretariado",
                                            "Pedagogia",
                                            "Letras",
                                            "Geografia",
                                            "Educação",
                                            "Ensino Médio",
                                            "Tecnólogo em Gestão de Recursos Humanos"),1, # PRETO
                         ifelse(formacao.em %in% c("Terapia Ocupacional",
                                "Psicologia",
                                "Farmacêutica Bioquímica",
                                "Nutrição",
                                "Medicina Veterinária",
                                "Biomedicina",
                                "Farmácia",
                                "Educação Física",
                                "Fonoaudiologia",
                                "Enfermagem",
                                "Ciências Biológicas",
                                "Odontologia",
                                "Fisioterapia",
                                "Medicina",
                                "Zootecnia"), 2, # VERMELHO
                                ifelse(formacao.em %in% c("Ciências Atuariais",
                                                          "Engenharia de Pesca",
                                                          "Ciências Econômicas",
                                                          "Engenharia Metalurgica",
                                                          "Engenharia de Alimentos",
                                                          "Engenharia da Computação",
                                                          "Engenharia Florestal",
                                                          "Agronomia",
                                                          "Engenharia Industrial",
                                                          "Engenharia de Produção",
                                                          "Engenharia Química",
                                                          "Engenharia Bioquimica",
                                                          "Computação",
                                                          "Engenharia Civil",
                                                          "Engenharia Agricola",
                                                          "Telecomunicações",
                                                          "Engenharia Ambiental",
                                                          "Biotecnologia",
                                                          "Ensino Tecnológico",
                                                          "Gestão da Informação",
                                                          "Estatística",
                                                          "Engenharia de Controle e Automação",
                                                          "Engenharia Elétrica",
                                                          "Engenharia Mecânica",
                                                          "Agrimensura",
                                                          "Geologia",
                                                          "Química",
                                                          "Engenharia Hidrica",
                                                          "Física",
                                                          "Eletrotécnica",
                                                          "Mecânica",
                                                          "Matemática",
                                                          "Engenharia da Segurança no Trabalho"), 3, # VERDE
                                       0))))

#pca1 = prcomp(my.newdata.form[,7:14], scale. = TRUE, center = TRUE)
# calculando os scores
#my.prev.form <- as.data.frame(predict(pca1, newdata=my.newdata.form))
#my.prev.form <- cbind(formacao.em = my.newdata.form$formacao.em, my.prev.form)
#my.prev.form <-
#    my.prev.form %>%
#    group_by(formacao.em) %>%
##    summarise(PC1.medio = mean(PC1),
#              PC2.medio = mean(PC2),
#              PC3.medio = mean(PC3),
#              PC4.medio = mean(PC4),
#              PC5.medio = mean(PC5),
#              PC6.medio = mean(PC6),
#              PC7.medio = mean(PC7),
#              PC8.medio = mean(PC8))

my.prev.form <- cbind(area.formacao = my.area$area.formacao, my.prev.form)
#-------------------------------------------------
# plot de cluster 
#--------------------------------
plot(my.prev.form$PC1.medio, my.prev.form$PC2.medio, col=my.prev.form$area.formacao)
legend("topright", inset=.05,
       bty="n", cex=.5,
       title="Área de Formação",
       c("BIOLÓGICAS", "HUMANAS", "EXATAS"), fill=c("red", "blue", "green"))

# --- PLOT COLORIDO DE HUMANAS, BIOLOGICAS E EXATAS TERMINA AQUI

set.seed(101)
km.form <- kmeans(my.prev.form[,3:10], 3) # cria os clusters independente da formção
plot(my.prev.form$PC1.medio, my.prev.form$PC2.medio, col=km.form$cluster)
points(km.form$centers[,c(1,2)], col=c(2,3,1), pch=19, cex=2)

# seelcionando lista de ocupacoes e gravando em planilha para classificar
df_ocup <- df_raw_hg %>% select(profissao.na.area.de) %>% group_by(profissao.na.area.de) %>% summarise(n())
write.xlsx(df_ocup, "./data/Prosissoes.xlsx")


# PLOTLY
# install.packages("plotly")
library(plotly)
p <- plot_ly(midwest, x = percollege, color = state, type = "box")
# testar com plot de scores por tipo formacao
my.prev.form2 <-
    my.prev.form %>%
    mutate(area.form.text = ifelse(area.formacao == 1, "HUMANAS",
                                   ifelse(area.formacao == 2, "BIOLÓGICAS", "EXATAS")))
p <- plot_ly(my.prev.form2, x = PC3.medio, color = area.form.text, type = "box")
p
# para postar na cloud
plotly_POST(p, filename = "r-docs/midwest-boxplots", world_readable=TRUE)

# CRIAR PLOT MÉDIO DE PCs 1, 2 e 3 por classe de carreira
############################################################
# ler dados de categorias de formação
require(xlsx)
require(dplyr)
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

# abaixo funciona, mas sapply acima é mais enxuto!
#df_class.carr <-
#    df_class.carr %>%
#    mutate(CFM = gsub('á', 'a', df_class.carr$CFM),
#           CFM = gsub('á', 'a', df_class.carr$CFM),
#           CFM = gsub('á', 'a', df_class.carr$CFM),
#           CFM = gsub('á', 'a', df_class.carr$CFM),
#           CFM = gsub('á', 'a', df_class.carr$CFM),
#           CFM = gsub('á', 'a', df_class.carr$CFM),
 #          CFM = gsub('á', 'a', df_class.carr$CFM),
#           CFM = gsub('á', 'a', df_class.carr$CFM),
#           CFM = gsub('á', 'a', df_class.carr$CFM),
#           CFM = gsub('á', 'a', df_class.carr$CFM),
#           
#           )



# Obtendo os dados das respostas
my.newdata.carr <- df_tidy_hg
# obtendo os scores previstos
pca1 = prcomp(my.newdata.carr[,7:14], scale. = TRUE, center = TRUE)
# calculando os scores
my.prev.carr <- as.data.frame(predict(pca1, newdata=my.newdata.carr))
my.prev.carr <- cbind(profissao.na.area.de = my.newdata.carr$profissao.na.area.de, my.prev.carr)
# obtendo o score médio por formacao
# OBS: repetir esta análise para agrupar por campo de atividade
my.prev.carr <-
    my.prev.carr %>%
    group_by(profissao.na.area.de) %>%
    summarise(PC1.medio = mean(PC1),
              PC2.medio = mean(PC2),
              PC3.medio = mean(PC3),
              PC4.medio = mean(PC4),
              PC5.medio = mean(PC5),
              PC6.medio = mean(PC6),
              PC7.medio = mean(PC7),
              PC8.medio = mean(PC8))

# criar coluna class.carr no df_tidy agrupado por media 
# 1. percorrer dataframe df_class.carr para cada célula de profissao.na.area.de do dataframe my.prev.carr
library(stringr)
# tirando acentuação e espaço em branco em excesso e forçando minusculas
my.prev.carr <- mutate_each(my.prev.carr, funs(tolower)) # forçando minúsculas
my.prev.carr <- as.data.frame(sapply(my.prev.carr, FUN = function(x) as.character(gsub("á", "a", x))))
my.prev.carr <- as.data.frame(sapply(my.prev.carr, FUN = function(x) as.character(gsub("é", "e", x))))
my.prev.carr <- as.data.frame(sapply(my.prev.carr, FUN = function(x) as.character(gsub("í", "i", x))))
my.prev.carr <- as.data.frame(sapply(my.prev.carr, FUN = function(x) as.character(gsub("ó", "o", x))))
my.prev.carr <- as.data.frame(sapply(my.prev.carr, FUN = function(x) as.character(gsub("ú", "u", x))))
my.prev.carr <- as.data.frame(sapply(my.prev.carr, FUN = function(x) as.character(gsub("ã", "a", x))))
my.prev.carr <- as.data.frame(sapply(my.prev.carr, FUN = function(x) as.character(gsub("õ", "o", x))))
my.prev.carr <- as.data.frame(sapply(my.prev.carr, FUN = function(x) as.character(gsub("ç", "c", x))))
my.prev.carr <- as.data.frame(sapply(my.prev.carr, FUN = function(x) as.character(gsub("â", "a", x))))
my.prev.carr <- as.data.frame(sapply(my.prev.carr, FUN = function(x) as.character(gsub("ê", "e", x))))
my.prev.carr <- as.data.frame(sapply(my.prev.carr, FUN = function(x) as.character(gsub("ô", "o", x))))
my.prev.carr <- as.data.frame(sapply(my.prev.carr, FUN = function(x) as.character(gsub("  ", " ", x))))

# não funciona direito abaixo!!!! rodando um a um funciona!!!
#my.prev.carr <-
#    my.prev.carr %>%
#    mutate(profissao.na.area.de = gsub('ã', 'a', my.prev.carr$profissao.na.area.de),
#           profissao.na.area.de = gsub('á', 'a', my.prev.carr$profissao.na.area.de),
#           profissao.na.area.de = gsub('é', 'e', my.prev.carr$profissao.na.area.de),
#           profissao.na.area.de = gsub('í', 'i', my.prev.carr$profissao.na.area.de),
#           profissao.na.area.de = gsub('ó', 'o', my.prev.carr$profissao.na.area.de),
#           profissao.na.area.de = gsub('ú', 'u', my.prev.carr$profissao.na.area.de),
#           profissao.na.area.de = gsub('â', 'a', my.prev.carr$profissao.na.area.de),
#           profissao.na.area.de = gsub('ê', 'e', my.prev.carr$profissao.na.area.de),
#           profissao.na.area.de = gsub('ô', 'o', my.prev.carr$profissao.na.area.de),
#           profissao.na.area.de = gsub('ç', 'c', my.prev.carr$profissao.na.area.de),
#           profissao.na.area.de = gsub('õ', 'o', my.prev.carr$profissao.na.area.de))

# inicializando a coluna com NA
my.prev.carr <-
    my.prev.carr %>%
    mutate(class.carr = NA)


# percorre todas as colunas de df_class.carr
for (j in 1:ncol(df_class.carr)) {
    # percorre toda uma coluna de df_class.carr marcando em my.prev.carr$class.carr se encontra
    for (i in 1:length(my.prev.carr$profissao.na.area.de)) {
        #print(i)
        y <- which(!is.na(str_match(df_class.carr[,j],as.character(my.prev.carr$profissao.na.area.de[i]))))
    
        if (length(y) != 0) {
            my.prev.carr[i,10] = colnames(df_class.carr)[j]
        } 
    }
}
# mudando NA para "INDEFINIDO" em coluna class.carr
my.prev.carr <-
    my.prev.carr %>%
    mutate(class.carr = ifelse(is.na(class.carr), "INDEFINIDO", as.character(class.carr)))

# plotando os resultados médios nso PC1, PC2 e PC3
require("plotly")
pc1 <- plot_ly(my.prev.carr, x = PC1.medio, y = PC2.medio, z = PC3.medio, color = class.carr, type = "scatter3d", mode = "markers")
pc1 <- layout(pc1, title = "Scores Médios por Classe de Profissão - PC1 x PC2 X PC3")
pc1

# plotando os resultados discriminados para amostra aleatória nos PC1, PC2 e PC3
require("plotly")
pc1 <- plot_ly(my.prev.carr, x = PC1.medio, y = PC2.medio, z = PC3.medio, color = class.carr, type = "scatter3d", mode = "markers")
pc1 <- layout(pc1, title = "Scores Médios por Classe de Profissão - PC1 x PC2 X PC3")
pc1

# CRIAR PLOT PARA AMOSTRA DE OCORRÊNCIAS DE PCs 1, 2 e 3 por classe de carreira
############################################################
# Obtendo os dados das respostas
my.newdata.carr2 <- df_tidy_hg

# obtendo os scores previstos
pca1 = prcomp(my.newdata.carr2[,7:14], scale. = TRUE, center = TRUE)
my.newdata.carr2 <- 
    df_tidy_hg %>%
    sample_n(1000)
# calculando os scores
my.prev.carr2 <- as.data.frame(predict(pca1, newdata=my.newdata.carr2))
my.prev.carr2 <- cbind(profissao.na.area.de = my.newdata.carr2$profissao.na.area.de, my.prev.carr2)

my.prev.carr2 <- mutate_each(my.prev.carr2, funs(tolower)) # forçando minúsculas
my.prev.carr2 <- as.data.frame(sapply(my.prev.carr2, FUN = function(x) as.character(gsub("á", "a", x))))
my.prev.carr2 <- as.data.frame(sapply(my.prev.carr2, FUN = function(x) as.character(gsub("é", "e", x))))
my.prev.carr2 <- as.data.frame(sapply(my.prev.carr2, FUN = function(x) as.character(gsub("í", "i", x))))
my.prev.carr2 <- as.data.frame(sapply(my.prev.carr2, FUN = function(x) as.character(gsub("ó", "o", x))))
my.prev.carr2 <- as.data.frame(sapply(my.prev.carr2, FUN = function(x) as.character(gsub("ú", "u", x))))
my.prev.carr2 <- as.data.frame(sapply(my.prev.carr2, FUN = function(x) as.character(gsub("ã", "a", x))))
my.prev.carr2 <- as.data.frame(sapply(my.prev.carr2, FUN = function(x) as.character(gsub("õ", "o", x))))
my.prev.carr2 <- as.data.frame(sapply(my.prev.carr2, FUN = function(x) as.character(gsub("ç", "c", x))))
my.prev.carr2 <- as.data.frame(sapply(my.prev.carr2, FUN = function(x) as.character(gsub("â", "a", x))))
my.prev.carr2 <- as.data.frame(sapply(my.prev.carr2, FUN = function(x) as.character(gsub("ê", "e", x))))
my.prev.carr2 <- as.data.frame(sapply(my.prev.carr2, FUN = function(x) as.character(gsub("ô", "o", x))))
my.prev.carr2 <- as.data.frame(sapply(my.prev.carr2, FUN = function(x) as.character(gsub("  ", " ", x))))

my.prev.carr2 <-
    my.prev.carr2 %>%
    mutate(class.carr = NA)

# percorre todas as colunas de df_class.carr
for (j in 1:ncol(df_class.carr)) {
    # percorre toda uma coluna de df_class.carr marcando em my.prev.carr$class.carr se encontra
    for (i in 1:length(my.prev.carr2$profissao.na.area.de)) {
        #print(i)
        y <- which(!is.na(str_match(df_class.carr[,j],as.character(my.prev.carr2$profissao.na.area.de[i]))))
        
        if (length(y) != 0) {
            my.prev.carr2[i,10] = colnames(df_class.carr)[j]
        } 
    }
}
# mudando NA para "INDEFINIDO" em coluna class.carr
my.prev.carr2 <-
    my.prev.carr2 %>%
    mutate(class.carr = ifelse(is.na(class.carr), "INDEFINIDO", as.character(class.carr)))

# plotando os resultados discriminados para amostra aleatória nos PC1, PC2 e PC3
require("plotly")
pc2 <- plot_ly(my.prev.carr2, x = PC1, y = PC2, z = PC3, color = class.carr, type = "scatter3d", mode = "markers")
pc2 <- layout(pc2, title = "Scores da Amostra por Classe de Profissão - PC1 x PC2 X PC3")
pc2
# box plot
pc3 <- plot_ly(my.prev.carr2, x = PC1, color = class.carr, type = "box")
pc3 <- layout(pc3, title = "Scores da Amostra por Classe de Profissão - PC1")
pc3
#which(!is.na(str_match(df_class.carr[,1],"Desenv")))
#which(!is.na(str_match(df_class.carr[,"CFM"],"Desenv")))
#which(!is.na(str_match(df_class.carr$CFM,"Desenv")))
#which(!is.na(str_match(df_class.carr[,8],as.character(my.prev.carr$profissao.na.area.de[10]))))

#----------------------------------------
# ANALISE DE MOVIMENTACAO (TESTE)
#----------------------------------------


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
pca1 = prcomp(df_tidy_hg[,7:14], scale. = TRUE, center = TRUE)
# selecionando apenas amostra para rapidez de processamanto (posteriormente colocar em combobox)
tam.amostra = 1000
my.newdata <- 
    df_change %>%
    sample_n(tam.amostra)
# fazendo para toda a base
#my.newdata.carr <- df_tidy_hg
#tam.amostra = dim(my.newdata)[1]

# calculando os scores
my.prev <- as.data.frame(predict(pca1, newdata=my.newdata))
# criando dataframe por carreira e profissão
my.prev <- cbind(ID = my.newdata$ID, profissao.na.area.de = my.newdata$profissao.na.area.de, 
                 formacao.em = my.newdata$formacao.em,
                 my.prev)

# PROFISSAO x CLASSE
# criando novo dataframe para tratar mais de uma profissão por classe de carreira
#----------------------------------------------------------------------------------
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
pc1 <- layout(pc1, title = "OPENNESS/EXPLORATION x COMPLIANCE/OBSERVÂNCIA") 
pc1

# testar ggplotly!!!

#PAREI AQUI: só falhou para engenharia quimica!!!?? pQ????
#Testar ID 31669
#my.prev[my.prev$ID == 31669,] # OK
#tem formacao e ocupacao iguais
#my.prev.carr[my.prev.carr$ID == 31669,] # OK
#Depois do melt perdeu o ID???!!! POIS AS CLASSES ESTAVAM COM NA ANTES DO MELT!!??
#MAS DEVERIA TER ACHADO ENGENHARIA QUIMICA EM CFM e CFQ
#RESP: precisa tirar brancos no início de cada celula lida do excel!!!!
# ESTRATÉGIA PARA CRIAR ALGORITMO PCs -> COMPONENTES HUMAN GUIDE
#-------------------------------------------------------
# 1. obter o valor do score do componente e aplicar intervalo de confiança (como?)

DF<-data.frame(Maths=c(80, 90, 95), Science=c(85, 85, 80), English=c(60, 70, 40), Music=c(55, 45, 50))
prcomp(DF, scale = FALSE)$x


# criação de heatmap e clusterização de Campos x PC
#################################################################
# 1. criar matriz Campo x PCn.médio

# usar my.prev.carr preparado em Rmd Análise Descritiva - Exploratoria
my.PC.Campo <-
    my.prev.carr %>%
    group_by(class.carr) %>%
    summarise(PC1.medio = mean(PC1),
              PC2.medio = mean(PC2),
              PC3.medio = mean(PC3),
              PC4.medio = mean(PC4),
              PC5.medio = mean(PC5),
              PC6.medio = mean(PC6),
              PC7.medio = mean(PC7))

# inserindo numero sequencial para representar os campos
my.PC.Campo <-
    my.PC.Campo %>%
    mutate(ID.campo = seq(nrow((my.PC.Campo))))

plot(my.PC.Campo$class.carr,my.PC.Campo$PC1.medio)
plot(my.PC.Campo$class.carr, my.PC.Campo$PC1.medio, col=my.PC.Campo$class.carr, type = "p")
require("plotly")
pc1 <- plot_ly(my.PC.Campo, y = PC1.medio, color = class.carr, type = "box")
pc1 <- layout(pc1, title = "AUSTERIDADE/PONDERAÇÃO x ADAPTABILIDADE/AJUSTAMENTO)")
pc1
# fazer cluster em relação ao PC1 e PC2
require(ggplot2)
p1 <- ggplot(my.PC.Campo, aes(x=class.carr, y=PC2.medio, colour=class.carr, group=class.carr)) +
    geom_line() +
    ggtitle("Growth curve for individual chicks")

# aplicar abaixo para os dados tidy
library(mclust)
fit <- Mclust(df_change[,4:11])
plot(fit) # plot results 
summary(fit) # display the best model

# analisando clusters para média e valores tidy
# Talvez abaixo seja a melhor maneira de classificar os componentes por campo !!!!!
#---------------------------------------------------
# para as medias
(kc <- kmeans(my.PC.Campo[,2:8], 7))
table(my.PC.Campo$class.carr, kc$cluster)
plot(my.PC.Campo[c("PC1.medio", "PC2.medio")], col=kc$cluster)
points(kc$centers[,c("PC1.medio", "PC2.medio")], col=1:10, pch=8, cex=2)
# para as medias transpostas    #AQUI
my.PC.Campo.t <- t(my.PC.Campo)
colnames(my.PC.Campo.t) <- my.PC.Campo.t[1,]

my.PC.Campo.t <- my.PC.Campo.t[-9,] # elimina colunas indesejadas
my.PC.Campo.t <- my.PC.Campo.t[-1,] # elimina colunas indesejadas
my.PC.Campo.t <- as.data.frame(my.PC.Campo.t)
my.PC.Campo.t <-
    my.PC.Campo.t %>%
    mutate(class.carr = rownames(my.PC.Campo.t))
my.PC.Campo.t <-
    my.PC.Campo.t %>%
    mutate(CFM = as.numeric(as.vector(CFM)),
           CFQ = as.numeric(as.vector(CFQ)),
           CCF = as.numeric(as.vector(CCF)),
           COA = as.numeric(as.vector(COA)),
           CJS = as.numeric(as.vector(CJS)),
           CCP = as.numeric(as.vector(CCP)),
           CSL = as.numeric(as.vector(CSL)),
           CMA = as.numeric(as.vector(CMA)),
            CCE = as.numeric(as.vector(CCE)),
            CBS = as.numeric(as.vector(CBS)))
# inserindo numero sequencial para representar os campos
my.PC.Campo.t <-
    my.PC.Campo.t %>%
    mutate(ID.PC = seq(nrow((my.PC.Campo.t))))
# cluster por PC
(kc <- kmeans(my.PC.Campo.t[,1:10], 6))
table(my.PC.Campo.t$class.carr, kc$cluster)
# cluster por Campo
(kc <- kmeans(my.PC.Campo[,2:8], 3))
table(my.PC.Campo$class.carr, kc$cluster)
plot(my.PC.Campo[c("PC1.medio", "PC2.medio")], col=kc$cluster)

# plotando mapa de componentes para cada 
#plot(my.PC.Campo.t[c("CFM", "CFQ")], col=kc$cluster)
#points(kc$centers[,c("PC1.medio", "PC2.medio")], col=1:10, pch=8, cex=2)
# para todos os dados
# AQUI
carrMelt.smp <- sample_n(carrMelt,100)
(kc <- kmeans(carrMelt.smp[,3:9], 7))
table(carrMelt.smp$class.carr, kc$cluster)
plot(carrMelt.smp[c("PC1", "PC2")], col=kc$cluster)

pc1 <- plot_ly(carrMelt, y = PC1, color = class.carr, type = "box")
pc1 <- layout(pc1, title = "AUSTERIDADE/PONDERAÇÃO x ADAPTABILIDADE/AJUSTAMENTO)")
pc1

pc2 <- plot_ly(my.PC.Campo, y = PC1.medio, color = class.carr, type = "box")
pc2 <- layout(pc2, title = "AUSTERIDADE/PONDERAÇÃO x ADAPTABILIDADE/AJUSTAMENTO)")
pc2

# error bars
library(dplyr)
library(plotly)

p <- ggplot2::mpg %>% group_by(class) %>%
    summarise(mn = mean(hwy), sd = 1.96 * sd(hwy)) %>%
    arrange(desc(mn)) %>%
    plot_ly(x = class, y = mn, error_y = list(value = sd),
            mode = "markers", name = "Highway") %>%
    layout(yaxis = list(title = "Miles Per Gallon"))
p

#points(kc$centers[,c("PC1", "PC2")], col=1:10, pch=8, cex=2)

# fazer heatmap de ambos para ver se batem (muito demorado para todos os dados, usar 1000)
heatmap(as.matrix(my.PC.Campo[,2:8]), Colv = my.PC.Campo$class.carr) #AQUI

# plot por campo 
p1 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick)) +
    geom_line() +
    ggtitle("Growth curve for individual chicks")
p1 <- ggplot(carrMelt[1:100,], aes(x=PC1, y=PC2)) +
    geom_line() + geom_smooth()
    ggtitle("Growth curve for individual chicks")
# abaixo funciona
pc1 <- ggplot(my.PC.Campo.t, aes(x=class.carr, y=CFM)) +
        geom_point() +
    ggtitle("CFM x PC1 médio")
pc2 <- ggplot(my.PC.Campo, aes(x=class.carr, y=PC2.medio)) +
        geom_point()
    ggtitle("Campo Profisisonal x PC2 médio")
pc3 <- ggplot(my.PC.Campo, aes(x=class.carr, y=PC3.medio)) +
        geom_point()
    ggtitle("Campo Profisisonal x PC3 médio")
pc4 <- ggplot(my.PC.Campo, aes(x=class.carr, y=PC4.medio)) +
        geom_point()
    ggtitle("Campo Profisisonal x PC4 médio")
pc5 <- ggplot(my.PC.Campo, aes(x=class.carr, y=PC5.medio)) +
        geom_point()
    ggtitle("Campo Profisisonal x PC5 médio")
pc6 <- ggplot(my.PC.Campo, aes(x=class.carr, y=PC6.medio)) +
        geom_point()
    ggtitle("Campo Profisisonal x PC6 médio")
#pc7 <- ggplot(my.PC.Campo, aes(x=class.carr, y=PC7.medio)) +
#        geom_point()
#    ggtitle("Campo Profisisonal x PC7 médio")

library(Rmisc)
multiplot(pc1, pc2, pc3, pc4, pc5, pc6, pc7, cols=2)    

# IDEIA INICIAL PARA CLASSIFICAR A PESSOA

# ABAIXO USAR PARA COMPARAR QUAIS CAMPOS TEM DISTRIBUICAO SEMELHANTE DE SCORES POR PC
#-------------------------------------------------------------------------------------
pc1 <- ggplot(my.PC.Campo.t, aes(x=ID.PC, y=CFM)) +
    #geom_pointrange(ymin = 0.01, ymax = -0.01) +
    #geom_errorbar(ymin = 0.01, ymax = -0.01) +
    #geom_line() + geom_smooth() +
    geom_line() +
    ggtitle("CFM x PC médio") 
#pc2 <- ggplot(my.PC.Campo.t, aes(x=class.carr, y=CFQ)) +
pc2 <- ggplot(my.PC.Campo.t, aes(x=ID.PC, y=CFQ)) +
    geom_line() +
    ggtitle("CFQ x PC médio")
pc3 <- ggplot(my.PC.Campo.t, aes(x=ID.PC, y=CCF)) +
    geom_line() +
    ggtitle("CCF x PC médio")
pc4 <- ggplot(my.PC.Campo.t, aes(x=ID.PC, y=COA)) +
    geom_line() +
    ggtitle("COA x PC médio")
pc5 <- ggplot(my.PC.Campo.t, aes(x=ID.PC, y=CJS)) +
    geom_line() +
    ggtitle("CJS x PC médio")
pc6 <- ggplot(my.PC.Campo.t, aes(x=ID.PC, y=CCP)) +
    geom_line() +
    ggtitle("CCP x PC1 médio")
pc7 <- ggplot(my.PC.Campo.t, aes(x=ID.PC, y=CSL)) +
    geom_line() +
    ggtitle("CSL x PC1 médio")
pc8 <- ggplot(my.PC.Campo.t, aes(x=ID.PC, y=CMA)) +
    geom_line() +
    ggtitle("CMA x PC médio")
pc9 <- ggplot(my.PC.Campo.t, aes(x=ID.PC, y=CCE)) +
    geom_line() +
    ggtitle("CCE x PC médio")
pc10 <- ggplot(my.PC.Campo.t, aes(x=ID.PC, y=CBS)) +
    geom_line() +
    ggtitle("CBS x PC médio")
    
multiplot(pc1, pc2, pc3, pc4, pc5, pc6, pc7, pc8, pc9, pc10, cols=2)    


# ggplots de errorbar
# exemplo
df <- data.frame(
    trt = factor(c(1, 1, 2, 2)),
    resp = c(1, 5, 3, 4),
    group = factor(c(1, 2, 1, 2)),
    se = c(0.1, 0.3, 0.3, 0.2)
)
df2 <- df[c(1,3),]    
# Define the top and bottom of the errorbars
limits <- aes(ymax = resp + se, ymin=resp - se)

p <- ggplot(df, aes(fill=group, y=resp, x=trt))
p + geom_bar(position="dodge", stat="identity")

# Because the bars and errorbars have different widths
# we need to specify how wide the objects we are dodging are
# DEPOIS USAR ABAIXO PARA DEFINIR INTERVALOS A CONSIDERAR NO ALGORITMO
dodge <- position_dodge(width=0.9)
p + geom_bar(position=dodge) + geom_errorbar(limits, position=dodge, width=0.25)

p <- ggplot(df2, aes(fill=group, y=resp, x=trt))
p + geom_bar(position=dodge)
p + geom_bar(position=dodge) + geom_errorbar(limits, position=dodge, width=0.25)

p <- ggplot(df, aes(colour=group, y=resp, x=trt))
p + geom_point() + geom_errorbar(limits, width=0.2)
p + geom_pointrange(limits)
p + geom_crossbar(limits, width=0.2)
# fim exemplo errorbar

pc1 <- ggplot(my.PC.Campo, aes(x=ID.campo, y=PC1.medio)) +
    geom_line() + geom_smooth() +
    ggtitle("PC1 médio x Campo Profissional") 

# testando fiting de 2 curvas
c1 <- c(0, 0.8, 0.9, 0.9, 0.5, 0.1, 0.5)

c1b <- c(0, 0.8, 0.9, 0.9, 0.5, 0.1, 0.5) # perfect match! ideally score of 1

c1c <- c(1, 0.2, 0.1, 0.1, 0.5, 0.9, 0.5) # total opposite, ideally score of -1? (what would 0 be though?)

c2 <- c(0, 0.9, 0.9, 0.9, 0, 0.3, 0.3, 0.9) #pretty good, score of ???

ccf(c1,c1c)

c4 <- c(0, 0.6, 0.7, 0.7, 0.3, 0.1, 0.3) # very good, score of ??
# correlacao?
require(Hmisc)
rcorr(as.matrix(dfrm))[[1]]

# TALVEZ ABAIXO SEJA AO ALGORITMO A USAR !!!! MEDE A FORMA DA CURVA!!!! NÃO A POSIÇÃO EM RELACAO AO EIXO Y (SCORE)
# ESTA OK ASSIM??? ACHO QUE NÃO. PRECISA MEDIR A DIFERENÇA DAS MÉDIAS PARA CADA PONTO

# SE CONSEIERARMOS QUE CURVA É ASSINATURA PRECISAMOS MEDIR DUAS COISAS
# 1. Correlação da pessoa com as curvas: usando critério de corte (acima de 0.6????). ISSO NOS INDICA A 
# PROXIMIDADE ENTRE OS COMPONENTES MAIS IMPORTANTES DO CAMPO EM RELACAO A PESSOA, OU SEJA, SE A VARIAÇÃO DOS COMPONENTES
# PARA O CAMPO É PROXIMO AO DA PESSOA
# 2. PARA OS CAMPOS QUE PASSOU NO CRITERIO, precisamos medir o grau onde cada componente da pessoa 
# se aproxima do PC do campo. Podemos medir binariamente: PC1 da pessoa está dentro do IC PC.medio.

# 1. ASSINATURA DE CAMPO PROF MOSTRA OS VALORES MÉDIOS QUE SÃO CARACTERÍSTICAS DAQUELE CAMPO PARA CADA PC
# 2. OBTER COMPONENTES QUE SE DESTACAM PARA O CAMPO. EX. PCC tem PC1 e PC2


# EX. PARA CADA CAMPO PROF: PC1.CFF.medio = 0.2 (+-0.05) -> PC1.pessoa: se dentro do IC considera
cor( c(0, 0.8, 0.9, 0.9, 0.5, 0.1, 0.5),
     c(0, 0.6, 0.7, 0.7, 0.3, 0.1, 0.3) )

campo.1 <- c(0, -0.8, 0.2, 0.1, 0.13, 0.1, 0.5)
campo.2 <- c(0, -0.6, 0.7, 0.7, 0.3, 0.1, 0.3)
cor(campo.1,campo.2)
plot(campo.1, type = "l", ylim = c(-1.0, 1.0))
plot(campo.2, type = "l", ylim = c(-1.0, 1.0))
d <- data.frame(campo.1, campo.2)
# usar qplot para correlacionar as curvas campo x pessoa (fazer para cada campo e display p escolher limite de confiança)
qplot(campo.1,campo.2, data= d, geom=c("smooth"),method="lm")

campo.3 <- c(1, 1.6, 1.7, 1.7, 1.3, 1.1, 1.3)
campo.4 <- c(0, 0.6, 0.7, 0.7, 0.3, 0.1, 0.3)
cor(campo.3,campo.4)
# outra solucao é colocar um smooth ou IC nos pontos da curva padrão do campo e mostrar graficamente.
# Assim colocamos a curva da pessoa e fazemos analise visual (PARA IMPLEMENTACAO: OUTRO PROJETO PARA GERAR EM R)

# chamando markdown com parametros
rmarkdown::render("testeHG.Rmd", params = "ask")
