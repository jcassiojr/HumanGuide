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

# MELHORIA DO TESTE ACIMA PARA USAR TODAS AS CLASSES ACHADAS PARA CADA PROFISSAO
# 1. repetir a linha se ocupação aparece em mais de uma classe
my.prev.carr <- as.data.frame(predict(pca1, newdata=my.newdata.carr))
my.prev.carr <- cbind(profissao.na.area.de = my.newdata.carr$profissao.na.area.de, my.prev.carr)

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
# inicializando a coluna com NA
#my.prev.carr <-
#    my.prev.carr %>%
#    mutate(class.carr = NA)
df_class <- data.frame()

for (j in 1:ncol(df_class.carr)) {
    # percorre todas as linhas da coluna corrente de df_class.carr, marcando em my.prev.carr$class.carr
    # caso encontre
    for (i in 1:length(my.prev.carr$profissao.na.area.de)) {
        # somente pega o string com match exato (ex. administração)
        y <- sum(!is.na(str_match(df_class.carr[,j],paste0("^", as.character(my.prev.carr$profissao.na.area.de[i]), "$"))))
        # se achou, muda o conteúdo da coluna class.carr para última classe encontrada
        # se encontra mais de uma classe para mesma profissão, duplica a linha para esta classe (como?)
        # Resp: salvando vetor com a posição das colunas das classes encontradas em df_class.carr
        #       para cada profissao de my.prev.carr. Depois usar este vetor para criar linhas duplicadas
        #       para cada profissao x classe
        #if (length(y) != 0) {
        if(y) {
            df_class[i,j] = y
        } else {
            df_class[i,j] = NA
        }
    }
}
# colocando os nomes das classes no dataframe gerado
names(df_class) <- colnames(df_class.carr)

# concatenando a coluna de profissoes ao dataframe gerado
my.prev.carr <- cbind(my.prev.carr, df_class)

# duplicar colunas que aparecem com mais de uma classe
require(reshape2)
# transformando colunas em valores por linha, eliminando NAs
classMelt <- melt(my.prev.carr,id=c("profissao.na.area.de","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8"),
                  measure.vars=colnames(df_class.carr), na.rm = TRUE)   

# ESTRATÉGIA PARA CRIAR ALGORITMO PCs -> COMPONENTES HUMAN GUIDE
#-------------------------------------------------------
# 1. obter o valor do score do componente e aplicar intervalo de confiança (como?)


