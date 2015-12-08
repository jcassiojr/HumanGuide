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
