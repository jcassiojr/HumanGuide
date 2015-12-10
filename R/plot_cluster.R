# plot de cluster
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