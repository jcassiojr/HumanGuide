# teste de publicacao PLOTLY
require("plotly", quietly = TRUE, warn.conflicts = FALSE)
my.newdata <- df_tidy_hg
# obtendo os scores previstos
pca1 = prcomp(my.newdata[,7:14], scale. = TRUE, center = TRUE)
# calculando os scores
my.prev <- as.data.frame(predict(pca1, newdata=my.newdata))
my.prev.form <- cbind(formacao.em = my.newdata$formacao.em, my.prev)
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
# classificando por área de formação
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
my.prev.form <- cbind(area.formacao = my.area$area.formacao, my.prev.form)

my.prev.form2 <-
    my.prev.form %>%
    mutate(area.form.text = ifelse(area.formacao == 1, "HUMANAS",
                                   ifelse(area.formacao == 2, "BIOLÓGICAS", "EXATAS")))

f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
)
x <- list(
    title = "PC1 Axis",
    titlefont = f
)
y <- list(
    title = "PC2 Axis",
    titlefont = f
)
z <- list(
    title = "PC3 Axis",
    titlefont = f
)

pc1 <- plot_ly(my.prev.form2, x = PC1.medio, y = PC2.medio, z = PC3.medio, color = area.form.text, type = "scatter3d", mode = "markers")
pc1 <- layout(pc1, title = "Scores Médios por Área de Formação - PC1 x PC2 X PC3"),
              xaxis = x, yaxis = y, zaxis = z)
pc1

# publicando na WEB (coloquei em .Rprofile no working directory)
#Sys.setenv("plotly_username"="jcassiojr")
#Sys.setenv("plotly_api_key"="sdl04c2tix")
# filename sets the name of the file inside your online plotly account.
# world_readable sets the privacy of your chart. If TRUE, the graph is publically viewable, if FALSE, only you can view it.
plotly_POST(pc1, filename = "r-docs/scores-3D", world_readable=FALSE, fileopt = "overwrite")
