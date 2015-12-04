# geração de arquivos em excel para análise
require("doMC")
require("xlsx")
source("./R/f_le_raw_HG.R") # usar esta função para ler os dados novos. 
source("./R/f_tidy_scores_HG.R")
registerDoMC(5) # parallel processing
#df_raw_hg <- f_le_raw_HG_ori() # retorna colunas: ID, sexo, pontos
df_raw_hg <- f_le_raw_HG() # retorna colunas: todas
df_tidy_hg <- f_tidy_scores_HG(df_raw_hg)
# Tabela de dados
#write.xlsx(df_tidy_hg, "./data/DadosBrutos.xlsx")
# Tabela da correlação
my.cor <- cor(df_tidy_hg[,3:10], method = "spearman")
write.xlsx(my.cor, "./data/CorrelacaoFatores.xlsx")
# Tabela de p-values e intervalos de confiança
l_cor <- list()
l_cor[[1]] <- cor.test(~ sensibility + power, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)
l_cor[[2]] <- cor.test(~ sensibility + quality, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)
l_cor[[3]] <- cor.test(~ sensibility + exposure , data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)
l_cor[[4]] <- cor.test(~ sensibility + structure, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)
l_cor[[5]] <- cor.test(~ sensibility + imagination, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)
l_cor[[6]] <- cor.test(~ sensibility + stability, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)
l_cor[[7]] <- cor.test(~ sensibility + contacts, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)

l_cor[[8]] <- cor.test(~ power + quality, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)
l_cor[[9]] <- cor.test(~ power + exposure , data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)
l_cor[[10]] <- cor.test(~ power + structure, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)
l_cor[[11]] <- cor.test(~ power + imagination, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)
l_cor[[12]] <- cor.test(~ power + stability, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)
l_cor[[13]] <- cor.test(~ power + contacts, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)

l_cor[[14]] <- cor.test(~ quality + exposure , data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)
l_cor[[15]] <- cor.test(~ quality + structure, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)
l_cor[[16]] <- cor.test(~ quality + imagination, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)
l_cor[[17]] <- cor.test(~ quality + stability, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)
l_cor[[18]] <- cor.test(~ quality + contacts, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)

l_cor[[19]] <- cor.test(~ exposure + structure, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)
l_cor[[20]] <- cor.test(~ exposure + imagination, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)
l_cor[[21]] <- cor.test(~ exposure + stability, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)
l_cor[[22]] <- cor.test(~ exposure + contacts, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)

l_cor[[23]] <- cor.test(~ structure + imagination, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)
l_cor[[24]] <- cor.test(~ structure + stability, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)
l_cor[[25]] <- cor.test(~ structure + contacts, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)

l_cor[[26]] <- cor.test(~ imagination + stability, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)
l_cor[[27]] <- cor.test(~ imagination + contacts, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)

l_cor[[28]] <- cor.test(~ stability + contacts, data = df_tidy_hg, method = "spearman", conf.level = .95, exact = FALSE)

df_cor <- data.frame()

for (i in 1:length(l_cor)) {
    df_cor[i,1] <- l_cor[[i]]$data.name
    df_cor[i,2] <- l_cor[[i]]$estimate
    x <- CIr(r=l_cor[[i]]$estimate, n = 116603)
    df_cor[i,3] <- x[1]
    df_cor[i,4] <- x[2]
    df_cor[i,5] <- l_cor[[i]]$p.value
}
names(df_cor) <- c("Par", "Correlação", "Lower Confidence Limit", "Upper Confident Limit","p-value")

write.xlsx(df_cor, "./data/CorrelacaoConfidenceInterval.xlsx")

# Tabela de carga de componentes
pca1 = prcomp(df_tidy_hg[,3:10], retx = TRUE, scale. = TRUE, center = TRUE)
df_carga <- pca1$rotation
write.xlsx(df_carga, "./data/CargaFatores.xlsx")
# Tabela de desvios padrão
df_sdev <- data.frame(standard.deviation = pca1$sdev)
row.names(df_sdev) <- c("PC1", "PC2", "PC3","PC4", "PC5", "PC6", "PC7", "PC8")
write.xlsx(df_sdev, "./data/StdDevComponentes.xlsx")
# Tabela de carga rotacionada com Varimax
ncomp <- 8 # número de componentes
rawLoadings <- pca1$rotation[,1:ncomp] %*% diag(pca1$sdev, ncomp, ncomp) # idem
my.varimax <- varimax(rawLoadings, normalize = TRUE)
my.var.carga <- my.varimax$loadings[,1:ncomp]
colnames(my.var.carga) <- c("PC1", "PC2", "PC3","PC4", "PC5", "PC6", "PC7", "PC8")
write.xlsx(my.var.carga, "./data/CargaVarimax.xlsx")

