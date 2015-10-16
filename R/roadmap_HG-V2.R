#---------------------------------------
# Roadmap de Análise Human Guide
#---------------------------------------

# 1. ESTABELECER CLARAMENTE O PROBLEMA DE NEGÓCIO A SOLUCIONAR
#----------------------------------------------------------
# PROBLEMA:
# ABORDAGEM:

# 2. PREPARACAO DOS DADOS
#----------------------------------------------------------
# Um modelo candidato para os dados raw é:
# 9 colunas para as features pessoais
#    nome, idade, sexo, escolaridade, formação, empresa, endereço, telefone e e-mail
# 72 colunas para cada afirmação HG, com valores:
#    p (positivo), n (negativo) ou i (indiferente)
# 1 coluna com valor latente, com valores: 
#    sensibilidade, força, qualidade, exposição, estrutura, imaginação, estabilidade, 
#    contatos
#-------------------------------------------------------------------------------
# 1. obter os dados de candidatos com o feature vector de suas pontuações 
#    no teste HG. 
# 2. associar ao dataset a target variable (turnover: sim/não) da base de funcionários
#    da empresa
# 3. separar em train data e houdout data usando a mesma distribuição das prior classes
#    da população (obter a distribuição real de turnover na empresa)

# ANÁLISE EXPLORATÓRIA
#---------------------------------------
# usar os dados sem preparaçõa adicional para criar os clusters
# criando dados para simular seguindo as proporções
# da amostra da tese

# dados pessoais de 815 amostras
set.seed(1)
idade <- round(runif(815, 18, 60), digits = 0)
sexo <- sample(x=c("m","f"), size=815, replace=TRUE, prob=c(51.3/100, 48.7/100))
# superior completo, superior incompleto, posgraduacao completo,
# posgraduação incompleto, ensino médio completo/incompleto
escolaridade <- sample(x=c("sc","si","pc", "pi", "ec", "ei"), size=815,
                       replace=TRUE, prob=c(470/815, 11.5/100, 133/815, 0.5/100, 11.7/100, 2.6/100))
# humanas, exatas, biológicas, não informado
formacao <- sample(x=c("h","e","b", NA), size=815,
                   replace=TRUE, prob=c(46/100, 33/100, 6/100, 13.5/100))
# área de atuação: navegação, TI, autopeças, juridico, saneamento público,
# bancario, audiovisual, logística, educacional, saúde, químico, construcao civil, NA, outros
segmento <- sample(x=c("nv","ti","ap","ju","sp","bn","av","lg","ed","sd","qu","cc", NA, "ou"), size=815,
                   replace=TRUE, prob=c(27.5/100, 16.2/100, 3.7/100, 3.3/100, 2.8/100, 5.9/100, 4.6/100,
                                        4.6/100, 3/100, 1.7/100, 2.2/100, 2.5/100, 7.9/100, 30.3/100))
# procedência
procedencia <- sample(x=c("sp.cap","sp.abc","sp.int", "sp.lit", "mg", "sc", "rg", "rj",
                          "am", "ba", "ce", "df", "go", "pe", "pi", "pr"), size=815,
                   replace=TRUE, prob=c(43.2/100, 9/100, 10.6/100, 4.2/100,
                                        6.5/100, 2.3/100, 1.6/100, 1.2/100,
                                        1/100, 0.2/100, 1/100, 0.2/100, 1/100, 0.2/100,
                                        1/100,0.2/100))
# cargos
# assistente, analista, auxiliar, coordenador, gerente, consultor interno/externo
# diretor, estagiários/trainees, advogado, psicólogo, engenheiro, técnico, vendedor
# programador, planejador, NA
cargo <- sample(x=c("as","an","au", "co", "ge", "cs", "di", "es",
                          "ad", "ps", "en", "tc", "ve", "pr", "pl", NA), size=815,
                      replace=TRUE, prob=c(17.1/100, 19.4/100, 6.7/100, 7.5/100,
                                           6.3/100, 3.7/100, 2/100, 5.3/100,
                                           2/100, 1.5/100, 2/100, 2.2/100, 1.2/100, 1.2/100,
                                           1.2/100,6.2/100))
# features do teste HG sando os resultados da tese
# 72 colunas com valores p (positivo), n (negativo) ou i (indiferente)
f_11s <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(62.4/100, 17.7/100, 19.9/100))
f_12e <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(52.2/100, 35.5/100, 12.4/100))
f_13h <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(68.3/100, 15.1/100, 16.7/100))
f_14k <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(61.8/100, 28/100, 10.2/100))
f_15p <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(43.5/100, 24.7/100, 31.6/100))
f_16hy <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(33.9/100, 39.2/100, 26.9/100))
f_17d <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(41.4/100, 23.7/100, 34.9/100))
f_18m <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(34.4/100, 18.3/100, 47.3/100))
f_21h <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(59.7/100, 32.3/100, 8.1/100))
f_22e <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(70.4/100, 27.4/100, 2.2/100))
f_23k <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(24.2/100, 15.1/100, 60.8/100))
f_24d <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(12.9/100, 15.6/100, 71.5/100))
f_25m <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(58.6/100, 38.2/100, 3.2/100))
f_26p <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(79/100, 16.1/100, 4.8/100))
f_27s <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(64/100, 26.9/100, 9.1/100))
f_28hy <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(30.1/100, 28/100, 41.9/100))
f_31h <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(51.1/100, 40.3/100, 8.6/100))
f_32e <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(91.4/100, 7.5/100, 1.1/100))
f_33hy <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(5.9/100, 7/100, 87.1/100))
f_34k <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(76.3/100, 15.6/100, 8.1/100))
f_35s <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(45.2/100, 32.3/100, 22.6/100))
f_36p <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(62.4/100, 31.7/100, 5.9/100))
f_37d <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(32.3/100, 31.7/100, 36/100))
f_38m <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(37.6/100, 33.9/100, 28.5/100))
f_41hy <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(54.3/100, 36/100, 9.7/100))
f_42s <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(59.1/100, 17.2/100, 23.7/100))
f_43e <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(72.6/100, 15.1/100, 12.4/100))
f_44k <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(70.4/100, 18.3/100, 11.3/100))
f_45h <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(40.3/100, 33.9/100, 25.8/100))
f_46m <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(26.3/100, 28/100, 45.7/100))
f_47d <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(52.7/100, 34.9/100, 12.4/100))
f_48p <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(23.7/100, 17.7/100, 58.6/100))
f_51e <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(57.8/100, 19.4/100, 4.8/100))
f_52s <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(24.2/100, 23.7/100, 52.2/100))
f_53hy <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(39.2/100, 31.2/100, 29.6/100))
f_54k <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(47.8/100, 19.9/100, 32.3/100))
f_55d <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(32.3/100, 20.4/100, 47.3/100))
f_56h <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(51.1/100, 38.2/100, 10.8/100))
f_57p <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(58.6/100, 31.2/100, 10.2/100))
f_58m <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(71.5/100, 16.7/100, 11.8/100))
f_61m <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(84.9/100, 11.3/100, 3.8/100))
f_62s <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(36/100, 18.3/100, 45.7/100))
f_63e <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(16.1/100, 19.9/100, 64/100))
f_64hy <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(56.5/100, 19.9/100, 23.7/100))
f_65k <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(65.5/100, 21.5/100, 12.9/100))
f_66p <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(40.3/100, 43/100, 16.7/100))
f_67d <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(38.7/100, 36.6/100, 24.7/100))
f_68h <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(62.9/100, 29/100, 8.1/100))
f_71m <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(21.5/100, 23.1/100, 55.4/100))
f_72k <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(45.7/100, 40.9/100, 13.4/100))
f_73s <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(70.4/100, 23.7/100, 5.9/100))
f_74p <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(89.8/100, 10.2/100, 0/100))
f_75hy <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(28/100, 25.3/100, 46.8/100))
f_76h <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(51.6/100, 41.4/100, 7/100))
f_77e <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(84.4/100, 13.4/100, 2.2/100))
f_78d <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(8.6/100, 21.5/100, 69.9/100))
f_81p <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(39.2/100, 33.3/100, 27.4/100))
f_82h <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(72.6/100, 24.7/100, 2.7/100))
f_83e <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(88.7/100, 9.1/100, 2.2/100))
f_84s <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(57.5/100, 25.3/100, 17.2/100))
f_85m <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(30.1/100, 29.9/100, 40.9/100))
f_86k <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(61.8/100, 19.9/100, 18.3/100))
f_87hy <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(14.5/100, 14.5/100, 71/100))
f_88d <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(37.1/100, 43.5/100, 19.4/100))
f_91e <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(94.1/100, 4.8/100, 1.1/100))
f_92m <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(74.2/100, 22/100, 3.8/100))
f_93p <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(58.6/100, 28/100, 13.4/100))
f_94d <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(38.2/100, 34.9/100, 26.9/100))
f_95k <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(44.6/100, 28/100, 27.4/100))
f_96s <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(42.5/100, 23.1/100, 34.4/100))
f_97h <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(33.3/100, 37.6/100, 29/100))
f_98hy <- sample(x=c("p","i", "n"), size=815, replace=TRUE, prob=c(14.5/100, 21.5/100, 64/100))

# criando o dataframe
df_hg <- data.frame(idade = idade, sexo = sexo, escolaridade = escolaridade, formacao = formacao,
                    segmento = segmento, procedencia = procedencia, cargo = cargo,
                    f_11s, f_12e, f_13h, f_14k, f_15p, f_16hy, f_17d, f_18m, f_21h, f_22e, f_23k, f_24d, f_25m, f_26p,
                    f_27s, f_28hy, f_31h, f_32e, f_33hy, f_34k, f_35s, f_36p, f_37d, f_38m, f_41hy, f_42s, f_43e,
                    f_44k, f_45h, f_46m, f_47d, f_48p, f_51e, f_52s, f_53hy, f_54k, f_55d, f_56h, f_57p, f_58m,
                    f_61m, f_62s, f_63e, f_64hy, f_65k, f_66p, f_67d, f_68h, f_71m, f_72k, f_73s, f_74p, f_75hy, f_76h,
                    f_77e, f_78d, f_81p, f_82h, f_83e, f_84s, f_85m, f_86k, f_87hy, f_88d, f_91e, f_92m, f_93p,
                    f_94d, f_95k, f_96s, f_97h, f_98hy)

# Prepare Data - listwise deletion of missing (should I standardize variables?)
df_hg <- na.omit(df_hg) # listwise deletion of missing



# IDENTIFICAR ATRIBUTOS COM MAIOR INFORMATION GAIN PARA FEATURE SELECTION
#-------------------------------------------------
# CÁLCULO MANUAL
#--------------------
# chamar aqui minha função de cálculo de information gain categórica
Ep <- f_entropy(df_hg[,1])
# cálculo de information gain para feature
#----------------------------------------
v_nfeat <- c("f_11s", "f_12e", "f_13h", "f_14k", "f_15p", "f_16hy", "f_17d", 
             "f_18m", "f_21h", "f_22e", "f_23k", "f_24d", "f_25m", "f_26p",
             "f_27s", "f_28hy", "f_31h", "f_32e", "f_33hy", "f_34k", "f_35s", 
             "f_36p", "f_37d", "f_38m", "f_41hy", "f_42s", "f_43e", "f_44k", 
             "f_45h", "f_46m", "f_47d", "f_48p", "f_51e", "f_52s", "f_53hy", 
             "f_54k", "f_55d", "f_56h", "f_57p", "f_58m", "f_61m", "f_62s",
             "f_63e", "f_64hy", "f_65k", "f_66p", "f_67d", "f_68h", "f_71m",
             "f_72k", "f_73s", "f_74p", "f_75hy", "f_76h", "f_77e", "f_78d", 
             "f_81p", "f_82h", "f_83e", "f_84s", "f_85m", "f_86k", "f_87hy", 
             "f_88d", "f_91e", "f_92m", "f_93p", "f_94d", "f_95k", "f_96s", 
             "f_97h", "f_98hy")
IG <- vector()
for (i in 1:length(v_nfeat) ) {
    IG[i] <- f_ig_cat(df_hg, v_nfeat[i], "turnover")
}

# plota IG
df_ig <- data.frame(v_nfeat,IG)
# data.frame filtered: IG > 0.003 (for better viasualization)
df_ig_fltr <- 
    df_ig %>%
    filter(IG > 0.003)
# using ggplot
library(ggplot2)
ggplot(data=df_ig, 
       aes(x = reorder(df_ig[,1], IG), y=IG)) +
    geom_bar(stat="identity",color = "black",
             fill="#DD8888", 
             width=.8) +
    ggtitle("Information Gain for 72 features")
# using ggplot again for filtered data
library(ggplot2)
ggplot(data=df_ig_fltr, 
       aes(x = reorder(df_ig_fltr[,1], IG), y=IG)) +
    geom_bar(stat="identity",color = "black",
             fill="#DD8888", 
             width=.8) +
    ggtitle("Information Gain for 72 features")

# ou esta alternativa para selecionar as features via Information Gain
#data(iris)
#weights <- information.gain(Species~., iris)

# CÁLCULO USANDO pacote FSelector
#--------------------
library(FSelector)
weights <- information.gain(turnover~., df_hg)
print(weights)
# cria fórmula com os N features de maior information gain
subset <- cutoff.k(weights, 3)
f <- as.simple.formula(subset, "turnover")
print(f)
# cria fórmula com os N features information gain acima de 75 %
subset <- cutoff.k.percent(weights, 0.75)
f <- as.simple.formula(subset, "turnover")
print(f)
# esta é a melhor opção (para pegar os atributos que mais se diferenciam dos demais)
subset <- cutoff.biggest.diff(weights)
f <- as.simple.formula(subset, "turnover")
print(f)

# USANDO CARET
#-------------------------

# AQUI CRIAR DADOS DE TEST E TRAIN 
#----------------------------------
# OBS: PRIMEIRO DEVO SEPARAR EM DOIS DATASETS???: 1 só com a coluna TARGET e outro com demais features
# VER SE É A MELHOR ABORDAGEM (é o que fqz em exp_predic_model_with_caret.R)
# ou deixa no meso datasets TARGET + FEATURES???!!!

#**********************************
#******* PAREI AQUI ***************
#**********************************
#**********************************



set.seed(1)
# separa mantendo a mesma estratificação (prior class) dos originais!!
# colocando p % dos dados no training dataset
inTrain <- createDataPartition(turnover, p = 3/4, list = FALSE)
trainDescr <- descr[inTrain,]
testDescr  <- descr[-inTrain,]
trainClass <- mutagen[inTrain]
testClass  <- mutagen[-inTrain]





# MODELING WITH THE TRAINING DATA AND CROSS VALIDATION
# WITH DIFERENT MODELS
#-------------------------------------------------
require(caret)
# load the iris dataset
#data(iris)
# PREPARACAO DOS DADOS
#----------------------------------------------------------
# criando bases simuladas para training and testing (com 2 classes)
#set.seed(2969)
#imbal_train <- twoClassSim(10000, intercept = -20, linearVars = 20)
#imbal_test  <- twoClassSim(10000, intercept = -20, linearVars = 20)
#table(imbal_train$Class)

# define training control (cross validation, 10 folds)
#train_control <- trainControl(method="cv", number=10)
#train_control <- trainControl(method = "repeatedcv", number=10, repeats = 5,
#                     classProbs = TRUE,
#                     summaryFunction = twoClassSummary
                     )
# usar names(getModelInfo()) para ver todas as possibilidades
# ver doc completa em http://topepo.github.io/caret/bytag.html

# REMOVENDO ATRIBUTOS PARA MELHORAR PERFORMANCE DO MODELO
#-------------------------------------------------
# Data can contain attributes that are highly correlated with each other. 
# Many methods perform better if highly correlated attributes are removed
# Generally, you want to remove attributes with an absolute correlation 
# of 0.75 or higher.

# load the library
library(mlbench)
# calculate correlation matrix
correlationMatrix <- cor(imbal_train[,1:25])
#correlationMatrix <- cor(PimaIndiansDiabetes[,1:8])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

# ranking features by importance
# The example below loads the Pima Indians Diabetes dataset and constructs an 
# Learning Vector Quantization (LVQ) model. The varImp is then used to estimate
# the variable importance, which is printed and plotted. 

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
#model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control)
model <- train(Class~., data=imbal_train, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

# A popular automatic method for feature selection provided by the caret
# R package is called Recursive Feature Elimination or RFE.
# The example below provides an example of the RFE method on the Pima 
# Indians Diabetes dataset. A Random Forest algorithm is used on each 
# iteration to evaluate the model. The algorithm is configured to explore
# all possible subsets of the attributes. All 8 attributes are selected 
# in this example, although in the plot showing the accuracy of the 
# different attribute subset sizes, we can see that just 4 attributes 
# gives almost comparable results.
data(PimaIndiansDiabetes)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
#results <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9], sizes=c(1:8), rfeControl=control)
results <- rfe(imbal_train[,1:25], imbal_train[,26], sizes=c(1:25), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

# PREPROCESSANDO PARA NORMALIZAR DADOS (Z-SCALE)
#-----------------------------------------------------
# MELHOR ABORDAGEM É USAR PARAMETRO DE PREDICT PARA ISSO
# ver webinar caret
# ex.
# svmTune <- train (churn ~ .,
#data = churnTrain,
#method = "svmRadial",
# this pre processing will be applied to
# these data and new samples too
#preProc = c("center", "scale"),
# tune of diferent values of cost
#tuneLength = 10,
#metric = "ROC",
#trControl = ctrl)

preObj <- preProcess(df, method=c("center", "scale"))

# TRATANDO OVERFITING
#----------------------------------------------------------
# COMO TRATAR?
# EM KNN, usar o número de vizinhos
# EM TREES, limitar o número de nodes
# em geral, usando um test set verificamos se ocorre overfitting
# resampling o training set nos permite saber se a previsão é boa
# sem udo do test set (insere variações no training set para simlar futuras
# amostras)
# portanto, usando train de caret já estamos lidando com isso




# CRIANDO DIVERSOS TIPOS DE MODELOS
#----------------------------------------------------------
# CLASSIFICATION MODELS

# TREE BAG MODEL
#---------------
model <- train(Class~., data=imbal_train, 
               nbagg = 50,
               metric = "ROC",
               trControl=train_control, method="treebag")
                
# make predictions
predictions <- predict(model, imbal_train[, -ncol(imbal_train)])
#print(predictions)
# summarize results
tb_cf <- confusionMatrix(predictions, imbal_train$Class)
print (tb_cf$table) # confusion matrix as a table
print (tb_cf$byClass) # estatístics as a matrix
print (tb_cf$overall) # acuracy as a numeric vector

# CONDITIONAL INFERENCE TREE MODEL
ctree2_model <- train(Class~., data=imbal_train, 
                    #nbagg = 50,
                    metric = "ROC",
                    trControl=train_control, method="ctree2")
#lmt_model <- train(Class~., data=iris, trControl=train_control, method="lmt")
# make predictions
ctree2_predictions <- predict(ctree2_model, imbal_train[, -ncol(imbal_train)])
# summarize results
ctree2_cf <- confusionMatrix(ctree2_predictions, imbal_train$Class)
print (ctree2_cf$table) # confusion matrix as a table
print (ctree2_cf$byClass) # estatístics as a matrix
print (ctree2_cf$overall) # acuracy as a numeric vector


# K NEIGHBORS MODEL
knn_model <- train(Class~., data=imbal_train, 
               k = 5,
               metric = "ROC",
               trControl=train_control, method="knn")
#lmt_model <- train(Class~., data=iris, trControl=train_control, method="lmt")
# make predictions
knn_predictions <- predict(knn_model, imbal_train[, -ncol(imbal_train)])
# summarize results
knn_cf <- confusionMatrix(knn_predictions, imbal_train$Class)
print (knn_cf$table) # confusion matrix as a table
print (knn_cf$byClass) # estatístics as a matrix
print (knn_cf$overall) # acuracy as a numeric vector

# BAYESIAN GENERALIZING LINEAR MODEL
bglm_model <- train(Class~., data=imbal_train, 
                   #nbagg = 50,
                   metric = "ROC",
                   trControl=train_control, method="bayesglm")
#lmt_model <- train(Class~., data=iris, trControl=train_control, method="lmt")
# make predictions
bglm_predictions <- predict(bglm_model, imbal_train[, -ncol(imbal_train)])
# summarize results
bglm_cf <- confusionMatrix(bglm_predictions, imbal_train$Class)
print (bglm_cf$table) # confusion matrix as a table
print (bglm_cf$byClass) # estatístics as a matrix
print (bglm_cf$overall) # acuracy as a numeric vector

# GENERALIZING LINEAR MODEL
glm_model <- train(Class~., data=imbal_train, 
                    #nbagg = 50,
                    metric = "ROC",
                    trControl=train_control, method="glm")
#lmt_model <- train(Class~., data=iris, trControl=train_control, method="lmt")
# make predictions
glm_predictions <- predict(glm_model, imbal_train[, -ncol(imbal_train)])
# summarize results
glm_cf <- confusionMatrix(glm_predictions, imbal_train$Class)
print (glm_cf$table) # confusion matrix as a table
print (glm_cf$byClass) # estatístics as a matrix
print (glm_cf$overall) # acuracy as a numeric vector

# BOOSTED LOGISTIC REGRESSION MODEL
model <- train(Class~., data=imbal_train, 
                   #nbagg = 50,
                   metric = "ROC",
                   trControl=train_control, method="LogitBoost")
#lmt_model <- train(Class~., data=iris, trControl=train_control, method="lmt")
# make predictions
predictions <- predict(model, imbal_train[, -ncol(imbal_train)])
# summarize results
logit_cf <- confusionMatrix(predictions, imbal_train$Class)
print (logit_cf$table) # confusion matrix as a table
print (logit_cf$byClass) # estatístics as a matrix
print (logit_cf$overall) # acuracy as a numeric vector

# SVM WITH FORWARD SELECTION MODEL
# PAREI AQUI
model <- train(Class~., data=imbal_train, 
               #nbagg = 50,
               metric = "ROC",
               trControl=train_control, method="svmRadialWeights")
#lmt_model <- train(Class~., data=iris, trControl=train_control, method="lmt")
# make predictions
predictions <- predict(model, imbal_train[, -ncol(imbal_train)])
# summarize results
svm_cf <- confusionMatrix(predictions, imbal_train$Class)
print (svm_cf$table) # confusion matrix as a table
print (svm_cf$byClass) # estatístics as a matrix
print (svm_cf$overall) # acuracy as a numeric vector



# aqui prepara os dados pra trabalhar com REGRESSAO!! (TARGET NUMERICO)
# CLASSIFICATION MODELS

# LINEAR REGRESSION WITH FORWARD SELECTION MODEL
# testar com dados numericos somente!!!!
model <- train(Class~., data=imbal_train, 
               #nbagg = 50,
               metric = "ROC",
               trControl=train_control, method="lmStepAIC")
#lmt_model <- train(Class~., data=iris, trControl=train_control, method="lmt")
# make predictions
predictions <- predict(model, imbal_train[, -ncol(imbal_train)])
# summarize results
logit_cf <- confusionMatrix(predictions, imbal_train$Class)
print (logit_cf$table) # confusion matrix as a table
print (logit_cf$byClass) # estatístics as a matrix
print (logit_cf$overall) # acuracy as a numeric vector

# AGRUPANDO OS MODELOS PARA ANALISAR a ROC Curve e definir o melhor
# obs. depois acrescentar o exemplo do livro, onde tem um budget
# fixo e precisa pegar número de dados que se encaixa no mesmo
# inserir também a lift curve aqui
#-------------------------------------------------

#  <COLOCAR AQUI SCRIPT DE exp_subsampling_technique.R>

#------------------------------------------------

# COMPARANDO OS MODELOS via ROC
# e SELECIONANDO O MELHOR Cut POINT do modelo escolhido
# Usando Expected value e pacote OptimalCutpoint para
# calcular melhor cutt point in ROC curve
# a partir da matriz de custo x benefício calcula o
# valor do parâmetro costs.ratio;
# CR = (Cfp - Ctn) / (Cfn - Ctp), valores obtidos
# da matriz de custoxbenefício para o problema
# de negócio.
# Calcular para todos os modelos e selecionar aquele que tem
# o maior valor, usando o cutpoint com maior custo x benefício
# Selecionado o modelo e o cutpoint, usa esgte modelo para
# predição




# USANDO O MODELO PARA PREVISÃO
#-------------------------------------------

# usando exemplo do knn
# K NEIGHBORS MODEL
set.seed(1)
knnTune <- train (churn ~ .,
                  data = churnTrain,
                  method = "knn",
                  #metric = "ROC",
                  trControl = trainControl(method = "cv"))

# obtendo a previsão somente para os primeiros exemplos da base de teste
# notar que não pego a coluna 20, onde está o target, ams não faz diferença
# na saída da função chamada!
knnProbs <- predict(knnTune,churnTest[1:10,-20], type = "prob")
print (knnProbs)

# idem, abaixo tem a previsão pelo modelo das 10 primeiras ocorrências do arquivo
knnPred <- predict(knnTune,newdata = churnTest[1:10,-20]) 
print(knnPred) # imprime previsões do modelo para os 10 primeiros dados da amostra
print(churnTest$churn[1:10]) # imprime os 10 primeiros targets reais da amostra
table(knnPred,churnTest$churn[1:10]) # tabela de confusão dos 10 primeiros dados
confusionMatrix(knnPred,churnTest$churn[1:10])$table # idem por caret

# O Exemplo abaixo de uso de extractPredictions funciona, mas gerou erros
# quando tentei com os dados de churn??!!
knnFit <- train(Species ~ ., data = iris, method = "knn", 
                trControl = trainControl(method = "cv"))

rdaFit <- train(Species ~ ., data = iris, method = "rda", 
                trControl = trainControl(method = "cv"))

allModels <- list(knn = knnFit, rda = rdaFit)

extractPrediction(allModels, unkX = iris[1:10, -5])
