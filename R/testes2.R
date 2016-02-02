# análise alternativa FA, alpha de Cronbach e Ômega
library(foreign)
library(Rcmdr)
# Start by using the 'foreign' library to import 'Example Data 5.sav' (available 
# from the web page) assigning the name 'example5'. 

example5 <- read.spss("http://www.unt.edu/rss/class/Jon/R_SC/Module3/ExampleData5.sav", 
                      use.value.labels=FALSE, max.value.labels=Inf, to.data.frame=TRUE)
attach(example5)
nrow(example5)
names(example5)
# Remove missing data so the matrix/data set is made of complete observations.
ex5 <- data.frame(na.omit(example5[,c("b1","b2","b3","b4","b5","b6","b7","b8",
                                      "b9","b10","bt","ap1","ap2","ap3","ap4","ap5","ap6","ap7","ap8","ap9","ap10",
                                      "ap11","ap12","ap13","ap14","ap15","apt")]))
nrow(ex5)
detach(example5)
attach(ex5)
###############################################################################
# Before we begin with the analysis script; let's take a moment to address and 
# hopefully clarify one of the most confusing and misarticulated issues in 
# statistical teaching and practice literature. An ambitious goal to be sure.

# First, Principal Components Analysis (PCA) is a variable reduction technique which 
# maximizes the amount of variance accounted for in the observed variables by a 
# smaller group of variables called COMPONENTS. As an example, consider the 
# following situation. Let's say, we have 500 questions on a survey we designed 
# to measure stubbornness. We want to reduce the number of questions so that it 
# does not take someone 3 hours to complete the survey. It would be appropriate to 
# use PCA to reduce the number of questions by identifying and removing redundant 
# questions. For instance, if question 122 and question 356 are virtually identical 
# (i.e. they ask the exact same thing but in different ways), then one of them is 
# not necessary. The PCA process allows us to reduce the number of questions or 
# variables down to their PRINCIPAL COMPONENTS.

# PCA is commonly, but very confusingly, called exploratory factor analysis (EFA). 
# The use of the word factor in EFA is inappropriate and confusing because we are 
# really interested in COMPONENTS, not factors. This issue is made more confusing 
# by some software packages (e.g. PASW / SPSS, SAS) which list or use PCA under the 
# heading factor analysis.

# Second, Factor Analysis (FA) is typically used to confirm the latent factor 
# structure for a group of measured variables. Latent factors are unobserved 
# variables which typically can not be directly measured; but, they are assumed to 
# cause the scores we observe on the measured or indicator variables. FA is a model 
# based technique. It is concerned with modeling the relationships between measured 
# variables, latent factors, and error.

# As stated in O'Rourke, Hatcher, and Stepanski (2005): 
#     "Both (PCA & FA) are methods that can be used to identify groups of observed 
#      variables that tend to hang together empirically. Both procedures can also 
#      be performed with the SAS FACTOR procedure and they generally tend to provide 
#      similar results. Nonetheless, there are some important conceptual differences 
#      between principal component analysis and factor analysis that should be 
#      understood at the outset. Perhaps the most important deals with the assumption 
#      of an underlying causal structure. Factor analysis assumes that the covariation 
#      in the observed variables is due to the presence of one or more latent variables 
#      (factors) that exert causal influence on these observed variables" (p. 436).

# Final thoughts. Both PCA and FA can be used as exploratory analysis. But; PCA is 
# predominantly used in an exploratory fashion and almost never used in a confirmatory 
# fashion. FA can be used in an exploratory fashion, but most of the time it is used 
# in a confirmatory fashion because it is concerned with modeling factor structure. 
# The choice of which is used should be driven by the goals of the analyst. If you 
# are interested in reducing the observed variables down to their principal components 
# while maximizing the variance accounted for in the observed variables by the components, 
# then you should be using PCA. If you are concerned with modeling the latent factors 
# (and their relationships) which cause the scores on your observed variables, then 
# you should be using FA. 

#### REFERENCE####
# O'Rourke, N., Hatcher, L., & Stepanski, E.J. (2005). A step-by-step approach to using 
#     SAS for univariate and multivariate statistics, Second Edition. Cary, NC: SAS 
#     Institute Inc. 

##############################################################################

############ PRINCIPAL COMPONENT ANALYSIS ############

# Using PRINCIPAL COMPONENTS EXTRACTION 
# Commonly called Principal Components Analysis (PCA)

##### FIRST PCA using 'princomp' function (as would be done using menus in Rcmdr).

pca.1 <- princomp(~ap1+ap2+ap3+ap4+ap5+ap6+ap7+ap8+ap9+ap10+ap11+ap12+ap13+ap14+ap15+b1+b2+b3+b4+b5+b6+b7+b8+b9+b10, 
                  cor=TRUE, data=ex5, scores = TRUE)



unclass(loadings(pca.1))  # component loadings
pca.1$sd^2  # component variances
summary(pca.1) # proportions of variance
screeplot(pca.1) # if you would like a scree plot

# To extract the component scores use below, where the [,1] through [,nc] where
# 'nc' represents the number of components retained.

first.component.scores <- pca.1$scores[,1]
summary(first.component.scores)
length(first.component.scores)

second.component.scores <- pca.1$scores[,2]
summary(second.component.scores)
length(second.component.scores)

##### SECOND & THIRD PCA using library(psych) 'principal' function.
# Create a correlation matrix (also called a matrix of association) object 
# of the items for PCA.

cor.matrix.1 <- cor(ex5[,c("b1","b2","b3","b4","b5","b6","b7","b8","b9",
                           "b10","ap1","ap2","ap3","ap4","ap5","ap6","ap7","ap8","ap9","ap10","ap11",
                           "ap12","ap13","ap14","ap15")])
cor.matrix.1

library(psych)

# Script for PCA with VARIMAX rotation (an orthogonal rotation strategy).

pca.2 <- principal(r = cor.matrix.1, nfactors = 25, residuals = FALSE, rotate = "varimax")
pca.2

# Load library for using oblimin rotation.

library(GPArotation)

# Script for PCA with Direct Oblimin rotation (an oblique rotation strategy).

pca.3 <- principal(r = cor.matrix.1, nfactors = 25, residuals = FALSE, rotate = "oblimin")
pca.3

##### FOURTH & FIFTH PCA using Spearman correlations.
# New correlation matrix using Spearman correlation instead of default Pearson.
# Spearman if preferred when using Likert response style items, because the items 
# provide nominal or ordinal data rather than interval/ratio data which is required 
# for a Pearson correlation.

cor.matrix.2 <- cor(ex5[,c("b1","b2","b3","b4","b5","b6","b7","b8","b9",
                           "b10","ap1","ap2","ap3","ap4","ap5","ap6","ap7","ap8","ap9","ap10","ap11",
                           "ap12","ap13","ap14","ap15")], method = "spearman")
cor.matrix.2

# Script for PCA with VARIMAX rotation; with Spearman correlations.

pca.4 <- principal(r = cor.matrix.2, nfactors = 25, residuals = FALSE, rotate = "varimax")
pca.4

# Script for PCA with Direct Oblimin rotation; with Spearman correlations.

pca.5 <- principal(r = cor.matrix.2, nfactors = 25, residuals = FALSE, rotate = "oblimin")
pca.5

# Take a look at the residuals; first use the 'names' function to find the 
# the output part of the output we want (residuals; which are not displayed by
# default), then use the '$' convention to reference the residual values.

names(pca.4)

pca.4$residual

head(pca.4$residual)
tail(pca.4$residual)

summary(pca.4$residual)
hist(pca.4$residual)

############ FACTOR ANALYSIS ############

# Using maximum likelihood extraction with the 'factanal' function as would 
# be done through the menus of Rcmdr.

##### FIRST Factor Analysis (FA). 
# Specifying: rotation 'none' and factors '2'. Automatically suppresses 
# loadings less than 0.100 in the output.

fa.1 <- factanal(~ap1+ap2+ap3+ap4+ap5+ap6+ap7+ap8+ap9+ap10+ap11+ap12+ap13+ap14
                 +ap15+b1+b2+b3+b4+b5+b6+b7+b8+b9+b10,factors=2, 
                 rotation="none", scores="none", data=ex5)
fa.1

##### SECOND FA; specifying: rotation 'varimax' and factors '2'.

fa.2 <- factanal(~ap1+ap2+ap3+ap4+ap5+ap6+ap7+ap8+ap9+ap10+ap11+ap12+ap13+ap14
                 +ap15+b1+b2+b3+b4+b5+b6+b7+b8+b9+b10,factors=2, 
                 rotation="varimax", scores="none", data=ex5)
fa.2

##### THIRD FA; specifying: rotation 'oblimin' and factors '2'.

fa.3 <- factanal(~ap1+ap2+ap3+ap4+ap5+ap6+ap7+ap8+ap9+ap10+ap11+ap12+ap13+ap14
                 +ap15+b1+b2+b3+b4+b5+b6+b7+b8+b9+b10,factors=2, 
                 rotation="oblimin", scores="none", data=ex5)
fa.3

##### Saving Thompson's regression style factor scores from "fa.2" above. 

fa.4 <- factanal(~ap1+ap2+ap3+ap4+ap5+ap6+ap7+ap8+ap9+ap10+ap11+ap12+ap13+ap14
                 +ap15+b1+b2+b3+b4+b5+b6+b7+b8+b9+b10,factors=2, 
                 rotation="varimax", scores="regression", data=ex5)
fa.4

# Display just the factor scores, and assign them to an object for later use (if desired).

fa.4$scores
fa4scores <- fa.4$scores

# Display the loadings without suppression. 

print(fa.4,digits = 3, cutoff = .000001)

##### Using the psych package (and GPArotation) to get the best choice of factor scores.

library(psych)
library(GPArotation)  # <-- Required for using 'oblimin' rotation.

# Taking the variables of interest and putting them into a subset data frame.

names(ex5)
ss.df <- ex5[,-c(11,27)]  # <-- removing the two 'total score' variables.
names(ss.df)

# According to Revelle (see: http://www.personality-project.org/), the "tenBerge"
# method of calculating factor scores is the best choice for oblique rotation
# solutions and the "Anderson" method is the best choice for orthogonal rotation
# solutions (it insures that the factors scores will be orthogonal).

fa.5 <- fa(r = ss.df, nfactors = 2, rotate = "oblimin", scores = "tenBerge", fm = "ml")
fa.5

head(fa.5$scores, 25)

fa.6 <- fa(r = ss.df, nfactors = 2, rotate = "varimax", scores = "Anderson", fm = "ml")
fa.6

head(fa.6$scores, 25)

help(fa)

help(factor.scores)

##### Using polycor/hetcor 

library(polycor)
# Create the correlation matrix.

h.cor <- hetcor(ex5)$cor
h.cor

# Run the factor analysis using the (hetergeneous) correlation matrix. 

fa.5 <- factanal(covmat = h.cor, factors=2, rotation="varimax", scores="none")
fa.5

############ Total Scale Score Correlations ############
# Checking the correlation between each scale's total score.

cor(bt,apt)
cor(bt,apt,method="spearman")

# With correlations so low (r = -0.04); it would be more appropriate to use 
# an orthogonal rotation strategy (such as Varimax) rather than an oblique
# rotation strategy (such as Direct Oblimin).

##############################################################################

############ Scale Reliability/Internal Consistency ############
# Keep in mind, Cronbach's Alpha coefficient is one of those statistics that 
# just seems to continue hanging around long after it is recognized as being 
# not terribly useful. A much better metric for conveying the reliability is
# the omega coefficient (covered below). 

# Reliability coefficient (Cronbach's Alpha) for the AP scale.
library(Rcmdr)

reliability(cov(ex5[,c("ap1","ap2","ap3","ap4","ap5","ap6","ap7","ap8",
                       "ap9","ap10","ap11","ap12","ap13","ap14","ap15")], use="complete.obs"))

# Reliability coefficient (Cronbach's Alpha) for the B scale.

reliability(cov(ex5[,c("b1","b2","b3","b4","b5","b6","b7","b8","b9","b10")],
                use="complete.obs"))

### Another (easier) way to get Cronbach's Alpha (here on all the items & total scores together):

library(psy)
cronbach(ex5)

###### USE OMEGA RATHER THAN ALPHA.

# The omega coefficient is a much better estimate than alpha for estimating
# the reliability of a scale. For a brief discussion of this issue, please
# read the RSS Matters article from June 2012.
# http://web3.unt.edu/benchmarks/issues/2012/06/rss-matters
# Using a different data set and the 'psych' package, we can easily compute the
# omega coefficient.

data.df <- read.table("http://www.unt.edu/rss/class/Jon/R_SC/Module7/bifactor_data.txt",
                      header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
summary(data.df)
nrow(data.df)
sem.cor <- cor(data.df)
o1 <- omega(sem.cor, nfactors = 3, n.iter = 1, fm = "ml", poly = F, digits = 3,
            n.obs = 5000, rotate = "oblimin")
o1

help(omega)


# APLICANDO OMEGA ACIMA NOS DADOS DE NOSSA ANALISE (FALTA REFAZER USANDO FA) E RODAR DE NOVO
HG.cor <- cor(df_change[,4:11])
HG.o1 <- omega(HG.cor, nfactors = 3, n.iter = 1, fm = "ml", poly = F, digits = 3,
               n.obs = 76545, rotate = "oblimin")
HG.o1
###############################################################################
# As is the case with most things in R; there are a great number of ways to do 
# factor analysis. So, please do not consider this script an exhaustive review 
# of factor analytic methods available in R.
# 
# For those interested in moving beyond Classical Test Theory construction and 
# analysis; you can research Item Response Theory (IRT) analysis in R using 
# the packages/libraries 'irtProb' & 'irtoys'. See http://cran.r-project.org/
# and then 'Packages' to find documentation on those two packages/libraries.





#+++++++++++++++++++++++++++++++
# REPETINDO TUDO ACIMA PARA HG
#+++++++++++++++++++++++++++++++

############ PRINCIPAL COMPONENT ANALYSIS ############

# Using PRINCIPAL COMPONENTS EXTRACTION 
# Commonly called Principal Components Analysis (PCA)

##### FIRST PCA using 'princomp' function (as would be done using menus in Rcmdr).

#pca1.1 = prcomp(df_tidy_hg[,7:14], scale. = TRUE, center = TRUE, score.alpha = TRUE, cor=TRUE) #HG
# obs: uso acima dá os mesmo loadings (rotations que os obtidos com pricomp!!)
# MAS COM SINAIS INVERTIDOS!!!!! PORQUE????
#unclass(pca1.1$rotation)  # component loadings

pca1.2 <- princomp(~sensibility+power+quality+exposure+structure+imagination+stability+contacts, 
                  cor=TRUE, data=df_tidy_hg, scores = TRUE)

unclass(loadings(pca1.2))  # component loadings
pca1.2$sd^2  # component variances
summary(pca1.2) # proportions of variance
screeplot(pca1.2) # if you would like a scree plot

# To extract the component scores use below, where the [,1] through [,nc] where
# 'nc' represents the number of components retained.

first.component.scores <- pca1.2$scores[,1]
summary(first.component.scores)
length(first.component.scores)

second.component.scores <- pca1.2$scores[,2]
summary(second.component.scores)
length(second.component.scores)

##### SECOND & THIRD PCA using library(psych) 'principal' function.
# Create a correlation matrix (also called a matrix of association) object 
# of the items for PCA.

cor.matrix.1 <- cor(df_tidy_hg[,c("sensibility","power","quality","exposure",
                                  "structure","imagination","stability","contacts")])
cor.matrix.1
library("corrplot")
corrplot.mixed(cor.matrix.1, insig = "p-value", sig.level = -1, is.corr = TRUE)

library(psych)

# Script for PCA with VARIMAX rotation (an orthogonal rotation strategy).

pca.2 <- principal(r = cor.matrix.1, nfactors = 8, residuals = FALSE, rotate = "varimax")
pca.2

# Load library for using oblimin rotation.

library(GPArotation)

# Script for PCA with Direct Oblimin rotation (an oblique rotation strategy).

pca.3 <- principal(r = cor.matrix.1, nfactors = 8, residuals = FALSE, rotate = "oblimin")
pca.3

##### FOURTH & FIFTH PCA using Spearman correlations.
# New correlation matrix using Spearman correlation instead of default Pearson.
# Spearman if preferred when using Likert response style items, because the items 
# provide nominal or ordinal data rather than interval/ratio data which is required 
# for a Pearson correlation.

cor.matrix.2 <- cor(df_tidy_hg[,c("sensibility","power","quality","exposure",
                                  "structure","imagination","stability","contacts")], method = "spearman")
cor.matrix.2

# Script for PCA with VARIMAX rotation; with Spearman correlations.

pca.4 <- principal(r = cor.matrix.2, nfactors = 8, residuals = FALSE, rotate = "varimax")
pca.4

# Script for PCA with Direct Oblimin rotation; with Spearman correlations.

pca.5 <- principal(r = cor.matrix.2, nfactors = 8, residuals = FALSE, rotate = "oblimin")
pca.5

# Take a look at the residuals; first use the 'names' function to find the 
# the output part of the output we want (residuals; which are not displayed by
# default), then use the '$' convention to reference the residual values.

names(pca.4)

pca.4$residual

head(pca.4$residual)
tail(pca.4$residual)

summary(pca.4$residual)
hist(pca.4$residual)

############ FACTOR ANALYSIS ############

# Using maximum likelihood extraction with the 'factanal' function as would 
# be done through the menus of Rcmdr.

##### FIRST Factor Analysis (FA). 
# Specifying: rotation 'none' and factors '2'. Automatically suppresses 
# loadings less than 0.100 in the output.

fa.1 <- factanal(~sensibility+power+quality+exposure+structure+imagination+stability+contacts,factors=4, 
                 rotation="none", scores="none", start=c(1, 1, 1, 1, 1, 1, 1, 1), data=df_tidy_hg)
fa.1

##### SECOND FA; specifying: rotation 'varimax' and factors '2'.

fa.2 <- factanal(~sensibility+power+quality+exposure+structure+imagination+stability+contacts,factors=4, 
                 rotation="varimax", scores="regression", start=c(1, 1, 1, 1, 1, 1, 1, 1), data=df_tidy_hg)
fa.2
load <- fa.2$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(df_tidy_hg[,7:14]),cex=.7) # add variable names
##### THIRD FA; specifying: rotation 'oblimin' and factors '2'.

fa.3 <- factanal(~sensibility+power+quality+exposure+structure+imagination+stability+contacts,factors=4, 
                 rotation="oblimin", scores="none", start=c(1, 1, 1, 1, 1, 1, 1, 1), data=df_tidy_hg)
fa.3

##### Saving Thompson's regression style factor scores from "fa.2" above. 

fa.4 <- factanal(~sensibility+power+quality+exposure+structure+imagination+stability+contacts,factors=2, 
                 rotation="varimax", scores="regression", start=c(1, 1, 1, 1, 1, 1, 1, 1), data=df_tidy_hg)
fa.4

FA <- 
    factanal(~contacts+exposure+imagination+power+quality+sensibility+stability+structure,
             factors=4, rotation="varimax", scores="none", data=df_tidy_hg)
print(FA)
# Display just the factor scores, and assign them to an object for later use (if desired).

fa.4$scores
fa4scores <- fa.4$scores

# Display the loadings without suppression. 

print(fa.4,digits = 3, cutoff = .000001)

##### Using the psych package (and GPArotation) to get the best choice of factor scores.

library(psych)
library(GPArotation)  # <-- Required for using 'oblimin' rotation.

# Taking the variables of interest and putting them into a subset data frame.

names(df_tidy_hg)
ss.df <- df_tidy_hg[,-c(1:6)]  # <-- removing the two 'total score' variables.
names(ss.df)

# According to Revelle (see: http://www.personality-project.org/), the "tenBerge"
# method of calculating factor scores is the best choice for oblique rotation
# solutions and the "Anderson" method is the best choice for orthogonal rotation
# solutions (it insures that the factors scores will be orthogonal).

fa.5 <- fa(r = ss.df, nfactors = 2, rotate = "oblimin", scores = "tenBerge", SMC=FALSE, fm="minres")
fa.5

head(fa.5$scores, 25)

fa.6 <- fa(r = ss.df, nfactors = 2, rotate = "varimax", scores = "Anderson", fm = "ml")
fa.6

head(fa.6$scores, 25)

help(fa)

help(factor.scores)

##### Using polycor/hetcor 

library(polycor)
# Create the correlation matrix.

h.cor <- hetcor(df_tidy_hg)$cor
h.cor

# Run the factor analysis using the (hetergeneous) correlation matrix. 

fa.5 <- factanal(covmat = h.cor, factors=2, rotation="varimax", scores="none")
fa.5

############ Total Scale Score Correlations ############
# Checking the correlation between each scale's total score.

cor(bt,apt)
cor(bt,apt,method="spearman")

# With correlations so low (r = -0.04); it would be more appropriate to use 
# an orthogonal rotation strategy (such as Varimax) rather than an oblique
# rotation strategy (such as Direct Oblimin).

##############################################################################

############ Scale Reliability/Internal Consistency ############
# Keep in mind, Cronbach's Alpha coefficient is one of those statistics that 
# just seems to continue hanging around long after it is recognized as being 
# not terribly useful. A much better metric for conveying the reliability is
# the omega coefficient (covered below). 

# Reliability coefficient (Cronbach's Alpha) for the AP scale.
library(Rcmdr)


reliability(cor(df_tidy_hg[,c("sensibility","power","quality","exposure",
                              "structure","imagination","stability","contacts")], use="complete.obs"))

reliability(cor(df_tidy_hg[,c("sensibility","power","quality")], use="complete.obs"))
reliability(HG.cor)

# abaixo, não sei se faz sentido aplicar nos componentes!
#reliability(cov(df_change[,c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8")], use="complete.obs"))

### Another (easier) way to get Cronbach's Alpha (here on all the items & total scores together):

library(psy)
cronbach(df_tidy_hg[,c("sensibility","power","quality","exposure",
                       "structure","imagination","stability","contacts")])
# abaixo, não sei se faz sentido aplicar nos componentes!
#cronbach(df_change[,4:11])

###### USE OMEGA RATHER THAN ALPHA.

# The omega coefficient is a much better estimate than alpha for estimating
# the reliability of a scale. For a brief discussion of this issue, please
# read the RSS Matters article from June 2012.
# http://web3.unt.edu/benchmarks/issues/2012/06/rss-matters
# Using a different data set and the 'psych' package, we can easily compute the
# omega coefficient.

data.df <- read.table("http://www.unt.edu/rss/class/Jon/R_SC/Module7/bifactor_data.txt",
                      header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
summary(data.df)
nrow(data.df)
sem.cor <- cor(data.df)
o1 <- omega(sem.cor, nfactors = 3, n.iter = 1, fm = "ml", poly = F, digits = 3,
            n.obs = 5000, rotate = "oblimin")
o1

help(omega)


# APLICANDO OMEGA ACIMA NOS DADOS DE NOSSA ANALISE (FALTA REFAZER USANDO FA) E RODAR DE NOVO
summary(df_change[,4:11])
HG.cor <- cor(df_change[,4:11])
HG.o1 <- omega(HG.cor, nfactors = 3, n.iter = 1, fm = "ml", poly = F, digits = 3,
               n.obs = 76545, rotate = "oblimin")
# tentar acima com viramax
HG.o1
###############################################################################
# As is the case with most things in R; there are a great number of ways to do 
# factor analysis. So, please do not consider this script an exhaustive review 
# of factor analytic methods available in R.
# 
# For those interested in moving beyond Classical Test Theory construction and 
# analysis; you can research Item Response Theory (IRT) analysis in R using 
# the packages/libraries 'irtProb' & 'irtoys'. See http://cran.r-project.org/
# and then 'Packages' to find documentation on those two packages/libraries.


# Using package coefficientalpha
#+++++++++++++++++++++++++++++++++++++
# alpha de Cronbach
library(coefficientalpha)
alpha(df_tidy_hg[,7:14], varphi = 0.1, se = FALSE, complete =FALSE)
# omega de Mc'donalds
omega(df_tidy_hg[,7:14], varphi = 0.1, se = FALSE, 
      complete =FALSE)

# teste
df1 <- data.frame(a = c(1,1,1,1), b = c(1,1,1,1), c = c(1,1,1,1))
df2 <- data.frame(a = c(1,2,3,4), b = c(9,10,11,12), c = c(4,3,2,1))
reliability(cor(df2, use="complete.obs"))
QUEST <- data.frame(
    Q1=c(1,5,2,3,4,2,3,4,3,2), 
    Q2=c(2,4,1,2,4,1,2,5,2,1), 
    Q3=c(2,5,1,3,3,2,2,4,2,2))
reliability(cor(QUEST, use="complete.obs"))
plot(QUEST)
cor1 <- cor(QUEST)

library("corrplot")
corrplot.mixed(cor1, insig = "p-value", sig.level = -1, is.corr = TRUE)
alpha(QUEST, varphi = .01)
library(fmsb)
CronbachAlpha(QUEST)
cor2 <- cor(df_tidy_hg[,7:14])
reliability(cor2)

cronbach(df_tidy_hg[,7:14])
CronbachAlpha(cor(df_tidy_hg[,7:14]))

#+++++++++++++++++++++++++++
# clusters para fatores
r2 <- cor(df_tidy_hg[,7:14]) # correlacão entre os fatores
k2 <- kmeans(r2, centers = 2)
print(k2,digits=2)

# com 2 clusters, identificados: 
#    power, exposure, imagination, contacts
#    sensibility, quality, structure, stability
#    
# agora aplicar alpha
cor3 <- cor(df_tidy_hg[,c(7,9,11,13)])
reliability(cor3)
cor4 <- cor(df_tidy_hg[,c(8,10,12,14)])
reliability(cor4)
cor5 <- cor(df_tidy_hg[,7:14])
reliability(cor5)

#library(cluster)
#library(fpc)
#plotcluster(r2, k2$cluster)

#library(psych)
#keys2 <- cluster2keys(k2)
#k4 <- kmeans(r2,4)
#keys4 <- cluster2keys(k4)
#keys24 <- cbind(keys2,keys4)
#colnames(keys24) <- c("C2.1","C2.2","C4.1","C4.2","C4.3", "C4.4")
#cluster.cor(keys24,r2)

#+++++++++++++++++++++++++++
# clusters para componentes
r2 <- cor(scores.total) # correlacão entre os fatores
k2 <- kmeans(r2, centers = 3)
print(k2,digits=2)

# com 2 clusters, identificados: 
#    power, exposure, imagination, contacts
#    sensibility, quality, structure, stability
#    
# agora aplicar alpha
cor3 <- cor(scores.total[,c("PC1","PC2","PC3","PC6")])
reliability(cor3)
cor4 <- cor(scores.total[,c("PC5","PC7","PC8")]) # parece ser o maior alpha!!
reliability(cor4)
cor5 <- cor(scores.total)
reliability(cor5)


# ++++++++++++++++++
# Wiks Lambda Test
X <- structure(c(9, 6, 9, 3, 2, 7), .Dim = as.integer(c(3, 2)))
Y <- structure(c(0, 2, 4, 0), .Dim = as.integer(c(2, 2)))
Z <- structure(c(3, 1, 2, 8, 9, 7), .Dim = as.integer(c(3, 2)))
U <- rbind(X,Y,Z)
# abaixo: 
# formula indicando
f <- factor(rep(1:3, c(3, 2, 3)))
#m <- manova(U~factor(rep(1:3, c(3, 2, 3))))
# aqui, U representa os valores independentes e f os valores que identificam os grupos
m <- manova(U~f)
summary(m,test="Wilks")

#+++++++++++
# com HG
#+++++++++++

f_acentos <- function(df_in) { # funcao que tira os acentos e deixa tudo em minuscula
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
df_tidy_hg <- f_acentos(df_tidy_hg)
# restaura fatores como numericos
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

# obtendo os scores previstos de acordo com a análise de componentes principais
pca1 = prcomp(df_tidy_hg[,7:14], scale. = TRUE, center = TRUE)
# scores obtidos
scores.total <- as.data.frame(pca1$x)
my.scores.total <- as.data.frame(cbind(ID = df_tidy_hg$ID, 
                                       sexo = df_tidy_hg$sexo, 
                                       tipouser = df_tidy_hg$TIPOUSER, 
                                       profissao.na.area.de = df_tidy_hg$profissao.na.area.de, 
                                       formacao.em = df_tidy_hg$formacao.em,
                                       scores.total))
my.scores.total <-
    my.scores.total %>%
    mutate(tipouser = ifelse(tipouser == "", "indefinido", as.character(tipouser)),
           sexo = ifelse(sexo == "f", "feminino","masculino"))
# SEXO
my.scores.sex <- 
    my.scores.total %>%
    select(sexo,PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8) %>%
    mutate(sexo = as.factor(sexo))
#anova(lm(U~factor(rep(1:3, c(3, 2, 3)))), test="Wilks")
#m <- manova(my.scores.sex~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8)
# aqui tem a media em relacao a todos os ocmponentes
library(rrcov)
Wilks.test(sexo~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8,data=my.scores.sex)
# consigo a média em relação a cada um separado???
library(rrcov)
x <- my.scores.sex %>% select(sexo,PC1,PC2)
Wilks.test(sexo~.,data=x)
y <- my.scores.sex %>% select(sexo,PC3,PC4)
Wilks.test(sexo~.,data=y)
z <- my.scores.sex %>% select(sexo,PC5,PC6)
Wilks.test(sexo~.,data=z)
w <- my.scores.sex %>% select(sexo,PC7,PC8)
Wilks.test(sexo~.,data=w)
#Wilks.test(sexo~.,data=my.scores.sex)

# TIPOUSER
my.scores.tipouser <- 
    my.scores.total %>%
    select(tipouser,PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8) %>%
    mutate(tipouser = as.factor(tipouser))
#anova(lm(U~factor(rep(1:3, c(3, 2, 3)))), test="Wilks")
#m <- manova(my.scores.sex~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8)

# o teste abaixo mostra que as médias dos respondentes agrupados por tipo de usuário
# têm diferença significativa em relação aos componentes considerados
library(rrcov)
Wilks.test(tipouser~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8,data=my.scores.tipouser)
Wilks.test(tipouser~.,data=my.scores.tipouser)

# exemplo
data(hemophilia)
hemophilia$gr <- as.factor(hemophilia$gr)
Wilks.test(gr~., data=hemophilia, method="c")
#Wilks.test(gr~., data=hemophilia, method="rank")

# teste de anova
# preparando os dados
my.smp.PC1.k <-
    my.scores.tipouser %>%
    select(tipouser,PC1) %>%
    filter(tipouser == "kroton") %>%
    sample_n(15)
my.smp.PC1.a <-
    my.scores.tipouser %>%
    select(tipouser,PC1) %>%
    filter(tipouser == "ambev") %>%
    sample_n(15)
my.smp.PC1.f <-
    my.scores.tipouser %>%
    select(tipouser,PC1) %>%
    filter(tipouser == "func publico") %>%
    sample_n(15)

# montando o dataframe
library(psych)
# abaixo acho que não faz sentido!!
#my.smp.PC1 <- data.frame(ambev = my.smp.PC1.a[,"PC1"],
#                         func.publico = my.smp.PC1.f[,"PC1"],
#                         kroton = my.smp.PC1.k[,"PC1"])

fivenum(my.smp.PC1$ambev) #Tukey fivenumbers min, lowerhinge, median, upper hinge, max
mad(my.smp.PC1$ambev) #(median absolute deviation)
var(my.smp.PC1$ambev)     #produces the variance covariance matrix
sd(my.smp.PC1$ambev)      #standard deviation
describe(my.smp.PC1)
epiz <- scale(my.smp.PC1)          #centers (around the mean) and scales by the sd (sd = 1)
epic <- scale(my.smp.PC1,scale=FALSE) #centers but does not Scale (sd remain as they were)
describe(epiz)
describe(epic)
sd(my.smp.PC1$ambev)
stem(my.smp.PC1$ambev)       #stem and leaf diagram
round(cor(my.smp.PC1),2) # correlation matrix with values rounded to 2 decimals
corr.test(my.smp.PC1)


# exemplo de uso de anova em R abaixo
# ONE WAY ANALYSIS
#+++++++++++++++++++++++
datafilename="http://personality-project.org/r/datasets/R.appendix1.data"
data.ex1=read.table(datafilename,header=T)   #read the data into a table
aov.ex1 = aov(Alertness~Dosage,data=data.ex1)  #do the analysis of variance
# obs: comando summary abaixo mostra na primeira linha os valores between group
# enquanto a segunda linha mostra statisticas within-group
summary(aov.ex1)                                    #show the summary table

print(model.tables(aov.ex1,"means"),digits=3) #report the means and the number of subjects/cell

boxplot(Alertness~Dosage,data=data.ex1) #graphical summary appears in graphics window

# TWO WAY ANALYSIS
#+++++++++++++++++++++++
datafilename="http://personality-project.org/r/datasets/R.appendix2.data"
data.ex2=read.table(datafilename,header=T)   #read the data into a table
data.ex2                                      #show the data
# obs: usar * abaixo se correlação entre variaveis independentes é importante, usar + caso contrario
aov.ex2 = aov(Alertness~Gender*Dosage,data=data.ex2)         #do the analysis of variance
summary(aov.ex2)
print(model.tables(aov.ex2,"means"),digits=3)       #report the means and the number of subjects/cell
boxplot(Alertness~Dosage*Gender,data=data.ex2) #graphical summary of means of the 4 cells

aov.ex2 = aov(Alertness~Gender+Dosage,data=data.ex2)         #do the analysis of variance
summary(aov.ex2)
# aplicado a HG
#++++++++++++++++++++
# ONE WAY ANALYSIS
#+++++++++++++++++++++++
# a analise abaixo identifica se os grupos definidos em relação aos tipos de usuários 
# apresentam diferença nos seus scores médios em relação ao primeiro componente(PC1)
# os resultados de aplicação de anova analysis mostram que sim.
# repetir abaixo para sexo, ext, para todos os componentes e gerar tabela
# em seguida, rodar o Wilks para todos os grupos mas para todos os PCs tb
my.aov.tipouser <-
    my.scores.tipouser %>%
    filter(!(tipouser == "indefinido"))
    #select(tipouser,PC1) 
    #filter(tipouser == "func publico") %>%
    #sample_n(15)
# AOV
# aplica analise de variancia para cada componente
aov.tipouser.PC1 = aov(PC1~tipouser,data=my.aov.tipouser)  #do the analysis of variance    
summary(aov.tipouser.PC1)
print(model.tables(aov.tipouser.PC1,"means"),digits=3)
boxplot(PC1~tipouser,data=my.aov.tipouser) #graphical summary appears in graphics window

aov.tipouser.PC2 = aov(PC2~tipouser,data=my.aov.tipouser)  #do the analysis of variance    
summary(aov.tipouser.PC2)
aov.tipouser.PC3 = aov(PC3~tipouser,data=my.aov.tipouser)  #do the analysis of variance    
summary(aov.tipouser.PC3)
aov.tipouser.PC4 = aov(PC4~tipouser,data=my.aov.tipouser)  #do the analysis of variance    
summary(aov.tipouser.PC4)
aov.tipouser.PC1 = aov(PC1~tipouser,data=my.aov.tipouser)  #do the analysis of variance    
summary(aov.tipouser.PC1)
aov.tipouser.PC5 = aov(PC5~tipouser,data=my.aov.tipouser)  #do the analysis of variance    
summary(aov.tipouser.PC5)
aov.tipouser.PC6 = aov(PC6~tipouser,data=my.aov.tipouser)  #do the analysis of variance    
summary(aov.tipouser.PC6)
aov.tipouser.PC7 = aov(PC7~tipouser,data=my.aov.tipouser)  #do the analysis of variance    
summary(aov.tipouser.PC7)
aov.tipouser.PC8 = aov(PC8~tipouser,data=my.aov.tipouser)  #do the analysis of variance    
summary(aov.tipouser.PC8)#show the summary table
# WILKS
grp <- as.factor(my.aov.tipouser$tipouser)
x <- as.matrix(my.aov.tipouser[,2:9])
Wilks.test(x, grouping=grp, method="c")


# USAR ACIM PARA CONSIDERAÇÕES FINAIS (TANTO AOV quanto Wilks)

# TWO WAY ANALYSIS
#+++++++++++++++++++++++
my.twoway <-
    my.scores.total %>%
    filter(!(tipouser == "indefinido")) %>%
    select(tipouser,sexo,PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8) %>%
    mutate(tipouser = as.factor(tipouser),
           sexo = as.factor(sexo))

aov.twoway.PC1 = aov(PC1~tipouser*sexo,data=my.twoway)  #do the analysis of variance    
summary(aov.twoway.PC1)
print(model.tables(aov.twoway.PC1,"means"),digits=3)
boxplot(PC1~tipouser*sexo,data=my.twoway) #graphical summary appears in graphics window

# USING MANOVA
#+++++++++++++++++++++++++
Wilks.test(tipouser~., data=my.aov.tipouser, method="c")
Wilks.test(tipouser~., grouping = tipouser, data=my.aov.tipouser, method="c")
# explo
library(MASS)
data(anorexia)
attach(anorexia)
grp <- as.factor(anorexia[,1])
x <- as.matrix(anorexia[,2:3])
Wilks.test(x, grouping=grp, method="c")
# agora aplicar para HG
# acredito que precise mudar as linhas por colunas (transpose) para comparar os grupos
# ou seja, grupos ficam nas colunas
grp <- as.factor(my.aov.tipouser$tipouser)
x <- as.matrix(my.aov.tipouser[,2:9])
Wilks.test(x, grouping=grp, method="c")


# TESTAR: manova usando uma coluna para cada média de PC1 para cada
# FUNCIONOU!!!
m <- manova(cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8) ~ as.factor(tipouser), data = my.aov.tipouser)
summary(m, test = "Wilks")
# anova
m <- anova(lm(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8 ~ tipouser,data = my.aov.tipouser), )
summary(m)

# TESTE DE AOV: categorizar PC1 em positivo e negativo!!!
# repetir o memso para multiplo (ex. sexo e PC1)
df.PC1 <-
    my.scores.sex %>%
    mutate(PC1tipo = ifelse(PC1 >=0,"POS","NEG"))
df.PC1 <-
    df.PC1 %>%
    select(-sexo)

aov.PC1 = aov(PC1~PC1tipo,data=df.PC1)  #do the analysis of variance    
summary(aov.PC1)
# agora aplicando manova para tipouser + PC1tipo
my.twoway.PC1 <-
    my.scores.total %>%
    filter(!(tipouser == "indefinido")) %>%
    mutate(tipouser = as.factor(tipouser),
           PC1tipo = ifelse(PC1 >=0,"POS","NEG")) %>%
    select(-sexo, -ID, -profissao.na.area.de, -formacao.em)

aov.twoway.PC1 = aov(PC1~tipouser+PC1tipo,data=my.twoway.PC1)  #do the analysis of variance    
summary(aov.twoway.PC1)
