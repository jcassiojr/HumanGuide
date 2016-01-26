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



# REPETINDO TUDO ACIMA PARA HG
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

fa.1 <- factanal(~sensibility+power+quality+exposure+structure+imagination+stability+contacts,factors=2, 
                 rotation="none", scores="none", start=c(1, 1, 1, 1, 1, 1, 1, 1), data=df_tidy_hg)
fa.1

##### SECOND FA; specifying: rotation 'varimax' and factors '2'.

fa.2 <- factanal(~sensibility+power+quality+exposure+structure+imagination+stability+contacts,factors=2, 
                 rotation="varimax", scores="none", start=c(1, 1, 1, 1, 1, 1, 1, 1), data=df_tidy_hg)
fa.2

##### THIRD FA; specifying: rotation 'oblimin' and factors '2'.

fa.3 <- factanal(~sensibility+power+quality+exposure+structure+imagination+stability+contacts,factors=2, 
                 rotation="oblimin", scores="none", start=c(1, 1, 1, 1, 1, 1, 1, 1), data=df_tidy_hg)
fa.3

##### Saving Thompson's regression style factor scores from "fa.2" above. 

fa.4 <- factanal(~sensibility+power+quality+exposure+structure+imagination+stability+contacts,factors=2, 
                 rotation="varimax", scores="regression", start=c(1, 1, 1, 1, 1, 1, 1, 1), data=df_tidy_hg)
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

reliability(cov(df_tidy_hg[,c("sensibility","power","quality","exposure",
                              "structure","imagination","stability","contacts")], use="complete.obs"))

# abaixo, não sei se faz sentido aplicar nos componentes!
reliability(cov(df_change[,c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8")], use="complete.obs"))

### Another (easier) way to get Cronbach's Alpha (here on all the items & total scores together):

library(psy)
cronbach(df_tidy_hg[,c("sensibility","power","quality","exposure",
                       "structure","imagination","stability","contacts")])
# abaixo, não sei se faz sentido aplicar nos componentes!
cronbach(df_change[,4:11])

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



