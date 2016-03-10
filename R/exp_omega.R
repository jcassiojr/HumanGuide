# This R script implements the example from Dunn, Baguley & Brunsden (2013, in press)

# these examples assume the data files are in a folder "omega example" on your desktop

# set the working directory to the folder you are using on a PC
#setwd("./omega example")

# set the working directory to the folder you are using on a Mac
setwd("./omega example")

# alternatively, set the working directory on Mac, Linux or PC with this generic command
#setwd(file.path(Sys.getenv("HOME"), "Desktop", "omega example"))

# read in the data from a comma separated variable (.csv) file
SES <- read.csv("SES.csv")

# look at the first few cases to (if you'd like to check them)
head(SES)

# load the 'foreign' library and read in the data from an SPSS (.sav) data file
# you make get a warning such as: "Unrecognized record type 7, subtype 18"
# these warnings (relating to SPSS versions) do not appear to affect the output of the function
library(foreign)
SES <- read.spss("SES.sav", to.data.frame=TRUE)

# select the columns you want to work with (e.g., that form a single subscale)
# then remove cases with missing values
subscale1 <- SES[1:7]
subscale1 <- na.omit(subscale1)

# install the 'MBESS' package with all its dependent packages
install.packages('MBESS', dependencies=TRUE)

# load the MBESS package
library(MBESS)

# set the seed for the random number generator
# this step is required if you need to duplicate the exact results in the example
set.seed(1)

# either of these calls will output omega and a 95% CI for omega
my.omega <-ci.reliability(data=subscale1, type="omega", conf.level=0.95, interval.type="bca", B=1000)
sprintf ("Coeficiente Ômega: %f, Nível de confiança de 95%%: [%.3f, %.3f]",my.omega$est, my.omega$ci.lower,my.omega$ci.upper)


ci.reliability(subscale1, interval.type="bca")

# this call increases the number of simulations to 10,000
# these results will be slightly more accurate (but may take some time)
ci.reliability(subscale1, interval.type="bca", B=10000)

# testando com os 8 fatores
my.omega <-ci.reliability(data=subscale1, type="omega", conf.level=0.95, interval.type="bca", B=1000)
sprintf ("Coeficiente Ômega: %f, Nível de confiança de 95%%: [%.3f, %.3f]",my.omega$est, my.omega$ci.lower,my.omega$ci.upper)

# outro exemplo Omega
library(psych)
library(MASS)
library(GPArotation)
set.seed(20130403)

