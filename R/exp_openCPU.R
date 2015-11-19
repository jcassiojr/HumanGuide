# examplo OpenCPU. Do exemplo em: https://opencpu.ocpu.io/tvscore/www/
#This script was used to create the model that is included with the package

#General Social Survey data
#For info see http://www3.norc.org/GSS+Website/Download/SPSS+Format/
download.file("http://publicdata.norc.org/GSS/DOCUMENTS/OTHR/2012_spss.zip", destfile="2012_spss.zip")
unzip("2012_spss.zip")
GSS <- foreign::read.spss("GSS2012.sav", to.data.frame=TRUE)

#GAM model
library(mgcv)
mydata <- na.omit(GSS[c("age", "tvhours", "marital")])
tv_model <- gam(tvhours ~ s(age, by=marital), data = mydata)

#Vizualize the model
library(ggplot2)
qplot(age, predict(tv_model), color=marital, geom="line", data=mydata) +
    ggtitle("gam(tvhours ~ s(age, by=marital))") +
    ylab("Average hours of TV per day")

#Save the model
dir.create("data", showWarnings=FALSE)
save(tv_model, file="data/tv_model.rda")

# scoring function
f <- function (input) 
{
    newdata <- if (is.character(input) && file.exists(input)) {
        read.csv(input)
    }
    else {
        as.data.frame(input)
    }
    stopifnot("age" %in% names(newdata))
    stopifnot("marital" %in% names(newdata))
    newdata$age <- as.numeric(newdata$age)
    newdata$tv <- as.vector(predict.gam(tv_model, newdata = newdata))
    return(newdata)
}
# using for prediction
prevdata <- read.csv ("./data/tvmovies_newdata.csv")
res <- f(prevdata)
