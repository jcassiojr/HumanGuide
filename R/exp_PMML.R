# PMML example
library(pmml)
library(rpart)
data(iris)
# The root node test is on the variable Petal.Length against a value of 2.45.
# The “left” branch tests for Petal.Length < 2.45 and deliv- ers a decision 
# of setosa. The “right” branch splits on a further variable, Petal.Width, 
# to distinguish between versicolor and virginica.
my.rpart <- rpart(Species ~ ., data=iris)
my.rpart

pmml(my.rpart)

saveXML(pmml(my.rpart), file="./data/my_rpart.xml")


# outro exemplo
# The example below shows how to train a sup- port vector machine to 
# perform binary classifi- cation using the audit dataset
library(kernlab)
audit <- read.csv(file( "http://rattle.togaware.com/audit.csv") )
myksvm <- ksvm(as.factor(TARGET_Adjusted) ~ .,
                             data=audit[,c(2:10,13)],
                             kernel = "rbfdot",
                             prob.model=TRUE)
pmml(myksvm, data=audit)
