# protable R models using DeployR
require(devtools)
install_github('deployr/deployrUtils')
library(deployrUtils)
# package portability
## Ensures this package dependency is installed & loaded from the start
deployrPackage("ggplot2")
# Creates an integer variable named 'age' if it does not already exist
deployrInput('{ "name": "age", "label": "Age", "render": "integer", "default": 6 } ')
## READ CSV FILE
## Locally: finds in current working directory
## In DeployR: finds in user's external directory
data <- read.csv(file = deployrExternal("./data/menarche.csv"))
# ou
# LOAD THE MODEL
if(!exists('fraudModel')) { load('fraudModel.rData')}
# SCORE THE MODEL
score <- data.frame(balance=bal, numTrans=trans, creditLine=credit)
x<- predict(fraudModel, score)
