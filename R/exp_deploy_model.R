# deploy_model from OpenCPU (from https://github.com/opencpu/tvscore)
# Install in R the package from github
library(devtools)
install_github("opencpu/tvscore")

# Score in R
library(tvscore)
mydata <- data.frame(
    age=c(24, 54, 32, 75),
    marital=c("MARRIED", "DIVORCED", "WIDOWED", "NEVER MARRIED")
)
tv(input = mydata)

# Score remotely
library(jsonlite)
#library(curl)
curl (https://public.opencpu.org/ocpu/github/opencpu/tvscore/R/tv/json 
-H "Content-Type: application/json" 
-d '{"input" : [ {"age":26, "marital" : "MARRIED"}, {"age":41, "marital":"DIVORCED"} ]}')

# score remotely using OpenCPU single-user server (package already insttaled)
library(opencpu)
opencpu$browse()
