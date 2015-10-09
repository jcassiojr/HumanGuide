# normalizing data
# Age vector
age <- c(25, 35, 50)
# Salary vector
salary <- c(200000, 1200000, 2000000)
# Data frame created using age and salary
df <- data.frame( "Age" = age, "Salary" = salary, stringsAsFactors = FALSE)
plot(df$Age,df$Salary)
# min-max normalization (usado em knn)
normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}
dfNorm <- as.data.frame(lapply(df, normalize))
plot(dfNorm$Age,dfNorm$Salary)
# Z-score standardization
dfNormZ <- as.data.frame( scale(df[1:2] ))
plot(dfNormZ$Age,dfNormZ$Salary)

# testando com caret (tem o mesmo resultado que uso de scale acima!!!!)
preObj <- preProcess(df, method=c("center", "scale"))
newData <- predict(preObj, df)
