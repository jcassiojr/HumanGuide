# sequential forward selection (AIC = An Information Criterion)
data("EuStockMarkets")
step(glm(DAX ~ SMI + CAC + FTSE, data=EuStockMarkets), direction = "backward")
# or forward
step(glm(DAX ~ 1, data=EuStockMarkets), direction = "forward", scope = ~ SMI + CAC + FTSE)
# or both
step(glm(DAX ~ SMI + CAC + FTSE, data=EuStockMarkets), direction = "both")
