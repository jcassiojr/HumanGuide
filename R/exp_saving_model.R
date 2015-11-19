# saving models as object using saveRDS
require(mgscv)
mod <- glm(Ozone ~ ., data = airquality)
mod

save(mod, file = "mymodel.Rdata")
ls()
load(file = "mymodel.Rdata")
ls()

# or using saveRDA para não carregar pro cima do original
ls()
saveRDS(mod, "mymodel.rds")
mod2 <- readRDS("mymodel.rds")
ls()

identical(mod, mod2, ignore.environment = TRUE)

