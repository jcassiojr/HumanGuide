# normalization 
#Example Data
x = sample(-100:100,50)

#Normalized Data
normalized = (x-min(x))/(max(x)-min(x))

#Histogram of example data and normalized data
par(mfrow=c(1,2))
hist(x,xlab="Data",col="lightblue",main="")
hist(normalized,xlab="Normalized Data",col="lightblue",main="")
