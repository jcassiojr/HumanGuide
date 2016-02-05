# teste Mongo DB

#Init connection to local mongod
library(mongolite)
m <- mongo(collection = "diamonds")

# Insert test data
data(diamonds, package="ggplot2")
m$insert(diamonds)

# Check records
m$count()
nrow(diamonds)

# Perform a query and retrieve data
out <- m$find('{"cut" : "Premium", "price" : { "$lt" : 1000 } }')

# Compare
nrow(out)
nrow(subset(diamonds, cut == "Premium" & price < 1000))

# Cross-table (abaixo não funcionou!!!)
tbl <- m$mapreduce(
    map = "function(){emit({cut:this.cut, color:this.color}, 1)}",
    reduce = "function(id, counts){return Array.sum(counts)}"
)
# Same as:
data.frame(with(diamonds, table(cut, color)))

# Stream jsonlines into a connection (tmbém deu erro!!!)
tmp <- tempfile()
m$export(file(tmp))

# Stream it back in R
library(jsonlite)
mydata <- stream_in(file(tmp))

# Or into mongo
m2 <- mongo("diamonds2")
m2$count()
m2$import(file(tmp))
m2$count()

# Remove the collection
m$drop()
m2$drop()

# OUTRSO EXEMPLOS
# Insert some data
data(flights, package = "nycflights13")
m <- mongo(collection = "nycflights")
m$insert(flights)

# Basic queries
m$count('{"month":1, "day":1}')
jan1 <- m$find('{"month":1, "day":1}')

# Sorting
jan1 <- m$find('{"month":1,"day":1}', sort='{"distance":-1}')
head(jan1)

# Sorting on large data requires index (erro)
m$index(add = "distance")
allflights <- m$find(sort='{"distance":-1}')

# Select columns
jan1 <- m$find('{"month":1,"day":1}', fields = '{"_id":0, "distance":1, "carrier":1
               }')

# List unique values (erro)
m$distinct("carrier")
m$distinct("carrier", '{"distance":{"$gt":3000}}')
# Tabulate
m$aggregate('[{"$group":{"_id":"$carrier", "count": {"$sum":1}, "average":{"$avg":"
            $distance"}}}]')

# Map-reduce (binning)
hist <- m$mapreduce(
    map = "function(){emit(Math.floor(this.distance/100)*100, 1)}",
    reduce = "function(id, counts){return Array.sum(counts)}"
)
# Dump to bson
dump <- tempfile()
m$export(file(dump), bson = TRUE)
# Remove the collection
m$drop()
# Restore
m$count()
m$import(file(dump), bson = TRUE)
m$count()

