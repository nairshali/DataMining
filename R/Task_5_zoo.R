library(rpart) 

# Load data
# Load data
zoo <-read.csv(file.choose(),header=T,sep=",") 
zoo$animal.name <- NULL
zoo <- transform(zoo, type = as.factor(type))
