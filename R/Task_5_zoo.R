library(rpart) 

# Load data
# read data file and store in zoo variable
zoo <-read.csv(file.choose(),header=T,sep=",") 
zoo$animal.name <- NULL
zoo <- transform(zoo, type = as.factor(type))

# Data sampling
sampleRate <- 0.10 
sampleSize <- nrow(zoo) * sampleRate 
paste("sample size is: ", sampleSize) 
