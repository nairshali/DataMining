library(rpart) 

# Load data
wine <-read.table(file.choose(),header=T,sep=",") 
wine <- transform(wine, class = as.factor(class))

## Resubstitution / Hold- Out Method

  # Data sampling
      sampleRate <- 0.10 
      sampleSize <- nrow(wine) * sampleRate 
      paste("sample size is: ", sampleSize) 
