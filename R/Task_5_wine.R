library(rpart) 

# Load data
wine <-read.table(file.choose(),header=T,sep=",") 
wine <- transform(wine, class = as.factor(class))

## Resubstitution / Hold- Out Method

  # Data sampling
      sampleRate <- 0.10 
      sampleSize <- nrow(wine) * sampleRate 
      paste("sample size is: ", sampleSize) 

   # 20 Trials - Loop for each type of method
      for (t in 1:20){
        #set.seed(1234) 
        testSampleIdx <- sample(nrow(wine), size=sampleSize) #testSampleIdx 
        testSet <- wine[testSampleIdx,] #testSet 
        trainingSet <- wine[-testSampleIdx,] #trainingSet 
        paste("Test set size<",nrow(testSet),">; Training set size<", nrow(trainingSet), ">", sep="") 
        
      }
