library(rpart) 

# Load data
wine <-read.table(file.choose(),header=T,sep=",") 
wine <- transform(wine, class = as.factor(class))

## Resubstitution / Hold- Out Method

  # Data sampling
      sampleRate <- 0.10 
      sampleSize <- nrow(wine) * sampleRate 
      paste("sample size is: ", sampleSize) 
  
  # initialize Variable 
      resub_accuracy <- list()
      hold_out_accuracy <- list()

   # 20 Trials - Loop for each type of method
      for (t in 1:20){
        #set.seed(1234) 
        testSampleIdx <- sample(nrow(wine), size=sampleSize) #testSampleIdx 
        testSet <- wine[testSampleIdx,] #testSet 
        trainingSet <- wine[-testSampleIdx,] #trainingSet 
        paste("Test set size<",nrow(testSet),">; Training set size<", nrow(trainingSet), ">", sep="") 
        
        
        wine.dt <- rpart(class ~., data=trainingSet, method="class")  #wine.dt   
        
        # Resubstitution method: acc/error on the training set 
        prediction <- predict(wine.dt, newdata=trainingSet, type="class") 
        cM <- table(trainingSet$class, prediction) #print(cM) 
        acc <- sum(diag(cM))/sum(cM) 
        resub_accuracy[t] = acc
        rErr <- 1.0 - acc 
        print(paste("resub method: accuracy = ", round(acc*100,1), "% and resubstitution error = ", round(rErr*100,1), "%", sep="")) 
        
        
      }
