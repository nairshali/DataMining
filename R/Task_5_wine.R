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
        
        # Hold-out method: acc/error on the test set 
        prediction <- predict(wine.dt, newdata=testSet, type="class") 
        cM <- table(testSet$class, prediction) #print(cM) 
        acc <- sum(diag(cM))/sum(cM) 
        hold_out_accuracy[t] = acc
        rErr <- 1.0 - acc 
        print(paste("hold-out method: accuracy = ", round(acc*100,1), "% and error = ", round(rErr*100,1), "%", sep=""))
     
      }

   # Compute Accuracy (Average and Std deviation ) 
      average_resub <- mean(unlist(resub_accuracy)) # Average of Resubstitutions
      average_hold_out <- mean(unlist(hold_out_accuracy)) # Average of HoldOut
      
      sd_resub <- sd(unlist(resub_accuracy))  # std deviation of Resubstitutions
      sd_hold_out <- sd(unlist(hold_out_accuracy)) # std deviation of HoldOut

## Cross Validation 10-Fold Method
      
      library(plyr)
      library(rpart)
      N <- nrow(wine)
      xVal_accuracy <- list()
