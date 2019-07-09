library(rpart) 

# Load data
iris <-read.table(file.choose(),header=T,sep=",") 
## Resubstitution / Hold- Out Method

      # Data sampling
      sampleRate <- 0.10 
      sampleSize <- nrow(iris) * sampleRate 
      paste("sample size is: ", sampleSize) 
      resub_accuracy <- list()
      hold_out_accuracy <- list()
      
      # 20 Trials
      for (t in 1:20){
        #set.seed(1234) 
        testSampleIdx <- sample(nrow(iris), size=sampleSize) #testSampleIdx 
        testSet <- iris[testSampleIdx,] #testSet 
        trainingSet <- iris[-testSampleIdx,] #trainingSet 
        paste("Test set size<",nrow(testSet),">; Training set size<", nrow(trainingSet), ">", sep="") 
        
        iris.dt <- rpart(Class ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=trainingSet, method="class")  #iris.dt   
        
        # Resubstitution method: acc/error on the training set 
        prediction <- predict(iris.dt, newdata=trainingSet, type="class") 
        cM <- table(trainingSet$Class, prediction) #print(cM) 
        acc <- sum(diag(cM))/sum(cM) 
        resub_accuracy[t] = acc
        rErr <- 1.0 - acc 
        print(paste("resub method: accuracy = ", round(acc*100,1), "% and resubstitution error = ", round(rErr*100,1), "%", sep="")) 
        
        # Hold-out method: acc/error on the test set 
        prediction <- predict(iris.dt, newdata=testSet, type="class") 
        cM <- table(testSet$Class, prediction) #print(cM) 
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
      N <- nrow(iris)
      xVal_accuracy <- list()
      
      for (t in 1:20){
        # iris <- shuffle(iris)
        correct_predictions <- 0
        folds <- split(iris, cut(sample(1:nrow(iris)),10) )
        
        for (f in 1:length(folds)){
          # dt build a predictive model from all records n data but i
          trainingSet <- ldply(folds[-f], data.frame) #trainingSet
          testSet <- ldply(folds[f], data.frame) #testSet
          # paste("Test set size<",nrow(testSet),">; Training set size<", nrow(trainingSet), ">", sep="") 
          iris.dt <- rpart(Class ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=trainingSet, method="class")  #iris.dt   
          # apply the model dt to the record
          prediction <- predict(iris.dt, newdata=testSet, type="class") 
          cM <- table(testSet$Class, prediction) 
          x <- sum(diag(cM))
          # print(cM)
          correct_predictions <- correct_predictions + x
        }
        
        acc <- correct_predictions / N 
        rErr <- 1.0 - acc 
        print(paste("xVal method: accuracy = ", round(acc*100,1), "% and xVal error = ", round(rErr*100,1), "%", sep="")) 
        xVal_accuracy[t] <- acc
      }
      
      
      # Compute Accuracy (Average and Std deviation ) 
      average_xVal <- mean(unlist(xVal_accuracy))   # Average of 10-fold
      sd_xVal <- sd(unlist(xVal_accuracy)) # Std deviation of 10-fold
      
## Leave One Out Method
      
      N <- nrow(iris)
      sampleSize <- ( N * 0.9 ) + 1
      loocv_accuracy <- list()
      
      for (t in 1:20){
        #set.seed(1234) 
        SampleIdx <- sample(N, size=sampleSize) #testSampleIdx 
        correct_predictions <- 0
        
        for (i in SampleIdx){
          # dt build a predictive model from all records n data but i
          trainingSet <- iris[-i,] #trainingSet
          testSet <- iris[i,] #testSet
          # paste("Test set size<",nrow(testSet),">; Training set size<", nrow(trainingSet), ">", sep="")
          
          iris.dt <- rpart(Class ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=trainingSet, method="class")  #iris.dt
          
          # apply the model dt to the record
          prediction <- predict(iris.dt, newdata=testSet, type="class")
          cM <- table(testSet$Class, prediction)
          x <- sum(diag(cM))
          # print(cM)
          correct_predictions <- correct_predictions + x
        }
        
        acc <- correct_predictions/sampleSize
        rErr <- 1.0 - acc 
        print(paste("LOOCV method: accuracy = ", round(acc*100,1), "% and LOOCV error = ", round(rErr*100,1), "%", sep="")) 
        loocv_accuracy[t] <- acc
      }
      
      # Compute Accuracy (Average and Std deviation ) 
      average_loocv <- mean(unlist(loocv_accuracy))    # Average of leave one out
      sd_loocv <- sd(unlist(loocv_accuracy))  # Std deviation of leave one out
      
## Bar chart 
      # build dataframe
      data <- data.frame("Method" = c('Resubstitutions','Hold-Out','10-Fold','LOOCV'))
      
      # bar plot Accuracy ( Mean )
      library(plotly)
      plot_ly(data, x = ~Method, y = ~ c(average_resub,average_hold_out,average_xVal,average_loocv) , type = 'bar', name = 'Average') %>%
             add_trace(y = ~c(sd_resub,sd_hold_out,sd_xVal,sd_loocv), name = 'Std Deviations') %>%
             layout(yaxis = list(title = 'Accuracy Percentage(%)'), barmode = 'group')
   
      # build dataframe
      iris_accuracy_df <- data.frame(cbind("Method" = c('Resubstitutions','Hold-Out','10-Fold','LOOCV'), 
                                  "Average " = c(average_resub,average_hold_out,average_xVal,average_loocv),
                                  "Std Deviation" = c(sd_resub,sd_hold_out,sd_xVal,sd_loocv)
                                  ))