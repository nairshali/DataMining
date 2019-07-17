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

resub_accuracy <- list()
hold_out_accuracy <- list()

# Loop for 20 iteration and identify training / test sample data 90/10 ratio for each iterations 
for (t in 1:20){
        #set.seed(1234) 
        testSampleIdx <- sample(nrow(zoo), size=sampleSize) #testSampleIdx 
        testSet <- zoo[testSampleIdx,] #testSet 
        trainingSet <- zoo[-testSampleIdx,] #trainingSet 
        paste("Test set size<",nrow(testSet),">; Training set size<", nrow(trainingSet), ">", sep="") 
        
        zoo.dt <- rpart(type ~. , data=trainingSet, method="class")  #zoo.dt  
    }
