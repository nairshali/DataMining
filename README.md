## DataMining
# Problem #2 - Classification You are required to build and test a classification model for four datasets:  
1.iris dataset  2.wine dataset  3.breast-cancer-wisconsin.data  4. Zoo dataset


Classification algorithm are required to carry out a comparison of the accuracy (average and standard deviation over 20 trials) of these four evaluation methods for each of the four datasets using the same classification algorithm.  
1. resub: resubstitution error method  
2. hold-out-10%: hold-out method with 10% - 90% partition split   
3. xVal-10f: 10-fold cross-validation method   
4. LOOCV: leave-one-out cross-validation method


Problem # 2: Classification:-The classification algorithm used is decision tree. 
Decision Tree algorithm belongs to the family of supervised learning algorithms. Unlike other supervised learning algorithms, decision tree algorithm can be used for solving regression and classification problems too. It tries to solve the problem, by using tree representation. They provide the easier way to represent the information and extract IF-THEN classification rules. Decision Tree classification algorithm can be done in serial or parallel steps according to the amount of data, efficiency of the algorithm and memory available. A serial tree is a logical model as a binary tree constructed using a training data set. It helps in predicting the value of a target variable by making use of predictor variables. It consists of hierarchically organized sets of rules. It is a plain recursive structure for representing a decision procedure in which a future instance is classified in present predefined classes and it attempts to divide observations into mutually exclusive subgroups. Each part in a tree corresponds to one or more records from the original data set The top most nodes are named as the root node (no incoming link) and represent all of the rows in the given dataset. The other nodes are named as internal or decision nodes (only one incoming link) use to test on an attribute. The down most nodes are named as terminal nodes (no out coming link) and denote a decision class[2]. 
