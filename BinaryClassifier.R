# Copyright (C) 2015 Rustem Bekmukhametov
# This program is free software: you can redistribute it and/or modify it under the terms of the 
# GNU General Public License as published by the Free Software Foundation, either version 3 of the 
# License, or (at your option) any later version.

## Dependencies
library(rpart)
source("DecisionTreeClassifier.R")

## Define

optimalBinFormulas <- list(
      emotion ~ X41+X54+X93+X119+X124+X127
    , emotion ~ X54+X103+X124
    , emotion ~ X37+X49+X55+X57+X99+X104+X117+X121+X127
    , emotion ~ X19+X55+X128
    , emotion ~ X89+X106+X117+X135
    , emotion ~ X90+X100+X106+X121+X134
    , emotion ~ X46+X57+X59+X87+X103
) 

optimalDiffFormulas <- list(
      "1-3" = emotion ~ X35+X76+X101+X121+X122+X124+X128+X130
    , "3-4" = emotion ~ X36+X48+X52+X134
    , "3-5" = emotion ~ X45+X55+X118
    , "4-7" = emotion ~ X18+X55+X134
    , "1-6" = emotion ~ X66+X91+X106+X124
    , "2-6" = emotion ~ X23+X30+X57
) 

## Creates binary classifier object, consisted of binary classifiers for 
## each emotion and methods for prediction, cross validation, etc.

initBinClassifier <- function(trainingSet = loadData()) {
    # Training phase
    binTrees <- list()
    diffClass <- initDifferentiatingClassifier(trainingSet)
    
    # Train binary trees
    for(em in 1:7) {
        train <- oneVsAll(trainingSet, em)
        formula <- optimalBinFormulas[[em]]
        tree <- rpart(formula, data = train)  # control=rpart.control(cp=0.006147541)) 
        
        if (em == 1) {
            binTrees <- list(tree)
        } else {
            binTrees <- c(binTrees, list(tree))
        }
    }
    
    ## Prediction approach based on polling
    
    binTrees.predictOne <- function(entry) {
        classes <- c(0, 0, 0, 0, 0, 0, 0)
        
        for (treeId in 1:length(binTrees)) {
            classProb <- predict(binTrees[[treeId]], entry)
            classes[treeId] <- classProb
        }
        
        # Applying weights, estimated by binary classificators
        weights <- c(0.9679, 0.9703, 0.9626, 0.9813, 0.982, 0.96, 0.985)
        classes <- weights * classes
        
        # Identifying the elected winner
        max      <- -1
        winnerId <-  0
        voteId   <-  1  
        
        for (prob in classes) {
            if (prob > max) {
                max <- prob
                winnerId <- voteId
            }
            voteId <- voteId + 1
        }
        print(classes)
        winnerId
    }
    
    # A hybrid prediction approach based on binary and differentiating classifiers
    
    binTrees.predictOne2 <- function(entry) {
        classProbs <- c(0, 0, 0, 0, 0, 0, 0)
        
        for (treeId in 1:length(binTrees)) {
            classProb <- predict(binTrees[[treeId]], entry)
            classProbs[treeId] <- classProb
        }
        #print(classProbs)
        
        # Applying weights, estimated by binary classificators
        weights <- c(0.9679, 0.9703, 0.9626, 0.9813, 0.982, 0.96, 0.985)
        classProbs <- weights * classProbs
        
        # Identifying the most probable classes
          n <- length(classProbs)
        sortedProbs <- sort(classProbs, partial=n)
        max1 <- sortedProbs[n]
        max2 <- sortedProbs[n-1]
        
        # Handling the case when the two top indices are a tie
        # or when the second top element has a tie
        max1Ind <- max(which(classProbs == max1))
        max2Ind <- min(which(classProbs == max2))
        
        if (max1 - max2 >= 0.5) {
            # If the top probability is high enough, return the result
            res <- max1Ind
        } else {
            # Launching differentiating classifiers for dealing with the confusion 
            fitDiff <- diffClass$fit(max1Ind, max2Ind)
            pred <- predict(fitDiff, entry) #, type="class")
            #print(pred)
            maxId <- which.max(pred)
            res <- as.numeric(dimnames(pred)[[2]][maxId])
        }
        res
    }
    
    ## Applies predicting function to each instance of the provided testSet 
    
    binTrees.predict <- function(testSet) {
        predictions <- c()
        for (testId in 1:nrow(testSet)) {
            winnerId <- binTrees.predictOne2(testSet[testId, ])
            predictions <- c(predictions, winnerId)
        }
        predictions
    }
    
    ## Counts positive predictions of the testSet
    
    binTrees.hitsNum <- function(testSet, trueLabels) {
        predictions <- binTrees.test(testSet)
        hits <- predictions == trueLabels
        sum(hits)
    }
    
    ## Performs cross validation on the specified testSet
    
    binTrees.crossValidation <- function(testSet = trainingSet, K = 10) {
        crossValidationBin(testSet, K)
    }
    
    ## Returns a list representation of the object with methods and properties accessed through indexed keys
    list(classifier = binTrees, predict = binTrees.predict, test = binTrees.predictOne, hitsNum = binTrees.hitsNum, crossValidation = binTrees.crossValidation)
}

## Differentiating classifier object. Caches the results across calls

initDifferentiatingClassifier <- function(dataSet = loadData()) {
    differentiatingClassifiers <- list() 
    
    fit <- function(emotionIdX, emotionIdY) {
        if (emotionIdX == emotionIdY) {
            stop("The same emotion codes are passed")
        }
        if (is.na(emotionIdX) || is.na(emotionIdY)) {
            stop("NA value passed")
        }
        emotionIdX <- as.numeric(emotionIdX)
        emotionIdY <- as.numeric(emotionIdY)
        
        if (emotionIdX > emotionIdY) {
            tmp <- emotionIdY
            emotionIdY <- emotionIdX
            emotionIdX <- tmp
        }
        key <- paste(emotionIdX, emotionIdY, sep = "-")
        #if (key %in% names(differentiatingClassifiers) ) {
            # Retrieve and return cached entry
            #differentiatingClassifiers[key]	
        #} else {
            
            emX <- dataSet[dataSet[,"emotion"] == emotionIdX, ]
            emY <- dataSet[dataSet[,"emotion"] == emotionIdY, ]
            differentiatorDt <- rbind(emX, emY) 
            if (key %in% names(optimalDiffFormulas)) {
                formula <- optimalDiffFormulas[[key]]
                #print(key)
                #print(formula)
            } else {
                formula <- emotion ~ X66+X76+X91+X117+X119+X121+X128
            }
            fitDiff <- rpart(formula, data = data.frame(differentiatorDt), method = "class", control = rpart.control(minsplit = 1, cp = 0.0061475))
            #differentiatingClassifiers[key] <<- fitDiff
            fitDiff
        #}
    }
    
    list(fit = fit)
}

## Transforms the dataset to a binary for the specified class 

oneVsAll <- function(dataSet, binClass) {
    oneVsAll <- dataSet
    oneVsAll[oneVsAll[,"emotion"] != binClass, "emotion"] <- FALSE
    oneVsAll[oneVsAll[,"emotion"] != FALSE, "emotion"] <- TRUE
    oneVsAll
}		

## Creates a binary classifier for a specified emotion. Based on rpart 
## Decision Tree classificator

fitBinaryClassifier <- function(dataSet = loadData(), emotionId) {
    labels <- dataSet[, "emotion"]
    labelsBin <- labels == emotionId
    dataSet[, "emotion"] <- labelsBin
    fit <- rpart(emotion ~ ., dataSet)
    fit			
}

# Experimental hybrid classifier with differentiating trees used to deal with confusions

expC <- function(dataSet, K = 10) {
    accuracy <- numeric(0)
    folds    <- cvFolds(nrow(dataSet), K = K)
    totalCm  <- matrix(rep(0, 49), 7, 7)
    for(i in 1:K) {
        train <- dataSet[folds$subsets[folds$which != i], ]
        validation <- dataSet[folds$subsets[folds$which == i], ]
        classifier <- fitDTClassifier(train)
        diffClass <- initDifferentiatingClassifier(train)
        prob <- predict(classifier, validation)
        validation <- c(validation[, ncol(dataSet)]) 
        ans <- c(1:length(prob))
        
        
        for (i in 1:nrow(prob)) {
            classProbs <- prob[i,] 
            print(classProbs)
            # Identifying the most probable classes
            n <- length(classProbs)
            sortedProbs <- sort(classProbs, partial=n)
            max1    <- sortedProbs[n]
            max2    <- sortedProbs[n-1]
            
            # Handling the case when the two top indices are a tie
            # or when the second top element has a tie
            max1Ind <- max(which(classProbs == max1))
            max2Ind <- min(which(classProbs == max2))
            
            if (max1 - max2 >= 0.5) {
                # If the top probability is high enough, return the result
                ans[i] <- max1Ind
                
            } else {
                # Launching differentiating classifiers for dealing with the confusion 
                print(paste(max1Ind, max2Ind, max1, max2))
                print(sortedProbs)
                fitDiff <- diffClass$fit(max1Ind, max2Ind)
                predDiff <- predict(fitDiff, dt[i, ]) #, type="class")
                #print(pred)
                maxId <- which.max(predDiff)
                ans[i] <- as.numeric(dimnames(predDiff)[[2]][maxId])
            }
        }
        
        acc <- sum(ans == validation) / length(validation)
        accuracy <- rbind(accuracy, acc)
    }
    accuracy <- mean(accuracy)
    print(accuracy)
    accuracy
}