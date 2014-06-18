library(rpart)

## Creates Decision Trees based classificator object, containing 
## methods for prediction, cross validation, etc.

initTree <- function(trainingSet) {
	# Training phase
	tree <- fitDTClassifier(trainingSet)
	
	tree.predict <- function(data) {
		predict(tree, data)
	}
		
	tree.hitsNum <- function(inputs, trueLabels) {
		hitsNum  <- 0
		labels <- c(8) 
		falses <- c()
		for (inputId in 1: nrow(inputs)) {
			colsNum <- ncol(inputs)
			probs      <-  predict(tree, inputs[inputId, 1:colsNum])
			max        <- -1
			maxLabelId <-  1
			labelId    <-  1  
			
			for (labelProb in probs) {
				if (labelProb > max) {
					max   <- labelProb
					maxLabelId <- labelId
				}
				labelId <- labelId + 1
			}
			labels <- c(labels, maxLabelId)
			if (maxLabelId == trueLabels[inputId]) {
				hitsNum <- hitsNum + 1
			} else {
				falses <- c(falses, inputId) 
			}
		}
		print(falses)
		hitsNum
	}
	
	tree.crossValidation <- function(K=10) {
		crossValidationDT(trainingSet, K)
	}
	
	## Returns a list representation of the object with methods and properties accessed through indexed keys
	list(classifier=tree, predict=tree.predict, hitsNum = tree.hitsNum, crossValidation=tree.crossValidation)
}  

## Fits rpart based Decision Tree classifier

fitDTClassifier <- function(dataSet = loadData()) {
	# formulaOptimal <- emotion ~ X52+X87+X89+X92+X101+X102+X117+X118+X120+X123+X125+X1+X23+X40+X24+X99
	# formula <- emotion ~ X52+X87+X89+X92+X101+X102+X117+X118+X120+X123+X125
	# formula <- emotion ~ X52+X87+X89+X92+X101+X102+X117+X118+X120+X123+X125+X94+X100+X68+X55
	# formulaSelfComposed <- emotion ~ X109+X48+X54+X117+X103+X89+X99+X121+X127+X91+X53+X116+X135+X125+ #X122	
	# mouseChange   <- emotion ~ X116+X117+X118+X119+X120+X121+X122+X123+X124+X125+X126+X127 + X48+X49+X50+X51+X52+X53+X54+X55+X56+X57+X58+X59 
	# browsChange   <- emotion ~ X85+X86+X87+X88+X89+X90+X91+X92+X93+X94 + X17+X18+X19+X20+X21+X22+X23+X24+X25+X26
	# noseChange    <- emotion ~ X99+X100+X101+X102+X103 + X31+X32+X33+X34+X35
	# eyesChange    <- emotion ~ X36+X37+X38+X39+X40+X41+X42+X43+X44+X45+X46+X47 + X104+X105+X106+X107+X108+X109+X110+X111+X112+X113+X114+X115
	optimalChange <- emotion ~ X52+X87+X89+X92+X103+X102+X101+X100+X117+X118+X120+X123+X125+X1+X23+X40+X24+X99+X38+X42+X45+X67+X88#+X48+X54+X116+X122
	optimalChange2 <- emotion ~ X90+X35+X99+X17+X91+X104+X28+X80+X75+X82+X98+X83+X102+X107+X89+X8+X96+X38+X74+X17+X101+X77+X5
	optimalCfs <- emotion ~ X26+X53+X71+X77+X78+X104+X105+X117+X124+X125+X126
	withoutJaw <- emotion ~ X17+X18+X19+X20+X21+X22+X23+X24+X25+X26+X27+X28+X29+X30+X31+X32+X33+X34+X35+X36+X37+X38+X39+X40+X41+X42+X43+X44+X45+X46+X47+X48+X49+X50+X51+X52+X53+X54+X55+X56+X57+X58+X59+X60+X61+X62+X63+X64+X65+X66+X67+X68+X85+X86+X87+X88+X89+X90+X91+X92+X93+X94+X95+X96+X97+X98+X99+X100+X101+X102+X103+X104+X105+X106+X107+X108+X109+X110+X111+X112+X113+X114+X115+X116+X117+X118+X119+X120+X121+X122+X123+X124+X125+X126+X127+X128+X129+X130+X131+X132+X133+X134+X135+X136+X76
	optimalForwardSearch <- emotion ~  X66+X76+X91+X117+X119+X121+X128 #+X136#+X112+X113
	optBin <- emotion ~ X46+X57+X59+X87+X103 # X90+X100+X106+X121+X134 #X89+X106+X117+X135 #X19+X55+X128 #X37+X49+X55+X57+X99+X104+X117+X121+X127 #X54+X103+X124 #X41+X54+X93+X119+X124+X127
	optDif <- emotion ~ X23+X30+X57 #X23+X30+X57+X76 # X66+X91+X133 # X36+X48+X52+X134 #X30+X65+X103+X112+X122 # # Alt:  X35+X76+X101+X121+X122+X124+X128+X130
	formula <- optDif #optBin # optimalForwardSearch #optimalForwardSearchGLOB #optimalChange 
	fit <- rpart(formula, method="class", data=dataSet, control=rpart.control(minsplit=1, cp= 0.006147541))
	fit
}

## Estimates cross validation of the Decision Trees based classifier for 
## the specified data set, performing splits into K subsets. 
## Returns mean accuracy cross validation  

crossValidationDT <- function(dataSet, K = 10) {
	accuracy <- numeric(0)
	folds    <- cvFolds(nrow(dataSet), K=K)
	totalCm  <- matrix(rep(0, 49), 7, 7)
	for(i in 1:K) {
		train <- dataSet[folds$subsets[folds$which != i], ]
		validation <- dataSet[folds$subsets[folds$which == i], ]
		classifier <- fitDTClassifier(train)
		pred <- predict(classifier, validation, type="class")
		validation <- c(validation[, ncol(dataSet)]) 
		validation <- c(validation, c(1, 2, 3, 4, 5, 6, 7))
		pred <-  c(pred, c(1, 2, 3, 4, 5, 6, 7))
		cm   <- table(pred, validation)
		acc  <- (cm[1,1]+cm[2,2]+cm[3,3]+cm[4,4]+cm[5,5]+cm[6,6]+cm[7,7]-7)/(sum(cm)-7)
		totalCm <- totalCm + cm
		accuracy <- rbind(accuracy, acc)
	}
	print (totalCm)
	accuracy <- mean(accuracy)
	print(accuracy)
	accuracy
}

trainTestClassifier <- function(trainFolder=trainingDataFolder, testFolder=testDataFolder) {
	trainData  <- loadData(trainFolder)
	testData   <- loadData(testFolder)
	trueLabels <- testData[, 137]
	fit <- fitDTClassifier(trainData)
	res <- testDTClassifier(fit, testData, trueLabels)
	res/length(trueLabels)
}