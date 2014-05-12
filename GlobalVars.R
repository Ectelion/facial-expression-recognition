## File system configuration variables
emotionLabelsFolder   <- "C:/Users/user/Documents/Docs_HP/TUM/Semester II/Data Mining Praktikum/FER Dataset/Data provided/CK+/Emotion_labels/Emotion"
landmarksFolder       <- "C:/Users/user/Documents/Docs_HP/TUM/Semester II/Data Mining Praktikum/FER Dataset/Data provided/CK+/Landmarks/Landmarks"
AULabelsFolder        <- "C:/Users/user/Documents/Docs_HP/TUM/Semester II/Data Mining Praktikum/FER Dataset/Data provided/CK+/FACS_labels/FACS"
faceExpressionsFolder <- "C:/Users/user/Documents/Docs_HP/TUM/Semester II/Data Mining Praktikum/FER Dataset/facial-expression-recognition/Facial Expressions Sorted"
outputFolder          <- "Observations"

## Folders by types of emotions
angerFolder     = paste(faceExpressionsFolder, 'Emotion 1. Anger',sep="/")
contemptFolder  = paste(faceExpressionsFolder, 'Emotion 2. Contempt',sep="/")
disgustFolder   = paste(faceExpressionsFolder, 'Emotion 3. Disgust',sep="/")
fearFolder      = paste(faceExpressionsFolder, 'Emotion 4. Fear',sep="/")
happinessFolder = paste(faceExpressionsFolder, 'Emotion 5. Happiness',sep="/")
sadnessFolder   = paste(faceExpressionsFolder, 'Emotion 6. Sadness',sep="/")
surpriseFolder  = paste(faceExpressionsFolder, 'Emotion 7. Surprise',sep="/")

## Global variables
emotionLabels <- list(
	neutral  = 0, 
	anger    = 1, 
	contempt = 2, 
	disgust  = 3, 
	fear     = 4, 
	happy    = 5, 
	sadness  = 6, 
	surprise = 7
)
