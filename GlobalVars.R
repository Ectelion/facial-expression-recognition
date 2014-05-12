## Working directory, where the project is located
WORK_DIR     = "C:/Users/user/Documents/Docs_HP/TUM/Semester II/Data Mining Praktikum/FER Dataset/facial-expression-recognition"
## Directory for the data set, containing "CK" and "CK+" folders from the orinigal Cohn-Kanade data set 
DATA_SET_DIR = "C:/Users/user/Documents/Docs_HP/TUM/Semester II/Data Mining Praktikum/FER Dataset/Data provided"

## File system configuration variables
emotionLabelsFolder   <- paste(DATA_SET_DIR, "CK+/Emotion_labels/Emotion" , sep="/")
landmarksFolder       <- paste(DATA_SET_DIR, "CK+/Landmarks/Landmarks"    , sep="/")
AULabelsFolder        <- paste(DATA_SET_DIR, "CK+/FACS_labels/FACS"       , sep="/")
faceExpressionsFolder <- paste(WORK_DIR, "Facial Expressions Sorted"      , sep="/")
outputFolder          <- "Observations"

## Folders by types of emotions
angerFolder     = paste(faceExpressionsFolder, 'Emotion 1. Anger'     , sep="/")
contemptFolder  = paste(faceExpressionsFolder, 'Emotion 2. Contempt'  , sep="/")
disgustFolder   = paste(faceExpressionsFolder, 'Emotion 3. Disgust'   , sep="/")
fearFolder      = paste(faceExpressionsFolder, 'Emotion 4. Fear'      , sep="/")
happinessFolder = paste(faceExpressionsFolder, 'Emotion 5. Happiness' , sep="/")
sadnessFolder   = paste(faceExpressionsFolder, 'Emotion 6. Sadness'   , sep="/")
surpriseFolder  = paste(faceExpressionsFolder, 'Emotion 7. Surprise'  , sep="/")

## Emotional labels sorted in ascending emotional code orders
## Usage example: emotionFolders[emotionLabels$happy]
emotionFolders <- list(angerFolder, contemptFolder, disgustFolder, fearFolder, happinessFolder, sadnessFolder, surpriseFolder)

## Emotional labels with codes
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
