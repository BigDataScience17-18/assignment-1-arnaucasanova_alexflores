install.packages("stringr")

file1 <- file.choose()

#Read first lines of file to acquire necessary information

setwd("C:/Users/Usuari/Documents/UNI/4/4_2/BigDataScience/assignment1/ARCHIVES")
getwd()

file1 <- file("C:/Users/Usuari/Documents/UNI/4/4_2/BigDataScience/assignment1/ARCHIVES/co2a0000447.rd.002")

process_file <- function(filename){
  conn <<- file(filename,open="r")
  linn <-readLines(conn)
  
  userAux <- str_sub(linn[1],3,13)
  stateAux <- str_sub(linn[1],6,6)
  expTypeA <- str_split_fixed(linn[4], ",", n=2)
  trialA <- expTypeA[2]
  expTypeA <- expTypeA[1]
  expTypeA <- str_split_fixed(expTypeA, "#", n=2)
  expTypeA <- expTypeA[2]
  expTypeA <- str_trim(expTypeA, side = c("left"))
  trialA <- linn[4]
  trialA <- str_trim(trialA, side = c("left"))
  
  
  user <<- userAux
  print(user)
  
  state <<- stateAux
  print(state)
  
  expType <<- expTypeA
  print(expType)
  
  trial <<- trialA
  print(trial)
  
  userValues <- list()
  sensors <- list()
  sensorsIndex <- 1;
  valuesIndex <- 1;
  
  
  for (i in 5:length(linn)){
    firstChar <- str_split_fixed(linn[i], " ", n=4)
    firstChar <- firstChar[1]
    if(firstChar == "#"){
      testLinia <- linn[i]
      testLinia <- str_split_fixed(testLinia, " ", n=4)
      testLinia <- testLinia[2]
      sensors[sensorsIndex] <- testLinia
      sensorsIndex <- sensorsIndex + 1
    }
    else{
      linia <- linn[i]
      value <- str_split_fixed(linia, " ", n=4) 
      value <- value[4]
      userValues[valuesIndex] <- value
      valuesIndex <- valuesIndex + 1
    }
  }
}




connFile <- file("C:/Users/Usuari/Documents/UNI/4/4_2/BigDataScience/assignment1/userTable.txt", 'a')


sensorsRow <- list()
userInfo <- list()
userInfo[1] <- "USER"
userInfo[2] <- "STATE"
userInfo[3] <- "EXP_TYPE"
userInfo[4] <- "SAMPLE"


if(state == "a"){
  state <- "1"
}else{
  state <- "0"
}

for (i in 1: length(sensors)){
  sensors[i] <- toString(sensors[i])
}

header <- c(userInfo, sensors)
header <- paste(unlist(header), collapse= " ")
write(header, connFile, sep = " ")




fileList <- list.files("C:/Users/Usuari/Documents/UNI/4/4_2/BigDataScience/assignment1/ARCHIVES")
for (x in 1: length(fileList)){
  connFile <- file("C:/Users/Usuari/Documents/UNI/4/4_2/BigDataScience/assignment1/userTable.txt", 'a')
  
process_file(fileList[x])

for(i in 1: 256){
  indexAux <- 0
  for(j in 5: 68){
    sensorsRow[1] <- user
    sensorsRow[2] <- state
    sensorsRow[3] <- expType
    sensorsRow[4] <- toString(i)
    sensorsRow[j] <- toString(userValues[i+indexAux])
    indexAux <- indexAux + 256
  }
  insertRow <- paste(unlist(sensorsRow), collapse=" ")
  write(insertRow, connFile, sep = " ")
}
  close(connFile)
  
}

file1 <- file.choose()
df <- read.table(file1, header = TRUE)
df



























close(conn)
linia
linia <- str_split_fixed(linia, " ", n=4)
value <- linia[4]








df <- read.table(file1, header = TRUE, sep = "#")
df





ff <- list.files(path="C:/Users/Usuari/Documents/UNI/4/4_2/BigDataScience/assignment1/assignmentArchives/co2a0000447", full.names=TRUE)
ff2 <- list.files(path="C:/Users/Usuari/Documents/UNI/4/4_2/BigDataScience/assignment1/assignmentArchives/co2c0000337", full.names=TRUE)

myfilelist <- lapply(ff, read.table)
names(myfilelist) <- list.files(path="C:/Users/Usuari/Documents/UNI/4/4_2/BigDataScience/assignment1/assignmentArchives/co2a0000447", full.names=FALSE)
myfilelist

myfilelist2 <- lapply(ff2, read.table)
names(myfilelist2) <- list.files(path="C:/Users/Usuari/Documents/UNI/4/4_2/BigDataScience/assignment1/assignmentArchives/co2c0000337", full.names=FALSE)
myfilelist2

myfilelist














