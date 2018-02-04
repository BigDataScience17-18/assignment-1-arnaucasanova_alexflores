install.packages("stringr")
install.packages("pheatmap")
require(ggplot2) 
require(reshape2)

file1 <- file.choose()

#Read first lines of file to acquire necessary information


#Step 3: Data acquisition - Manual download of the full egg tar, using two diferent users, alcoholic and non-alcoholic
# with 120 trials each

setwd("C:/Users/Usuari/Documents/UNI/4/4_2/BigDataScience/assignment1/assignmentArchives")
getwd()

user <- ""
state <- ""
trial <- ""
expType <- ""
sensors <- list()
userValues <- list()


#Step 4: 

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
  
  user <<- userAux
  print(user)
  
  state <<- stateAux
  print(state)
  
  expType <<- expTypeA
  print(expType)
  
  trial <<- trialA
  print(trial)

  sensorsIndex <- 1;
  valuesIndex <- 1;
  
  
  for (i in 5:length(linn)){
    firstChar <- str_split_fixed(linn[i], " ", n=4)
    firstChar <- firstChar[1]
    if(firstChar == "#"){
      testLinia <- linn[i]
      testLinia <- str_split_fixed(testLinia, " ", n=4)
      testLinia <- testLinia[2]
      sensors[sensorsIndex] <<- testLinia
      sensorsIndex <- sensorsIndex + 1
    }
    else{
      linia <- linn[i]
      value <- str_split_fixed(linia, " ", n=4) 
      value <- value[4]
      userValues[valuesIndex] <<- value
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
userInfo[5] <- "TRIAL"

trial

fileList <- list.files("C:/Users/Usuari/Documents/UNI/4/4_2/BigDataScience/assignment1/assignmentArchives")
for (x in 1: length(fileList)){
  connFile <- file("C:/Users/Usuari/Documents/UNI/4/4_2/BigDataScience/assignment1/userTable.txt", 'a')
  fileList[x]
  process_file(fileList[x])
  
  if(state == "a"){
    state <- "1"
  }else{
    state <- "0"
  }
  
  if (x == 1){
    for (i in 1: length(sensors)){
      sensors[i] <- toString(sensors[i])
    }
    header <- c(userInfo, sensors)
    header <- paste(unlist(header), collapse= " ")
    write(header, connFile, sep = " ")
  }

  for(i in 1: 256){
    indexAux <- 0
    for(j in 6: 69){
      sensorsRow[1] <- user
      sensorsRow[2] <- state
      expType <- str_replace_all(expType, " ", "")
      sensorsRow[3] <- expType
      sensorsRow[4] <- toString(i)
      trial <- str_replace_all(trial, " ", "")
      sensorsRow[5] <- trial
      sensorsRow[j] <- toString(userValues[i+indexAux])
      indexAux <- indexAux + 256
    }
  insertRow <- paste(unlist(sensorsRow), collapse=" ")
  write(insertRow, connFile, sep = " ")
}
  close(connFile)
  
}

file1 <- file("C:/Users/Usuari/Documents/UNI/4/4_2/BigDataScience/assignment1/userTable.txt")
df <- read.table(file1, header = TRUE, sep = " ")
df

p <- matrix()
p <- df
p




#Step 4: Data exploration
summary(df)
pheatmap(t(scale(as.matrix(df))), show_colnames=TRUE)


head(df)
#4.1. Exercise 1 - Represent the 'FP1' channel (first one).

table(df$FP1)

title('EEG reading Uiii Repjjj Paradkkk (Alcoholic')
xlabel('Time(sec)')
ylabel('Voltage(mv)')
ggplot(df, aes(x=SAMPLE, y=FP1)) + geom_point()


#4.2. Represent the 'FP1' channel as well as the next three

ggplot(df, aes(x=SAMPLE, y=FP1)) +geom_point()
  
ggplot(df, aes(x=SAMPLE, y=FP2))+geom_point() 
ggplot(df, aes(x=SAMPLE, y=F7))+geom_point() 
ggplot(df, aes(x=SAMPLE, y=F8))+geom_point()

#4.3. Represent all 64 channels










rm(list = ls())
