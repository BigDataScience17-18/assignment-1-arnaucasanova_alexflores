install.packages("stringr")

file1 <- file.choose()

#Read first lines of file to acquire necessary information

conn <- file(file1,open="r")
linn <-readLines(conn)

user <- str_sub(linn[1],3,13)
state <- str_sub(linn[1],6,6)
expType <- str_split_fixed(linn[4], ",", n=2)
trial <- expType[2]
expType <- expType[1]
expType <- str_split_fixed(expType, "#", n=2)
expType <- expType[2]
expType <- str_trim(expType, side = c("left"))
trial <- linn[4]
trial <- str_trim(trial, side = c("left"))

#USER ID
user

#CONTROL(C) OR ALCOHOLIC(A)
state

#TYPE OF EXPERIMENT(S1,S2 MATCH,S2 NOMATCH)
expType

#NUMBER OF TRIAL
trial

userValues <- list()
sensors <- list()
sensorsIndex = 1;
valuesIndex = 1;


for (i in 5:length(linn)){
  firstChar <- str_split_fixed(linn[i], " ", n=4)
  firstChar <- firstChar[1]
  if(firstChar == "#"){
    testLinia <- linn[i]
    testLinia <- str_split_fixed(testLinia, " ", n=4)
    testLinia <- testLinia[2]
    sensors[sensorsIndex] = testLinia
    sensorsIndex = sensorsIndex + 1
  }
  else{
  linia <- linn[i]
  value <- str_split_fixed(linia, " ", n=4) 
  value <- value[4]
  userValues[valuesIndex] = value
  valuesIndex <- valuesIndex + 1
  }
}
length(FP1)
length(sensors)

userTable <- "C:/Users/Usuari/Documents/UNI/4/4_2/BigDataScience/assignment1/userTable.txt"

conn <- file(userTable, open ="w")
for (i in 1: length(sensors)){
  text <- "sensors[i]"
  documentoUser <- writeLines(text, conn)

}
documentoUser <-writeLines("\n", con)
FP1[1]
FP1[257]
FP1[513]

file1
sensors

























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














