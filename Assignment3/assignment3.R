file <- file.choose()

df <- read.csv(file, header = TRUE, sep = ",")
summary(df)

df2 <- read.csv2(file, header = TRUE, sep= ",")
df2

dfSubset <- df2[grep("Ajuntament", df2$NOM_COMPLERT), ]
summary(dfSubset)

dfSubset <- dfSubset[!grepl("2017", dfSubset$ANY_EXERCICI), ]
summary(dfSubset)




