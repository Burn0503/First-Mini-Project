#Bernie Joseph S. Juyanan
#BS Statistics III
#LATE SUBMISSION PERMITTED BY INSTRUCTOR AS PER EMAIL

#Problem no. 1
pollutantMean <- function(directory, pollutant, id=1:332){
  #directory is the location of the csv files
  #pollutant is a character vector where we will compute the mean
  #id is the excel files
  mylist1 <- list.files(path=directory, pattern=".csv")
  a <- numeric()
  for(i in id){
    mydata <- read.csv(mylist[i])
    a <- c(a, mydata[[pollutant]])
  }
  mean(a, na.rm=TRUE)
}
#pollutantMean("C:/Users/ASUS/Documents/3RD YEAR 1ST SEM/CMSC197/Mini-project 1/specdata/", "sulfate", 1:10)
##[1] 4.064128
#pollutantMean("C:/Users/ASUS/Documents/3RD YEAR 1ST SEM/CMSC197/Mini-project 1/specdata/", "nitrate", 70:72)
##[1] 1.706047
#pollutantMean("C:/Users/ASUS/Documents/3RD YEAR 1ST SEM/CMSC197/Mini-project 1/specdata/", "nitrate", 23)
##[1] 1.280833



#Problem no. 2
complete <- function(directory, id=1:332){
  mylist <- list.files(path=directory, pattern=".csv")
  b <- numeric()
  #b is the number of observations
  for(i in id){
    mydata <- read.csv(mylist[i])
    mysum <- sum(complete.cases(mydata))
    b <- c(b, mysum)
  }
  data.frame(id, b)
}
#complete("C:/Users/ASUS/Documents/3RD YEAR 1ST SEM/CMSC197/Mini-project 1/specdata/", c(2, 4, 8, 10, 12))
##  id   b
##1 2   1041
##2 4   474
##3 8   192
##4 10  148
##5 12  96



#Problem no. 3
corr <- function(directory, threshold=0){
  mylist <- list.files(path=directory, pattern=".csv")
  df <- complete(directory)
  ids <- df[df["b"] > threshold, ]$id
  d <- numeric()
  for(i in ids){
    mydata <- read.csv(mylist[i])
    dff <- mydata[complete.cases(data), ]
    d <- c(d, cor(dff$sulfate, dff$nitrate))
  }
  return(d)
}
#cr <- corr("C:/Users/ASUS/Documents/3RD YEAR 1ST SEM/CMSC197/Mini-project 1/ProgHospData/", 150)
##head(cr)
## [1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814



#Problem no. 4
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
str(outcome)
head(outcome)
ncol(outcome)
names(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11], xlab = "Deaths", main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack")