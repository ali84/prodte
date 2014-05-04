#Ali Reza Asadi 3/21/2014
#Ali Reza Asadi Modified 3/25/2014

#this library is good for dealing with date and time
install.packages("chron")
library(chron)

#reading data from the dir
setwd("C:/Users/KY_lab/Documents/DTE/05")
getwd()

list.files()
FileList <- read.table("FileList.txt", header = F)    #read the name the list of name/addresses
#View(FileList)
#class(FileList)
case1 <- vector("numeric", length=1000)
case2 <- vector("numeric", length=1000)
case3 <- vector("numeric", length=1000)
case4 <- vector("numeric", length=1000)
case5 <- vector("numeric", length=1000)
case6 <- vector("numeric", length=1000)
case7 <- vector("numeric", length=1000)
case8 <- vector("numeric", length=1000)
case9 <- vector("numeric", length=1000)
case10 <- vector("numeric", length=1000)
case11 <- vector("numeric", length=1000)
case12 <- vector("numeric", length=1000)
case13 <- vector("numeric", length=1000)
case14 <- vector("numeric", length=1000)
case15 <- vector("numeric", length=1000)
case16 <- vector("numeric", length=1000)
case17 <- vector("numeric", length=1000)
case18 <- vector("numeric", length=1000)
case19 <- vector("numeric", length=1000)
case20 <- vector("numeric", length=1000)
case21 <- vector("numeric", length=1000)
case22 <- vector("numeric", length=1000)
case23 <- vector("numeric", length=1000)
case24 <- vector("numeric", length=1000)

p1 = 1
p2 = 1
p3 = 1
p4 = 1
p5 = 1
p6 = 1
p7 = 1
p8 = 1
p9 = 1
p10 = 1
p11 = 1
p12 = 1
p13 = 1
p14 = 1
p15 = 1
p16 = 1
p17 = 1
p18 = 1
p19 = 1
p20 = 1
p21 = 1
p22 = 1
p23 = 1
p24 = 1

head(case1, 50)
plot(case1)
plot(case2)
plot(case3)
plot(case4)
plot(case5)
plot(case6)
plot(case7)
plot(case8)
plot(case9)
plot(case10)
plot(case11)
plot(case12)
plot(case13)
plot(case14)
plot(case15)
plot(case16)
plot(case17)
plot(case18)
plot(case19)
plot(case20)
plot(case21)
plot(case22)
plot(case23)
plot(case24)

for (c in 1:nrow(FileList)){
  name <- as.character(FileList[c,1])
  print(name)
  dat <- read.table(name, header = TRUE, sep=",")   #each single customers information -> temperary 'dat'
  
  for (i in 1:nrow(dat)){
    stt <- as.character(dat[i, 3])
    time <- timefunction(stt)
    #dat$SECOND[i] <- 0      #dat$DAY[i] <- time[4]    #dat$MONTH[i] <- time[3]    #dat$YEAR[i] <- time[5]
    dat$MIN[i] <- time[1]
    dat$HOUR[i] <- time[2]
    dat$DATE[i] <- format(ISOdate(time[5],time[3],time[4]),'%Y-%m-%d')
    dat$WEEKDAY[i]<- weekdays(as.Date(dat$DATE[i], abbreviate = FALSE))
    dat$DAY[i] <- time[4]
    dat$MONTH[i] <- time[3]
    dat$QUARTER[i] <- quarters(as.Date(dat$DATE[i]), abbreviate = T) #monthes 1,2,3 is Q1 and ...
    dat$DAYNUMBER[i] <- as.numeric(as.Date(dat$DATE[i]))
    
    if (dat$QUARTER[i] == "Q1"){
      if (dat$WEEKDAY[i] == "Monday"){
        if (dat$HOUR[i] == 1){
          case1[p] <- dat$VALUE[i]
          p1 = (p1 +1)
        }else if(dat$HOUR[i] == 2){
          case2[p] <- dat$VALUE[i]
          p2 = (p2 +1)
        }else if(dat$HOUR[i] == 3){
          case3[p] <- dat$VALUE[i]
          p3 = (p3 +1)
        }else if(dat$HOUR[i] == 4){
          case4[p] <- dat$VALUE[i]
          p4 = (p4 +1)
        }else if(dat$HOUR[i] == 5){
          case5[p] <- dat$VALUE[i]
          p5 = (p5 +1)
        }else if(dat$HOUR[i] == 6){
          case6[p] <- dat$VALUE[i]
          p6 = (p6 +1)
        }else if(dat$HOUR[i] == 7){
          case7[p] <- dat$VALUE[i]
          p7 = (p7 +1)
        }else if(dat$HOUR[i] == 8){
          case8[p] <- dat$VALUE[i]
          p8 = (p8 +1)
        }else if(dat$HOUR[i] == 9){
          case9[p] <- dat$VALUE[i]
          p9 = (p9 +1)
        }else if(dat$HOUR[i] == 10){
          case10[p] <- dat$VALUE[i]
          p10 = (p10 +1)
        }else if(dat$HOUR[i] == 11){
          case11[p] <- dat$VALUE[i]
          p11 = (p11 +1)
        }else if(dat$HOUR[i] == 12){
          case12[p] <- dat$VALUE[i]
          p12 = (p12 +1)
        }else if(dat$HOUR[i] == 13){
          case13[p] <- dat$VALUE[i]
          p13 = (p13 +1)
        }else if(dat$HOUR[i] == 14){
          case14[p] <- dat$VALUE[i]
          p14 = (p14 +1)
        }else if(dat$HOUR[i] == 15){
          case2[p] <- dat$VALUE[i]
          p15 = (p15 +1)
        }else if(dat$HOUR[i] == 16){
          case16[p] <- dat$VALUE[i]
          p16 = (p16 +1)
        }else if(dat$HOUR[i] == 17){
          case17[p] <- dat$VALUE[i]
          p17 = (p17 +1)
        }else if(dat$HOUR[i] == 18){
          case18[p] <- dat$VALUE[i]
          p18 = (p18 +1)
        }else if(dat$HOUR[i] == 19){
          case19[p] <- dat$VALUE[i]
          p19 = (p19 +1)
        }else if(dat$HOUR[i] == 20){
          case20[p] <- dat$VALUE[i]
          p20 = (p20 +1)
        }else if(dat$HOUR[i] == 21){
          case21[p] <- dat$VALUE[i]
          p21 = (p21 +1)
        }else if(dat$HOUR[i] == 22){
          case22[p] <- dat$VALUE[i]
          p22 = p22 +1
        }else if(dat$HOUR[i] == 23){
          case23[p] <- dat$VALUE[i]
          p23 = (p23 +1)
        }else if(dat$HOUR[i] == 24){
          case24[p] <- dat$VALUE[i]
          p24 = (p24 +1)
        }
      }
    } 
  }
  
}

View(dat)
head(dat)
#cut the top of data
dat <- dat[-1,]

#creating date and time column


head(dat, 5)
dat[3550:3555,]
tail(dat, 2)
dim(dat)
View(dat)
#counting number of days in the dataset
#This is function that needs to be loaded into R
countday(dat)

#adding season lable
#This is function that needs to be loaded into R 
#season(dat)

seasonv <- vector(mode = "character", length=nrow(dat))
for (i in 1:nrow(dat)){
  if (dat$month[i] == 1){
    dat$seasonv[i] <- "winter"
  }else if (dat$month[i] == 2){
    dat$seasonv[i] <- "winter"
  }else if (dat$month[i] == 3){
    dat$seasonv[i] <- "spring"
  }else if (dat$month[i] == 4){
    dat$seasonv[i] <- "spring"
  }else if (dat$month[i] == 5){
    dat$seasonv[i] <- "spring"
  }else if (dat$month[i] == 6){
    dat$seasonv[i] <- "summer"
  }else if (dat$month[i] == 7){
    dat$seasonv[i] <- "summer"
  }else if (dat$month[i] == 8){
    dat$seasonv[i] <- "summer"
  }else if (dat$month[i] == 9){
    dat$seasonv[i] <- "fall"
  }else if (dat$month[i] == 10){
    dat$seasonv[i] <- "fall"
  }else if (dat$month[i] == 11){
    dat$seasonv[i] <- "fall"
  }else if(dat$month[i] == 12){
    dat$seasonv[i] <- "winter"
  }
}

#finding jumps in the dataset
x <- 1
for(i in 2:(nrow(dat)-1)){
  dat$jum[1] = 0
  x <- dat$total[(i-1)]  #x is the last hour usage
  y <- dat$total[i] #y is this hour curent usage
  z <- dat$total[(i+1)]  #z is the next hour usage
  if (y > (x+1700)){
    dat$jum[i] <- 1
  }else if{
    if (dat$jum[(i-1)] == 1){
      if (z < (y-2000)){
        dat$jum[i] <- 1
      }else{
        dat$jum[i] <- 0
      }
    }else{
      dat$jum[i] <- 0
    }
  }
  
}
View(dat)

#finding picks in the dataset



#marking the traning data point if there is an elecrtic vehicle or not
evehicle = ifelse(dat$evse >= 12, "Yes", "No")
data = data.frame(dat, evehicle)
head(data)
dim(data)


#getting read of real evse variable
data = data[, -1]
data = data[, -1]
data = data[, -1]
data = data[, -3]
data = data[, -3]
data = data[, -3]
data = data[, -5]
data = data[, -1]
data = data[, -1]
data = data[, -1]
data = data[, -3]
head(data)
dim(data)

##classification tree
#load the library
install.packages("tree")
library(tree)
#split the data to testing and traning
set.seed(2)
train = sample(1:nrow(data), nrow(data)/2)
test = -train
training_data = data[train,]
testing_data = data[test, ]
testing_high = evehicle[test]

tree_model = tree(evehicle~., training_data)
plot(tree_model)
text(tree_model, pretty=0)

#check how the model is doing, using the test data
tree_pred = predict(tree_model, testing_data, type="class")
#compare what tree prediction with the reality
mean(tree_pred != testing_high)

#Prune tha tree
cv_tree = cv.tree(tree_model, FUN=prune.misclass)
names(cv_tree)

plot(cv_tree$size, cv_tree$dev, type="b")

##Prune the tree
pruned_model = prune.misclass(tree_model, best=8)
plot(pruned_model)
text(pruned_model, pretty=0)

##check how it is doing
tree_pred = predict(pruned_model, testing_data, type ="class")
mean(tree_pred != testing_high)








#creating a boxplots of the usage distributions 
boxplot(dat$residential[1:5000] ~ dat$weekday[1:5000])
Hboxplot(dat$evse ~ dat$weekday)
boxplot(dat$total ~ dat$weekday)

#creating hourly matrix
tdatamatrix <- matrix(0, nrow= nd, ncol=168)
rdatamatrix <- matrix(0, nrow= nd, ncol=168)
#filling hourly data matrix
for (i in 1:nd){
  for(j in 1:7){
    for (k in 1:24){
      x <- ((i-1)*168)+((j-1)*24)+k
      tdatamatrix[i, (((j-1)*24)+k)] <- dat$total[x]
      rdatamatrix[i, (((j-1)*24)+k)] <- dat$residential[x]
    }
  }
}
tmu <- colMeans(tdatamatrix, na.rm = T, dims = 1)
rmu <- colMeans(rdatamatrix, na.rm = T, dims = 1)
#pdf('1tplot.pdf')
plot(tmu[1:24], main="mean TOTAL usagefor one customer", ylab="average usage over total number of days", xlab=" Monday - hours in a day"  )
#dev.off()
plot(rmu[1:24], main="mean Residential usagefor one customer", ylab="average usage over total number of days", xlab=" Tuesday - hours in a day"  )
plot(mu[49:72], main="mean TOTAL usagefor one customer", ylab="average usage over total number of days", xlab=" Wednesday - hours in a day"  )
plot(mu[73:96], main="mean TOTAL usagefor one customer", ylab="average usage over total number of days", xlab=" Thursday - hours in a day" )
plot(mu[97:120], main="mean TOTAL usagefor one customer", ylab="average usage over total number of days", xlab=" Friday - hours in a day" )
plot(mu[121:144], main="mean TOTAL usagefor one customer", ylab="average usage over total number of days", xlab=" Saturday - hours in a day" )
plot(mu[145:168], main="mean TOTAL usagefor one customer", ylab="average usage over total number of days", xlab=" Sunday - hours in a day" )

Kmean <- kmeans(dat$total, centers=2)

names(Kmean)
Kmean$cluster
Kmean$centers
Kmean$totss
Kmean$withinss
Kmean$tot.withinss
Kmean$betweenss




ClusterCenters <- Kmean$centers
ClusterCenters
plot(dat, col=Kmean$cluster, pch=19, cex=2)
points(Kmean$centers, col=1:2, pch=3, lwd=3)





















#manupolating the data and time
datetime <- as.vector(dat$TIMESTAMP)
head(datetime)
DATE <- vector("numeric", length=nrow(dat))
TIME <- vector("numeric", length=nrow(dat))
for (i in 1:nrow(dat)){
  a <- strsplit(datetime[i], "-")
  a
  e <- vector("numeric", length=4)
  e <- a[[1]]
  e
  Dat <- cbind(e[3],e[2], e[1])
  Dat
  Dat <- paste(Dat, collapse="-")
  Dat
  #D <- as.Date(Dat)
  #D
  DATE[i] <- Dat
  t <- paste(cbind(e[4], "00"), collapse=":")
  t
  TIME[i] <- t
}
head(DATE, 2)
head(TIME, 2)


plot(dat$TIMESTAMP, dat$VALUE)

y <- as.numeric(dat$VALUE)
class(y)
plot(y)
x <- as.Date(dat$TIMESTAMP, "%m-%d-%Y-%H-%M")
class(x)
plot(x)
x

plot (y)















