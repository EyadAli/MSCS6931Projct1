# Ethics of Algorithms class 
# Project Script 
# By: Eyad Aldawod
# Dataset: 

# loading dataset to R
dat <- read.csv("prison-admissions-beginning-2008.csv", header=TRUE)

# MAking sure the dataset is loaded properly 
head(dat)

# examining the dataset for missing data
dat[!complete.cases(dat),]

# ommiting missing data 
dat <- na.omit(dat)

# Find and removing one case that is with "NOT CODED" in "Gender"
which(dat$Gender=="NOT CODED")
dat <- dat[-104436,]

# Creating a subset of the data set without 
# Admission.Month, County.of. Commitment, Last.Known.Residance, Month.Code and Most.Serious.Crime variables   
dat <- data.frame(dat[,-c(2,3,5,6,9)])

#attach the datasets
attach(dat) 

# view the dataset's dimensions
dim(dat)

# view a basic summary of the dataset 
summary(dat)

# Creating Gender tables 
genderTable <- table(dat$Gender)
genderTable2 <- table(dat$Gender) / 228928

# cleaning the tables 
genderTable <-  genderTable[-3]
genderTable2 <-  genderTable2[-3]

# Plots of Admissions based on gender

png(file='Admissions By Gender.png')
par(mfcol = c(1, 2))
barplot(genderTable, col=rainbow(2), main="Admissions By Gender", ylab="Admission", xlab="Gender")
barplot(genderTable2, col=rainbow(2), main="Percentage", ylab="Admission", xlab="Gender", las=1)
dev.off()

# creating admission.year 
yrTable <- table(Admission.Year)
yrTable2 <- yrTable/228928

png(file='Admissions By Year.png')
par(mfcol = c(2, 2))
barplot(yrTable, col=gray.colors(10),main="Admissions By Year", ylab="Admissions", xlab="Year", las=1, cex.axis=0.7)
plot.new()
plot.new()
plot(yrTable2,main="Rate", ylab="Admissions", xlab="Year", las=1, cex.axis=0.7, type="b")
dev.off()

# creating a relationship table between Gender and Admission.Type
tGtable <- table(Gender, Admission.Type)
tGtable <- tGtable[-3,] # removing "NOT CODED"

#plotting the comparison 
png(file='comparison:Admissions Type and Gender.png')
barplot(tGtable, beside = T, legend.text = T, main="Gender and Admission Type" ,las=1, ylab="Admissions", cex.axis=0.7)
dev.off()


# Creating the new datasets 

# A dataset with only females observations 
fTable1 <- subset(dat, Gender=="FEMALE")


# A dataset with only  newly committed females observations 
fTable2 <- subset(dat, Gender=="FEMALE" & Admission.Type=="NEW COURT COMMITMENT")

#A dataset with only repeated offenders females observations
fTable3 <- subset(dat, Gender=="FEMALE" & Admission.Type=="RET PAROLE VIOLATOR")

# A dataset with only males observations 
mTable1 <- subset(dat, Gender=="MALE")

# A dataset with only  newly committed males observations 
mTable2 <- subset(dat, Gender=="MALE" & Admission.Type=="NEW COURT COMMITMENT")

# A dataset with only repeated offenders males observations. 
mTable3 <- subset(dat, Gender=="MALE" & Admission.Type=="RET PAROLE VIOLATOR")

###########
# creatinga table for the data
tb1<-table(dat$Admission.Type, dat$Gender)
tb1<-tb1[,-3]
tb2<-table(dat$Admission.Type, dat$Gender)/228928
tb2<-tb2[,-3]
tb1
tb2
##################

# Figures of mean and median for age at admission over the years for Male and Female (Done)


### MD and FD ###

## MD and FD: admissions over the years ***
# creating Male admission.year 
yrmTable <- table(mTable1$Admission.Year)
yrmTable2 <- yrmTable/213953

# creating Female admission.year 
yrfTable <- table(fTable1$Admission.Year)
yrfTable2 <- yrfTable/14975

png(file='MD and FD admissions.png')
par(mfcol = c(2, 2))
barplot(yrmTable, col=gray.colors(10),main="Male Admissions By Year", ylab="Admissions", xlab="Year", las=1, cex.axis=0.7)
barplot(yrfTable, col=gray.colors(10),main="Female Admissions By Year", ylab="Admissions", xlab="Year", las=1, cex.axis=0.7)
plot(yrmTable2,main="Male Rate", ylab="Admission", xlab="Year", las=1, cex.axis=0.7, type="b")
plot(yrfTable2,main="Female Rate", ylab="Admissions", xlab="Year", las=1, cex.axis=0.7, type="b")
dev.off()

## Male: calculating the mean of age over the years ***
mtm1<- aggregate(mTable1[, 4], list(mTable1$Admission.Year), mean)
# Female calculating the mean of age over the years ***
ftm1<- aggregate(fTable1[, 4], list(fTable1$Admission.Year), mean)
# Male: calculating the median of age over the years ***
mtm2<- aggregate(mTable1[, 4], list(mTable1$Admission.Year), median)
# Female calculating the median of age over the years ***
ftm2<- aggregate(fTable1[, 4], list(fTable1$Admission.Year), median)

# Plotting
png(file='MD and FD mean and median.png')
par(mfcol = c(2, 2))
plot(mtm1, type="b", ylab="Age", xlab="Year", main="Male: Mean of Age Over Years")
plot(ftm1, type="b", ylab="Age", xlab="Year", main="Female: Mean of Age Over Years")
plot(mtm2, type="b", ylab="Age", xlab="Year", main="Male Median of Age Over Years")
plot(ftm2, type="b", ylab="Age", xlab="Year", main="Female Median of Age Over Years")
dev.off()


### MND and FND ### 

## MND and FND: calculating admissions over the years ***
# creating Male admission.year 
yrmnTable <- table(mTable2$Admission.Year)
yrmnTable2 <- yrmnTable/129564

# creating Female admission.year 
yrfnTable <- table(fTable2$Admission.Year)
yrfnTable2 <- yrfnTable/10185

png(file='MND and FND admmisions.png')
par(mfcol = c(2, 2))
barplot(yrmnTable, col=gray.colors(10),main="Male Admissions By Year", ylab="Admissions", xlab="Year", las=1, cex.axis=0.7)
barplot(yrfnTable, col=gray.colors(10),main="Female Admissions By Year", ylab="Admissions", xlab="Year", las=1, cex.axis=0.7)
plot(yrmnTable2,main="Male Rate", ylab="Admissions", xlab="Year", las=1, cex.axis=0.7, type="b")
plot(yrfnTable2,main="Female Rate", ylab="Admissions", xlab="Year", las=1, cex.axis=0.7, type="b")
dev.off()

## Male: calculating the mean of age over the years ***

mntm1<- aggregate(mTable2[, 4], list(mTable2$Admission.Year), mean)

# Female calculating the mean of age over the years ***
fntm1<- aggregate(fTable2[, 4], list(fTable2$Admission.Year), mean)

# Male: calculating the median of age over the years ***
mntm2<- aggregate(mTable2[, 4], list(mTable2$Admission.Year), median)

# Female calculating the median of age over the years ***
fntm2<- aggregate(fTable2[, 4], list(fTable2$Admission.Year), median)

png(file='MND and FND mean and median.png')
par(mfcol = c(2, 2))
plot(mntm1, type="b", ylab="Age", xlab="Year", main="Male: Mean of Age Over Years")
plot(fntm1, type="b", ylab="Age", xlab="Year", main="Female: Mean of Age Over Years")
plot(mntm2, type="b", ylab="Age", xlab="Year", main="Male Median of Age Over Years")
plot(fntm2, type="b", ylab="Age", xlab="Year", main="Female Median of Age Over Years")
dev.off()

### MRD and FRD ###

## MRD and FRD: calculating admissions over the years ***
# creating Male admission.year 
yrmrTable <- table(mTable3$Admission.Year)
yrmrTable2 <- yrmrTable/84389

# creating Female admission.year 
yrfrTable <- table(fTable3$Admission.Year)
yrfrTable2 <- yrfrTable/4790

png(file='MRD and FRD admissions.png')
par(mfcol = c(2, 2))
barplot(yrmrTable, col=gray.colors(10),main="Male Admissions By Year", ylab="Admissions", xlab="Year", las=1, cex.axis=0.7)
barplot(yrfrTable, col=gray.colors(10),main="Female Admissions By Year", ylab="Admissions", xlab="Year", las=1, cex.axis=0.7)
plot(yrmrTable2,main="Male Rate", ylab="Admissions", xlab="Year", las=1, cex.axis=0.7, type="b")
plot(yrfrTable2,main="Female Rate", ylab="Admissions", xlab="Year", las=1, cex.axis=0.7, type="b")
dev.off()

## Male: calculating the mean of age over the years ***
mrtm1<- aggregate(mTable3[, 4], list(mTable3$Admission.Year), mean)

# Female calculating the mean of age over the years ***
frtm1<- aggregate(fTable3[, 4], list(fTable3$Admission.Year), mean)

# Male: calculating the median of age over the years ***
mrtm2<- aggregate(mTable3[, 4], list(mTable3$Admission.Year), median)

# Female calculating the median of age over the years ***
frtm2<- aggregate(fTable3[, 4], list(fTable3$Admission.Year), median)


png(file='MRD and FRD mean and median.png')
par(mfcol = c(2, 2))
plot(mrtm1, type="b", ylab="Age", xlab="Year", main="Male: Mean of Age Over Years")
plot(frtm1, type="b", ylab="Age", xlab="Year", main="Female: Mean of Age Over Years")
plot(mrtm2, type="b", ylab="Age", xlab="Year", main="Male Median of Age Over Years")
plot(frtm2, type="b", ylab="Age", xlab="Year", main="Female Median of Age Over Years")
dev.off()

# Close all plots 
#dev.off()


