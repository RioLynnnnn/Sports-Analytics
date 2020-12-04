# -- Preparing the data
# : https://www.kaggle.com/ronitf/heart-disease-uci
# We are reading our data
heart <- read.csv('heart.csv', stringsAsFactors=FALSE)
str(heart)
# select ï..age, sex, thalach, target

vars <- c('ï..age', "sex", "thalach", "target")

# Only interested in heart disease data
heart <- heart[vars]
str(heart)
head(heart)
# convert to factor type
heart$sex <- factor(heart$sex, labels = c("Female","Male"))
heart$target <- factor(heart$target, labels = c("Haven't Disease","Have Disease"))
names(heart) <- c("Age","Gender", "Thalach", "Target")# rename columns

# -- Analyzing the data

#-----------------------------------------------------------------
# Analysis of at least one categorical variable
#-----------------------------------------------------------------

#  Gender Analysis

table(heart$Gender)
tab <- as.data.frame(prop.table(table(heart$Gender)))
tab$Freq <- scales::percent(tab$Freq)
barplot(table(heart$Gender), col = rainbow(2), xlab = "Gender", ylab = "Frequency", main="Distribution of Gender")
abline(h = 0)
text(x=0.7,y=120,tab$Freq[1], cex =2)
text(x=2,y=150,tab$Freq[2], cex =2)

# Target analysis
table(heart$Target)
tab <- as.data.frame(prop.table(table(heart$Target)))
tab$Freq <- scales::percent(tab$Freq)
tablab <- paste(names(table(heart$Target)),tab$Freq,sep = "\n")
tab

par(mar = c(2,0,2,1))
pie(table(heart$Target), main="Pie Chart of Heart Disease", col = rainbow(4),
    labels = tablab,cex = 1.5)


#-----------------------------------------------------------------
# Analysis of at least one numerical variable
#-----------------------------------------------------------------
# Age Analysis
dev.off()
# # Set plot layout
nf <- layout(matrix(c(1,2),nrow=2), heights = c(3,2),TRUE)
layout.show(nf)
# define area for the histogram
par(mar = c(0,4,2,1))# c(3,4,1,1)
hist(heart$Age, xlab='', main='Distribution of Patient Ages', freq = F, col="blue",xaxt = "n")
lines(density(heart$Age), col="red", lwd=2)
abline(v=median(heart$Age), col="gray", lwd=2)
# define area for the boxplot
par(mar = c(4,4,0,1))# c(4,4,0,0)
boxplot(heart$Age, horizontal = T, col="blue", xlab= "Age (years)",frame = FALSE)
#  Statistical parameters for patient age
summary(heart$Age)
sd(heart$Age)
mean(heart$Age, trim = 0.1)

#  outliers
f1 <- fivenum(heart$Age)
c(f1[2]-1.5*(f1[4]-f1[2]),f1[4]+1.5*(f1[4]-f1[2]))

# Maximum Heart Rate Analysis
dev.off()
# # Set plot layout
nf <- layout(matrix(c(1,2),nrow=2), heights = c(3,2),TRUE)
layout.show(nf)
# define area for the histogram
par(mar = c(0,4.5,2,1))# c(3,4,1,1)
hist(heart$Thalach, xlab='', main='Distribution of Max. Heart Rate', freq = F, col="blue",xaxt = "n")
lines(density(heart$Thalach), col="red", lwd=2)
abline(v=median(heart$Thalach), col="gray", lwd=2)
# define area for the boxplot
par(mar = c(4,5,0,2))# c(4,4,0,0)
boxplot(heart$Thalach, horizontal = T, col="blue", xlab= "Maximum Heart Rate (bpm)",frame = FALSE)

#  Statistical parameters for patient Maximum Heart Rate
summary(heart$Thalach)
sd(heart$Thalach)
mean(heart$Thalach, trim = 0.1)

#  outliers
f1 <- fivenum(heart$Thalach)
(Int <- c(f1[2]-1.5*(f1[4]-f1[2]),f1[4]+1.5*(f1[4]-f1[2])))
heart[heart$Thalach<Int[1] |heart$Thalach>Int[2] ,]

#---------------------------------------------------------------------
# Analysis for at least one set of two or more variables
#---------------------------------------------------------------------

# Gender vs. Heart Disease
addmargins(table(heart$Target, heart$Gender))
prop.table(table(heart$Target, heart$Gender), margin = 1)


## Formula interface for tabulated data:
dev.off()
par(mar = c(0,2,3,0))
mosaicplot(~  Target+ Gender, data = heart,main='Gender vs. Heart Disease',
           color = rainbow(4),cex.axis=1.2)

dev.off()
# Age versus Maximum Heart Rate versus target
with(heart, plot(Age, Thalach, col=Target,pch=20, 
                 xlab = "Age (years)", ylab = "Maximum Heart Rate(bpm)",
                 main = "Age vs. Max. Heart Rate vs. target"))
abline(lm(Thalach~ Age, data=heart, subset = Target=="Haven't Disease"), col="black", lwd=2)
abline(lm(Thalach~ Age, data=heart, subset = Target=="Have Disease"), col="red", lwd=2)
legend("bottomleft",legend = c("no Disease", "Heart Disease"), col = c(1, 2),lty = c(-1, 1),
       pch = c(3, 4), bty="n")
legend("topright",
       legend =round(c(100*cor(heart$Thalach[heart$Target=="Haven't Disease"],heart$Age[heart$Target=="Haven't Disease"]), 
                             100*cor(heart$Thalach[heart$Target=="Have Disease"],
                                     heart$Age[heart$Target=="Have Disease"])),2),
       text.col = c(1, 2), bty="n")
#---------------------------------------------------------------------
# Pick one variable with numerical data and examine the distribution of the data
#---------------------------------------------------------------------

hist(heart$Thalach, main='Histogram of Max. Heart Rate', freq = F, col="blue",
     xlab= "Maximum Heart Rate (bpm)")
lines(density(heart$Thalach), col="red", lwd=2)
abline(v=median(heart$Thalach), col="gray", lwd=3)

summary(heart$Thalach)


#---------------------------------------------------------------------
# Draw various random samples of the data and show applicability of the Central Limit Theorem 
#for this Variable
#---------------------------------------------------------------------
(mean_population <- round(mean(heart$Thalach),2))
(sd_population <- round(sd(heart$Thalach),2))

sample10 <-sample30 <-sample50 <-sample100 <- list()
# sampling
for (i in 1:1000){
sample10[[i]] <- sample(x = heart$Thalach, size = 10, replace = FALSE)
sample30[[i]] <- sample(x = heart$Thalach, size = 30, replace = FALSE)
sample50[[i]] <- sample(x = heart$Thalach, size = 50, replace = FALSE)
sample100[[i]] <- sample(x = heart$Thalach, size = 100, replace = FALSE)
}
# sample means
sample10 <- rowMeans( matrix(unlist(sample10) ,nrow=10,ncol=1000, byrow = T))
sample30 <- rowMeans( matrix(unlist(sample30) ,nrow=30,ncol=1000, byrow = T))
sample50 <- rowMeans( matrix(unlist(sample50) ,nrow=50,ncol=1000, byrow = T))
sample100 <- rowMeans( matrix(unlist(sample100) ,nrow=100,ncol=1000, byrow = T))

par(mfrow=c(2,2), mar=c(4,3.6,1,1))
hist(sample10,  main='Sample Size = 10', xlab='xbar', col="blue", cex=0.03)
abline(v=mean(sample10), col="gray", lwd=3)
hist(sample30,  main='Sample Size = 30', xlab='xbar', col="blue")
abline(v=mean(sample30), col="gray", lwd=3)
hist(sample50,    main='Sample Size = 50', xlab='xbar', col="blue")
abline(v=mean(sample50), col="gray", lwd=3)
hist(sample100,    main='Sample Size = 100', xlab='xbar', col="blue")
abline(v=mean(sample100), col="gray", lwd=3)

dev.off()

cat("Population:", "Mean=",round(mean(heart$Thalach),2), "SD=",round(sd(heart$Thalach),2),
"\nSample Size = 10:", "Mean=",round(mean(sample10),2), "SD=",round(sd(sample10),2),
"\nSample Size = 30:", "Mean=",round(mean(sample30),2), "SD=",round(sd(sample30),2),
"\nSample Size = 50:", "Mean=",round(mean(sample50),2), "SD=",round(sd(sample50),2),
"\nSample Size = 100:", "Mean=",round(mean(sample100),2), "SD=",round(sd(sample100),2))

#---------------------------------------------------------------------
# Show how various sampling methods can be used on your data.
#What are your conclusions if these samples are used instead of the whole dataset
#---------------------------------------------------------------------


# Sampling method #1: Simple Random Sampling without Replacement
idx <- sample(1:nrow(heart), size=30, replace = FALSE)
sample1 <- heart[idx,]
table(sample1$Target);prop.table(table(sample1$Target))
table(heart$Target);prop.table(table(heart$Target))

cat("Population:", "Mean=",round(mean(heart$Thalach),2), "SD=",round(sd(heart$Thalach),2),
    "\nSample Size = 10:", "Mean=",round(mean(sample1$Thalach),2), "SD=",round(sd(sample1$Thalach),2))

# Sampling method #2: Systematic Sampling
(N <- nrow(heart))
n <- 30

# items in each group
k <- round(N/n)
r <- sample(k, 1)

# select every kth item
sequence <- seq(r, by=k, length=n); sequence
sample2 <- heart[sequence,]

table(sample2$Target);prop.table(table(sample2$Target))

cat("Population:", "Mean=",round(mean(heart$Thalach),2), "SD=",round(sd(heart$Thalach),2),
    "\nSample Size = 10:", "Mean=",round(mean(sample2$Thalach),2), "SD=",round(sd(sample2$Thalach),2))
