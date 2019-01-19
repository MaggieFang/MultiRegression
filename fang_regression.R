folder <- "/Users/xfang7/Google\ Drive/Courses/CSC591-004/hw/project/regression/"
data <- read.csv(paste(folder,"xfang7.csv", sep=''),header = FALSE)
x1 <- data[[1]]
x2 <- data[[2]]
x3 <- data[[3]]
x4 <- data[[4]]
x5 <- data[[5]]
y <- data[[6]]
names(data) <- c("x1", "x2","x3","x4","x5","y")

pdf("result.pdf")
hist(x1)
hist(x2)
hist(x3)
hist(x4)
hist(x5)
dev.off()

mean(x1)
var(x1)

mean(x2)
var(x2)

mean(x3)
var(x3)

mean(x4)
var(x4)

mean(x5)
var(x5)


# task 1.2
r <- cor(data)
round(r,4)

# task 1.3
png("x1.png")
boxplot(x1,horizontal = T,main="Boxplot of x1",xlab ="x1",col=("red"))
dev.off()
sink("outlier",append= T,split = T)
summary(x1)
sink()


png("x2.png")
boxplot(x2,horizontal = T,main="Boxplot of x2",xlab ="x2",col=("red"))
dev.off()
sink("outlier",append= T,split = T)
summary(x2)
sink()

png("x3.png")
boxplot(x3,horizontal = T,main="Boxplot of x3",xlab ="x3",col=("red"))
dev.off()
sink("outlier",append= T,split = T)
summary(x3)
sink()

png("x4.png")
boxplot(x4,horizontal = T,main="Boxplot of x4",xlab ="x4",col=("red"))
dev.off()
sink("outlier",append= T,split = T)
summary(x4)
sink()

png("x5.png")
boxplot(x5,horizontal = T,main="Boxplot of x5",xlab ="x5",col=("red"))
dev.off()
sink("outlier",append= T,split = T)
summary(x5)
sink()

# Task 2
slr.lm <- lm(y~x1)
summary(slr.lm)
summary(slr.lm)$coefficients[,4] # p values
summary(slr.lm)$r.square

png("slr.png")
plot(x1,y,xlab = "x1",ylab = "y")
abline(slr.lm)
dev.off()

# slr.res <- resid(slr.lm)
# png("res.png")
# plot(density(slr.res))
# qqnorm(slr.res)
# qqline(slr.res)
# dev.off()

slr.stdres <- rstandard(slr.lm) #compute the standardized residual 
png("qqplot.png")
qqnorm(slr.stdres) 
qqline(slr.stdres)
dev.off()


png("resHis.png")
hist(slr.stdres,main = "Histogram of Normal residual", xlab = "Normal residual")
dev.off()

png("resPlot.png")
plot(slr.stdres,main = "Plot of Normal residual", ylab = "Normal residual",xlab = "")
# abline(0,0)
dev.off()

slr.res <- resid(slr.lm)
ks.test(slr.res,"pnorm",mean(slr.res),sd(slr.res)) # carry out chi square test
model <- lm(y~ x1+I(x1^2))
summary(model)

mlr <- lm(y~x1+x2+x3+x4+x5)
(summary(mlr)$sigma)**2

mlrAdjust <- lm(y~x1+x3+x4+x5)
plot(mlrAdjust)
mlrAdjust.stdres <- rstandard(mlrAdjust) #compute the standardized residual 
png("mlrAdjustResHis.png")
hist(mlrAdjust.stdres,main = "Histogram of Normal residual", xlab = "Normal residual")
dev.off()

mlrAdjust.res <- resid(mlrAdjust)
ks.test(mlrAdjust.res,"pnorm",mean(mlrAdjust.res),sd(mlrAdjust.res))




