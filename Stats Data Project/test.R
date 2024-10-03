GroupData <- read.csv("t4data.csv", header = TRUE, sep = ",")
data <- read.csv("practice.csv", header = TRUE, sep = ",")

cor(GroupData$day, GroupData$weight)

fit <- lm(GroupData$weight~GroupData$day)
fit
summary(fit)

lm(formula = GroupData$weight~GroupData$day)

plot(GroupData$day, GroupData$weight, xlab = "Day", ylab = "Weight", main = "Scatterplot")
abline(fit, col=2, lwd=3)

par(mfrow=c(1,2))
plot(GroupData$day, fit$residuals)
plot(fit$fitted.values, fit$residuals)

qqnorm(fit$residuals)
qqline(fit$residuals)


par(mfrow=c(2,2))
plot(fit)

#  Adding  the third variable crop using different colors
plot(GroupData$day[GroupData$crop == 'A'], GroupData$weight[GroupData$crop == 'A'], pch=1, col="red", ylim=c(0,10), xlim=c(3,10), xlab = "Day", ylab = "Weight", main = "Weight by Day and Crop Group")
points(GroupData$day[GroupData$crop =='B'], GroupData$weight[GroupData$crop == 'B'], pch=3, col="blue")
legend("bottomright", legend=c("A", "B"), pch=c(1,3), col=c("red", "blue"), cex=0.8)


plot(GroupData$day['crop' == 'A'], GroupData$weight['crop' == 'A'], pch=1, col="red", ylim=c(0,10), xlim=c(3,10))
points(GroupData$day['crop' == 'B'], GroupData$weight['crop' == 'B'], pch=1, col="blue")

fit2 <- lm(GroupData$height~GroupData$day+factor(GroupData$crop))
summary(fit2)

fit3 <- lm(GroupData$height~GroupData$day*factor(GroupData$crop))
summary(fit3)
#
tapply(GroupData$weight,GroupData$crop, mean)
tapply(GroupData$weight,GroupData$crop, sd)

anova1 <- aov(GroupData$weight~GroupData$crop)
anova1

summary(anova1)

qqnorm(anova1$residuals)
qqline(anova1$residuals)

par(mfrow=c(1,2))
qqnorm(GroupData$weight[GroupData$crop=="A"])
qqnorm(GroupData$weight[GroupData$crop=="B"])

stripchart(anova1$residuals~GroupData$crop, vertical=TRUE)

par(mfrow=c(2,2))
plot(anova1)

pairwise.t.test(GroupData$weight, GroupData$crop)
pairwise.t.test(GroupData$weight, GroupData$day)



TukeyHSD(anova1)

TukeyHSD(anova1, conf.level = 0.90)

plot(TukeyHSD(anova1), las=1)

plot(TukeyHSD(anova1))

kruskal.test(GroupData$weight~GroupData$crop)
t.test(GroupData$weight~ GroupData$crop, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)

t.test(GroupData$height~GroupData$crop, alt="two.sided",  mu=0, conf=0.95, var.eq=F, paired=F)

#-----------------------------------------------------------------------------------------------

#Total number of A items
total_A <- nrow(GroupData[(GroupData$crop=="A"),])
#Total number of B items
total_B <- nrow(GroupData[(GroupData$crop=="B"),])
#Total number of A items with height >= 10
gl10_A <- nrow(GroupData[(GroupData$crop=="A" & GroupData$height >=10),])
#Total number of B items with height >= 10
gl10_B <- nrow(GroupData[(GroupData$crop=="B" & GroupData$height >=10),])

matrixGL10 <- matrix(c(gl10_A, total_A-gl10_A, gl10_B, total_B-gl10_B), ncol=2)
rownames(matrixGL10) <- c("A", "B")
colnames(matrixGL10) <- c("Height>=10", "Height<10" )
matrixGL10
barplot(prop.table(matrixGL10, margin = 2), beside=FALSE, ylim=c(0,1), legend = rownames(matrixGL10), 
        main="Height proportions >=10 and <10", ylab="proportion", cex.names = 0.9 )

prop.test(c(gl10_A, gl10_B ), c(total_A-gl10_A, total_B-gl10_B))


