t.test(1:20, alternative = "two.sided", mu = 10, conf.level = 0.95)
summary(aov(weight ~ feed, data = chickwts))
TukeyHSD(aov(weight ~ feed, data = chickwts))
boxplot(TukeyHSD(aov(weight ~ feed, data = chickwts)))
model.tables(aov(weight ~ feed, data = chickwts))
table(survey$Exer,survey$Smoke)
chisq.test(table(survey$Exer,survey$Smoke))
chisq.test(table(survey$Height,survey$Pulse))
wilcox.test(1:10,2:20, correct = FALSE)
kruskal.test(survey$Height ~ survey$Pulse)
wilcox.test(c(2730,1350,565,280,140,69,42),c(3500,17500,875,450,235,140,77,43),correct = FALSE)
ks.test(1:10,1:10)
ks.test(runif(1:100),runif(1:100))
shapiro.test(rnorm(100))











