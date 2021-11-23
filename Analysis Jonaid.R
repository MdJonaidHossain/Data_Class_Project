read.csv("data/PG.csv")

summary(PG)

cor(Treat,PH)

ANOVA <- aov(Treat ~ CGRc, data = PG)