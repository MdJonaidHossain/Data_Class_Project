# install
#install.packages("ggplot2")
#install.packages("dplyr")

# library
library(ggplot2)
library(dplyr)
library(agricolae)

PG <- read.csv("data/PG.csv")

#Rename the Treatments
l <- c("T10","T1","T2","T3","T4","T5","T6","T7","T8","T9")
la <- c("Control","NPK100","CM100","Bio","CM50Bio","NPK50CM","NPK50Bio","NPK50CMBio","CM50","NPK50")
PG$Treat <-  factor(PG$Treat, levels = l, labels = la)
summary(PG)



###CGRc

#Making ANOVA
ANOVA <- aov(CGRc ~ Treat, data = PG)
summary(ANOVA)
anova(ANOVA)

#Plotting the ANOVA Result on BoxPlot
A <- summary(lm(CGRc ~ Treat, data = PG))
LO <- Ahsd$groups$groups
pcgrc1 <- plot(CGRc ~ Treat, data = PG)

#Tukay HSD
TukeyHSD(ANOVA)
Ahsd <- agricolae::HSD.test(ANOVA, "Treat", console = TRUE)
LO <- Ahsd$groups$groups
PG$Treat <- factor(PG$Treat, levels = row.names(Ahsd$groups))
pcgrc2 <- plot(CGRc ~ Treat, data = PG,main = "PG", ylim = c(15, 35)) 
text(x = 1:10, y = 35, LO, cex = 2, col = "red")

#LSD for
model<-aov(CGRc ~ Treat, data = PG)
out <- LSD.test(model,"Treat")
pcgrc3 <- plot(out)

#For Visualization
par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))

pcgrc1 <- plot(CGRc ~ Treat, data = PG, main = "ANOVA", ylim = c(15,35))
pcgrc2 <- plot(CGRc ~ Treat, data = PG,main = "Ahsd", ylim = c(15, 35))

###CGRSs

#Making ANOVA
ANOVA <- aov(CGRSs ~ Treat, data = PG)
summary(ANOVA)
anova(ANOVA)

#Plotting the ANOVA Result on BoxPlot
A <- summary(lm(CGRSs ~ Treat, data = PG))
LO <- Ahsd$groups$groups
pcgrSs1 <- plot(CGRSs ~ Treat, data = PG)

#Tukay HSD
TukeyHSD(ANOVA)
Ahsd <- agricolae::HSD.test(ANOVA, "Treat", console = TRUE)
LO <- Ahsd$groups$groups
PG$Treat <- factor(PG$Treat, levels = row.names(Ahsd$groups))
pcgrSs2 <- plot(CGRSs ~ Treat, data = PG,main = "PG", ylim = c(7, 10)) 
text(x = 1:10, y = 10, LO, cex = 2, col = "red")

#LSD for
model<-aov(CGRSs ~ Treat, data = PG)
out <- LSD.test(model,"Treat")
pcgrSs3 <- plot(out)

#For Visualization
par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))

pcgrSs1 <- plot(CGRSs ~ Treat, data = PG, main = "ANOVA", ylim = c(7, 10))
pcgrsS2 <- plot(CGRSs ~ Treat, data = PG,main = "Ahsd", ylim = c(7, 10))




###LAIc

#Making ANOVA
ANOVA <- aov(LAIc ~ Treat, data = PG)
summary(ANOVA)
anova(ANOVA)

#Plotting the ANOVA Result on BoxPlot
A <- summary(lm(LAIc ~ Treat, data = PG))
LO <- Ahsd$groups$groups
plaic1 <- plot(LAIc ~ Treat, data = PG)

#Tukay HSD
TukeyHSD(ANOVA)
Ahsd <- agricolae::HSD.test(ANOVA, "Treat", console = TRUE)
LO <- Ahsd$groups$groups
PG$Treat <- factor(PG$Treat, levels = row.names(Ahsd$groups))
plaic2 <- plot(LAIc ~ Treat, data = PG,main = "PG", ylim = c(1.5, 3)) 
text(x = 1:10, y = 3, LO, cex = 2, col = "red")

#LSD for
model<-aov(LAIc ~ Treat, data = PG)
out <- LSD.test(model,"Treat")
plaic3 <- plot(out)

#For Visualization
par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))

plaic1 <- plot(LAIc ~ Treat, data = PG, main = "ANOVA", ylim = c(1.5, 3))
plaic2 <- plot(LAIc ~ Treat, data = PG,main = "Ahsd", ylim = c(1.5, 3))



###LAIs

#Making ANOVA
ANOVA <- aov(LAIs ~ Treat, data = PG)
summary(ANOVA)
anova(ANOVA)

#Plotting the ANOVA Result on BoxPlot
A <- summary(lm(LAIs ~ Treat, data = PG))
LO <- Ahsd$groups$groups
plais1 <- plot(LAIs ~ Treat, data = PG)

#Tukay HSD
TukeyHSD(ANOVA)
Ahsd <- agricolae::HSD.test(ANOVA, "Treat", console = TRUE)
LO <- Ahsd$groups$groups
PG$Treat <- factor(PG$Treat, levels = row.names(Ahsd$groups))
plais2 <- plot(LAIs ~ Treat, data = PG,main = "PG", ylim = c(2.5, 3.5)) 
text(x = 1:10, y = 3.5, LO, cex = 2, col = "red")

#LSD for
model<-aov(LAIs ~ Treat, data = PG)
out <- LSD.test(model,"Treat")
plais3 <- plot(out)

#For Visualization
par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))

plais1 <- plot(LAIs ~ Treat, data = PG, main = "ANOVA", ylim = c(2.5, 3.5))
plais2 <- plot(LAIs ~ Treat, data = PG,main = "Ahsd", ylim = c(2.5, 3.5))




###PHc

#Making ANOVA
ANOVA <- aov(PHc ~ Treat, data = PG)
summary(ANOVA)
anova(ANOVA)

#Plotting the ANOVA Result on BoxPlot
A <- summary(lm(PHc ~ Treat, data = PG))
LO <- Ahsd$groups$groups
pphc1 <- plot(PHc ~ Treat, data = PG)

#Tukay HSD
TukeyHSD(ANOVA)
Ahsd <- agricolae::HSD.test(ANOVA, "Treat", console = TRUE)
LO <- Ahsd$groups$groups
PG$Treat <- factor(PG$Treat, levels = row.names(Ahsd$groups))
pphc2 <- plot(PHc ~ Treat, data = PG,main = "PG", ylim = c(125, 200)) 
text(x = 1:10, y = 200, LO, cex = 2, col = "red")

#LSD for
model<-aov(PHc ~ Treat, data = PG)
out <- LSD.test(model,"Treat")
pphc3 <- plot(out)

#For Visualization
par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))

pphc1 <- plot(PHc ~ Treat, data = PG, main = "ANOVA", ylim = c(125, 200))
pphc2 <- plot(PHc ~ Treat, data = PG,main = "Ahsd", ylim = c(125, 200))



###PHs

#Making ANOVA
ANOVA <- aov(PHs ~ Treat, data = PG)
summary(ANOVA)
anova(ANOVA)

#Plotting the ANOVA Result on BoxPlot
A <- summary(lm(PHs ~ Treat, data = PG))
LO <- Ahsd$groups$groups
pphs1 <- plot(PHs ~ Treat, data = PG)

#Tukay HSD
TukeyHSD(ANOVA)
Ahsd <- agricolae::HSD.test(ANOVA, "Treat", console = TRUE)
LO <- Ahsd$groups$groups
PG$Treat <- factor(PG$Treat, levels = row.names(Ahsd$groups))
pphs2 <- plot(PHs ~ Treat, data = PG,main = "PG", ylim = c(60, 120)) 
text(x = 1:10, y = 120, LO, cex = 2, col = "red")

#LSD for
model<-aov(PHs ~ Treat, data = PG)
out <- LSD.test(model,"Treat")
pphs3 <- plot(out)

#For Visualization
par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))

pphs1 <- plot(PHs ~ Treat, data = PG, main = "ANOVA", ylim = c(60, 120))
pphs2 <- plot(PHs ~ Treat, data = PG,main = "Ahsd", ylim = c(60, 120))


###FYc

#Making ANOVA
ANOVA <- aov(FYc ~ Treat, data = PG)
summary(ANOVA)
anova(ANOVA)

#Plotting the ANOVA Result on BoxPlot
A <- summary(lm(FYc ~ Treat, data = PG))
LO <- Ahsd$groups$groups
pfyc1 <- plot(FYc ~ Treat, data = PG)

#Tukay HSD
TukeyHSD(ANOVA)
Ahsd <- agricolae::HSD.test(ANOVA, "Treat", console = TRUE)
LO <- Ahsd$groups$groups
PG$Treat <- factor(PG$Treat, levels = row.names(Ahsd$groups))
pfyc2 <- plot(FYc ~ Treat, data = PG,main = "PG", ylim = c(13, 35)) 
text(x = 1:10, y = 35, LO, cex = 2, col = "red")

#LSD for
model<-aov(FYc ~ Treat, data = PG)
out <- LSD.test(model,"Treat")
pfyc3 <- plot(out)

#For Visualization
par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))

pfyc1 <- plot(FYc ~ Treat, data = PG, main = "ANOVA", ylim = c(13, 35))
pfyc2 <- plot(FYc ~ Treat, data = PG,main = "Ahsd", ylim = c(13, 35))



###FYs

#Making ANOVA
ANOVA <- aov(FYs ~ Treat, data = PG)
summary(ANOVA)
anova(ANOVA)

#Plotting the ANOVA Result on BoxPlot
A <- summary(lm(FYs ~ Treat, data = PG))
LO <- Ahsd$groups$groups
pfys1 <- plot(FYs ~ Treat, data = PG)

#Tukay HSD
TukeyHSD(ANOVA)
Ahsd <- agricolae::HSD.test(ANOVA, "Treat", console = TRUE)
LO <- Ahsd$groups$groups
PG$Treat <- factor(PG$Treat, levels = row.names(Ahsd$groups))
pfys2 <- plot(FYs ~ Treat, data = PG,main = "PG", ylim = c(13, 35)) 
text(x = 1:10, y = 35, LO, cex = 2, col = "red")

#LSD for
model<-aov(FYs ~ Treat, data = PG)
out <- LSD.test(model,"Treat")
pfys3 <- plot(out)

#For Visualization
par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))

pfys1 <- plot(FYs ~ Treat, data = PG, main = "ANOVA", ylim = c(13, 35))
pfys2 <- plot(FYs ~ Treat, data = PG,main = "Ahsd", ylim = c(13, 35))



###FYmix

#Making ANOVA
ANOVA <- aov(FYmix ~ Treat, data = PG)
summary(ANOVA)
anova(ANOVA)

#Plotting the ANOVA Result on BoxPlot
A <- summary(lm(FYmix ~ Treat, data = PG))
LO <- Ahsd$groups$groups
pfymix1 <- plot(FYmix ~ Treat, data = PG)

#Tukay HSD
TukeyHSD(ANOVA)
Ahsd <- agricolae::HSD.test(ANOVA, "Treat", console = TRUE)
LO <- Ahsd$groups$groups
PG$Treat <- factor(PG$Treat, levels = row.names(Ahsd$groups))
pfymix2 <- plot(FYmix ~ Treat, data = PG,main = "PG", ylim = c(20, 60)) 
text(x = 1:10, y = 60, LO, cex = 2, col = "red")

#LSD for
model<-aov(FYmix ~ Treat, data = PG)
out <- LSD.test(model,"Treat")
pfymix3 <- plot(out)

#For Visualization
par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))

pfymix1 <- plot(FYmix ~ Treat, data = PG, main = "ANOVA", ylim = c(20, 60))
pfymix2 <- plot(FYmix ~ Treat, data = PG,main = "Ahsd", ylim = c(20, 60))



###DMc

#Making ANOVA
ANOVA <- aov(DMc ~ Treat, data = PG)
summary(ANOVA)
anova(ANOVA)

#Plotting the ANOVA Result on BoxPlot
A <- summary(lm(DMc ~ Treat, data = PG))
LO <- Ahsd$groups$groups
pdmc1 <- plot(DMc ~ Treat, data = PG)

#Tukay HSD
TukeyHSD(ANOVA)
Ahsd <- agricolae::HSD.test(ANOVA, "Treat", console = TRUE)
LO <- Ahsd$groups$groups
PG$Treat <- factor(PG$Treat, levels = row.names(Ahsd$groups))
pdmc2 <- plot(DMc ~ Treat, data = PG,main = "PG", ylim = c(2.5, 8)) 
text(x = 1:10, y = 8, LO, cex = 2, col = "red")

#LSD for
model<-aov(DMc ~ Treat, data = PG)
out <- LSD.test(model,"Treat")
pdmc3 <- plot(out)

#For Visualization
par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))

pdmc1 <- plot(DMc ~ Treat, data = PG, main = "ANOVA", ylim = c(2.5, 8))
pdmc2 <- plot(DMc ~ Treat, data = PG,main = "Ahsd", ylim = c(2.5, 8))


# Groupeed for the BoxPlot

PG$Treat <- factor(PG$Treat, levels = c("Control",
                                        "Bio","CM50","CM50Bio",
                                        "NPK50Bio","NPK50","CM100",
                                        "NPK50CM","NPK100","NPK50CMBio"))

#DMc for Corn Dry Matter

ggplot(PG, aes(x=Treat, y=DMc, levels = row.names(Ahsd$groups$groups ))) + 
  geom_boxplot() + 
  text(x = 1:10, y = 8, LO, cex = 2, col = "Black")
  theme_bw()




#DMs for Soybean Dry Matter

ggplot(PG, aes(x=Treat, y=DMs )) + 
  geom_boxplot() +
  theme_bw()


#DMmix for Total Dry Matter
ggplot(PG, aes(x=Treat, y=DMmix )) + 
  geom_boxplot() +
  theme_bw()

#For Visualization 














#Principal Component Analysis
if(!require(devtools)) install.packages("factoextra")

library(factoextra)
