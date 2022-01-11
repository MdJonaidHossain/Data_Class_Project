################################################################################
### Date: 21122021   Author: Md Jonaid Hossain
###
###  Working script for Assignment
################################################################################
## ----SETUP, include=FALSE------------------------------------------------
#### LIBRARIES ####
source("R/00_Libraries.R")

### Install ggbiplot for plotting biplots with ggplot
if (!require("ggbiplot")) remotes::install_github("vqv/ggbiplot")
library(ggbiplot)



## ----Import data frame from excel, cache=FALSE---------------------------
### cache = FALSE for changes in original data

### Look for xlsx archives in working directory
# list.files(pattern = "\\.xlsx$")

##Create a folder for the Raw Data
if(!dir.exists("data")) dir.create("data")


### Download the File from the Journal's Supplemental Information

#Determination of ethylene production by gas chromatography
download.file("https://dfzljdn9uc3pi.cloudfront.net/2018/5280/1/Plant_Growth.xlsx", "data/Plant_Growth.xlsx")
#Forage nutritive quality
download.file("https://dfzljdn9uc3pi.cloudfront.net/2018/5280/1/Forage_quality.xlsx", "data/Forage_nutritive_quality.xlsx")
#Silage volatile fatty acids (VFA)
download.file("https://dfzljdn9uc3pi.cloudfront.net/2018/5280/1/VFA.xlsx", "data/Silage_volatile_fatty_acids.xlsx")

### Better with readxl::read_xlsx (no Java needed) + Omitting the NonRequired data for the Analysis
PlantGrowth <- read_excel("data/Plant_Growth.xlsx", range = "A2:N42" , col_names = TRUE)
FNQ <- read_excel("data/Forage_nutritive_quality.xlsx", range = "A2:I42", col_names = TRUE)
VFA <- read_excel("data/Silage_volatile_fatty_acids.xlsx", range = "A2:J42", col_names = TRUE)

### Organizing & Merging the Data into PG data frame
PG <- merge(PlantGrowth,FNQ, by=c("Rep", "Treat"))
PG <- merge(PG,VFA[-which(names(VFA) == "Total")], by=c("Rep", "Treat"))


###Renaming the Treatments for Better Understanding
l <- c("T10","T1","T2","T3","T4","T5","T6","T7","T8","T9")
la <- c("Control","NPK100","CM100","Bio","CM50Bio","NPK50CM","NPK50Bio","NPK50CMBio","CM50","NPK50")
PG$Treat <-  factor(PG$Treat, levels = l, labels = la)

### Reordering the Treatments for Better Understanding of the Result as well as Grouping
PG$Treat <- factor(PG$Treat, levels = c("Control",
                                        "Bio","CM50","CM50Bio",
                                        "NPK50Bio","NPK50","CM100",
                                        "NPK100","NPK50CM","NPK50CMBio"))

## To View the Data 
View(PG)

##### BASIC PLOTS WITH IRIS ---------------------------------
# plot.data.frame() and pairs() will output same results.  

plot(PG [3:28], col = PG$Treat)  # Colored by Treatments Data frame. All variables as.numeric


#### tiff plot ------------------------------------------
tiff("Results/PGplot.tiff", width = 16, height = 9, units = "in", 
      res = 300)       # Open the device "Plot1.tiff"
plot(PG [3:28], col = PG$Treat)      # make the plot into the device
dev.off()              # Close the device. Do not forget to put the ()
#### See the plot into the working directory. getwd() to see where.


## To save the Data in .csv format
write.csv(PG, file = "data/PG.csv")









#Principal Component Analysis

df <- PG[3:28]
pca_res <- prcomp(df, scale. = TRUE)

### Percentage of Explained Variance
fviz_eig(pca_res)  

#### tiff plot ------------------------------------------
tiff("Results/Percentage of Explained Variance.tiff", width = 16, height = 9, units = "in", 
     res = 300)       # Open the device "Plot1.tiff"
fviz_eig(pca_res)      # make the plot into the device
dev.off()              # Close the device. Do not forget to put the ()
#### See the plot into the working directory. getwd() to see where.



### Cluster Formation of the Data PC1 & PC2

autoplot(pam(PG, 10), frame = TRUE, frame.type = 'norm')

#### tiff plot ------------------------------------------
tiff("Results/Cluster of PCA Data.tiff", width = 16, height = 9, units = "in", 
     res = 300)       # Open the device "Plot1.tiff"
autoplot(pam(PG, 10), frame = TRUE, frame.type = 'norm')      # make the plot into the device
dev.off()              # Close the device. Do not forget to put the ()
#### See the plot into the working directory. getwd() to see where.


### Showing all the PCA 
pca_res

###PCA1 showa all the Variance
pca_res$rotation[,1]


### PCA Biplot
fviz_pca_biplot(pca_res, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

#### tiff plot ------------------------------------------
tiff("Results/PCA Biplot.tiff", width = 16, height = 9, units = "in", 
     res = 300)       # Open the device ".tiff"
fviz_pca_biplot(pca_res, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)                                    # make the plot into the device
dev.off()              # Close the device. Do not forget to put the ()
#### See the plot into the working directory. getwd() to see where.

### PCA Biplot Another Way

df.pca <- prcomp(df, scale. = TRUE)
ggbiplot(df.pca, obs.scale = 1, var.scale = 1, groups = , ellipse = TRUE, circle = TRUE)


#### tiff plot ------------------------------------------
tiff("Results/PCA Biplot2.tiff", width = 16, height = 9, units = "in", 
     res = 300)       # Open the device ".tiff"
ggbiplot(df.pca, obs.scale = 1, var.scale = 1, groups = , ellipse = TRUE, circle = TRUE) # make the plot into the device
dev.off()              # Close the device. Do not forget to put the ()
#### See the plot into the working directory. getwd() to see where.




### Variables PCA
fviz_pca_var(pca_res,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#### tiff plot ------------------------------------------
tiff("Results/Variables PCA.tiff", width = 16, height = 9, units = "in", 
     res = 300)       # Open the device "Plot1.tiff"
fviz_pca_var(pca_res,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
                              # make the plot into the device
dev.off()              # Close the device. Do not forget to put the ()
#### See the plot into the working directory. getwd() to see where.



### Adding the Quality Numerical to the PG Dataframe
PG$Quality_pca <- pca_res$x[,1]

## To View the Data 
View(PG)


### Performing the Boxplot on Quality
ggplot(data = PG, aes(x = Treat, y = Quality_pca, fill = Treat
)) +
  geom_boxplot()+
  geom_path()+
  geom_violin()+
  geom_jitter(size = 2, color = "black" ) +
  geom_smooth(method = "lm", formula = y ~ x) +
  theme_bw()

#### tiff plot ------------------------------------------
tiff("Results/Boxplot on Quality.tiff", width = 16, height = 9, units = "in", 
     res = 300)       # Open the device "Plot1.tiff"
### Performing the Boxplot on Quality
ggplot(data = PG, aes(x = Treat, y = Quality_pca, fill = Treat
)) +
  geom_boxplot()+
  geom_path()+
  geom_violin()+
  geom_jitter(size = 2, color = "black" ) +
  geom_smooth(method = "lm", formula = y ~ x) +
  theme_bw()
                            # make the plot into the device
dev.off()              # Close the device. Do not forget to put the ()
#### See the plot into the working directory. getwd() to see where.


















##autoplot(clara(PG, Treat))







#












































###DMc
Variable <- "DMc"
source("R/00_Loop.R")







































###DMc
Variable <- "DMc"
source("R/00_Loop.R")

Variable <- "DMs"
source("Loop.R")





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
pcgrc2 <- plot(CGRc ~ Treat, data = PG,main = "PG", ylim = c(15, 30)) 
text(x = 1:10, y = 30, LO, cex = 2, col = "red")

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
par(mfrow = c(1, 2), oma = c(0, 0, 1.1, 0))

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







d0 <- PG[, c(which(names(PG)==Variable), which(names(PG)=="Treat"))]
names(d0)[1] <- "x"

#Making ANOVA
ANOVA <- aov(Variable ~ Treat, data = PG)
summary(ANOVA)
anova(ANOVA)

#Plotting the ANOVA Result on BoxPlot
A <- summary(lm(x ~ Treat, data = d0))
LO <- Ahsd$groups$groups
pdmc1 <- plot(x ~ Treat, data = d0, ylab = Variable)

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
mpg %>%
  mutate(class = fct_reorder(Treat, DMs, .fun='median')) %>%
  ggplot(aes(x=reorder(Treat, DMs), y=DMs, fill=class)) + 
  geom_boxplot() +
  xlab("class") +
  theme(legend.position="none") +
  xlab("")


#DMmix for Total Dry Matter
ggplot(PG, aes(x=Treat, y=DMmix )) + 
  geom_boxplot() +
  theme_bw()



p <- ggplot(df, aes(x=Treat)) + 
  geom_density()
p

p <- ggplot(PG, aes(x=Treat, y=AmmoniaN )) + 
  geom_density()
  


#For Visualization 


d0 <- PG[, c(which(names(PG)==Variable), which(names(PG)=="Treat"))]
# Using median
# Using median
mpg %>%
  mutate(class = fct_reorder(Treat, DMs, .fun='median')) %>%
  ggplot(aes(x=reorder(Treat, DMs), y=DMs, fill=class)) + 
  geom_boxplot() +
  xlab("class") +
  theme(legend.position="none") +
  xlab("")


# reorder is close to order, but is made to change the order of the factor levels.
mpg$class = with(mpg, reorder(class, hwy, median))

p <- mpg %>%
  ggplot( aes(x=class, y=hwy, fill=class)) + 
  geom_violin() +
  xlab("class") +
  theme(legend.position="none") +
  xlab("")
#p




pairs(PG)






for (i in 2:29) {
  model<-aov(Treat,   data = PG)
  out <- LSD.test(model,"Treat")
  
}
