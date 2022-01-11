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

##### BASIC PLOTS ---------------------------------
# plot.data.frame() and pairs() will output same results.  

#plot(PG [3:28], col = PG$Treat)  # Colored by Treatments Data frame. All variables as.numeric


#### tiff plot ------------------------------------------
# tiff("Results/PGplot.tiff", width = 16, height = 9, units = "in", 
#       res = 300)       # Open the device "PGplot.tiff"
# plot(PG [3:28], col = PG$Treat)      # make the plot into the device
# dev.off()              # Close the device. Do not forget to put the ()
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
     res = 300)       # Open the device "Percentage of Explained Variance.tiff"
fviz_eig(pca_res)      # make the plot into the device
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
     res = 300)       # Open the device "PCA Biplot.tiff"
fviz_pca_biplot(pca_res, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)                                    # make the plot into the device
dev.off()              # Close the device. Do not forget to put the ()
#### See the plot into the working directory. getwd() to see where.

### PCA Biplot Another Way

df.pca <- prcomp(df, scale. = TRUE)
ggbiplot(df.pca, obs.scale = 1, var.scale = 1, groups = PG$Treat , ellipse = TRUE, circle = TRUE)


#### tiff plot ------------------------------------------
tiff("Results/PCA Biplot2.tiff", width = 16, height = 9, units = "in", 
     res = 300)       # Open the device "PCA Biplot2.tiff"
ggbiplot(df.pca, obs.scale = 1, var.scale = 1, groups = PG$Treat , ellipse = TRUE, circle = TRUE) # make the plot into the device
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
     res = 300)       # Open the device "Variables PCA.tiff"
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
  labs(title = "Boxplot of the Quality Data") +
  theme_bw()

#### tiff plot ------------------------------------------
tiff("Results/Boxplot on Quality.tiff", width = 16, height = 9, units = "in", 
     res = 300)       # Open the device "Boxplot on Quality.tiff"
### Performing the Boxplot on Quality
ggplot(data = PG, aes(x = Treat, y = Quality_pca, fill = Treat
)) +
  geom_boxplot()+
  geom_path()+
  geom_violin()+
  geom_jitter(size = 2, color = "black" ) +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(title = "Boxplot of the Quality Data") +
  theme_bw()
                            # make the plot into the device
dev.off()              # Close the device. Do not forget to put the ()
#### See the plot into the working directory. getwd() to see where.


# Tukay HSD & Box Plot of the Variables

###CGRc
Variable <- "CGRc"
source("R/00_Loop.R")

###CGRSs
Variable <- "CGRSs"
source("R/00_Loop.R")

###LAIc
Variable <- "LAIc"
source("R/00_Loop.R")

###LAIs
Variable <- "LAIs"
source("R/00_Loop.R")

###PHc
Variable <- "PHc"
source("R/00_Loop.R")

###PHs
Variable <- "PHs"
source("R/00_Loop.R")

###FYc
Variable <- "FYc"
source("R/00_Loop.R")

###FYs
Variable <- "FYs"
source("R/00_Loop.R")

###FYmix
Variable <- "FYmix"
source("R/00_Loop.R")

###DMc
Variable <- "DMc"
source("R/00_Loop.R")

###DMs
Variable <- "DMs"
source("R/00_Loop.R")

###DMmix
Variable <- "DMmix"
source("R/00_Loop.R")

###CPmix
Variable <- "CPmix"
source("R/00_Loop.R")

###NDFmix
Variable <- "NDFmix"
source("R/00_Loop.R")

###ADFmix
Variable <- "ADFmix"
source("R/00_Loop.R")

###DMDmix
Variable <- "DMDmix"
source("R/00_Loop.R")

###WSCmix
Variable <- "WSCmix"
source("R/00_Loop.R")

###ADLmix
Variable <- "ADLmix"
source("R/00_Loop.R")

###ProteinYmix
Variable <- "ProteinYmix"
source("R/00_Loop.R")

###PH
Variable <- "PH"
source("R/00_Loop.R")

###DM
Variable <- "DM"
source("R/00_Loop.R")

###Lactic
Variable <- "Lactic"
source("R/00_Loop.R")

###Acetic
Variable <- "Acetic"
source("R/00_Loop.R")

###Propionic
Variable <- "Propionic"
source("R/00_Loop.R")

###Butyric
Variable <- "Butyric"
source("R/00_Loop.R")

###AmmoniaN
Variable <- "AmmoniaN"
source("R/00_Loop.R")

###Quality_pca
Variable <- "Quality_pca"
source("R/00_Loop.R")

## Correlation test
cor.test(PG$ADFmix, PG$NDFmix)
plot(PG$ADFmix, PG$NDFmix, col= PG$Treat) 



# D.CGRc <- PG %>% 
# group_by(Treat) %>% 
#   summarise(mean.CGRc = mean(CGRc), 
#             sd.CGRC= sd(CGRc))
# 
# ggplot(D.CGRc, aes(x = Treat, y = mean.CGRc), fill = Treat) +
#          geom_col()




