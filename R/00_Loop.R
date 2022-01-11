#### FUNCTIONS Activates all functions in /R folder#############################
### Date: 21122021   Author: Md Jonaid Hossain
################################################################################

#### LIBRARIES ####
#source("R/00_Libraries.R")


### This is the Loop Source code for the Tukay HSD test, ANOVA, LSD

## Forming a data set only with the Numerical Variables
d0 <- PG[, c(which(names(PG)==Variable), which(names(PG)=="Treat"))]

##Naming the "Treat" with the Source code functionn
names(d0)[1] <- "X0"


## Making ANOVA
ANOVA <- aov(X0 ~ Treat, data = d0)
summary(ANOVA)
anova(ANOVA)


## Tukay HSD
TukeyHSD(ANOVA)
THSD <- agricolae::HSD.test(ANOVA, "Treat", console = TRUE)

LO <- THSD$groups$groups
tr <- row.names(THSD$groups)
d1 <- data.frame(Treat = tr, l0 = LO)

d0$Treat <- factor(d0$Treat, levels = row.names(THSD$groups))

Y0 <- max(d0$X0) + max(d0$X0)/20
# plot(X0 ~ Treat, data = d0, ylab = Variable, ylim = c(min(d0$X0), max(d0$X0) + max(d0$X0)/20))
# text(x = 1:10, y = Y0, LO, cex = 2, col = "blue")
# labs(title = paste("Tukay HSD for the ", Variable,"Variable"))
# THSDplot <- recordPlot()


## Box Plot for the Variable
LO <- THSD$groups$groups
tr <- row.names(THSD$groups)
d1 <- data.frame(Treat = tr, l0 = LO)

KillMe <- ggplot(d0, aes(x = Treat, y = X0, fill = Treat)) +
    geom_boxplot() +
    geom_text(data = d1, aes(y = Y0, label = l0)) + 
    labs(title = paste("Box Plot for the ", Variable,"Variable"), y = Variable)
  
print(KillMe)

ggsave(paste0("Results/Boxplot of ", Variable , ".png"), width = 16, height = 10, units = "in")

#### tiff plot ------------------------------------------
# tiff(paste0("~Results/BoxPlot of ", Variable, ".tiff"), width = 16, height = 9, units = "in", 
#      res = 300)       # Open the device ".tiff"
# ### Performing the Boxplot on Quality
# ###par(mfrow = c(1, 2), oma = c(0, 0, 1.1, 0))
# 
# ####plot(X0 ~ Treat, data = d0,main = "Tukay HSD", ylab = Variable, ylim = c(min(d0$X0), max(d0$X0) + max(d0$X0)/20))
# ####text(x = 1:10, y = Y0, LO, cex = 2, col = "blue")
# 
# ggplot(d0, aes(x = Treat, y = X0)) +
#   geom_boxplot() +
#   geom_text(data = d1, aes(y = Y0, label = l0)) + 
#   labs(title = paste("Box Plot for the ", Variable,"Variable"))
# 
# 
# # make the plot into the device
# dev.off()              # Close the device. Do not forget to put the ()
#### See the plot into the working directory. getwd() to see where.









