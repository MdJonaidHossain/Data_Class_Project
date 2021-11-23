if (!require("ggbiplot", character.only = T)) install_github("ggbiplot", "vqv")
library(ggbiplot)
library(readxl)

if(!dir.exists("data")) dir.create("data")

download.file("https://dfzljdn9uc3pi.cloudfront.net/2018/5280/1/Plant_Growth.xlsx", "data/Plant_Growth.xlsx")
download.file("https://dfzljdn9uc3pi.cloudfront.net/2018/5280/1/Forage_quality.xlsx", "data/Forage_nutritive_quality.xlsx")
download.file("https://dfzljdn9uc3pi.cloudfront.net/2018/5280/1/VFA.xlsx","data/Silage volatile fatty acids")


PlantGrowth <- read_excel("data/Plant_Growth.xlsx", range = "A2:N42" , col_names = TRUE)
FNQ <- read_excel("data/Forage_nutritive_quality.xlsx", range = "A2:I42", col_names = TRUE)
VFA <- read_excel("data/Silage volatile fatty acids", range = "A2:J42", col_names = TRUE)

PG <- merge(PlantGrowth,FNQ, by=c("Rep", "Treat"))
PG <- merge(PG,VFA[-which(names(VFA) == "Total")], by=c("Rep", "Treat"))
View(PG)

write.csv(PG, file = "data/PG.csv")


