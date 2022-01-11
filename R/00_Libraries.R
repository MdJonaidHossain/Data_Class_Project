#### LIBRARIES #################################################################
### Date: 21122021   Author: Md Jonaid Hossain
################################################################################


### List libraries to activate
LIB <- c("knitr"            # To knit the document in any format
        , "tools"          # For security copies
        , "plotrix"         # for standard error: std.error()
        , "readxl"          # Idem
        , "writexl"         # same than xlsx but without Java problems
        , "ggplot2"         # Advanced graphics
        , "plyr"            # Grouping variables for ggplot
        , "tidyr"           # Arrange data
        , "devtools"        # To install github packages
        , "agricolae"       # For skewness and kurtosis
        , "ggpubr"          # Advanced graphics
        , "vcd"             # Mosaic plots
        , "dplyr"           #A Grammar of Data Manipulation
        , "FactoMineR"
        , "factoextra"
        , "missMDA"
        , "ggfortify"
        , "cluster"
        , "forcats"
        , "remotes"
        )
# citation()
# citation("agricolae")
# citation("plotrix")
# citation("ggpubr")
# citation("readxl")
# citation("writexl")
# citation("plyr")
# citation("tidyr")
# citation("devtools")
# citation("ggpubr")
# citation("car")
# citation("knitr")
# citation("vcd")
# citation("dplyr")
# citatin("FactoMineR")
# citation("factoextra")
# citation("missMDA")
# citation("ggfortify")
# citation("cluster")
# citation("forcats")
# citation("remotes")


### Install libraries only if necessary (first time)
### And activate them
for (i in LIB) {
    if (!require(i, character.only = T)) install.packages(i, dep = T, repos="http://cran.rstudio.com/")
    library(i, character.only = T)
}

### Remove unnecessary objects
rm(LIB, i)
