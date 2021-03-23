#' LIBRARIES
#' -----------------
# List of packages for session
#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")

#BiocManager::install("affyio")

# sudo apt-get install libmpfr-dev
# sudo apt-get install libgmp-dev
#install.packages("hydromad", repos="http://hydromad.catchment.org")

#if package installation cannot find gfortran use this
#sudo ln -s /usr/lib/gcc/x86_64-linux-gnu/7/libgfortran.so /usr/lib/x86_64-linux-gnu/


.packages = c("rgdal","data.table","tidyverse","here","maptools","broom","ggmap","devtools", "sf",
              "tmap","ggspatial","rgeos", "raster","XML", 
              "ncdf4", "rasterVis", "fields","sf","rgdal","ncdf4","rgdal", "broom", "gridExtra", "lubridate", 
              "chron","epwshiftr")

# 
# .packages = c("rgdal","data.table","tidyverse","here","maptools","broom","ggmap","devtools", "sf",
#               "tmap","ggspatial","rgeos", "raster","spData","spDataLarge","sp","rnaturalearth",
#               "rnaturalearthdata","rnaturalearthhires", "readxl","scales","GGally","network",
#               "sna","RColorBrewer","grDevices","colorRamps","colorspace","RNetCDF","parallel",
#               "doSNOW","emdbook","plotly","ggiraphExtra","viridis", "cowplot", "mapdata", "XML", 
#               "ncdf4", "rasterVis", "fields","sf","rgdal","ncdf4","rgdal", "broom", "gridExtra", "lubridate", 
#               "chron", "png", "grid", "lemon","")


# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], dependencies = TRUE)

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

# if these packages are not present uncomment and run this code
# then uncomment before running chunk  
 #   install.packages('spDataLarge',
 # repos='https://nowosad.github.io/drat/', type='source')
 #   devtools::install_github("ropenscilabs/rnaturalearth")
 #  devtools::install_github("ropenscilabs/rnaturalearthdata")
 #   install.packages("rnaturalearthhires",
 #                  repos = "http://packages.ropensci.org",
 #                  type = "source")


lapply(.packages, require, character.only=TRUE)
