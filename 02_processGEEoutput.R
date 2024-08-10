library(googledrive)
library(sf)
library(ggplot2)
library(tidyverse)
library(polyglotr)

source("00_globals.R")
unzip("data/drive-download-20240725T151651Z-001.zip", exdir = "data/")


nuts <- sf::read_sf("data/NUTSstatsAll_2024_07_25_16_25_46.shp",
                    options = "ENCODING=WINDOWS-1252")

nuts$LEVL_CODE<-  as.factor(nuts$LEVL_CODE)
unique(nuts$CNTR_CODE)

for(country in unique(nuts$CNTR_CODE) ){
  nutsf <- nuts %>% filter(CNTR_CODE==country)
  nutsf.l1 <- nutsf %>% filter(LEVL_CODE==1)
  nutsf.l1$'Subdivisions - NUTS 1' <- sprintf("%s (%s)",
                                              nutsf.l1$NAME_LATN,
                                              nutsf.l1$NUTS_ID)
  nuts2print <- nutsf.l1 %>% select(`Subdivisions - NUTS 1`,
                                    pot_area,pot_biom,
                                    eff_area,eff_biom,
                                    eff_mcaSum,eff_mcaSum
                                    )
}

drive_download(sprintf("restrictionMask_%s", country) ,
               path =sprintf("restrictionMask_%s", country) ,
               type = "tif"  )
