library(leaflet)
library(shiny)
library(shinydashboard)
library(mapview)
library(sf)
library(stars)
library(sjPlot)
library(shinycssloaders)
library(car)
library(rgee)
library(stringr)
library(shinyWidgets)
library(ggplot2)
library(tidyverse)


extractData <- function(nameofRegion="Veneto", res=500){



  # Sys.getenv("EARTHENGINE_PYTHON")
  # ee_users()
  # ee_Initialize(user = 'cirgeo' )
  # ee_Authenticate(user = 'cirgeo')
  #  reticulate::py_run_string("import ee; ee.Initialize( )")
  # reticulate::py_config()
  # rgee::ee_install(py_env = "r-reticulate")

  trees <- c("users/cirgeo/FIRE-RES/open/veg_abies_alba_anv_v3",
             "users/cirgeo/FIRE-RES/open/veg_picea_abies_anv_v3",
             "users/cirgeo/FIRE-RES/open/veg_pinus_sylvestris_anv_v3",
             "users/cirgeo/FIRE-RES/open/veg_pinus_pinea_anv_v3",
             "users/cirgeo/FIRE-RES/open/veg_quercus_cerris_anv_v3",
             "users/cirgeo/FIRE-RES/open/veg_quercus_robur_anv_v3",
             "users/cirgeo/FIRE-RES/open/veg_quercus_ilex_anv_v3",
             "users/cirgeo/FIRE-RES/open/veg_pinus_halepensis_anv_v3",
             "users/cirgeo/FIRE-RES/open/veg_fagus_sylvatica_anv_v3",
             "users/cirgeo/FIRE-RES/open/veg_corylus_avellana_anv_v3",
             "users/cirgeo/FIRE-RES/open/veg_pinus_nigra_anv_v3",
             "users/cirgeo/FIRE-RES/open/veg_castanea_sativa_anv_v3")

  treenames <-   paste(str_to_title(sapply(strsplit(trees, split = "_"),`[`,2 )),
                       sapply(strsplit(trees, split = "_"),`[`,3 ))


  corine = ee$Image('COPERNICUS/CORINE/V20/100m/2018')$select('landcover')
  cc<-corine$getInfo()
  corineInfo<-cc$properties


  treesEE <- list()
  for(tree in trees){
    treesEE[[tree]] <- ee$Image(tree)
  }

  treesEEC = ee$ImageCollection( unname(unlist(treesEE)) )$sort("system:id")

  treesMBI = treesEEC$toBands()$rename(sort(treenames))



  natura = ee$FeatureCollection("projects/ee-pysoloctfc/assets/Natura2000/natura2000habitatsDirective")
  faoCountries = ee$FeatureCollection("FAO/GAUL/2015/level0")
  nuts = ee$FeatureCollection("projects/progetto-eu-h2020-cirgeo/assets/NUTS_RG_01M_2021_4326")
  nutsf <- nuts$filter(ee$Filter$eq("NUTS_NAME",nameofRegion))

  info<-list(treenames=treenames,
             corineInfo=corineInfo,
             veneto=ee_as_sf(nutsf))



  regione <- nutsf$geometry()$bounds()
  # nutsinfo<-nutsf$getInfo()

  biomass = ee$Image("projects/progetto-eu-h2020-cirgeo/assets/fire-res/biomass")
   biomass = biomass$unmask()

  soil1 = ee$Image("projects/ee-pysoloctfc/assets/SoilGroups/Soilgroups_Greece")$unmask()
  soil2 = ee$Image("projects/ee-pysoloctfc/assets/SoilGroups/Soilgroups_Italy")$unmask()
  soil3 = ee$Image("projects/ee-pysoloctfc/assets/SoilGroups/Soilgroups_Spain")$unmask()
  soils = ee$ImageCollection(c(soil1, soil2, soil3))$mosaic()
  soilmask = soils$expression("b('b1') != 0 && b('b1') != 8 && b('b1') != 12 && b('b1') != 14 && b('b1') != 16 && b('b1') != 21")

  productivity = ee$FeatureCollection("projects/ee-pysoloctfc/assets/biomassProductivity")
  productivity = productivity$reduceToImage(
    properties= list('NAI_ha_FOR'),
    reducer=ee$Reducer$first()
  )

  roads1 = ee$Image("projects/ee-pysoloctfc/assets/Roads/Greece_Proximity2road")$unmask()
  roads2 = ee$Image("projects/ee-pysoloctfc/assets/Roads/Italy_Proximity2road")$unmask()
  roads3 = ee$Image("projects/ee-pysoloctfc/assets/Roads/Spain_Proximity2road")$unmask()
  roads = ee$ImageCollection(c(roads1, roads2, roads3))$mosaic()

  dni1 = ee$Image("projects/ee-pysoloctfc/assets/DNI/DNI_Greece")$unmask()$multiply(365)
  dni1 = dni1$mask(dni1$gt(0))
  dni2 = ee$Image("projects/ee-pysoloctfc/assets/DNI/DNI_Italy")$unmask()$multiply(365)
  dni2 = dni2$mask(dni2$gt(0))
  dni3 = ee$Image("projects/ee-pysoloctfc/assets/DNI/DNI_Spain")$unmask()$multiply(365)
  dni3 = dni3$mask(dni3$gt(0))
  dni = ee$ImageCollection(c(dni1, dni2, dni3))$mosaic()

  forestcover = ee$Image("projects/progetto-eu-h2020-cirgeo/assets/fire-res/canopyCover")$unmask()



  restrictions <- list(
    Forest_Cover=forestcover,
    Corine_Classes=corine,
    SOIL=soils,
    Tree_Species=treesMBI
  )

  mca <- list(Biomass=biomass,
              DNI=dni,
              Road_Proximity=roads,
              Forest_Productivity=productivity
  )
  mm<-names(mca)
  names(mm)<-mm
  info$mca <- as.list(mm)
  rr<-names(restrictions)
  names(rr)<- rr
  info$restrictions <- as.list(rr)
  save(info, file="pysoloMCA/info.rda")



  task <- list()
  for(item in names(restrictions)){
    message(item, " - ")

    task[[item]] <-  ee_image_to_drive(
      image= restrictions[[item]]$toUint16(),
      description=sprintf("%s", item) ,
      folder= "PYSOLO_restrictions",
      fileFormat= 'GeoTIFF',
      timePrefix = FALSE,
      scale= as.integer(res),
      region= regione,
      # fileDimensions=  as.integer(1000),
      maxPixels = as.integer(400000000)
    )

    task[[item]] $start()

  }

  for(item in names(mca)){
    message(item, " - ")

    task[[item]] <-  ee_image_to_drive(
      image= mca[[item]]$toUint16(),
      description=sprintf("%s", item) ,
      folder= "PYSOLO_Criteria",
      fileFormat= 'GeoTIFF',
      timePrefix = FALSE,
      scale= as.integer(res),
      region= regione,
      # fileDimensions=  as.integer(1000),
      maxPixels = as.integer(400000000)
    )

    task[[item]] $start()

  }

    # ee_drive_to_local(task,consider = )

}

#
# toExtract<-F
 load("info.rda")
  names(info$corineInfo$landcover_class_values)<-
    sprintf("<span style='width:40px;height:20px; background-color:#%s'>&nbsp;(%s)&nbsp;</span> - %s",
            info$corineInfo$landcover_class_palette,
            info$corineInfo$landcover_class_values,
          info$corineInfo$landcover_class_names)

# if(toExtract){
#
#   extractData("Veneto", 200)
#
# }
#
# restrictionRasters <- list()
#
#
# setwd("pysoloMCA/")
mv <- mapview::mapview(info$veneto, layer.name="Veneto", legend=F, hide=T)

corine <- file.path("data/PYSOLO_restrictions",
                    paste(sep="", info$restrictions$Corine_Classes, ".tif"))

COR <- raster::ratify( raster::raster(corine) )

rat<-data.frame(ID=info$corineInfo$landcover_class_values,
                code=info$corineInfo$landcover_class_names,
          landcover=info$corineInfo$landcover_class_names)

levels(COR)<- rat


COR_colors<-sprintf("#%s", info$corineInfo$landcover_class_palette)
selectedCorine <- which(info$corineInfo$landcover_class_values > 243 &
                        info$corineInfo$landcover_class_values < 350 )

mvcorine <- mv + mapview::mapview(COR, layer.name= "CORINE",
                           col.regions = COR_colors, legend=FALSE )


cc <-   raster::raster(file.path("data/PYSOLO_restrictions",
                 paste(sep="", info$restrictions$Forest_Cover, ".tif")))

if(!exists("tree")){
  print("do tree")
  tree <-   raster::stack(file.path("data/PYSOLO_restrictions",
                                    paste(sep="", info$restrictions$Tree_Species, ".tif")))

  tree[tree[]==0] <-NA
}

### criteria ----------


dni <-  raster::raster( file.path("data/PYSOLO_Criteria",
                    paste(sep="", info$mca$DNI, ".tif")) )
dni[dni[]==0]<-NA


soil <-  raster::raster( file.path("data/PYSOLO_restrictions",
                                  paste(sep="", info$restrictions$SOIL, ".tif")) )
soil[soil[]==0]<-NA


Road_Proximity <-  raster::raster( file.path("data/PYSOLO_Criteria",
                                   paste(sep="", info$mca$Road_Proximity, ".tif")) )
Road_Proximity[Road_Proximity[]==0]<-NA

Biomass <-  raster::raster( file.path("data/PYSOLO_Criteria",
                                             paste(sep="", info$mca$Biomass, ".tif")) )
Biomass[Road_Proximity[]==0]<-NA

###
