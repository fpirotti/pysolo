library(rgee)
library(rgeeExtra)
library("inborutils")
library("parallel")
library("terra")

ee_Initialize(user = 'pysolo.ctfc' )

tree = ee$Image("users/cirgeo/FIRE-RES/open/veg_abies_alba_anv_v3")
tree2 = ee$Image("users/cirgeo/FIRE-RES/open/veg_picea_abies_anv_v3")
tree3 = ee$Image("users/cirgeo/FIRE-RES/open/veg_pinus_sylvestris_anv_v3")
tree4 = ee$Image("users/cirgeo/FIRE-RES/open/veg_pinus_pinea_anv_v3")
tree5 = ee$Image("users/cirgeo/FIRE-RES/open/veg_quercus_cerris_anv_v3")
tree6 = ee$Image("users/cirgeo/FIRE-RES/open/veg_quercus_robur_anv_v3")
tree7 = ee$Image("users/cirgeo/FIRE-RES/open/veg_quercus_ilex_anv_v3")
tree8 = ee$Image("users/cirgeo/FIRE-RES/open/veg_pinus_halepensis_anv_v3")
tree9 = ee$Image("users/cirgeo/FIRE-RES/open/veg_fagus_sylvatica_anv_v3")
tree10 = ee$Image("users/cirgeo/FIRE-RES/open/veg_corylus_avellana_anv_v3")
tree12 = ee$Image("users/cirgeo/FIRE-RES/open/veg_pinus_nigra_anv_v3")
tree11 = ee$Image("users/cirgeo/FIRE-RES/open/veg_castanea_sativa_anv_v3")
natura = ee$FeatureCollection("projects/ee-pysoloctfc/assets/Natura2000/natura2000habitatsDirective")
faoCountries = ee$FeatureCollection("FAO/GAUL/2015/level0")
nuts = ee$FeatureCollection("projects/progetto-eu-h2020-cirgeo/assets/NUTS_RG_01M_2021_4326")

biomass = ee$Image("projects/progetto-eu-h2020-cirgeo/assets/fire-res/biomass")
biomass = biomass$unmask()

soil1 = ee$Image("projects/ee-pysoloctfc/assets/SoilGroups/Soilgroups_Greece")$unmask()
soil2 = ee$Image("projects/ee-pysoloctfc/assets/SoilGroups/Soilgroups_Italy")$unmask()
soil3 = ee$Image("projects/ee-pysoloctfc/assets/SoilGroups/Soilgroups_Spain")$unmask()
productivity = ee$FeatureCollection("projects/ee-pysoloctfc/assets/biomassProductivity")
roads1 = ee$Image("projects/ee-pysoloctfc/assets/Roads/Greece_Proximity2road")$unmask()
roads2 = ee$Image("projects/ee-pysoloctfc/assets/Roads/Italy_Proximity2road")$unmask()
roads3 = ee$Image("projects/ee-pysoloctfc/assets/Roads/Spain_Proximity2road")$unmask()
dni1 = ee$Image("projects/ee-pysoloctfc/assets/DNI/DNI_Greece")$unmask()$multiply(365)
dni1 = dni1$mask(dni1$gt(0))
dni2 = ee$Image("projects/ee-pysoloctfc/assets/DNI/DNI_Italy")$unmask()$multiply(365)
dni2 = dni2$mask(dni2$gt(0))
dni3 = ee$Image("projects/ee-pysoloctfc/assets/DNI/DNI_Spain")$unmask()$multiply(365)
dni3 = dni3$mask(dni3$gt(0))

source("00_globals.R")


  bounds = list(
  "Greece"= faoCountries$filter(ee$Filter$eq("ADM0_NAME","Greece")),
  "Italy"= faoCountries$filter(ee$Filter$eq("ADM0_NAME","Italy")),
  "Spain"=  faoCountries$filter(ee$Filter$eq("ADM0_NAME","Spain"))
)

 geometrySpainBounds = ee$Geometry(list(
  'type'= 'Polygon',
  'coordinates'=
    list(list(c(-9.3, 35.87),
      c(-9.3, 43.8),
      c(3.4, 43.8),
      c(3.4, 35.87)) )
));

 bounds2 = list(
  "Greece"= faoCountries$filter(ee$Filter$eq("ADM0_NAME","Greece"))$geometry()$bounds(),
  "Italy"= faoCountries$filter(ee$Filter$eq("ADM0_NAME","Italy"))$geometry()$bounds(),
  "Spain"= geometrySpainBounds$bounds()
)



productivity = productivity$reduceToImage(
   properties= list('NAI_ha_FOR'),
   reducer=ee$Reducer$first()
 );

roads = ee$ImageCollection( c(roads1, roads2, roads3) )$mosaic()




############################7
# RESTRICTIONS --------
  ############################/
corine = ee$Image('COPERNICUS/CORINE/V20/100m/2018')$select('landcover')
corineMask = corine$gt(243)$And(corine$lt(350))

# species
treeList = ee$List( c(tree,  tree2,  tree3,  tree4,
                tree5,  tree6,  tree7,  tree8,
                tree9, tree10, tree11, tree12 ) )

 trees = ee$ImageCollection( treeList )$sort("system=id")

# treeStack = trees$toBands()$rename(treeNames);
treesMask = trees$qualityMosaic("b1")$gt(0)

#elevation
ele = ee$Image("projects/progetto-eu-h2020-cirgeo/assets/fire-res/elevation")$unmask()$lt(2500)
# slope is in degrees, so 60% is 30 degrees
slo = ee$Image("projects/progetto-eu-h2020-cirgeo/assets/fire-res/slope")$unmask()$lt(31)
# forest Cover
forestcover = ee$Image("projects/progetto-eu-h2020-cirgeo/assets/fire-res/canopyCover")$unmask()$gt(50)
# soil
soils = ee$ImageCollection(c(soil1, soil2, soil3))$mosaic()
soilmask = soils$expression("b('b1') != 0 && b('b1') != 8 && b('b1') != 12 && b('b1') != 14 && b('b1') != 16 && b('b1') != 21")

restrictionsMaskColl = ee$ImageCollection(c( corineMask$toByte()$rename("mask"),
                                      ele$toByte()$rename("mask"),
                                      treesMask$toByte()$rename("mask"),
                                      slo$toByte()$rename("mask"),
                                      forestcover$toByte()$rename("mask"),
                                      soilmask$toByte()$rename("mask") ))

restrictionsMaskStack =  restrictionsMaskColl$toBands()$rename(c("corine","elevation","treeSpecies","slope",
                       "forestcover","soilType"));

restrictionsMask = restrictionsMaskColl$min()

###########################
######/ MULTI CRITERIA ANALYSIS ########
###########################


  # cutoff <- function(img, min, max){
  #   cutoff = img$gt(max)$multiply(img)$add(img$lt(max))
  #   return(img$divide(cutoff))
  # }


# DNI
dni = ee$ImageCollection(c(dni1, dni2, dni3))$mosaic()

weights = list( biomass=c(40,35),
                productivity=c(30,35),
                dni=c(20,35),
                roads=c(10,35) )

mcaUnweightedUnscaled=list(
  biomass  =  biomass ,
  productivity  =  productivity  ,
  dni  =  dni ,
  roads  =  roads
)

ranges <- list(
  biomass  =  c(0,250),
  productivity  =  c(0.4,10),
  dni  =  c(400,2000),
  roads  =  c(0,20000)
)

mcaScaled<- sapply(names(mcaUnweightedUnscaled), function(x){
    rr <- ranges[[x]]
    mcaUnweightedUnscaled[[x]]$unitScale(rr[[1]], rr[[2]])$clamp(0, 1)
})
## exception for Road proximity, the lower the better so 0==1 and 7000=0
mcaUnweightedUnscaled$roads <- ee$Image(1)$subtract(mcaUnweightedUnscaled$roads)

mcaScaledWeighted=sapply(names(mcaUnweightedUnscaled), function(x){
  mcaScaled[[x]]$multiply(weights[[x]][[1]])
})


# suitabilityColl = ee$ImageCollection( unname(mca) )

suitabilityStack = ee$Image( unname(mcaUnweightedUnscaled) )$rename(list("Biomass",
                                                                   "Productivity",
                                                                   "DNI",
                                                                   "RoadProximity"))

suitabilityStackScaled = ee$Image( unname(mcaScaled) )$rename(list("BiomassScaled",
                                                                     "ProductivityScaled",
                                                                     "DNIScaled",
                                                                     "RoadProximityScaled"))

suitabilityStackScaledWeighted = ee$Image( unname(mcaScaledWeighted) )$rename(list("BiomassScaledWeighted",
                                                                     "ProductivityScaledWeighted",
                                                       "DNIScaledWeighted",
                                                       "RoadProximityScaledWeighted"))

suitability = mcaScaledWeighted$biomass$add(mcaScaledWeighted$productivity)$add(mcaScaledWeighted$dni)$add(mcaScaledWeighted$road)$unmask()
suitabilityClassified <- suitability$gt(0)$add(suitability$gt(20))$add(suitability$gt(40))$add(suitability$gt(60))$add(suitability$gt(80))$unmask()
#Map$addLayer(restrictionsMask, {}, "restrictionsMask")
NUTS = ee$FeatureCollection("projects/progetto-eu-h2020-cirgeo/assets/NUTS_RG_01M_2021_4326")$filter(ee$Filter$expression("CNTR_CODE == 'ES' || CNTR_CODE == 'IT' || CNTR_CODE == 'EL'") )

# reds = ee$Reducer$sum()$combine(ee$Reducer$mean()); #mean()$combine(ee$Reducer$stdDev())$combine(ee$Reducer$min())$combine(ee$Reducer$max())
reds = ee$Reducer$sum()$setOutputs(list("Potential_Area"))
finalStack = corineMask
reds = reds$combine(ee$Reducer$sum()$setOutputs(list("Potential_Biomass"))) #mean()$combine(ee$Reducer$stdDev())$combine(ee$Reducer$min())$combine(ee$Reducer$max())
finalStack = finalStack$addBands(biomass$multiply(corineMask))

reds = reds$combine(ee$Reducer$sum()$setOutputs(list("Effective_Area")))
finalStack = finalStack$addBands(restrictionsMask)

reds = reds$combine(ee$Reducer$sum()$setOutputs(list("Effective_Biomass")))
finalStack = finalStack$addBands(biomass$multiply(restrictionsMask))

reds = reds$combine(ee$Reducer$min()$setOutputs(list("DNI_Minimum")))
finalStack = finalStack$addBands(dni)


reds = reds$combine(ee$Reducer$max()$setOutputs(list("DNI_Maximum")))
finalStack = finalStack$addBands(dni)

reds = reds$combine(ee$Reducer$mean()$unweighted()$setOutputs(list("DNI_Mean")))
finalStack = finalStack$addBands(dni)

reds = reds$combine(ee$Reducer$stdDev()$unweighted()$setOutputs(list("DNI_StdDev")))
finalStack = finalStack$addBands(dni)

for(class in 1:5){
  reds = reds$combine(ee$Reducer$sum()$unweighted()$setOutputs(list(sprintf("MCA_Class%d_Area",class) )))
  finalStack = finalStack$addBands(suitabilityClassified$multiply(restrictionsMask)$eq(class))
}


for(class in 1:5){

  reds = reds$combine(ee$Reducer$sum()$unweighted()$setOutputs(list(sprintf("MCA_Class%d_Biomass",class) )))
  finalStack = finalStack$addBands(suitabilityClassified$multiply(restrictionsMask)$eq(class)$multiply(biomass))
}

percentiles <- c(1,5,10,25,50,75,90,95,99)

reds = reds$combine(
  ee$Reducer$percentile(percentiles)$unweighted()$setOutputs(
    sprintf("Effective_Biomass_%02dPerc", percentiles)
  )
)
finalStack = finalStack$addBands(biomass$mask(restrictionsMask))


reds = reds$combine(
  ee$Reducer$percentile(percentiles)$unweighted()$setOutputs(
   sprintf("Effective_Productivity_%02dPerc", percentiles)
  )
)
finalStack = finalStack$addBands(productivity$mask(restrictionsMask))


reds = reds$combine(
  ee$Reducer$percentile(percentiles)$unweighted()$setOutputs(
   sprintf("Effective_DNI_%02dPerc", percentiles)
  )
)
finalStack = finalStack$addBands(dni$mask(restrictionsMask))


reds = reds$combine(
  ee$Reducer$percentile(percentiles)$unweighted()$setOutputs(
   sprintf("Effective_RoadProximity_%02dPerc", percentiles)
  )
)
finalStack = finalStack$addBands(roads$mask(restrictionsMask))


ftr_polygons = finalStack$reduceRegions(
  collection= NUTS,
  reducer=reds,
  scale= 100
)


task <- ee_table_to_drive(
  collection= ftr_polygons,
  description= 'NUTSstatsAll',
  folder='STATS',
  fileFormat ="GeoJSON"
)

task$start()

# imgToExp <- restrictionsMaskStack
#
suits <- list(

  restrictionsMask = restrictionsMask$toByte(),
  restrictionsMaskStack = restrictionsMaskStack$toByte(),

  suitabilityClassified = suitabilityClassified$toByte(),
  suitabilityStack = suitabilityStack$toFloat(),
  suitabilityStackScaled = suitabilityStackScaled$toFloat(),
  suitabilityStackScaledWeighted = suitabilityStackScaledWeighted$toByte(),

  suitabilityStackPotential = suitabilityStack$toFloat()$mask( corineMask ),
  suitabilityStackScaledPotential = suitabilityStackScaled$toFloat()$mask( corineMask ),
  suitabilityStackScaledWeightedPotential = suitabilityStackScaledWeighted$toFloat()$mask( corineMask ),

  suitabilityStackEffective = suitabilityStack$toFloat()$mask( restrictionsMask ),
  suitabilityStackScaledEffective = suitabilityStackScaled$toFloat()$mask( restrictionsMask ),
  suitabilityStackScaledWeightedEffective = suitabilityStackScaledWeighted$toFloat()$mask( restrictionsMask )
)

for(imgN in names(suits)){
  img <- suits[[imgN]]

for(country in names(boundsGeom)){
   message(country, " - ", imgN)

   ccode = boundsGeom[[country]][[4]]
   bb = bounds[[country]]


  task <-  ee_image_to_drive(
    image= img$clip(bb),
    description=sprintf("%s_%s", imgN, country) ,
    folder= imgN,
    fileFormat= 'GeoTIFF',
    timePrefix = FALSE,
    scale= as.integer(100),
    region= bounds2[[country]],
    # fileDimensions=  as.integer(1000),
    maxPixels = as.integer(400000000)
  )

  task$start()

 }

}


