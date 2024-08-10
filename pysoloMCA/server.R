
# Define server logic required to draw a histogram
function(input, output, session) {

  restrictionsMasks <- global.restrictionMasks

  observeEvent(input$corineapply, {
    print(restrictionsMasks)
    showSpinner("corinemapSpin")
    CORmExcluded<- COR
    CORmIncluded<- COR
    excl<-(COR[] %in% isolate(input$corinemask))
    CORmExcluded[ excl ]<-NA
    CORmIncluded[ !excl ]<-NA
    restrictionsMasks[["corine"]]<<- CORmIncluded

    pal <- function(x){
      if (length(x) == 0 || all(is.na(x))) {
        return(rep("#00000000", length(x)) )
      }
      col <- match(x, info$corineInfo$landcover_class_values )
      cols <- info$corineInfo$landcover_class_palette[col]
      cols[!is.na(cols)]<-paste("#", cols[!is.na(cols)], sep="" )
      cols[is.na(cols)]<-"#00000000"
      cols

    }

    leafletProxy("map.corine" ) %>%
      clearShapes() %>%
      clearImages() %>%
      addRasterImage(CORmIncluded,
                     layerId = "CORINE Included",
                     colors = pal,
                     group = "CORINE Included",
                     options = gridOptions(zIndex=100000)
                     )%>%
      addRasterImage(CORmExcluded,
                     layerId = "CORINE Excluded",
                     colors = pal,
                     group = "CORINE Excluded",
                     options = gridOptions(zIndex=100000)
      )



    shinycssloaders::hideSpinner("corinemapSpin")

  },ignoreInit = T )


  ## start render  ----
  ### CORINE -------
    output$map.corine <- renderLeaflet({

      CORmExcluded<- COR
      CORmIncluded<- COR

      excl<-(COR[] %in%  isolate(input$corinemask))
      CORmExcluded[ excl ]<-NA
      CORmIncluded[ !excl ]<-NA
      restrictionsMasks[["corine"]]<<- CORmIncluded
      mvcorine <- mv +
        mapview::mapview(CORmIncluded, layer.name= "CORINE Included",
                         col.regions = COR_colors,
                         legend=F,query.digits = 0  )+
        mapview::mapview(CORmExcluded, layer.name= "CORINE Excluded",
                         col.regions = COR_colors,
                         legend=F,query.digits = 0  )
      mvcorine@map
    })

  ### SOIL -------
  output$map.soils <- renderLeaflet({

    soilmasked <- soil
    soilmasked2 <- soil
    soilno <- input$soilmask
    excl<-(soil[] %in%  soilno)
    soilmasked[excl] <- NA
    soilmasked2[!excl] <- NA

    restrictionsMasks[["soils"]]<<- soilmasked2
    mvcorine <- mv +
      mapview::mapview(soilmasked2, layer.name= "SOILS Included",
                       col.regions = COR_colors,
                       na.color ="#00000000",
                       legend=F,query.digits = 0  )+
      mapview::mapview(soilmasked, layer.name= "SOILS Excluded",
                       col.regions = COR_colors,
                       na.color ="#00000000", hide=T,
                       legend=F,query.digits = 0  )
    mvcorine@map
  })


  ### CANOPY COVER -------
  output$map.cc <- renderLeaflet({

    ccmasked <- cc
    ccmasked[cc[]< input$rest.cc.thr ]<-NA
    restrictionsMasks[["canopy"]]<<- ccmasked
    mvcc <- mv + mapview::mapview(ccmasked, layer.name= "Canopy Cover",
                                  legend=T,query.digits = 0,
                                  na.color ="#00000000",
                                  # col.regions =  viridis::viridis(100),
                                  at = seq(0, 100, 10)  )

    mvcc@map
  })

  ### TREE SPECIES -------
  output$map.trees <- renderLeaflet({
    req(input$treeapply)
    treemasked <- tree[[ isolate(input$treemask)]]
    mults <- raster::stackApply(treemasked, indices = c(1), sum)
    mults2 <- raster::stackApply(as.logical(treemasked), indices = c(1), sum)
    fin <- mults/mults2

    restrictionsMasks[["trees"]]<<- fin

    mvtrees <- mv +
      mapview::mapview(fin, layer.name= "Species Occurance",
                             legend=T,query.digits = 0,
                             hide=F,
                             # col.regions =  viridis::viridis(100),
                             at = seq(0, 100, 20),
                             na.color ="#00000000" )
    mvtrees@map
  })



  ### DNI -------
  output$map.dni <- renderLeaflet({

    mvdni <- mv +
      mapview::mapview(dni, layer.name= "DNI",
                       legend=T,query.digits = 0,
                       # col.regions =  viridis::viridis(100),
                       # at = seq(0, 100, 20),
                       na.color ="#00000000" )


    mvdni@map
  })

  output$hist.dni <- renderPlot({
    df  <- data.frame(DNI=dni[])
    p <- ggplot(df, aes_string(x="DNI")) +
      geom_density(fill='blue', alpha=0.2) +
      theme_classic()
    print(p)
  })


  ### BIOMASS -------
  ###


  output$hist.biomass <- renderPlot({
    df  <- data.frame(Biomass=Biomass[])
    p <- ggplot(df, aes_string(x="Biomass")) +
      geom_density(fill='blue', alpha=0.2) +
      theme_classic()
    print(p)
  })

  output$map.biomass <- renderLeaflet({

    mvB <- mv +
        mapview::mapview(Biomass, layer.name= "AGB Biomass (Mg per ha)",
                         legend=T,query.digits = 2,
                         # col.regions =  viridis::viridis(100),
                         at = seq(1, 350, 20),
                         na.color ="#00000000" )


    mvB@map
  })


  ### ROADS -------
  output$map.rp <- renderLeaflet({

    mvrp <- mv +
      mapview::mapview(log10(Road_Proximity), layer.name= "Meters from roads (log10)",
                       legend=T,query.digits = 3,
                       # col.regions =  viridis::viridis(100),
                       at = seq(1, 4, 0.1),
                       na.color ="#00000000" ) +
      mapview::mapview(Road_Proximity, layer.name= "Meters from roads",
                       legend=T,query.digits = 1, hide=T,
                       # col.regions =  viridis::viridis(100),
                       at = seq(1, 10000, 1000),
                       na.color ="#00000000" )


    mvrp@map
  })


  output$hist.road <- renderPlot({
    df  <- tibble::tibble('Distance from roads'=Road_Proximity[], t="meters")
    df1  <-  tibble::tibble('Distance from roads'=log10(Road_Proximity[]), t="log10.meters")
    fn <- na.omit(rbind(df, df1))
    p <- ggplot(fn, aes(x=`Distance from roads`)) +
      geom_density(fill='blue', alpha=0.2) +
      facet_wrap(vars(t), scales="free") +
      theme_classic()
    print(p)
  },  res = 90)
  ### WEIGHTS -------
  output$weights.out <- renderPrint({
    print(restrictionsMasks)

    paste( input$weights.slider,     diff(input$weights.slider))
  }   )


}
