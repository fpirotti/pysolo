

dashboardPage(skin = "black",
              dashboardHeader(title = "MultiCriteria Sensibility",
                              titleWidth = 450),
              dashboardSidebar(

                sidebarMenu(
                  menuItem("Map", tabName = "dashboardmap",
                           badgeLabel = "mapit",
                           badgeColor = "green",
                           icon = icon("map")),

                  menuItem("Restrictions", tabName = "rest", icon = icon("layer-group"),

                           menuSubItem(text ="Corine" , tabName = "corine",
                                       icon = icon("map") )
                        ,  menuSubItem(text ="Canopy Cover" , tabName = "canopy",
                                       icon = icon("map"))
                        ,  menuSubItem(text ="Tree Species" , tabName = "trees",
                                       icon = icon("map"))
                        ,  menuSubItem(text ="Soil Class" , tabName = "soils",
                                       icon = icon("map"))

                           ),
                  menuItem("Criteria", tabName = "mca", icon = icon("layer-group"),

                             menuSubItem(text ="Biomass" , tabName = "biomass",
                                          icon = icon("map"))
                          , menuSubItem(text ="DNI - direct normal irradiance" ,
                                       tabName = "dni",
                                       icon = icon("map") )
                           ,  menuSubItem(text ="Forest productivity" , tabName = "prod",
                                          icon = icon("map"))
                           ,  menuSubItem(text ="Road proximity" , tabName = "roads",
                                          icon = icon("map"))

                  ),
                  menuItem("Plots", tabName = "3dreg", icon = icon("chart-bar")),
                  menuItem("Tables", tabName = "tabl", icon = icon("table"))
                )


              ),
              dashboardBody(
                includeCSS("www/styles.css"),
                tabItems(
                  tabItem(tabName = "dashboardmap",
                            leafletOutput("map" ),

                          noUiSliderInput(
                            inputId = "weights.slider", label = "Weights: DNI",
                            min = 0, max = 100,
                            value = c(40,70, 90),
                            tooltips = TRUE,
                            format = wNumbFormat(suffix = "", decimals = 0),
                            step = 1
                          ),
                          verbatimTextOutput(outputId = "weights.out")

                  ) ,
                  tabItem( tabName = "biomass",
                           shinycssloaders::withSpinner(
                             leafletOutput("map.biomass",height = 500),

                             type = 4,caption = "drawing map ",
                             id = "biomassmapSpin", hide.ui = F ),

                           shinycssloaders::withSpinner(
                             plotOutput("hist.biomass" ),

                             type = 4,caption = "drawing  plot ",
                             id = "biomassplotSpin", hide.ui = F )
                  ),
                  tabItem( tabName = "dni",
                           box(title = "Direct Normal Irradiance Map",
                               collapsible = T,
                             shinycssloaders::withSpinner(
                             leafletOutput("map.dni",height = 500),

                             type = 4,caption = "drawing map ",
                             id = "dnimapSpin", hide.ui = F )
                             ),
                           box(title = "Direct Normal Irradiance Density Plot",
                               collapsible = T,
                             shinycssloaders::withSpinner(
                             plotOutput("hist.dni" ),

                             type = 4,caption = "drawing  plot ",
                             id = "dniplotSpin", hide.ui = F )
                           )
                          ),
                  tabItem(  tabName = "prod"
                  ),
                  tabItem(  tabName = "roads",
                            fluidRow(
                              box(width = 12,  title = "Road Proximity Map",
                                  collapsible = T,
                                  shinycssloaders::withSpinner(
                                leafletOutput("map.rp",height = 500),

                                type = 4,caption = "drawing map",
                                id = "rpmapSpin", hide.ui = F )
                                )
                            ),
                            fluidRow(
                              box(width = 12, title = "Road Proximity Density Plots",
                                  collapsible = T,
                                  shinycssloaders::withSpinner(
                                    plotOutput("hist.road" ),
                                    type = 4,caption = "drawing  plot ",
                                    id = "roadplotSpin", hide.ui = F )
                              )
                            )

                  ),
                  tabItem(  tabName = "soils",
                            fluidRow(
                              column(8, shinycssloaders::withSpinner(
                                leafletOutput("map.soils",height = 500),type = 4,caption = "drawing map ",
                                id = "soilmapSpin", hide.ui = F ) )
                              ,column(4,
                                      actionButton("soilapply", "Apply mask"),
                                      checkboxGroupInput("soilmask", "Select soil code to include"
                                                         # choices =  info$corineInfo$landcover_class_values
                                                         ,choiceNames =  sort(unique(soil[]))
                                                         ,choiceValues = sort(unique(soil[]))
                                                         ,selected =  setdiff(sort(unique(soil[])), soilno)
                                      )
                              )
                            )

                  ),
                  tabItem(tabName = "trees",
                          h4("Tree species"),
                          fluidRow(
                            column(8, shinycssloaders::withSpinner(
                              leafletOutput("map.trees",height = 500),type = 4,caption = "drawing map ",
                              id = "treemapSpin", hide.ui = F ) )
                            ,column(4,
                                    actionButton("treeapply", "Apply mask"),
                                    checkboxGroupInput("treemask", "Select tree species to include"
                                                       # choices =  info$corineInfo$landcover_class_values
                                                       ,choiceNames =  names(tree)
                                                       ,choiceValues =  names(tree)
                                                       ,selected =  names(tree)
                                    )
                            )
                          )
                  ),
                  tabItem(tabName = "canopy",
                          h2("Canopy Cover (%) threshold"),
                      shiny::sliderInput("rest.cc.thr", "Minimum Canopy Cover",
                                         min=0, max=100, value=50)
                      , shinycssloaders::withSpinner(
                        leafletOutput("map.cc",height = 500),type = 4,
                        caption = "drawing map",
                        id = "ccmapSpin", hide.ui = F )
                      # ,plotOutput("rest.treesp.hist")

                    ),
                  tabItem(tabName = "corine",
                          h4("CORINE land cover classes"),
                      fluidRow(
                         column(8, shinycssloaders::withSpinner(
                           leafletOutput("map.corine",height = 500),type = 4,caption = "drawing map",
                           id = "corinemapSpin", hide.ui = F ) )
                        ,column(4, #shinyWidgets::awesomeCheckboxGroup(
                          actionButton("corineapply", "Apply mask"),
                                checkboxGroupInput("corinemask", "Select corine classes to include"
                            # choices =  info$corineInfo$landcover_class_values
                            ,choiceNames = lapply( names(info$corineInfo$landcover_class_values),  function(x){ HTML(x) })
                            ,choiceValues = unname(info$corineInfo$landcover_class_values)
                           ,selected = info$corineInfo$landcover_class_values[selectedCorine]
                        )
                       )
                      )
                    )
                )
              )
)
