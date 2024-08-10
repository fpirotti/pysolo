
# for(nutsid in unique(nutsf.l1.noGeom$NUTS_ID) ){
#
#   nutsf2 <- nuts2 %>% filter( grepl(nutsid, NUTS_ID) )
#   nutsfnoGeom2 <- sf::st_drop_geometry(nutsf2)
#   countryName2 <- nutsfnoGeom2 %>% filter(LEVL_CODE==1) %>% select(NAME_LATN)
#   countryName2 <- countryName2[[1]]
#
#   nutsf.l1 <- nutsf2 %>% filter(LEVL_CODE==2)
#   nutsf.l1.noGeom <- nutsfnoGeom2 %>% filter(LEVL_CODE==2)
#
#   nuts2print <- nutsf.l1.noGeom %>% select(`Subdivisions - NUTS 1`,
#                                     Potential_Area,Potential_Biomass,
#                                     Effective_Area,Effective_Biomass,
#                                     DNI_Minimum,DNI_Mean,
#                                     DNI_Maximum,DNI_StdDev   )
#
#
#   kk <- kable(nuts2print, caption = sprintf("Potential/Effective Total Area (ha) and Biomass (Mg): %s",countryName ),
#               digits = c(0, 0, 0, 0,0,0,0,0, 2 ),
#               format="latex", booktabs=TRUE,
#               format.args = list(big.mark = " ")) %>%
#     kable_styling(latex_options=c("scale_down", "hold_position"),  font_size = 7, html_font = "Arial Narrow") %>%
#     kable_paper(full_width = F)
#   print(kk)
#
#   cat("\n")
#
#
#
#   nuts2printPerHA <- nutsf.l1.noGeom %>%
#                                           mutate( EffectiveOverPotential= Effective_Area / Potential_Area *100) %>% select(`Subdivisions - NUTS 1`,
#                                                 `Potential_Area__%`,`Effective_Area__%`, EffectiveOverPotential)
#
#
#   kk <- kable(nuts2printPerHA, caption = sprintf("Potential/Effective Area (ha) as percent of NUTS Area and Biomass percentage Effective over Potential: %s",countryName ),
#               digits = c(0, 0, 0, 0 ),
#               format="latex", booktabs=TRUE,
#               format.args = list(big.mark = " ")) %>%
#     kable_styling(latex_options=c("scale_down", "hold_position"),   font_size = 7, html_font = "Arial Narrow") %>%
#     kable_paper(full_width = F)
#   print(kk)
#
#
#
#
#     nuts2printMCA <-  nutsf.l1.noGeom %>% select(`Subdivisions - NUTS 1`,
#                                     MCA_Class1_Area,
#                                     MCA_Class2_Area,
#                                     MCA_Class3_Area,
#                                     MCA_Class4_Area,
#                                     MCA_Class5_Area
#                                     )
#     names(nuts2printMCA) <- c("Subdivisions - NUTS 1",
#                                     "Poor", "Acceptable", "Medium", 'Very good', "Excellent"
#                                     )
#
#   nuts2printMCA <-  nuts2printMCA  %>% dplyr::arrange(`Subdivisions - NUTS 1`)
#
#
#     kk <- kable(nuts2printMCA, caption = sprintf("MCA Total Area: %s",countryName ),
#               digits = c(0  ),
#               format="latex", booktabs=TRUE,
#               format.args = list(big.mark = " ")) %>%
#     kable_styling(latex_options=c("scale_down", "hold_position"),   font_size = 7, html_font = "Arial Narrow") %>%
#     kable_paper(full_width = F)
#   print(kk)
#
#
#
#  nuts2printMCAPerHA <- nuts2printMCA  %>% mutate_if(is.numeric, ~normArea(., as.numeric(nutsf.l1$Area_Ha), T) )
#
#     kk <- kable(nuts2printMCAPerHA, caption = sprintf("MCA Quality (percent of total Area): %s",countryName ),
#               digits = c(0),
#               format="latex", booktabs=TRUE,
#               format.args = list(big.mark = " ")) %>%
#     kable_styling(latex_options=c("scale_down", "hold_position"),  font_size = 7, html_font = "Arial Narrow") %>%
#     kable_paper(full_width = F)
#   print(kk)
#
#
#   cat("\\pagebreak\n")
#
# }
