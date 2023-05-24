#!/usr/bin/Rscript

#-------------------------------------------#
#### Get deadwood information from SFNI3 ####
#     Aitor Vázquez Veloso, 03/05/2023      #
#-------------------------------------------#


#### Aim of the script ####

# That script is developed with the aim to use the 3rd Spanish National Forest
# Inventory (SFNI, IFN in spanish) to estimate plots with and without deadwood,
# as a first step to train a ML algorithm able to detect that difference from
# satellite images.
# Resources:
# SFNI: https://www.miteco.gob.es/es/biodiversidad/temas/inventarios-nacionales/inventario-forestal-nacional/default.aspx


#### Basic steps ####

# path (insert the path to the downloaded GitHub repo)
# here a guide: https://www.r4epi.com/file-paths.html
setwd("./BIP_2022-23/") 

# libraries
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("openxlsx")

library(plyr)
library(dplyr)
library(stringr)
library(openxlsx)

# SFNI3 data
trees3 <- read.csv('data/0_raw/pcmayores.csv', sep = ',')
plots3_type <- read.csv('data/0_raw/pcparcelas.csv', sep = ',')
plots3 <- read.csv('data/0_raw/pcdatosmap.csv', sep = ',')

# test data (already filtered on the raw data provided)
trees3 <- trees3[trees3$Origen == 49, ]
plots3 <- plots3[plots3$Provincia == 49, ]
plots3_type <- plots3_type[plots3_type$Origen == 49, ]

#### Data management ####

# Rename variables
trees3 <- rename(trees3, c("provincia" = "Origen",
                           "estadillo" = "Estadillo",
                           "clase" = "Cla",
                           "subclase" = "Subclase",
                           "TREE_ID_IFN3_2" = "nArbol",
                           "TREE_ID_IFN3" = "OrdenIf3",
                           "TREE_ID_IFN2" = "OrdenIf2",
                           "rumbo" = "Rumbo",
                           "distancia" = "Distanci",
                           "especie" = "Especie",
                           "dbh_1" = "Dn1",
                           "dbh_2" = "Dn2",
                           "h" = "Ht",
                           "calidad" = "Calidad",
                           "forma" = "Forma",
                           "parametros_especiales" = "ParEsp"
))

plots3 <- rename(plots3, c("X" = "CoorX",
                           "Y" = "CoorY",
                           "Fcc" = "FccArb"

                           
))



# Filter variables needed 
#trees3 <- select(trees3, )
plots3 <- select(plots3, Provincia, Estadillo, Clase, Subclase, X, Y, Fcc)

# create IDs
trees3$INVENTORY_ID <- 'IFN3'
plots3$INVENTORY_ID <- 'IFN3'
plots3_type$INVENTORY_ID <- 'IFN3'

trees3$PLOT_ID <- paste(trees3$provincia, trees3$estadillo, sep = '_')
plots3$PLOT_ID <- paste(plots3$Provincia, plots3$Estadillo, sep = '_')
plots3_type$PLOT_ID <- paste(plots3_type$Provincia, plots3_type$Estadillo, sep = '_')

# original tree ID
trees3$TREE_ID <- paste(trees3$provincia, trees3$estadillo, trees3$TREE_ID_IFN2, sep = '_')

# tree ID to compare among SFNI editions
trees3$IFN_TREE_ID <- paste(trees3$INVENTORY_ID, trees3$provincia, trees3$estadillo, trees3$TREE_ID_IFN2, sep = '_')

# special ID for IFN3
# plots3$IFN3_ID <- paste(plots3$PLOT_ID, plots3$Clase, plots3$Subclase, sep = '_') # has NAs
trees3$IFN3_ID <- paste(trees3$PLOT_ID, trees3$clase, trees3$subclase, sep = '_')
plots3_type$IFN3_ID <- paste(plots3_type$PLOT_ID, plots3_type$Cla, plots3_type$Subclase, sep = '_')


#### Calculate tree variables ####

# just variables that will be needed on SIMANFOR are calculated

# dbh (cm)
trees3$dbh <- (trees3$dbh_1 + trees3$dbh_2)/20

# expansion factor
trees3$expan <- with(trees3, 
                         ifelse (dbh < 7.5, 0, 
                                 ifelse(dbh < 12.5, 10000/(pi*(5^2)), 
                                        ifelse(dbh < 22.5, 10000/(pi*(10^2)), 
                                               ifelse(dbh < 42.5, 10000/(pi*(15^2)),
                                                      10000/(pi*(25^2)))))))

# basal area (cm2)
trees3$g <- ((pi)/4)*(trees3$dbh^2)

# Slenderness
trees3$slenderness <- trees3$h*100/trees3$dbh

# dead (1) or alive (0) tree
trees3$dead <- ifelse(trees3$calidad == 6, 1, 0)

# delete trees without IFN3 data (dead, harvested...)
trees3 <- trees3[!is.na(trees3$dbh), ]
trees3 <- trees3[trees3$dbh != 0, ]


#### Calculate plot variables ####

# just variables that will be needed on SIMANFOR are calculated

# variables from trees
plot3_vars <- ddply(trees3, c('IFN3_ID', 'PLOT_ID'), summarise,
                    SUM_DBH = sum(dbh*expan, na.rm = TRUE),
                    SUM_H = sum(h*expan, na.rm = TRUE),
                    G  = sum(g*expan/10000, na.rm = TRUE),
                    N = sum(expan, na.rm = TRUE),
                    CD_0_75 = sum(ifelse(dbh <= 7.5, expan, 0), na.rm = TRUE),
                    CD_75_125 = sum(ifelse(dbh > 7.5 & dbh <= 12.5, expan, 0), na.rm = TRUE),
                    CD_125_175 = sum(ifelse(dbh > 12.5 & dbh <= 17.5, expan, 0), na.rm = TRUE),
                    CD_175_225 = sum(ifelse(dbh > 17.5 & dbh <= 22.5, expan, 0), na.rm = TRUE),
                    CD_225_275 = sum(ifelse(dbh > 22.5 & dbh <= 27.5, expan, 0), na.rm = TRUE),
                    CD_275_325 = sum(ifelse(dbh > 27.5 & dbh <= 32.5, expan, 0), na.rm = TRUE),
                    CD_325_375 = sum(ifelse(dbh > 32.5 & dbh <= 37.5, expan, 0), na.rm = TRUE),
                    CD_375_425 = sum(ifelse(dbh > 37.5 & dbh <= 42.5, expan, 0), na.rm = TRUE),
                    CD_425_ = sum(ifelse(dbh > 42.5, expan, 0), na.rm = TRUE),
                    Deadwood = sum(ifelse(1 %in% dead, 1, 0))
)

plot3_vars$DBHm <- plot3_vars$SUM_DBH/plot3_vars$N
plot3_vars$Hm <- plot3_vars$SUM_H/plot3_vars$N
plot3_vars$Dg <- with(plot3_vars, 200*(G/N/pi)^0.5, na.rm=TRUE)

# We will need two functions to calculate Dominant Height

# Function 1
dominantHeight <- function(x, plotID="PLOT_ID") {
  if(plotID %in% names(x)) {
    PLOT_ID = unique(x[[plotID]])
    Ho = rep(NA, length(PLOT_ID))
    names(Ho) = PLOT_ID
    for(i in 1:length(PLOT_ID)) {
      Ho[i] = .domheight(x$h[x[[plotID]] ==PLOT_ID[i]],
                         x$dbh[x[[plotID]]  ==PLOT_ID[i]],
                         x$expan[x[[plotID]]  ==PLOT_ID[i]])
    }
    Hd <- data.frame(PLOT_ID, Ho)
    return(Hd)
  }
  return(.domheight(x$h, x$d, x$n))
}

# Function 2
.domheight<-function(h, d, n) {
  o <-order(d, decreasing=TRUE)
  h = h[o]
  n = n[o]
  ncum = 0 
  for(i in 1:length(h)) {
    ncum = ncum + n[i]
    if(ncum>100) return(sum(h[1:i]*n[1:i], na.rm=TRUE)/sum(h[1:i]*n[1:i]/h[1:i], na.rm=TRUE))
  }
  return(sum(h*n)/sum(n))
}

# calculate Ho and join data
Ho_3 <- dominantHeight(trees3, 'PLOT_ID')
plot3_vars <- merge(plot3_vars, Ho_3, by.x=c('PLOT_ID'), by.y=c('PLOT_ID'))

# join new data to the original 
plots3 <- merge(plot3_vars, plots3, all = TRUE, by.x=c('PLOT_ID'), by.y=c('PLOT_ID'))


# Do (cm)
# Función 1
DiametroDominante <- function(x, plotID = "PLOT_ID"){
  if(plotID %in% names(x)) {
    IDs = unique(x[[plotID]])
    Do = rep(NA, length(IDs))
    names(Do) = IDs
    for(i in 1:length(IDs)) {
      Do[i] = .DiametroDominante_2(x$d[x[[plotID]] == IDs[i]],
                                   x$dbh[x[[plotID]] == IDs[i]],
                                   x$expan[x[[plotID]] == IDs[i]])
    }
    Dd <- data.frame(IDs, Do)
    return(Dd)
  }
  return(.DiametroDominante_2(x$h, x$d, x$n))
}

# Función 2
.DiametroDominante_2 <- function(h, d, n){
  o <- order(d, decreasing=TRUE)
  d = d[o]
  n = n[o]
  ncum = 0 
  for(i in 1:length(d)){
    ncum = ncum + n[i]
    if(ncum>100) return(sum(d[1:i]*n[1:i], 
                            na.rm=TRUE)/sum(d[1:i]*n[1:i]/d[1:i], 
                                            na.rm=TRUE))
  }
  return(sum(d*n)/sum(n))
}

# Calculate Do and join datasets
Do_3 <- DiametroDominante(trees3, 'PLOT_ID')
plots3 <- merge(Do_3, plots3, all = TRUE, by.x=c('IDs'), by.y=c('PLOT_ID'))

# slenderness
plots3$slenderness <- plots3$Hm*100/plots3$DBHm  # esbeltez normal
plots3$dominant_slenderness <- plots3$Ho*100/plots3$Do  # esbeltez dominante

# SDI
valor_r <- -1.605 # valor editable dependiendo de la especie (consultar bibliografía)
plots3$SDI <- plots3$N*((25/plots3$Dg)**valor_r)

# Cálculo del Índice de Hart (S)
# Índice de Hart-Becking
plots3$S <- 10000/(plots3$Ho*sqrt(plots3$N))  
# Índice de Hart-Becking para masas al tresbolillo
plots3$S_staggered <- (10000/plots3$Ho)*sqrt(2/plots3$N*sqrt(3))  

# clean plots without trees
plots3 <- plots3[!is.na(plots3$N), ]

# clean environment
rm(dominantHeight, DiametroDominante, valor_r, plot3_vars, Ho_3, Do_3)


#### Variables by species ####

# We calculate variables by species to set the main stand species by G criteria

# Calculate G and N by species
plots3_vars_sp <- ddply(trees3, c('IFN3_ID', 'especie'), summarise, 
                       G_sp = sum(g*expan/10000, na.rm = TRUE), 
                       N_sp = sum(expan, na.rm = TRUE)
)

# organize information by PLOT_ID and G
plots3_vars_sp <- plots3_vars_sp %>%
  arrange(IFN3_ID, -G_sp)

# N and G by species
plots_useful_IFN3 <- data.frame()
plots_list <- plots3_vars_sp[!duplicated(plots3_vars_sp$IFN3_ID), ]  # skip duplicated plots
for (k in plots_list$IFN3_ID) {
  
  plots_k <- plots3_vars_sp[plots3_vars_sp$IFN3_ID %in% k,]
  
  plots_k$sp_1 <- plots_k$especie[1]
  plots_k$sp_2 <- plots_k$especie[2] 
  plots_k$sp_3 <- plots_k$especie[3] 
  plots_k$G_sp_1 <- plots_k$G_sp[1]
  plots_k$G_sp_2 <- plots_k$G_sp[2]
  plots_k$G_sp_3 <- plots_k$G_sp[3]
  plots_k$N_sp_1 <- plots_k$N_sp[1]
  plots_k$N_sp_2 <- plots_k$N_sp[2]
  plots_k$N_sp_3 <- plots_k$N_sp[3]
  
  plots_useful_IFN3 <- rbind(plots_useful_IFN3, plots_k)
}

# delete duplicated data
plots_useful_IFN3 <- plots_useful_IFN3[!duplicated(plots_useful_IFN3$IFN3_ID), ]
plots_useful_IFN3 <- select(plots_useful_IFN3, -c(especie, N_sp, G_sp))

# merge information with original data
plots3 <- merge(plots_useful_IFN3, plots3, all = TRUE, by.x=c('IFN3_ID'), by.y=c('IFN3_ID'))

# clean environment
rm(plots_k, plots_list, plots_useful_IFN3, plots3_vars_sp, k)


#### Variables by dead/alive ####

# Calculate G and N by dead/alive
plots3_vars_dead <- ddply(trees3, c('IFN3_ID', 'dead'), summarise, 
                        G_sp = sum(g*expan/10000, na.rm = TRUE), 
                        N_sp = sum(expan, na.rm = TRUE)
)

# organize information by PLOT_ID and G
plots3_vars_dead <- plots3_vars_dead %>%
  arrange(IFN3_ID, -dead)

# N and G by species
plots_useful_IFN3_dead <- data.frame()
plots_list_dead <- plots3_vars_dead[!duplicated(plots3_vars_dead$IFN3_ID), ]  # skip duplicated plots
for (k in plots_list_dead$IFN3_ID) {
  
  plots_k <- plots3_vars_dead[plots3_vars_dead$IFN3_ID %in% k,]
  
  plots_k$G_alive <- plots_k$G_sp[1]
  plots_k$G_dead <- plots_k$G_sp[2]
  plots_k$N_alive <- plots_k$N_sp[1]
  plots_k$N_dead <- plots_k$N_sp[2]

  plots_useful_IFN3_dead <- rbind(plots_useful_IFN3_dead, plots_k)
}

# delete duplicated data
plots_useful_IFN3_dead <- plots_useful_IFN3_dead[!duplicated(plots_useful_IFN3_dead$IFN3_ID), ]
plots_useful_IFN3_dead <- select(plots_useful_IFN3_dead, -c(dead, N_sp, G_sp))

# merge information with original data
plots3 <- merge(plots_useful_IFN3_dead, plots3, all = TRUE, by.x=c('IFN3_ID'), by.y=c('IFN3_ID'))

# clean environment
rm(plots_k, plots_list_dead, plots_useful_IFN3_dead, plots3_vars_dead, k)


#### Forest type information ####

# different between natural and plantation
plots3_type$forest_type <- ifelse(plots3_type$Nivel3 == 1, 'natural', ifelse(
  plots3_type$Nivel3 == 2, 'plantation', 'other'))

# select variables
plots3_type <- select(plots3_type, c(IFN3_ID, forest_type))

# merge with plots3
plots3 <- merge(plots3_type, plots3, all = TRUE, by.x=c('IFN3_ID'), by.y=c('IFN3_ID'))

# delete plots without information (no tree data)
plots3 <- plots3[!is.na(plots3$N), ]


#### Last details ####

# organize data
trees3 <- select(trees3,
                 INVENTORY_ID, provincia, estadillo, clase, subclase, PLOT_ID, 
                 IFN_TREE_ID, especie, dead, expan, dbh, g, h, slenderness, 
                 rumbo, distancia, calidad, forma, parametros_especiales, 
                 Agente, Import, Elemento)
plots3 <- select(plots3,
                 INVENTORY_ID, Provincia, Estadillo, Clase, Subclase, IFN3_ID, 
                 X, Y, forest_type, Deadwood, sp_1, sp_2, sp_3, 
                 N, N_dead, N_alive, N_sp_1, N_sp_2, N_sp_3,
                 CD_0_75, CD_75_125, CD_125_175, CD_175_225, CD_225_275, 
                 CD_275_325, CD_325_375, CD_375_425, CD_425_, DBHm, Dg, Do, 
                 G, G_dead, G_alive, G_sp_1, G_sp_2, G_sp_3,
                 Hm, Ho, Fcc, slenderness, dominant_slenderness, SDI, S, S_staggered)

# rename
trees3 <- rename(trees3, c(
  "province" = "provincia", 
  "notebook" = "estadillo", 
  "class" = "clase", 
  "subclass" = "subclase", 
  "species" = "especie", 
  "bearing" = "rumbo", 
  "distance" = "distancia", 
  "quality" = "calidad", 
  "shape" = "forma", 
  "special_parameters" = "parametros_especiales", 
  "agent" = "Agente", 
  "importance" = "Import", 
  "element" = "Elemento")
  )

plots3 <- rename(plots3, c(
  "province" = "Provincia", 
  "notebook" = "Estadillo", 
  "class" = "Clase", 
  "subclass" = "Subclase", 
  "species_1" = "sp_1",
  "species_2" = "sp_2",
  "species_3" = "sp_3",
  "Forest_type" = "forest_type")
)


#### Export data ####

# export to .csv
write.csv(plots3, file = "data/1_processed/plots_IFN3_Zamora.csv", fileEncoding = "UTF-8")
write.csv(trees3, file = "data/1_processed/trees_IFN3_Zamora.csv", fileEncoding = "UTF-8")

# export to .xlsx
wb <- createWorkbook(creator = "Aitor")
saveWorkbook(wb, file = "data/1_processed/IFN3_Zamora.xlsx", overwrite = TRUE)
renameWorksheet(wb, 1, "Plots")
addWorksheet(wb, "Trees")

writeData(wb,
          "Plots", 
          plots3,
          startCol = 1,
          startRow = 1,
          xy = NULL,
          colNames = TRUE,
          rowNames = FALSE)
writeData(wb,
          "Trees", 
          trees3,
          startCol = 1,
          startRow = 1,
          xy = NULL,
          colNames = TRUE,
          rowNames = FALSE)

saveWorkbook(wb, file = "data/1_processed/IFN3_Zamora.xlsx", overwrite = TRUE)
