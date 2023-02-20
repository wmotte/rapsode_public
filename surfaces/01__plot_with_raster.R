#!/usr/bin/env Rscript
#
# Load openCBS postcode shape and plot.
################################################################################
library( 'rgdal' )
library( 'sf' ) 
library( 'raster' )

# outdir
outdir <- 'out.01.plot_with_raster'
dir.create( outdir, showWarnings = FALSE )

# load ETRS-transformed shape file with postcodes
infile <- 'out.00.transform/buurt_etrs.dbf'


################################################################################
# PROJECTION
################################################################################

# open shapefile
shp <- sf::read_sf( infile )

# check projection [ETRS_1989_LAEA, CRS:3035 ]
sf::st_crs( shp )

# remove all rows with no postcodes
data <- shp[ !is.na( shp$POSTCODE ), colnames( shp ) %in% c( 'POSTCODE', 'geometry' ) ]

# get centroids
data$mid <- sf::st_centroid( data$geometry )

# Waddinxveen only
# data <- data[ data$POSTCODE == '2741', ]

datac <- data
datac$geometry <- datac$mid

#####################
# extract data
#####################

# import raster

# Annual mean O3 concentrations	µg/m3
ras_annual_mean_O3 <- raster::raster( "dgk_data_uu_nl/Physico-chemical/AirPollution/032010a.tif" ) 

# extract values from raster
data$annual_mean_O3 <- raster::extract( ras_annual_mean_O3, datac )

# get plot 
plot <- ggplot( data ) +
    geom_sf( colour = "white" ) +
    geom_sf( aes( geometry = mid, colour = annual_mean_O3 ) ) +
    theme( legend.position = 'top' ) +
    guides( colour = guide_legend( "Annual mean O3 (µg/m3)" ) )

# plot
ggsave( file = paste0( outdir, '/plot_annual_mean_O3.png' ), plot = plot, dpi = 120, width = 20, height = 20 )



# Fine particles 10 µm
ras_fine_particles_10microm <- raster::raster( "dgk_data_uu_nl/Physico-chemical/Emissions/pm10_emi_10km.tif" ) 

# extract values from raster
data$fine_particles_10microm <- raster::extract( ras_fine_particles_10microm, datac )

# get plot 
plot <- ggplot( data ) +
    geom_sf( colour = "white" ) +
    geom_sf( aes( geometry = mid, colour = fine_particles_10microm ) ) +
    theme( legend.position = 'top' ) +
    guides( colour = guide_legend( "Fine particles (10 µm; tonnes)" ) )

# plot
ggsave( file = paste0( outdir, '/plot_fine_particles_10microm.png' ), plot = plot, dpi = 120, width = 20, height = 20 )



# Sulphur oxides (SOx)
ras_sulphur_oxides <- raster::raster( "dgk_data_uu_nl/Physico-chemical/Emissions/sox_emi_10km.tif" ) 

# extract values from raster
data$sulphur_oxides <- raster::extract( ras_sulphur_oxides, datac )

# get plot 
plot <- ggplot( data ) +
    geom_sf( colour = "white" ) +
    geom_sf( aes( geometry = mid, colour = sulphur_oxides ) ) +
    theme( legend.position = 'top' ) +
    guides( colour = guide_legend( "Sulphur oxides (SOx; tonnes)" ) ) +
    scale_fill_gradient2(
        low = "grey", 
        mid = "white", 
        high = "brown", 
        midpoint = 2000
    )

# plot
ggsave( file = paste0( outdir, '/plot_sulphur_oxides.png' ), plot = plot, dpi = 120, width = 20, height = 20 )

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(1, 8))



#png( filename = 'postcodes.png', width = 3000, height = 3000 )

#plot( datac, alpha = 0.5 )
#plot( ras_water, add = TRUE  )

#dev.off()

################################################################################
#EXTRACT VALUE FROM RASTER TO POINTS



# check raster projection 
#sf::st_crs( ras )

# get example data for single city
#data_example <- data[ data$POSTCODE == '2741', ]
#plot( data_example, max.plot = 1 )


#plot( data_centroid, max.plot = 1 )



# extract values from raster
#data_example$extracted_value <- raster::extract( ras, data_example )
#data_centroid$extracted_value <- as.vector( raster::extract( ras, data_centroid ) )

#save the shapefile layer with the extracted values 
#write_sf( shp, "test_extracted.shp" )


#cols_to_remove <- "BU_CODE"    "BU_NAAM"    "WK_CODE"    "GM_CODE"    "GM_NAAM"    "IND_WBI"    "H2O"        
# "POSTCODE"   "DEK_PERC"   "OAD"        "STED"       "BEV_DICHTH"
#[13] "AANT_INW"   "AANT_MAN"   "AANT_VROUW" "P_00_14_JR" "P_15_24_JR" "P_25_44_JR" "P_45_64_JR" "P_65_EO_JR" 
# "P_ONGEHUWD" "P_GEHUWD"   "P_GESCHEID" "P_VERWEDUW"
#[25] "AANTAL_HH"  "P_EENP_HH"  "P_HH_Z_K"   "P_HH_M_K"   "GEM_HH_GR"  "P_WEST_AL"  "P_N_W_AL"   "P_MAROKKO"  
# "P_ANT_ARU"  "P_SURINAM"  "P_TURKIJE"  "P_OVER_NW" 
#[37] "OPP_TOT"    "OPP_LAND"   "OPP_WATER"  "JRSTATCODE" "JAAR"       "Shape_Leng" "Shape_Area" "geometry"

#library( 'ggplot2' )



# get plot
#plot <- ggplot( data ) + geom_sf( aes( fill = POSTCODE ) )


