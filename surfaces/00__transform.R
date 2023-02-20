#!/usr/bin/env Rscript
#
# Load openCBS postcode shape and plot.
################################################################################

library( 'rgdal' )
library( 'sf' ) 

# outdir
outdir <- 'out.00.transform'
dir.create( outdir, showWarnings = FALSE )

# shape file with postcodes
infile <- 'data/WijkBuurtkaart_2021_v1/buurt_2021_v1.shp'

################################################################################
# PROJECTION
################################################################################

# open shapefile
shp <- sf::read_sf( infile )

# check projection
sf::st_crs( shp )

# reproject to ETRS_1989_LAEA, CRS:3035 
shp_etrs <- sf::st_transform( shp, 3035 )

# check the new projection
sf::st_crs( shp_etrs )

# save the layer in the new projection 
write_sf( shp_etrs, paste0( outdir, '/buurt_etrs.shp' ) )

# plot example
data <- shp_etrs[ shp_etrs$POSTCODE == '2741' & !is.na( shp_etrs$POSTCODE ), ]
plot( data, max.plot = 1 )
