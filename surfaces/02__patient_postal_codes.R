###########################################################
# Plot postal codes of patients on map of the Netherlands #
###########################################################

# packages
library( "rgdal" )
library( "sf" ) 
library( "ggplot2" )

# wd
setwd( "Documents/PhD-traject/RAPSODE/" )

# outdir
outdir <- "out.patient.postal.codes"
dir.create( outdir, showWarnings = FALSE )

# patient postal codes
pc_list <- read.csv( "data/postal_codes.csv" )
colnames( pc_list ) <- "POSTCODE"

# shape file with postcodes per 'buurt'
infile <- "out.00.transform/buurt_etrs.shp"

# open shapefile
shp_etrs <- sf::read_sf( infile )
shp_etrs <- shp_etrs[ shp_etrs$POSTCODE != "-99999999" & !is.na( shp_etrs$POSTCODE ), colnames( shp_etrs ) %in% c( "POSTCODE", "geometry" ) ]

# center of 'buurten' as coordianates for the postal codes
sf_cent <- st_centroid( shp_etrs )

# df with unique postal codes
sf_cent_unique <- sf_cent[ ! duplicated( sf_cent$POSTCODE ) , ]

# average coordinates of same postal codes
for( i in 1:nrow( sf_cent_unique ) ){
  
  # get postal code
  post <- sf_cent_unique$POSTCODE[ i ]

  # subset df
  df <- subset( sf_cent, sf_cent$POSTCODE == post )

  # calculate coordinate means
  coord_x <- mean( st_coordinates( df$geometry )[ 1:nrow( df ) ] )
  coord_y <- mean( st_coordinates( df$geometry )[ ( nrow( df ) + 1 ):( nrow( df ) * 2 ) ] )
  
  # convert to sf object
  sf <- st_point( c( coord_x, coord_y ) )
  
  # replace old values with new mean values
  sf_cent_unique$geometry[ sf_cent_unique$POSTCODE == post ] <- sf
}

# calculate frequency of each postal code
pc_freq <- as.data.frame( table( pc_list$POSTCODE ) )
colnames( pc_freq ) <- c( "POSTCODE", "freq" )

# merge matrices -- all individual postal codes
sf_pc <- merge( pc_list, sf_cent_unique, by = "POSTCODE", all.x = T )
sf_pc <- st_as_sf( sf_pc )

# merge matrices -- postal codes with absolute frequency
sf_pc_freq <- merge( pc_freq, sf_cent_unique, by = "POSTCODE", all.x = T )
sf_pc_freq <- st_as_sf( sf_pc_freq )

# plot individual postal codes
# 3 missing postal codes (no coordinates)
p1 <- ggplot() + 
  geom_sf( data = shp_etrs, fill = "grey", lwd = 0 ) +
  geom_sf( data = sf_pc, col = "darkred" ) +
  theme( legend.position = "none", plot.title = element_text( size = 14, hjust = 0.5 ) ) +
  ggtitle( "Postal codes of included patients" )

# save plot
ggsave( p1, file = paste0( outdir, '/patient_postal_codes_1.png' ), dpi = 300, width = 10, height = 10 )

# plot with dot color relative to postal code frequency
# 3 missing postal codes (no coordinates)
p2 <- ggplot() + 
  geom_sf( data = shp_etrs, fill = "grey", lwd = 0 ) +
  geom_sf( data = sf_pc_freq, aes( color = freq ) ) +
  theme( legend.position = "right", plot.title = element_text( size = 14, hjust = 0.5 ) ) +
  scale_color_gradient( low = "dodgerblue3", high = "red" ) +
  ggtitle( "Postal codes of included patients (absolute frequency)" )

# save plot
ggsave( p2, file = paste0( outdir, '/patient_postal_codes_2.png' ), dpi = 300, width = 10, height = 10 )
