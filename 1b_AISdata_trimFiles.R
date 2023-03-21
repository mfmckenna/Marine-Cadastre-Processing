# Trim AIS files to region of interest

rm(list = ls())

WGS84proj = 4326

# TOTAL ANALYSIS AREA (set to Gulf of Mexico)
#-----------------------------------------------------
north_lat = -81.999996233
west_lng  = 24.5 
south_lat = -97.919325572
east_lng  =  30.982964192
gbsuy = "gbsuy"
df = data.frame(rbind(
  c( north_lat, south_lat, east_lng,  west_lng),
  c( north_lat, south_lat, east_lng,  west_lng) ) )

colnames(df) = c("north_lat","south_lat", "east_lng",  "west_lng")
lst <- lapply(1:nrow(df), function(x){
  ## create a matrix of coordinates that also 'close' the polygon
  res <- matrix(c(df[x, 'north_lat'], df[x, 'west_lng'],
                  df[x, 'north_lat'], df[x, 'east_lng'],
                  df[x, 'south_lat'], df[x, 'east_lng'],
                  df[x, 'south_lat'], df[x, 'west_lng'],
                  df[x, 'north_lat'], df[x, 'west_lng'])  ## need to close the polygon
                , ncol =2, byrow = T
  )
  ## create polygon objects
  st_polygon(list(res))
  
})
sfdf <- st_sfc ((lst), crs = WGS84proj)

# LARGE AIS FILES (Change direcory set up)
#-----------------------------------------------------
AIS.dir <- paste0("H:\\AIS_MarineCad\\data\\")
inFiles = list.files(path = AIS.dir, pattern="AIS_20", recursive=TRUE, full.names = TRUE) 
out.Dir = paste0(AIS.dir, "GoMexRegion")

# LOOP TO TRIM FILES TO Region of Interest-- sfdf
#-----------------------------------------------------
for (ff in 1:length(inFiles)) { # ff = 1 #loop through days
  
  cat("Processing...", ff, ' of ', length(inFiles), "\n") 
  
  df  = read.table(inFiles[ff], header=T, fill=T, sep=",", na.strings=c(""," ","null", "NA")) #HUGE DAILY FILES!!
  
  # data clean up
  df$timestamp  <- strptime(df$BaseDateTime, "%Y-%m-%dT%H:%M:%S", tz = "GMT") # prepare the timestamp 
  df = df[!is.na(df$timestamp),] #remove NA
  df$LON = as.numeric(as.character( df$LON))#format
  #make the AIS data "spatial" according to R
  df2 = st_as_sf(x = df,  coords = c("LON", "LAT"), crs = WGS84proj)
  DY = unique(as.Date(df$timestamp))
  
  #INTERSECT DATA IN ROI
  tst = lengths(st_intersects(df2, sfdf)) > 0
  df2t = df2[tst,]
  df2t$day = as.Date( df2t$timestamp )     
  df2t$hr  = hour( df2t$timestamp )
  df2t$Length = as.numeric(as.character(df2t$Length))
 
  #write out R data file
  save(df2t, file = paste0(out.Dir, "\\AIS_GoMex_", DY, ".RData") )
  
}

#setwd("H:\\AIS_MarineCad\\data\\GoMexRegion")
#load("H:/AIS_MarineCad/data/GoMexRegion/AIS_GoMex_2020-08-01.RData")
