# unzip downloaded daily files from https://marinecadastre.gov/ais/
# runs on specific month of data and generates daily files for the entire region


rm(list = ls())

library(utils)
yrmth = "202108" #change this for each month
inDir = choose.dir()
zipDir = paste0(inDir, yrmth)

outDir = choose.dir() #needs to be different directory
uzipDir= paste0(outDir, yrmth)

inFiles = list.files(zipDir, full.names = T)

for (ff in 1:length(inFiles)) {
  zipfile = inFiles[ff]
  unzip(zipfile, files = NULL, list = FALSE, overwrite = TRUE,
        junkpaths = FALSE, exdir = uzipDir, unzip = "internal",
        setTimes = FALSE)
}


