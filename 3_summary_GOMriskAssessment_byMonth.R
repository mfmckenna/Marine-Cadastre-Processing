# GENERATE RISK ASSESSMENT RESULTS FOR EACH Month-Site-Species

rm(list=ls())

# libraries #### 
library(lubridate)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(sf)
sf_use_s2(FALSE) #avoids error with st_intersects

# set up params #### 
# for mapping
WGS84proj = 4326
plt = "on"
sf_use_s2(FALSE) #avoids error with st_intersects
theme_set(theme_bw())
world = ne_countries(scale = "medium", returnclass = "sf")

mthName=NULL
mthName$Sp  = c(202107, 202008,202009,202010,202011,202012,202101,202102,202103,202104,202105,202106)
mthName$Lr  = c("August","August","August","October","October","October","February","February","Feburary","May","May","May") # for listening range data 
mthName$Sei  = c("July","August","September","October","November","December","January","February","March","April","May","June") # seismic surveys days
mthName$AIS  = c("2021-07", "2020-08","2020-09","2020-10","2020-11","2020-12","2021-01","2021-02","2021-03","2021-04","2021-05","2021-06") # for AIS data
mthName$PAM  = c(7,8,9,10,11,12,1,2,3,4,5,6) # for AIS data
mthName = as.data.frame(mthName) # unique file naming by month

# directories #### 
wrkDir = "G:\\My Drive\\ActiveProjects\\COA\\NFWF_GOM_workingDrive\\" 
AIS.dir =   paste0("H:\\AIS_MarineCad\\data\\GoMexRegion\\GoMexRegion_trim")
PAM.Dir   = paste0(wrkDir, "data\\PAM")

# Input data ####
## PAM ####
dataPAM   = read.csv(paste0(wrkDir, "data\\PAM\\GoM_PAMsites_CompiledV2.csv")) 

## species #### 
# (from 2_integrate_riskAssessmentV2.R)
dataSpecies   = list.files( path = paste0(wrkDir, "data\\RiskAssessment\\"), pattern = "SpOut_noVul", recursive=F, full.names = T )

## noise activity ####
# (from 2_integrate_riskAssessmentV2.R)
dataNoise  = list.files( path = paste0(wrkDir, "data\\RiskAssessment\\"), pattern = "NaOut_daily", recursive=FALSE, full.names = T )

#  vulnerability ####
flagV = 1 # do not use!
filesVul = list.files( path = paste0(wrkDir, "data\\EWG_GOMEX\\"), pattern = ".csv", recursive=TRUE, full.names = T )

#  seismic days ####
# NFWF seismic dates_ALL MONTHS_toR.xlsx AND GoMex_Seismic_MonthSummary.csv
dataSeiMth = read.csv( list.files( path = paste0(wrkDir, "data\\Seismic\\"), pattern = "MonthSummary.csv", recursive=TRUE, full.names = T ) )
dataSeiMth$rank = rank( dataSeiMth$MONTHLY.TOTAL.SEISMIC.DAYS )
dataSeiDay = NULL
for (ii in 1:nrow(mthName)){
  tmp = as.data.frame ( readxl::read_excel( list.files( path = paste0(wrkDir, "data\\Seismic\\"), pattern = "toR.xlsx", recursive=TRUE, full.names = T ), sheet = mthName$Sei [ii]) )
  tmp$Mth = mthName$Sei [ii]
  tmp$Date = as.Date(tmp$Date,format="%d-%b-%y")
  dataSeiDay = rbind(dataSeiDay,tmp)
}

## spatial layers ####
# (see 2_integrate_PAMsites.R)

# Monthly Processing  ####
for (mm in 1:nrow(mthName)){
  
  currrent_Mth = mthName$Sp[mm]
  currrent_MthLab = mthName$Sei[mm]
  cat("Processing data for...", currrent_Mth)
  
  #SPECIES PRESENCE ####
  # total density
  fileIdx = which( sapply(strsplit( gsub(".RData", "", basename( dataSpecies ) ) , "_"), tail, 1) == mthName$Sp[mm] )
  load(dataSpecies [fileIdx] ) # Species Out
  SpeciesOut$ListeingSpaceName = paste(SpeciesOut$Source, SpeciesOut$SNR, sep = "_")
  
  totalSpecies =  ( SpeciesOut %>% group_by(ListeingSpaceName, SpeciesLabel) %>%
                      summarize(totalDensity = sum(Density_LS) ) )
  totalSpecies$mth = mthName$Sp[mm]
  totalSpecies = as.data.frame(totalSpecies)
  
  # add vulnerability
  imth = mthName$Sei[ mthName$Sp == mthName$Sp[mm]]
  dataVul = read.csv ( list.files( path = paste0(wrkDir, "data\\EWG_GOMEX\\"), 
                                   pattern = paste0(imth, ".csv"), recursive=TRUE, full.names = T ) )
  # calculate relative density 
  monthSpecies = NULL
  uSite = unique(SpeciesOut$Site)
  
  for (ss in 1:length(uSite)){ # loop through each site
    
    tmp = SpeciesOut[SpeciesOut$Site== uSite[ss],]
    tmp$Density_LRtotal = NA
    
    #vulnerability zone 
    vzone  = dataPAM$VulerabilityZone[ dataPAM$SiteID == tmp$Site[1]]
    # vultmp = dataVul[dataVul$Zone == vzone,]
    
    #loops through each line and finds value from totalSpecies
    for (rr in 1:nrow(tmp)){
      tmp2 = as.data.frame( totalSpecies[ totalSpecies$ListeingSpaceName == tmp$ListeingSpaceName[rr],] ) 
      tmp2 = as.data.frame( tmp2[ tmp2$SpeciesLabel == tmp$SpeciesLabel[rr],] ) # trim by species
      
      tmp$Density_LRtotal[rr] = tmp2$totalDensity
      tmp$DensityRel[rr] = tmp$Density_LS[rr]/tmp2$totalDensity
      
      # vulnerability score
      #tmp$vulScore[rr] =  vultmp$Total.vulnerability [ vultmp$Species.ID == tmp2$SpeciesLabel]
      #tmp$vulRate[rr] =   vultmp$Rating [ vultmp$Species.ID == tmp2$SpeciesLabel]
      
      monthSpecies = rbind(monthSpecies, tmp[rr,] ) 
      rm( tmp2)
    }
    
    rm(tmp)
    
  } #end site loop
  
  uSite = unique(monthSpecies$Site)
  monthSpecies$MTH = currrent_Mth
  monthSpecies$mth = currrent_MthLab
  
  # select specific listening range results dependent on species hearing
  monthSpecies$Lab2 = paste(monthSpecies$Source, monthSpecies$SNR, sep = "_")
  monthSpecies$FQ = "HF"
  monthSpecies$FQ  = "LF"
  monthSpecies$SNR = as.numeric( as.character( monthSpecies$SNR)) #species hearing type
  monthSpecies$LR_SNR[monthSpecies$SNR > 100]  = "HF_SNR" 
  monthSpecies$LR_SNR[monthSpecies$SNR < 100]  = "LF_SNR"
  # remove rows for different species-- 2 LS for each species!
  monthSpecies$keep = 0
  monthSpecies$keep[monthSpecies$LR_SNR == "HF_SNR" & monthSpecies$FQ == "HF"] = 1
  monthSpecies$keep[monthSpecies$LR_SNR == "LF_SNR" & monthSpecies$FQ == "LF"] = 1
  monthSpeciesT = monthSpecies[monthSpecies$keep == 1, ]
  # Result: so for Rices whale (LF- on keep densities in low SNR values) CHECK:  monthSpeciesT[monthSpeciesT$SpeciesLabel == "Rices",]
  
  ##FIGURE 1 by species across sites for LR ####
  monthSpeciesT$Order = 1 #set order of the species on the graphic
  uSpecies = unique(monthSpeciesT$SpeciesLabel) # length(uSpecies)
  uorder = c("i","f","a","b","c","g","k","h","j","d","e" ) 
  ulabs =  c("OceanicAtlSpotted","OceanicBottlenose","Pantropical","SpinnerDolphin","StripedDolphin","Blackfish","Pilot","Rissos"
             ,"Beaked","Sperm","Rices" )
  for (i in 1:length(uSpecies)){
    idxO = which(monthSpeciesT$SpeciesLabel == uSpecies[i])
    tmpName = paste0(uorder[i],uSpecies[i])
    monthSpeciesT$Order[idxO] = tmpName
  }
  
  p1 =  ggplot(monthSpeciesT, aes(x = Site, y = Order)) +
    geom_tile(aes( fill = DensityRel*100), color = "white",lwd = 1.5 ) +
    geom_text(aes(label=round(DensityRel*100) ),size=3 ,fontface = "italic") +
    facet_wrap(~Source) +
    scale_y_discrete(labels = ulabs) +
    scale_fill_distiller(palette = "Blues",direction = 1, name="Relative Density",) +
    labs(title = "",       y = "", x = "")+
    ggtitle(monthSpeciesT$mth[1]) +
    theme_minimal()+
    theme(legend.position="bottom", text = element_text(size = 24)) 
  p1
  
  # OUTPUT species summary
  write.csv(monthSpeciesT, file = paste0(wrkDir, "data\\RiskAssessment\\", monthSpeciesT$mth[1], "_Species_RelDensity.csv"))
  ggsave(paste0(wrkDir, "data\\RiskAssessment\\", monthSpeciesT$mth[1], "_Species_RelDensity.png"), p1)
  
  monthSpeciesT$Rice[monthSpeciesT$SpeciesLabel  == "Rices"  &  monthSpeciesT$DensityRel > .10 ] = 1
  monthSpeciesT$Sperm[monthSpeciesT$SpeciesLabel == "Sperm" &  monthSpeciesT$DensityRel > .10 ]  = 1
  
  # collapse to all species
  out1 = as.data.frame( monthSpeciesT %>% group_by(Site, Source) %>%
                          summarize(above10 = sum(DensityRel > .10, na.rm = T), 
                                    #avgVul = round( mean(vulScore,na.rm = T) ),
                                    rice =  sum(Rice, na.rm = T), 
                                    sperm = sum(Sperm, na.rm = T) ) )
  
  out1$mth =  monthSpeciesT$mth[1]
  # monthSpeciesTLVSum = monthSpeciesTLV[monthSpeciesTLV$DensityRel > .10,]
  out1$Lab[ out1$sperm > 0 & out1$rice > 0 ] = "Rice + Sperm"
  out1$Tot[ out1$sperm > 0 & out1$rice > 0 ] = 2
  
  out1$Lab[ out1$sperm > 0 & out1$rice == 0 ] = "Sperm"
  out1$Tot[ out1$sperm > 0 & out1$rice == 0 ] = 1
  
  out1$Lab[ out1$sperm == 0 & out1$rice > 0 ] = "Rice"
  out1$Tot[ out1$sperm == 0 & out1$rice > 0 ] = 1
  
  out1$Lab[ out1$sperm == 0 & out1$rice == 0 ] = ""
  out1$Tot[ out1$sperm == 0 & out1$rice == 0 ] = 0
  
  # copy to table in google sheets for each mounth
  rawValues = t( out1[ out1$Source == "LargeVessel", ] )
  cb <- function(df, sep="\t", dec=",", max.size=(200*1000)){
    # Copy a data.frame to clipboard
    write.table(rawValues, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
  }
  cb(rawValues)
  
  rawValues = t(out1[ out1$Source == "MediumVessel", ])
  cb(rawValues)
  
  rawValues = t( out1[ out1$Source == "Seismic", ])
  cb(rawValues)
  
  ## FIGURE 2 species summary ####
  p2 = ggplot(out1, aes(x=Site,y=Source))+
    geom_tile(aes( fill = above10), color = "white",lwd = 1.5 )+
    geom_text(aes(label=Lab), size=4 ,fontface = "italic") +
    scale_fill_distiller(palette = "Blues",direction = 1, name = "Number of species above 10% \n relative density",) +
    labs(  y = "", x = "")+
    theme_minimal()+
    ggtitle(monthSpeciesT$mth[1]) +
    theme(legend.position="bottom", 
          plot.title = element_text(size = 24),  axis.text = element_text(size = 24),
          legend.title = element_text(size=24), 
          legend.text = element_text(size=12)  )
  p2
  write.csv(out1, file = paste0(wrkDir, "data\\RiskAssessment\\", monthSpeciesT$mth[1], "_AllSpecies_RelDensity.csv"))
  ggsave(paste0(wrkDir, "data\\RiskAssessment\\", monthSpeciesT$mth[1], "_AllSpecies_RelDensity.png"), p2)
  
  
  
  # NOISE ACTIVITY ####
  fileIdx = which( sapply(strsplit( gsub(".RData", "", basename( dataNoise ) ) , "_"), tail, 1) == mthName$Sp[mm] )
  load(dataNoise [fileIdx] )
  NoiseOut = as.data.frame(NoiseOut)
  NoiseOut$ListeingSpaceName = paste(NoiseOut$Source, NoiseOut$SNR, sep = "_")
  NoiseOut$LargeVessels = as.numeric(as.character(NoiseOut$LargeVessels ))
  NoiseOut = NoiseOut %>% mutate(across(.cols=7:17, .fns=as.numeric))

  
  totalNoise = as.data.frame( NoiseOut %>% group_by(ListeingSpaceName) %>%
                         summarize(totalLV    = sum(LargeVessels,na.rm = T),
                                   avgLVspeed = mean(LargeSpeed,na.rm = T),
                                   totalMV    = sum(OtherVessels,na.rm = T),
                                   totalCargo = sum(cargo,na.rm = T),
                                   totalTanker = sum(tanker,na.rm = T),
                                   totalFish = sum(fish,na.rm = T),
                                   totalPass = sum(passenger,na.rm = T), 
                                   totalTug = sum(other,na.rm = T),
                                   totalOther = sum(other,na.rm = T) ) )
  
  siteNoise = as.data.frame( NoiseOut %>% group_by(ListeingSpaceName, Site) %>%
                          summarize(totalLV    = sum(LargeVessels,na.rm = T),
                                    avgLVspeed = mean(LargeSpeed,na.rm = T),
                                    totalMV    = sum(OtherVessels,na.rm = T),
                                    totalCargo = sum(cargo,na.rm = T),
                                    totalTanker = sum(tanker,na.rm = T),
                                    totalFish = sum(fish,na.rm = T),
                                    totalPass = sum(passenger,na.rm = T), 
                                    totalTug = sum(other,na.rm = T),
                                    totalOther = sum(other,na.rm = T) ) )
  
  totalNoise$Mth = currrent_Mth
  siteNoise$Mth = currrent_Mth
  
  
  # get proportion of traffic for each vessel type
  for (ii in 1:nrow( siteNoise)){
    tmp = totalNoise[totalNoise$ListeingSpaceName ==  siteNoise$ListeingSpaceName[ii],] #total for all listening spaces
    tmp = tmp[tmp$Mth ==  siteNoise$Mth[ii],]
    
    siteNoise[ii,13:(13+8)] = siteNoise[ii,c(3, 5:11) ]/ tmp[c(2,4:10) ] #proportion of total
    siteNoise[ii,13+9]      = siteNoise[ii,4] - tmp[3] #difference from average speed- negative is less
  }
  

  #as.data.frame(colnames(siteNoise))
  siteNoise$mthNum = currrent_MthLab
  siteNoise$SNR = as.numeric( sapply(strsplit(  siteNoise$ListeingSpaceName  , "_"), tail, 1) )
  siteNoiseT = siteNoise[siteNoise$SNR < 100,]
  
  # get difference from average speed values-- copy column to table
  outSpeeed = siteNoiseT[ siteNoiseT$ListeingSpaceName == "LargeVessel_64", c(2,4,22)]
  cb <- function(df, sep="\t", dec=",", max.size=(200*1000)){
    # Copy a data.frame to clipboard
    write.table(spdValues, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
  }
  spdValues = t( outSpeeed )
  cb(spdValues) # COPY THIS TO RTABLES
  
  # keep only relevant listening areas
  siteNoiseTm = reshape2 :: melt(siteNoiseT, id.vars = c("Site", "ListeingSpaceName"), measure.vars =  colnames(siteNoiseT)[15:20] ) 
  unique(siteNoiseTm$variable)

  siteNoiseTm$keep = 0
  siteNoiseTm$keep[ siteNoiseTm$variable == "totalCargo.1"  & siteNoiseTm$ListeingSpaceName == "LargeVessel_64"] = 1
  siteNoiseTm$variable2[ siteNoiseTm$variable == "totalCargo.1"] = "cargo"
  siteNoiseTm$order[ siteNoiseTm$variable == "totalCargo.1"] = "fcargo"
  
  siteNoiseTm$keep[ siteNoiseTm$variable == "totalTanker.1" & siteNoiseTm$ListeingSpaceName == "LargeVessel_64"] = 1
  siteNoiseTm$variable2[ siteNoiseTm$variable == "totalTanker.1"] = "tanker"
  siteNoiseTm$order[ siteNoiseTm$variable == "totalTanker.1"] = "etanker"
  
  siteNoiseTm$keep[ siteNoiseTm$variable == "totalFish.1"   & siteNoiseTm$ListeingSpaceName == "MediumVessel_74"] = 2
  siteNoiseTm$variable2[ siteNoiseTm$variable == "totalFish.1"] = "fishing"
  siteNoiseTm$order[ siteNoiseTm$variable == "totalFish.1"] = "dfishing"
  
  siteNoiseTm$keep[ siteNoiseTm$variable == "totalPass.1"   & siteNoiseTm$ListeingSpaceName == "MediumVessel_74"] = 2
  siteNoiseTm$variable2[ siteNoiseTm$variable == "totalPass.1"] = "passenger"
  siteNoiseTm$order[ siteNoiseTm$variable == "totalPass.1"] = "cpassenger"
  
  siteNoiseTm$keep[ siteNoiseTm$variable == "totalTug.1"    & siteNoiseTm$ListeingSpaceName == "MediumVessel_74"] = 2
  siteNoiseTm$variable2[ siteNoiseTm$variable == "totalTug.1"] = "tug"
  siteNoiseTm$order[ siteNoiseTm$variable == "totalTug.1"] = "btug"
  
  siteNoiseTm$keep[siteNoiseTm$variable == "totalOther.1" & siteNoiseTm$ListeingSpaceName == "MediumVessel_74"] = 2
  siteNoiseTm$variable2[ siteNoiseTm$variable == "totalOther.1"] = "other"
  siteNoiseTm$order[ siteNoiseTm$variable == "totalOther.1"] = "aother"
  
  #unique( siteNoiseTm$variable2 )
  
  siteNoiseTm2 = filter(siteNoiseTm, siteNoiseTm$keep > 0)

  ## FIGURE 3 ####
  p3 = ggplot(siteNoiseTm2, aes( x=Site, y=order ) ) +
    geom_tile(aes( fill = round(value*100)), color = "white",lwd = 1.5 )+
    scale_fill_distiller(palette = "Reds", direction = 1, name="% of total unique vessels",) +
    geom_text(aes(label= round(value*100) ), size=4 ,fontface = "italic") +
    labs(title = "", y = "", x = "")+
    scale_y_discrete(labels = c("other","tug","passenger","fishing", "tanker","cargo")) +
    ggtitle(monthSpeciesT$mth[1]) +
    theme_minimal()+
    theme(legend.position="bottom", 
          plot.title = element_text(size = 24),  axis.text = element_text(size = 24),
          legend.title = element_text(size=24), 
          legend.text = element_text(size=12)  )
  p3
  write.csv(siteNoiseTm2, file = paste0(wrkDir, "data\\RiskAssessment\\", monthSpeciesT$mth[1], "_AllNoiseRel.csv"))
  ggsave(paste0(wrkDir, "data\\RiskAssessment\\", monthSpeciesT$mth[1], "_AllNoiseRel.png"), p3)
  
  #calculate number of types above 10% relative number of vessels
  # collapse to all species
  out2 = as.data.frame( siteNoiseTm2 %>% group_by(Site, keep) %>%
                          summarize(above10 = sum(value > .10, na.rm = T) ) )
  
   #copy to clipboard
  cb <- function(df, sep="\t", dec=",", max.size=(200*1000)){
    # Copy a data.frame to clipboard
    write.table(rawValues, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
  }
  rawValues = t( out2[ out2$keep == "1", ] )
  cb(rawValues) # COPY THIS TO RTABLES
  
  rawValues =t( out2[ out2$keep == "2", ] )
  cb(rawValues) # COPY THIS TO RTABLES
  
  #PAM DATA ####
  PAMFilesMedian  = list.files(path = PAM.Dir, pattern= "MFmedian" , recursive=TRUE, full.names = TRUE)
  medSPL = as.data.frame( read.csv(PAMFilesMedian ) )
  Fq = as.character( seq(from = 20, to = 4000, by = 10) )
  FqH = paste0("F", Fq, "Hz")
  colnames( medSPL)  = c("site", "date", FqH)
  colnames( medSPL)
  
  medSPL$date = as.Date( medSPL$date , format = " %d-%b-%Y")
  medSPL$Mth = month(medSPL$date ) 
  medSPL2 = medSPL[medSPL$site != "CE" , ]
  medSPL2 = medSPL2[medSPL2$site != "MR" , ]
  medSPL2 = medSPL2[medSPL2$site != "Y1C" , ]
  
  medSPLT = as.data.frame( medSPL2[ medSPL2$Mth == mthName$PAM[mm], ] )
  colnames( medSPLT)[402]= "UNK"

  PAM1 = t( as.data.frame( medSPLT %>% group_by(site) %>%
                          summarize(med125 =  median( F120Hz,  na.rm = T), 
                                    med1000 = median( F1000Hz, na.rm = T ) ) ) )
  cb <- function(df, sep="\t", dec=",", max.size=(200*1000)){
    # Copy a data.frame to clipboard
    write.table(PAM1, paste0("clipboard-", formatC(max.size, format="f", digits=2)), sep=sep, row.names=FALSE, dec=dec)
  }
  cb((PAM1))
  # COPY too 120 Hz ROW in : https://docs.google.com/spreadsheets/d/1j-pGWWX0Nj-MpZ9UUCqg9VQHhoAwqtw7nYA0UDJzgdc/edit#gid=1545634159
  
 } #end month loop













