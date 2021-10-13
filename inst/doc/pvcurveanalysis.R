## ---- eval = FALSE------------------------------------------------------------
#  pressure_volume_data
#  weather_data

## ---- eval = FALSE------------------------------------------------------------
#  pv <- RelativeWaterDeficit(pressure_volume_data)

## ---- eval = FALSE------------------------------------------------------------
#  df <- pressure_volume_data
#  FMSaturated(df)

## ---- eval = FALSE------------------------------------------------------------
#  pv <- RelativeWaterDeficit(pressure_volume_data)
#  TurgorLossPoint(pv)

## ---- echo = FALSE------------------------------------------------------------
suppressMessages(suppressWarnings(library(pvcurveanalysis)))
pv <- RelativeWaterDeficit(pressure_volume_data)
pv_sub <- subset(pv, sample == 3) 
tlp <- TurgorLossPoint(pv_sub)

## ---- eval = FALSE------------------------------------------------------------
#  pv <- RelativeWaterDeficit(pressure_volume_data)
#  OsmoticPot(pv)

## ---- echo = FALSE------------------------------------------------------------
library(pvcurveanalysis)
pv <- RelativeWaterDeficit(pressure_volume_data)
pv_sub <- subset(pv, sample == 3) 
osm <- OsmoticPot(pv_sub)

## ---- eval = FALSE------------------------------------------------------------
#  ModElasticity(pv)

## ---- echo = FALSE------------------------------------------------------------
pv <- RelativeWaterDeficit(pressure_volume_data)
pv_sub <- subset(pv, sample == 3) 
mod <- ModElasticity(pv_sub)

