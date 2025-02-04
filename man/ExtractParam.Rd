% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ExtractParam.R
\name{ExtractParam}
\alias{ExtractParam}
\title{Extracts parameters from result list}
\usage{
ExtractParam(result_list)
}
\arguments{
\item{result_list}{output list from the functions TurgorLossPoint, OsmoticPot or ModElasticity}
}
\value{
data frame containing the results from the curve analysis only, depending on the function used, relative water deficit at
turgor loss point (rwd.tlp), water potential at turgor loss point (water.pot.tlp), apoplastic fraction (apo.fract),
osmotic potential at full saturation (osmotic.pot.full.sat), modulus of elasticity (modulus.elasticity)
}
\description{
Extracts the curve parameters from the result lists of the functions analysing the pressure volume curve
(TurgorLossPoint, OsmoticPot and ModElasticity
}
\examples{
# use pressure volume data provided by package
pv_data <- pressure_volume_data

# do pressure volume curve analysis
pv_data <- RelativeWaterDeficit(pv_data)
results <- OsmoticPot(pv_data, graph = FALSE)

# extract curve values
ExtractParam(results)


}
