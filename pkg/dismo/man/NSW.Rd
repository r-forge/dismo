\name{NSW}

\alias{NSWtrain}
\alias{NSWbackground}
\alias{NSWtest}
\alias{NSWeval}

\docType{data}

\title{New South Wales species distribution data}

\description{
NSW (New South Wales, a state in Australia) species distribution data. There are four data sets with training (NSWtrain), background or pseudo-absence (NSWbackground), test (NSWtest), and evaluation (NSWeval) data for 54 species from 8 biological groups. 


Species and site variables:
\tabular{ll}{  
Code \tab Description \cr
recordid \tab  \cr
spid \tab first two letters indicate group: ba = bats (7 species); db = diurnal birds (8 species); nb = nocturnal birds (2 species); ot = open-forest trees (8 species); ou = open-forest understorey plants (8 species); rt = rainforest trees (7 species); ru = rainforest understorey plants (6 species); sr = small reptiles (8 species) \cr
genus \tab genus  \cr
species \tab species \cr
Longitude \tab latitude  \cr
Latitude \tab longitude  \cr
pa \tab  presence (1) or absence (0) \cr
}

Environmental variables:
\tabular{llll}{  
Code \tab Description \tab Units \tab Type\cr
rainann \tab mean annual rainfall \tab mm \tab Cont\cr
raindq \tab mean rainfall of the driest quarter \tab mm \tab Cont\cr
tempann \tab annual mean temperature \tab deg C \tab Cont\cr
tempmin \tab minimum temperature of the coldest month \tab deg C \tab Cont\cr
solrad \tab annual mean solar radiation (terrain adjusted) \tab MJm-2day-1 \tab Cont\cr
mi \tab moisture index. Index of site wetness derived from a water balance algorithm using rainfall, evaporation, radiation and soil depth as inputs. \tab Ranges between 0 (dry) and 1 (wet) \tab Cont \cr
soilfert \tab soil fertility class, derived from soil landscape mapping and modeling of geochemical data \tab  1 (low) to 5 (high) \tab Cont \cr
soildepth \tab mean soil depth predicted from a model relating sampled soil depths to climate, geology and topography \tab m \tab Cont \cr
rugged \tab ruggedness. Coefficient of variation of grid cells within 1km of cell of interest \tab percent \tab Cont \cr
topo \tab topographic position. Mean difference in elevation between grid cell of interest and all cells within 1km radius (-ve values are gullies, +ve are ridges) \tab m \tab Cont\cr
cti \tab "compound topographic index" a quantification of the position of a site in the local landscape. It is often referred to as the steady state wetness index and it is defined as: CTI = ln ( As / tanB ) where 'As' is the specific catchment area expressed as m2 per unit width orthogonal to the flow direction and 'B' is the slope angle \tab \tab Cont\cr
disturb \tab disturbance (clearing, logging etc) index. \tab 1 = light, 2 = moderate, 3 = heavy \tab Cont\cr
vegsys \tab broad vegetation type \tab 1 = Rainforest, 2 = Moist open forest, 3 = Dry open forest, 4 = Woodland, 5 = Coastal sclerophyll complex, 6 = Plateau sclerophyll complex, 7 = Disturbed remnant, 8 = Exotic (pine) plantation, 9 = Cleared \tab Categorical \cr
}
All of the predictors except 'vegsys' can be treated as continuous, or at least ranked, variables. However, the types in 'vegsys' are not ranked in any way, and this variable must therefore be treated as categorical (unordered) when fitting models.   

Latitude and latitude are in geographical coordinates using the WGS84 datum.

}

\usage{
data(NSWtrain)
data(NSWbackground)
data(NSWtest)
data(NSWeval)
}

\author{Simon Ferrier}

\source{
Data was compiled and provided by Simon Ferrier and colleagues and distributed with his permission. 

Training data: Fauna data came from the Atlas of NSW Wildlife (a database of incidental sighting records), and the flora data consisted of specimen records from both the University of New England Herbarium and the Sydney Herbarium (Royal Botanic Gardens). All available records used. No information on collection dates or accuracy. Reduced to one record per species per 100m grid cell. These data were used by Ferrier, S. and G. Watson,  1997. An evaluation of the effectiveness of environmental surrogates and modeling techniques in predicting the distribution of biological diversity. Environment Australia, Canberra (\url{http://www.ea.gov.au/biodiversity/publications/technical/surrogates/}). That report provides further detail on the origin and nature of the data.

Testing data is from designed surveys. These are described in Ferrier and Watson (1996; see above), and Pearce J., Ferrier S. & Scotts D. (2001) An evaluation of the predictive performance of distributional models for flora and fauna in north-east New South Wales., J. Environ. Manage., 62, 171-184.

These are the NSW data that were used in this model comparison study: 

Elith, J., C.H. Graham, R.P. Anderson, M. Dudik, S. Ferrier, A. Guisan, R.J. Hijmans, F. Huettmann, J. Leathwick, A. Lehmann, J. Li, L.G. Lohmann, B. Loiselle, G. Manion, C. Moritz, M. Nakamura, Y. Nakazawa, J. McC. Overton, A.T. Peterson, S. Phillips, K. Richardson, R. Scachetti-Pereira, R. Schapire, J. Soberon, S. Williams, M. Wisz and N. Zimmerman, 2006. Novel methods improve prediction of species' distributions from occurrence data. Ecography 29: 129-151. \url{http://dx.doi.org/10.1111/j.2006.0906-7590.04596.x}
}

\keyword{datasets}

