\name{SA}

\alias{SAtrain}
\alias{SAbackground}
\alias{SAtest}

\docType{data}

\title{South American plant species distribution data}

\description{
Species occurence data for 30 plant species (all from the Bignoniaceae family) from South America (SA) and associated environmental data. There are three data sets with training (SAtrain), background (SAbackground), and test (SAtest) data. The training data principally consist of environmental variables at locations where the species where observed (presence only), and at random locations for background data. The test data are presence/absence observations on the same species, independent from the training data.


Species and site variables:
\tabular{ll}{  
Code \tab Description \cr
recordid \tab  \cr
spid \tab  \cr
genus \tab genus  \cr
species \tab species \cr
Longitude \tab latitude  \cr
Latitude \tab longitude  \cr
pa \tab  presence (1) or absence (0) \cr
}

Environmental variables:
\tabular{llll}{  
Code \tab Description \tab Units \tab Type\cr
rainann \tab   \tab   \tab  Cont\cr
}

Latitude and latitude are in geographical coordinates using the WGS84 datum.

}

\usage{
data(SAtrain)
data(SAbackground)
data(SAtest)
}

\author{Bette Loiselle and Lucia Lohman}

\source{
Test data comes from the Missouri Botanical Garden database management system TROPICOS (http://www.mobot.org) and from Lucia Lohmann. Species localities were georeferenced from locality description. Coordiantes are have up to 4 decimal places, but that does not reflect that degree of accuracy.  Test data is from plot data collected by Al Gentry over a period of 22 years (1971-1993)
}

\keyword{datasets}

