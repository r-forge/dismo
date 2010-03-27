\name{CAN}

\alias{CANtrain}
\alias{CANbackground}
\alias{CANtest}

\docType{data}

\title{South American plant species distribution data}

\description{
Species occurence data for 20 bird species from Ontario, a province in Canada (CAN) and associated environmental data. There are three data sets with training (CANtrain), background (CANbackground), and test (CANtest) data. The training data principally consist of environmental variables at locations where the species where observed (presence only), and at random locations for background data. The test data are presence/absence observations on the CANme species, independent from the training data.


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
data(CANtrain)
data(CANbackground)
data(CANtest)
}

\author{M. Peck, M. Cadman, Falk Huettmann}

\source{
Training data are 'Ontario Nest Records' from the Royal Ontario Museum (ROM) obtain from M. Peck for NCEAS project. Temporal Span 1870-2002 (most from 1960-2001). Methods of Georeferencing: Coordinates derived from map by ROM; some locations ground-truthed with GPS. Taxonomic Authorities: individual nest recorders.

Test data: the Breeding Bird Atlas (BBA) Data for Ontario, provided by M. Cadman.

}

\keyword{datasets}

