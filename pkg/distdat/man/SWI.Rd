\name{SWI}

\alias{SWItrain}
\alias{SWIbackground}
\alias{SWItest}


\docType{data}

\title{Switzerland species distribution data}

\description{
Species occurence data for 30 tree species in Switzerland (SWI, a country in Europe) and associated environmental data. There are three data sets with training (SWItrain), background (SWIbackground), and test (SWItest). The training data principally consist of environmental variables at locations where the species where observed (presence only), and at random locations for background data. The test data are presence/absence observations of the same species, independent from the training data.


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
data(SWItrain)
data(SWIbackground)
data(SWItest)
}

\author{Antoine Guisan and Nicklaus Zimmerman}

\source{
Training data set is a forest inventory data set. A regular point lattice across Switzerland located on a 1km national grid. The lattice points that do contain forest according to the national definition within the Swiss forest inventory are sampled in the field. The data originate from the 1985 national inventory, actually collected from 1982-1986.

Test data set is from forest plots. A forest vegetation data base containing irregularly and nonsystematically sampled forest vegetation relevees throughout Switzerland. Dates: records start in 1913. The majority of the plots was collected after 1920, and about 3/4th of the data were sampled after 1940. The data base ends in 1998, but most records are from 1994 and earlier. The individual authors had their own local sampling design or used preferential sampling techniques. From the original data base, selected roughly 14,100 relevees that included minimal data standards such as coordinates. 

}

\keyword{datasets}

