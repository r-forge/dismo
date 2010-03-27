\name{AWT}

\alias{AWTtrain}
\alias{AWTbackground}
\alias{AWTtest}


\docType{data}

\title{Australian Wet Tropics species distribution data}

\description{
Species occurence data for 40 species (20 plants, 20 vertebrates) in the Australian Wet Tropics (AWT) and associated environmental data. There are three data sets with training (AWTtrain), background (AWTbackground), and test (AWTtest) data. The training data principally consist of environmental variables at locations where the species where observed (presence only), and at random locations for background data. The test data are presence/absence observations on the same species, independent from the training data.

Plant species records from Andrew Ford, CSIRO Atherton, & Karen Richardson, UQ Brisbane. Training data is from herbariums. These data have been cleaned and corrected by Andrew Ford for modeling by the Rainforest CRC, Townsville.  Most records are thought to be accurate to within 3km, and were collected within the last 40 years. Presence-absence from 102 of Andrew Ford's survey sites. These have very accurate coordinate data and were collected in the last 20 years. Vertebrate data was collected by Steve Williams. Training data is from incidental surveys using GPS. Presence-absence data from field surveys. 


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
data(AWTtrain)
data(AWTbackground)
data(AWTtest)
}

\author{Andrew Ford, Steven Williams, Karen Richardson}

\source{

These are the AWT data that were used in this model comparison study: 

Elith, J., C.H. Graham, R.P. Anderson, M. Dudik, S. Ferrier, A. Guisan, R.J. Hijmans, F. Huettmann, J. Leathwick, A. Lehmann, J. Li, L.G. Lohmann, B. Loiselle, G. Manion, C. Moritz, M. Nakamura, Y. Nakazawa, J. McC. Overton, A.T. Peterson, S. Phillips, K. Richardson, R. Scachetti-Pereira, R. Schapire, J. Soberon, S. Williams, M. Wisz and N. Zimmerman, 2006. Novel methods improve prediction of species' distributions from occurrence data. Ecography 29: 129-151. \url{http://dx.doi.org/10.1111/j.2006.0906-7590.04596.x}
}

\keyword{datasets}

