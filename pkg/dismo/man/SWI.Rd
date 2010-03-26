\name{SWI}

\alias{SWItrain}
\alias{SWIbackground}
\alias{SWItest}


\docType{data}

\title{Switzerland species distribution data}

\description{
Species occurence data for 30 tree species in Switzerland (SWI, a country in Europe) and associated environmental data. There are three data sets with training (SWItrain), background (SWIbackground), and test (SWItest). The training data principally consist of environmental variables at locations where the species where observed (presence only), and at random locations for background data. The test data are presence/absence observations on hte same species, independent from the training data.


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

These are the SWI data that were used in this model comparison study: 

Elith, J., C.H. Graham, R.P. Anderson, M. Dudik, S. Ferrier, A. Guisan, R.J. Hijmans, F. Huettmann, J. Leathwick, A. Lehmann, J. Li, L.G. Lohmann, B. Loiselle, G. Manion, C. Moritz, M. Nakamura, Y. Nakazawa, J. McC. Overton, A.T. Peterson, S. Phillips, K. Richardson, R. Scachetti-Pereira, R. Schapire, J. Soberon, S. Williams, M. Wisz and N. Zimmerman, 2006. Novel methods improve prediction of species' distributions from occurrence data. Ecography 29: 129-151. \url{http://dx.doi.org/10.1111/j.2006.0906-7590.04596.x}
}

\keyword{datasets}

