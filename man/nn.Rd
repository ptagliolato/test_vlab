% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vlab.R
\name{nn}
\alias{nn}
\title{Title}
\usage{
nn(alienNativeRichnessData)
}
\arguments{
\item{alienNativeRichnessData}{a dataset accounting for alien and native species richness 
with the following fields: 
   "locality": (string) a toponym where the observation occurred 
   "EunisL1": (string) eunis habitat level-1 code
   "family": (string) family name according to LifeWatch Global Name Architecture - cf.(for auth users) http://www.servicecentrelifewatch.eu/global-names-architecture.
   "native_richness": (int) richness of native species in the locality for the eunisspeciesgroup
   "alien_richness": (int) richness of alien species in the locality for the eunisspeciesgroup
   [DEPRECATED]"eunispeciesgroups": (string) species group according to eunis species list (cf. http://eunis.eea.europa.eu/species.jsp)}
}
\description{
Title
}
\note{
the parameter, for ocpu execution, should be the previous session id, in order for ocpu to retrieve the data computed by the preceding method
}

