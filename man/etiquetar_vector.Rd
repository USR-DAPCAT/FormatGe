% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/etiquetFormatGe.R
\name{etiquetar_vector}
\alias{etiquetar_vector}
\title{Etiquetar un vector}
\usage{
etiquetar_vector(
  vector = "vector_variables",
  camp = "camp",
  taulavariables = "variables_R.xls",
  camp_descripcio = "descripcio",
  ...
)
}
\arguments{
\item{vector}{Vector}

\item{camp}{Camp}

\item{taulavariables}{Conductor}

\item{camp_descripcio}{Descripcio del camp}

\item{...}{altres funcions}
}
\value{
etiquetar_vector
}
\description{
Etiquetar un vector a partir d'un conductor
}
\examples{


v1<-c("idp","dtindex","sexe","dnaix","situacio","entrada")

v1_etiq<-etiquetar_vector(vector=v1,
                         taulavariables=conductor1_test,
                         camp="camp",
                         camp_descripcio="descripcio")

v1_etiq

}
