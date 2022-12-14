% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/netejarFormatGe.R
\name{canvi_noms_variables}
\alias{canvi_noms_variables}
\title{Canviar noms de les variables}
\usage{
canvi_noms_variables(
  dt = "dt_temp",
  variable = "ajust2",
  nou_nom = "descripcio",
  conductor = "conductor_variables",
  ...
)
}
\arguments{
\item{dt}{Base de dades}

\item{variable}{Variables escollides a partir del conductor}

\item{nou_nom}{Nom NOUS de les variables}

\item{conductor}{Conductor}

\item{...}{Altres funcions}
}
\value{
canvi de noms de les variables escollides
}
\description{
Canviar noms de variables  de la nostra b.d a prtir d un conductor
}
\examples{
k1<-canvi_noms_variables(dt=dt_plana_test,variable="taula2",
nou_nom="var_nou",conductor=conductor1_test)
dt_plana_test
k1

}
