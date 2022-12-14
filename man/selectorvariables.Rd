% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formulaFormatGe.R
\name{selectorvariables}
\alias{selectorvariables}
\title{Seleccionem les variables}
\usage{
selectorvariables(
  taula = "table1",
  taulavariables = "variables_R.xls",
  dt = "dadestotal"
)
}
\arguments{
\item{taula}{Camp a on hi han les variables}

\item{taulavariables}{Conductor}

\item{dt}{Base de dades}
}
\value{
Base de dades amb les variables seleccionades
}
\description{
Seleccionem les variables a partir d'un conductor
}
\examples{
k<-selectorvariables(taula="taula1",taulavariables=conductor1_test,dt=dt_plana_test)
dt_plana_test               
k
}
