% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recodiFormatGe.R
\name{comptar_valors}
\alias{comptar_valors}
\title{Comptar valors}
\usage{
comptar_valors(
  dt = "dadesevents",
  variables = c("EV.TER.ARTER_PERIF", "EV.TER.AVC"),
  valor = "Yes"
)
}
\arguments{
\item{dt}{Base de dades}

\item{variables}{Variables}

\item{valor}{Valor}
}
\value{
Retorna valors
}
\description{
Comptar un valor concret d un conjunt de variables de la nostra B.D
}
\examples{
Comptar_D<-comptar_valors(dt_plana_test,variables=c("DG.HTA","DG.IC","GLICADA.valor"),valor="NA")
Comptar_D
}
