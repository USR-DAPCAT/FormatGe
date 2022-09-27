#' @title factoritzar FormatGe.
#' @description factoritzar una llista de variables donades unes dades i un vector de variables 
#' @param dt xxxxxxxxxx 
#' @param variables xxxxxxxxxx 
#' @return llista de variables
#' @export factoritzar 
#' @importFrom dplyr "%>%"
#
factoritzar<-function(dt="dades",variables=c("grup","situacio")) {
  
  # dt=dades
  # variables=c("grup","situacio","kk","sexe")
  
  # Només si variable existeix la variable en dt
  variables<-variables[variables %in% names(dt)]
  
  factoritzacio<-function(x) {if (!is.factor(x)) x<-as.factor(x) else x<-x}
  
  dt<-dt %>% dplyr::mutate_at(variables,factoritzacio)
}


#' @title factoritzar FormatGe.
#' @description factoritzar NO.Yes llista de variables "factor" situades a la taulavariables camp=factor 
#' @param dt xxxxxxxxxx 
#' @param columna xxxxxxxxxx 
#' @param taulavariables xxxxxxxxxx
#' @param ... altres funcions
#' @return NO.YES  
#' @export factoritzar.NO.YES 
#' @importFrom dplyr "%>%"
#
factoritzar.NO.YES<-function(dt="dadesDF",columna="factor",taulavariables="variables_FELIPE.xls",...){
  
  # dt=dades
  # columna="factor.YESNO"
  # taulavariables=conductor_variables
  
  # Extreure variables  
  x<-extreure.variables(taula=columna,taulavariables=taulavariables,...) 
  
  # Seleccionar només variables que estan en dt
  if (!x[!x%in%names(dt)] %>% length()<1) {print("No existeixen en dt:");print(x[!x%in%names(dt)])}
  
  # Selecciono nomes les vars en bdades
  x<-x[x%in%names(dt)]
  
  ###   Factoritzar-les
  dt[x]<-lapply(dt[x],function(y) factor(y,levels=c(0,1), labels=c("No","Yes")))
  dt
  
}


#' @title factoritzar FormatGe.
#' @description factoritzar NO.SI llista de variables "factor" situades a la taulavariables camp=factor 
#' @param dt xxxxxxxxxx 
#' @param columna xxxxxxxxxx 
#' @param taulavariables xxxxxxxxxx
#' @return NO.YES  
#' @export factoritzar.NO.SI
#' @importFrom dplyr "%>%"
#
factoritzar.NO.SI<-function(dt="dadesDF",columna="factor",taulavariables="variables_FELIPE.xls"){
  
  #dt=dt_plana
  #columna="factor"
  #taulavariables=conductor
  
  # Extreure variables  
  x<-extreure.variables(taula=columna,taulavariables=taulavariables) 
  
  # Seleccionar nomÃÂ©s variables que estan en dt
  if (!x[!x%in%names(dt)] %>% length()<1) {print("No existeixen en dt:");print(x[!x%in%names(dt)])}
  
  # Selecciono nomes les vars en bdades
  x<-x[x%in%names(dt)]
  
  ###   Factoritzar-les
  dt[x]<-lapply(dt[x],function(y) factor(y,levels=c(0,1), labels=c("No","Si")))
  dt
  
}
