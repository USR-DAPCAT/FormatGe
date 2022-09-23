# Cambia nom dels elements d'un vector 

etiquetar_vector<-function(vector=vector_variables,camp="camp",taulavariables="variables_R.xls",camp_descripcio="descripcio",...) {
  
  # vector=v1
  # taulavariables=conductor_variables
  # camp="camp"
  # camp_descripcio="descripcio2"
  
  ####  Llegir etiquetes i variables a analitzar ####
  variables <- read_conductor(taulavariables,...) 
  camp_sym<-sym(camp)
  variables<-variables %>% dplyr::filter(!is.na(!!camp_sym))
  
  # Canviar nom de camp de variables al de la taula 
  colnames(variables)[colnames(variables)=="camp"] <- camp
  # Canviar arguments per ser evaluats
  camp_eval<-sym(camp)
  camp_descripcio_eval<-sym(camp_descripcio)
  
  vectorX<-vector %>% tibble(value=.)  # Convertir vector a tibble i filtrar
  variablesX<-variables %>% semi_join(vectorX, by=c("camp"="value")) # Filtrar només variables vector
  
  stats::setNames(object=pull(variablesX,!!camp_descripcio_eval),variablesX$camp)
  
}