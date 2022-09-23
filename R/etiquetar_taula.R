#  Etiquetar Taula   ------------------

# Llanço taula i camp que vull etiquetar i cambia nom del camp en funció d'etiqueta

etiquetar_taula<-function(taula=resumtotal,camp="variable",taulavariables="variables_R.xls",camp_descripcio="descripcio",idcamp="camp") {
  
  # taula=dt_temp
  # taulavariables=conductor
  # camp="Parameter"
  # camp_descripcio="descripcio"
  # idcamp="camp"
  
  ####  Llegir etiquetes i variables a analitzar ####
  variables <- read_conductor(taulavariables)
  camp_sym<-sym(camp)
  idcamp_sym<-sym(camp_sym)
  
  # Canviar nom de camp de variables al de la taula 
  # colnames(variables)[colnames(variables)=="camp"] <- camp
  colnames(variables)[colnames(variables)==idcamp] <- camp
  
  # Canviar arguments per ser evaluats
  camp_eval<-sym(camp)
  camp_descripcio_eval<-sym(camp_descripcio)
  # Canviar el format de la taula 
  taula %>% left_join(dplyr::select(variables,c(!!camp_eval,camp_descripcio)),by=quo_name(camp_eval)) %>% 
    dplyr::mutate(!!camp_descripcio_eval:=ifelse(is.na(!!camp_descripcio_eval),!!camp_eval,!!camp_descripcio_eval)) %>% 
    dplyr::rename(descripcio:=!!camp_descripcio) %>% 
    mutate(!!camp_eval:=descripcio) %>% 
    dplyr::select(-descripcio)
  
  
}