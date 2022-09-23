#
#  Etiquetar les variables de les dades      #####
###

etiquetar<-function(d=dadestotal,taulavariables="variables_R.xls",camp_descripcio="descripcio",...) {
  
  # d=dades
  # taulavariables = conductor
  # camp_descripcio="descripcio"
  
  
  # Symbol
  camp_descripcio<-sym(camp_descripcio)
  
  #  Llegir etiquetes i variables a analitzar ####
  variables<-read_conductor(taulavariables,...)
  # variables<-read_conductor(taulavariables)
  
  variables<-variables %>% 
    dplyr::filter(!is.na(camp) & !is.na(!!camp_descripcio)) %>% # elimino els que no tenen etiqueta
    dplyr::select(camp,descripcio=!!camp_descripcio) # selecciono els camps necessaris (camp i descripcio) i amb etiqueta
  
  # Els que no tenen etiqueta assignar el mateix nom del camp (Eliminats previament)
  variables<-variables %>% mutate(descripcio=as.character(descripcio))
  variables<-variables %>% mutate(descripcio=ifelse(descripcio=="0" | is.na(descripcio),camp,descripcio)) 
  
  # Etiquetar variables         
  seleccio<-variables
  camp<- as.vector(seleccio$camp) #
  descripcio<- as.vector(seleccio$descripcio) #
  ### etiquetar variables seleccionades     #
  for (i in 1:length(descripcio)){if (any(colnames(d) == camp[i])) {Hmisc::label(d[[camp[i]]]) <- descripcio[i]}}
  d
}
