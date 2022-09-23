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



#  Etiquetar valors        ------------------------------------------

##    Retorna Data frame etiquetat en funció d'un conductor ##
## dataframe dades, conductor_variables     
etiquetar_valors<-function(dt=dades,variables_factors=conductor_variables,fulla="etiquetes",
                           camp_etiqueta="etiqueta",missings=F, new_vars=F,sufix=".2") {
  
  # dt=dades
  # variables_factors=conductor_variables
  # fulla="etiquetes"
  # camp_etiqueta="etiqueta2"
  # missings=F
  # new_vars=T
  # sufix=".cat"
  
  
  # Llegir conductor#
  variables_factors<-read_conductor(variables_factors,sheet=fulla) %>% 
    filter(!is.na(!!sym(camp_etiqueta))) # Selecciono només variables segons existeix camp etiqueta
  
  # Verifico si alguna variable ja existeix en la base de dades i si existeix comunico que es sobrescriura
  vars_generades<-variables_factors %>% distinct(camp) %>% mutate(camp=paste0(camp,sufix)) %>% pull(camp)
  
  # Llisto la llista de variables que es generara si no sobrescriu
  if (new_vars) message(paste0("Variables generated: ",  paste0(vars_generades,collapse = ", ")))
  
  # printar advertencia si algunes variables ja esta generarada i es sobreescriurà
  vars_sobresescrites<-names(dt) [names(dt) %in% vars_generades]
  if (vars_sobresescrites %>% length()>0) {message(paste0("Variables overwritten: ",paste0(vars_sobresescrites,collapse = ", ")))}
  
  # Les elimino de la base de dades abans de sobreescriurles si nova variable es creada
  if (new_vars) dt<-dt %>% select(-vars_sobresescrites)  #Tinc dubtes si cal eliminar-la
  
  # Split
  camp_etiqueta<-sym(camp_etiqueta)
  
  k<-variables_factors%>%dplyr::select(camp, valor,!!camp_etiqueta)
  pepe<-k %>% base::split(list(.$camp))
  
  #
  noms_variables<-names(pepe)
  num_vars<-length(noms_variables)
  
  # Elimina espais en blanc de les variables factor / character (treu nivells) nomes variables nomenades
  # dt<-dt %>% mutate_at(noms_variables,~ifelse(is.factor(.),trimws(.),.))
  
  dt_original<-dt # Faig copia original
  
  for (i in 1:num_vars) {
    # i<-1
    if (noms_variables[i] %in% colnames(dt)) {
      
      etiquetes_valors<-pepe[[i]] %>% pull(!!camp_etiqueta)
      
      # Transformar a caracter i treure espais en blanc
      if (is.factor(dt[[noms_variables[i]]])) {
        dt<-dt %>% mutate_at(noms_variables[i],~stringr::str_trim(as.factor(.)))}
      
      dt[noms_variables[i]]<-lapply(dt[noms_variables[i]],
                                    function(y) factor(y,levels=pepe[[i]]$valor,labels=etiquetes_valors))
      
      if (missings) dt<-missings_to_level(dt,noms_variables[i])
      
    }
  }
  
  # Si new_vars, selecciono renombro i fusiono a dt original 
  
  if (new_vars) {
    dt_recode<-dt %>% as_tibble() %>% select(noms_variables) %>% rename_at(noms_variables,function(x) paste0(x,sufix)) 
    dt<-cbind(dt_original,dt_recode) %>% as_tibble()}
  
  dt
  
}
#



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




# Retorna llista nomenada amb els mateixos noms dels objectes que inclou
# PEr exemple fer una llista de data frames que tinguin el mateix noms que el contenen
llistaNomenada <- function(...) {
  v1 <- as.list(substitute(list(...)))[-1L]  
  inputs <- list(...)
  i1 <- names(inputs)
  i2 <- i1 == ""
  if(is.null(i1)) {
    names(inputs) <- v1
  } else names(inputs)[i2] <- v1[i2]
  inputs }