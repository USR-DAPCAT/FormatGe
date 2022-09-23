#  Selector de Variables      -------
#
selectorvariables=function(taula="table1",taulavariables="variables_R.xls",dt=dadestotal) {
  
  # taula = "dades_imputacio2" 
  # taulavariables="variables_v2.xls"
  # dt=dades_test 
  
  vector_variables<-extreure.variables(taula=taula,taulavariables = taulavariables)
  
  # Selecciono les que no existeixen en DT 
  variables.no.existeixen<-vector_variables[!is.element(vector_variables,names(dt))]
  
  # Elimino les que no existeixen
  vector_variables<-vector_variables[is.element(vector_variables,names(dt))]
  moco<-dt %>% dplyr::select_at(vector_variables)
  
  message(paste0("Llista de variables que no existeixen en el dataset:",paste0(variables.no.existeixen ,collapse = ", ")))
  
  moco
  
  
}

#  Extreure.Variables: Selector de variables TAULA DE--------
#
extreure.variables=function(taula="table1",taulavariables="variables_R.xls",variable_camp="camp",dt=NA,...) {
  
  # taula="dates_excel"
  # taulavariables = conductor_variables
  # variable_camp="camp"
  # dt=dades
  
  ####  Llegir etiquetes i variables a analitzar ####
  # variables <- readxl::read_excel(taulavariables) %>% tidyr::as_tibble() %>% dplyr::select(!!variable_camp,!!taula)
  variables <- read_conductor(taulavariables,...) %>% dplyr::select(!!variable_camp,!!taula)
  taula_sym<-rlang::sym(taula)
  variables<-variables %>% dplyr::filter(!is.na(!!taula_sym))
  
  # Verificar si columnes del conductor estan en dt
  if (is.data.frame(dt)) {
    vars_not_dt<-variables %>% anti_join(names(dt) %>% as_tibble(camp=value),by=c("camp"="value")) %>% pull("camp")
    variables<-variables %>% semi_join(names(dt) %>% as_tibble(camp=value),by=c("camp"="value"))
    paste0("Variables not in data: ",paste0(vars_not_dt,collapse = ", "), ". So, it is not included in formula") %>% warning()
  }
  
  # filtratge 
  kk<-variables %>% dplyr::arrange(!!taula_sym) %>% dplyr::filter(!!taula_sym>0) %>% dplyr::select(!!variable_camp) %>% as.vector()
  kk<-as.vector(kk[[1]])
  purrr::set_names(kk,kk)
  
}


## Funcio per canviar noms de variables del data frame

## Entra data frame i cambia nom de variable

canvi_noms_variables<-function(dt=dt_temp,variable="ajust2", nou_nom="descripcio",conductor=conductor_variables,...) {
  
  # dt=dades_TAI
  # variable="camp"
  # nou_nom = "camp2"
  # conductor = conductor_variables
  # sheet="rename_SG"
  
  # dt=dt_temp
  # variable="ajust2"
  # conductor=conductor_variables  
  # etiqueta="descripcio"
  
  # extreure variables
  vars<-extreure.variables(variable,conductor,...)
  
  # Extreure etiquetes 
  etiquetes_noves<-extreure.variables(variable,conductor,variable_camp=nou_nom,...)
  
  # Canviar noms per etiqueta descripcio
  # setnames(dt, old = vars, new = etiquetes_noves)
  dt<-dt %>% rename_at(vars(vars), ~ etiquetes_noves)
  
}

# Retorna taula plana a partir d'un fitxer amb la data index, el fitxer del cataleg i el fitxer de parametres
# Els parametres contenen els seguens camps: 
#     fitxer (nom) ,
#     domini (refereix al tipus d'agregació i codis del cataleg
#     Finestra1 Finestra2 (Periode)
#     camp (Quin camp s'utilitza com agregació)
#     prefix (o suffix de les variables generades)
#     funcio (Funció d'agregacio en variables )
