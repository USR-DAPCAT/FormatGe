#' @title selectorvariables
#' @description selectorvariables 
#' @param  taula           xxx
#' @param  taulavariables  xxx
#' @param  dt              xxx
#' @return variables
#' @export selectorvariables
#' @importFrom dplyr "%>%"
#' @examples
#' domini="farmacs_prescrits"
#' cod=c("A10BB01","A10BD01","A10BD04","A10BA02","J01DD07")
#' agr_Farmac=c("Sulfonilureas","Biguanidas","Tiazolidinadiones","Biguanidas","Antibioticos")
#' dt_cataleg<-data.frame(domini=domini,cod=cod,agr_Farmac=agr_Farmac)
#' 
selectorvariables<-function(taula="table1",
                            taulavariables="variables_R.xls",
                            dt="dadestotal") {
  
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


#' @title Extreure.Variables
#' @description Extreure.Variables: Selector de variables TAULA 
#' @param  taula           xxx
#' @param  taulavariables  xxx
#' @param  variable_camp   xxx
#' @param  dt              xxx 
#' @param ... altres funcions
#' @return extreure variable
#' @export extreure.variables
#' @importFrom dplyr "%>%"
#' @examples
#' domini="farmacs_prescrits"
#' cod=c("A10BB01","A10BD01","A10BD04","A10BA02","J01DD07")
#' agr_Farmac=c("Sulfonilureas","Biguanidas","Tiazolidinadiones","Biguanidas","Antibioticos")
#' dt_cataleg<-data.frame(domini=domini,cod=cod,agr_Farmac=agr_Farmac)
#' 
extreure.variables=function(taula="table1",
                            taulavariables="variables_R.xls",
                            variable_camp="camp",
                            dt=NA,...) {
  
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
    vars_not_dt<-variables %>% dplyr::anti_join(names(dt) %>% tibble::as_tibble(camp=value),by=c("camp"="value")) %>% dplyr::pull("camp")
    variables<-variables %>%dplyr:: semi_join(names(dt) %>%tibble:: as_tibble(camp=value),by=c("camp"="value"))
    paste0("Variables not in data: ",paste0(vars_not_dt,collapse = ", "), ". So, it is not included in formula") %>% warning()
  }
  
  # filtratge 
  kk<-variables %>% dplyr::arrange(!!taula_sym) %>% dplyr::filter(!!taula_sym>0) %>% dplyr::select(!!variable_camp) %>% as.vector()
  kk<-as.vector(kk[[1]])
  purrr::set_names(kk,kk)
  
}



#' @title Funcio per canviar noms de variables del data frame
#' @description Funcio per canviar noms de variables del data frame
#' @param  dt              xxx
#' @param  variable        xxx
#' @param  nou_nom         xxx
#' @param  conductor       xxx 
#' @param ...              altres funcions
#' @return extreure variable
#' @export extreure.variables
#' @importFrom dplyr "%>%"
#' @examples
#' domini="farmacs_prescrits"
#' cod=c("A10BB01","A10BD01","A10BD04","A10BA02","J01DD07")
#' agr_Farmac=c("Sulfonilureas","Biguanidas","Tiazolidinadiones","Biguanidas","Antibioticos")
#' dt_cataleg<-data.frame(domini=domini,cod=cod,agr_Farmac=agr_Farmac)
canvi_noms_variables<-function(dt="dt_temp",
                               variable="ajust2", 
                               nou_nom="descripcio",
                               conductor="conductor_variables",
                               ...) {
  
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
  dt<-dt %>%dplyr:: rename_at(vars(vars), ~ etiquetes_noves)
  
}


