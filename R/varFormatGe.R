#' @title                    Seleccionem les variables
#' @description              Seleccionem les variables a partir d'un conductor
#' @param  taula             Camp a on hi han les variables
#' @param  taulavariables    Conductor
#' @param  dt                Base de dades
#' @return                   Base de dades amb les variables seleccionades
#' @export                   selectorvariables
#' @importFrom dplyr "%>%"
#' @examples
#'camp=c("idp",
#'       "dtindex",
#'       "sexe",
#'       "dnaix",
#'       "situacio",
#'       "entrada",
#'       "sortida", 
#'       "INCLUSIO.DM2",
#'       "DG.HTA",
#'       "DG.IC",
#'       "cHDL.valor",
#'       "cLDL.valor",
#'       "cT.valor",
#'       "GLICADA.valor",
#'       "IMC.valor")
#'descripcio=c("Identificacio Pacient",
#'             "data Index",
#'             "Sexe",
#'             "data Naixament",
#'             "Situacio",
#'             "Entrada",
#'             "Sortida",
#'             "Inclusio Diabetes Tipus 2",
#'             "Hipertensió arterial",
#'             "Insuficiencia Cardiaca",
#'             "Colesterol HDL(mg/dL)",
#'             "Colesterol LDL(mg/dL)",
#'             "Colesterol Total(mg/dL)",
#'             "HbA1c",
#'             "IMC" )
#'descripcio2=c("Identificacion Paciente",
#'              "data Indice",
#'              "Sexo",
#'              "data Naicimiento",
#'              "Situacion",
#'              "Entrada",
#'              "Salida",
#'              "Inclusion Diabetes Tipus 2",
#'              "Hipertensión arterial",
#'              "Insuficiencia Cardiaca",
#'              "Colesterol HDL(mg/dL)",
#'              "Colesterol LDL(mg/dL)",
#'              "Colesterol Total(mg/dL)",
#'              "HbA1c",
#'              "IMC" )
#'factor=c("","","","","","","",1,1,1,"","","","","")
#'dates=c("",1,"",1,"",1,1,"","","","","","","","")
#'recode=c("","","","","","","","","","","","","","7.0","")
#'taula1=c(1,1,1,1,1,1,1,1,1,1,1,1,"","","")
#'
#'conductor1<-data.frame(camp,descripcio,descripcio2,factor,dates,recode,taula1)
#'k<-selectorvariables(taula="taula1",taulavariables=conductor1,dt=dt_plana)
#'               
#'k
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


#' @title                       Extreure.Variables
#' @description                 Extreure.Variables: Selector de variables TAULA 
#' @param  taula                Camp a on hi han les variables
#' @param  taulavariables       Conductor
#' @param  variable_camp        Variable camp del Conductor
#' @param  dt                   Base de dades
#' @param                       ... altres funcions
#' @return                      extreure variable
#' @export                      extreure.variables
#' @importFrom                  dplyr "%>%"
#' @examples
#'camp=c("idp",
#'       "dtindex",
#'       "sexe",
#'       "dnaix",
#'       "situacio",
#'       "entrada",
#'       "sortida", 
#'       "INCLUSIO.DM2",
#'       "DG.HTA",
#'       "DG.IC",
#'       "cHDL.valor",
#'       "cLDL.valor",
#'       "cT.valor",
#'       "GLICADA.valor",
#'       "IMC.valor")
#'descripcio=c("Identificacio Pacient",
#'             "data Index",
#'             "Sexe",
#'             "data Naixament",
#'             "Situacio",
#'             "Entrada",
#'             "Sortida",
#'             "Inclusio Diabetes Tipus 2",
#'             "Hipertensió arterial",
#'             "Insuficiencia Cardiaca",
#'             "Colesterol HDL(mg/dL)",
#'             "Colesterol LDL(mg/dL)",
#'             "Colesterol Total(mg/dL)",
#'             "HbA1c",
#'             "IMC" )
#'descripcio2=c("Identificacion Paciente",
#'              "data Indice",
#'              "Sexo",
#'              "data Naicimiento",
#'              "Situacion",
#'              "Entrada",
#'              "Salida",
#'              "Inclusion Diabetes Tipus 2",
#'              "Hipertensión arterial",
#'              "Insuficiencia Cardiaca",
#'              "Colesterol HDL(mg/dL)",
#'              "Colesterol LDL(mg/dL)",
#'              "Colesterol Total(mg/dL)",
#'              "HbA1c",
#'              "IMC" )
#'factor=c("","","","","","","",1,1,1,"","","","","")
#'dates=c("",1,"",1,"",1,1,"","","","","","","","")
#'recode=c("","","","","","","","","","","","","","7.0","")
#'taula1=c(1,1,1,1,1,1,1,1,1,1,1,1,"","","")
#'
#'conductor1<-data.frame(camp,descripcio,descripcio2,factor,dates,recode,taula1)
#'k1<-extreure.variables(taula="taula1",taulavariables=conductor1,variable_camp="camp",dt=NA)
#'k2<-extreure.variables(taula="taula1",taulavariables=conductor1,variable_camp="camp",dt=dt_plana)               
#'k1
#'k2
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



#' @title                             Canviar noms de les variables
#' @description                       Canviar noms de variables del data frame
#' @param  dt                         Base de dades
#' @param  variable                   Variables escollides a partir del conductor
#' @param  nou_nom                    Nom NOUS de les variables
#' @param  conductor                  Conductor 
#' @param ...                         Altres funcions
#' @return                            canvi de noms de les variables escollides
#' @export                            canvi_noms_variables
#' @importFrom                        dplyr "%>%"
#' @examples
#'camp=c("idp",
#'       "dtindex",
#'       "sexe",
#'       "dnaix",
#'       "situacio",
#'       "entrada",
#'       "sortida", 
#'       "INCLUSIO.DM2",
#'       "DG.HTA",
#'       "DG.IC",
#'       "cHDL.valor",
#'       "cLDL.valor",
#'       "cT.valor",
#'       "GLICADA.valor",
#'       "IMC.valor")
#'descripcio=c("Identificacio Pacient",
#'             "data Index",
#'             "Sexe",
#'             "data Naixament",
#'             "Situacio",
#'             "Entrada",
#'             "Sortida",
#'             "Inclusio Diabetes Tipus 2",
#'             "Hipertensió arterial",
#'             "Insuficiencia Cardiaca",
#'             "Colesterol HDL(mg/dL)",
#'             "Colesterol LDL(mg/dL)",
#'             "Colesterol Total(mg/dL)",
#'             "HbA1c",
#'             "IMC" )
#'descripcio2=c("Identificacion Paciente",
#'              "data Indice",
#'              "Sexo",
#'              "data Naicimiento",
#'              "Situacion",
#'              "Entrada",
#'              "Salida",
#'              "Inclusion Diabetes Tipus 2",
#'              "Hipertensión arterial",
#'              "Insuficiencia Cardiaca",
#'              "Colesterol HDL(mg/dL)",
#'              "Colesterol LDL(mg/dL)",
#'              "Colesterol Total(mg/dL)",
#'              "HbA1c",
#'              "IMC" )
#'factor=c("","","","","","","",1,1,1,"","","","","")
#'dates=c("",1,"",1,"",1,1,"","","","","","","","")
#'recode=c("","","","","","","","","","","","","","7.0","")
#'taula1=c(1,1,"","","","","","","","","","","","","")
#'var_nou=c("idp_nou","index_nou","","","","","","","","","","","","","")
#'
#'conductor1<-data.frame(camp,descripcio,descripcio2,factor,dates,recode,taula1,var_nou)
#'
#'k1<-canvi_noms_variables(dt=dt_plana,variable="taula1",nou_nom="var_nou",conductor=conductor1)
#'k1
#'
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
  dt<-dt %>%dplyr::rename_at(dplyr::vars(vars), ~ etiquetes_noves)
  
}


