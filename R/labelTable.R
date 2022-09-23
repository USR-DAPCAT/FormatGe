#' @title labelTable
#' @description Exchange label for name of variable according external file
#' @param data data.frame
#' @param taulavariables excel file with description field
#' @param camp_descripcio Field with label in excel file
#' @param camp character name field
#' @return Data.frame or tibble with labeled variables
#' @export
#' @importFrom dplyr "%>%"
labelTable<-function(data,taulavariables,camp_descripcio,camp="camp") {

  # data=T3.MI
  # taulavariables=conductor_variables
  # camp="variables_taula"
  # camp_descripcio="descripcio"

  
  # Llegir etiquetes i variables a analitzar ####
  # variables <- readxl::read_excel(taulavariables) %>% tidyr::as_tibble()
  variables <- read_conductor(taulavariables)
  
  camp_sym<-rlang::sym(camp)
  variables<-variables %>% dplyr::filter(!is.na(!!camp_sym))

  # Canviar nom de camp de variables al de la data 
  colnames(variables)[colnames(variables)=="camp"] <- camp

  # Canviar arguments per ser evaluats
  camp_eval<-rlang::sym(camp)
  camp_descripcio_eval<-rlang::sym(camp_descripcio)
  # Canviar el format de la data 
  data<-data %>% 
    dplyr::left_join(dplyr::select(variables,c(!!camp_eval,camp_descripcio)),by=quo_name(camp_eval)) %>% 
    dplyr::mutate(!!camp_eval:=NULL) %>% 
    dplyr::rename(!!camp_eval:=camp_descripcio_eval)
  }
