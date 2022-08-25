#' Apply levels to values in variables according external file
#' 
#' @param data data.frame
#'
#' @param taulavariables excel file with description field
#'
#' @param fulla sheet in excel file with description field
#'
#' @param camp_etiqueta field with value and label in excel file
#'
#' @return Data.frame or tibble with leveled variables
#'
#' @import dplyr
#'
#' @export

##  Retorna Data frame etiquetat en funció d'un conductor ##
## dataframe dades, conductor_variables     
levelData<-function(data,taulavariables,fulla,camp_etiqueta){
  
  # data=dades
  # taulavariables=conductor_variables
  # fulla="etiquetes_valors"
  # camp_etiqueta="etiqueta"
  
  # Llegir conductor#
  taulavariables<-readxl::read_excel(taulavariables,sheet=fulla) %>% tidyr::as_tibble()
  
  # Split
  camp_etiqueta<-rlang::sym(camp_etiqueta)
  k<-taulavariables %>% dplyr::select(camp, valor,!!camp_etiqueta)
  pepe<-k %>% base::split(list(.$camp))
  #
  noms_variables<-names(pepe)
  num_vars<-length(noms_variables)
  
  # Elimina espais en blanc de totes les variables factor / character (treu nivells) tot el data_frame
  data[sapply(data,is.factor)] <- lapply(data[sapply(data,is.factor)], trimws)
  data[sapply(data,is.character)] <- lapply(data[sapply(data,is.character)], trimws)
  
  for (i in 1:num_vars) {
    # i<-1
    if (noms_variables[i] %in% colnames(data)) {
      etiquetes_valors<-pepe[[i]] %>% pull(!!camp_etiqueta)
      data[noms_variables[i]]<-lapply(data[noms_variables[i]],function(y) factor(y,levels=pepe[[i]]$valor,labels=etiquetes_valors))
      }
    }
  
  data
  }
#
