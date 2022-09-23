#' @title formula_table1.
#' @description NETEJA NOMS DE VARIABLES DE CARACTERS EXTRANYS 
#' @param dt 
#' @return NETEJA NOMS DE VARIABLES 
#' @export netejar.noms.variables
#' @importFrom dplyr "%>%"
netejar.noms.variables<-function(dt=LIPOS_EORTEGA){
  
  paco<-names(dt) %>% 
    iconv("UTF-8","ASCII","") %>% 
    stringr::str_replace("/","") %>% 
    stringr::str_replace_all("\\(","") %>% 
    stringr::str_replace_all("\\)","") %>% 
    stringr::str_replace_all("\\/","") %>% 
    stringr::str_trim() %>% 
    stringr::str_replace_all(" ","_") %>% 
    stringr::str_replace_all("-","_") %>%
    stringr::str_replace_all("([.])\\1+","\\1") %>% 
    stringr::str_replace_all("\\*","") %>% 
    stringr::str_replace_all("\\?","") %>% 
    stringr::str_replace_all("\\<","Inf") %>% 
    stringr::str_replace_all("\\>","Sup") %>% 
    stringr::str_replace_all("\\[","") %>% 
    stringr::str_replace_all("\\]","") 
  
  names(dt)<-paco
  dt
  
}



#' @title elimina accents dels noms de les variables.
#' @description Funció que elimina accents dels noms de les variables
#' @param dt 
#' @return dt sense accents 
#' @export netejar.accents.variables
#' @importFrom dplyr "%>%"
netejar.accents.variables <- function(dt=LIPOS_EORTEGA){
  paco<-names(dt) %>%
    iconv(to="ASCII//TRANSLIT")
  names(dt)<-paco
  dt
}


#' @title elimina espais dels noms de les variables.
#' @description elimina espais dels noms de les variables
#' @param dt 
#' @return dt  elimina espais
#' @export netejar_espais
#' @importFrom dplyr "%>%"
netejar_espais<-function(dt=dades) {
  # dt=dt_total
  dt<-dt %>% mutate_if(is.character,stringr::str_trim)
}


