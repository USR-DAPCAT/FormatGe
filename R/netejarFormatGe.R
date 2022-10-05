#' @title         Netejar els noms de les variables
#' @description   Netejar els noms de les variables
#' @param dt      Base de dades
#' @return        Base de dades amb la neteja de noms
#' @export        netejar.noms.variables
#' @importFrom    dplyr "%>%"
#' @examples
#' kk<-netejar.noms.variables(dt_plana)
#' kk
netejar.noms.variables<-function(dt="hoolywod"){
  
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



#' @title           Elimina accents dels noms de les variables.
#' @description     Elimina accents dels noms de les variables
#' @param dt        Base de dades
#' @return          Base de dades  sense accents 
#' @export          netejar.accents.variables
#' @importFrom      dplyr "%>%"
#' @examples
#' kk<-netejar.accents.variables(dt_plana)
#' kk
netejar.accents.variables <- function(dt="LIPOS_EORTEGA"){
  paco<-names(dt) %>%
    iconv(to="ASCII//TRANSLIT")
  names(dt)<-paco
  dt
}


#' @title          Elimina espais dels noms de les variables.
#' @description    Elimina espais dels noms de les variables
#' @param dt       Base de dades
#' @return         Base de dades elimina espais
#' @export         netejar_espais
#' @importFrom     dplyr "%>%"
#' @examples
#' kk<-netejar_espais(dt_plana)
#' kk
netejar_espais<-function(dt="dades") {
  # dt=dt_total
  dt<-dt %>% dplyr::mutate_if(is.character,stringr::str_trim)
}


