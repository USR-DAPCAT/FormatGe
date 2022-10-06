#' @title         Netejar els noms de les variables
#' @description   Netejar els noms de les variables ["/","(",")","/"],-> ""  [" ","-"]-> "_" ,..
#' @param dt      Base de dades
#' @return        Base de dades amb la neteja de noms
#' @export        netejar.noms.variables
#' @importFrom    dplyr "%>%"
#' @examples
#' dt_plana$"var//"<-1
#' kk<-netejar.noms.variables(dt_plana)
#' stats::variable.names(dt_plana)
#' stats::variable.names(kk)
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



#' @title           Elimina accents dels noms de les variables
#' @description     Elimina accents dels noms de les variables
#' @param dt        Base de dades
#' @return          Base de dades  sense accents 
#' @export          netejar.accents.variables
#' @importFrom      dplyr "%>%"
#' @examples
#' dt_plana$"sexé2"<-1
#' kk<-netejar.accents.variables(dt_plana)
#' stats::variable.names(dt_plana)
#' stats::variable.names(kk)
netejar.accents.variables <- function(dt="LIPOS_EORTEGA"){
  paco<-names(dt) %>%
    iconv(to="ASCII//TRANSLIT")
  names(dt)<-paco
  dt
}


#' @title          Elimina espais dels noms de les variables
#' @description    Elimina espais dels noms de les variables
#' @param dt       Base de dades
#' @return         Base de dades elimina espais
#' @export         netejar_espais
#' @importFrom     dplyr "%>%"
#' @examples
#' dt_plana$"  var3  "<-69
#' kk<-netejar_espais(dt=dt_plana)
#' stats::variable.names(dt_plana)
#' stats::variable.names(kk)
netejar_espais<-function(dt="dades") {
  paco<-names(dt) %>% 
    stringr::str_trim()
    
  names(dt)<-paco
  dt
  
  #dt=dt_total
  #dt<-dt%>% dplyr::mutate_if(is.character,stringr::str_trim)
  
}

