#' @title         Netejar els noms de les variables
#' @description   Netejar els noms de les variables ["/","(",")","/"],-> ""  [" ","-"]-> "_" ,..
#' @param dt      Base de dades
#' @return        Base de dades amb la neteja de noms
#' @export        netejar.noms.variables
#' @importFrom    dplyr "%>%"
#' @examples
#' dt_plana_test$"var//"<-1
#' kk<-netejar.noms.variables(dt_plana_test)
#' stats::variable.names(dt_plana_test)
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
#' dt_plana_test$"sexé2"<-1
#' kk<-netejar.accents.variables(dt_plana_test)
#' stats::variable.names(dt_plana_test)
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
#' dt_plana_test$"  var3  "<-69
#' kk<-netejar_espais(dt=dt_plana_test)
#' stats::variable.names(dt_plana_test)
#' stats::variable.names(kk)
netejar_espais<-function(dt="dades") {
  paco<-names(dt) %>% 
    stringr::str_trim()
    
  names(dt)<-paco
  dt
  
  #dt=dt_total
  #dt<-dt%>% dplyr::mutate_if(is.character,stringr::str_trim)
  
}

#' @title                             Canviar noms de les variables 
#' @description                       Canviar noms de variables  de la nostra b.d a prtir d un conductor
#' @param  dt                         Base de dades
#' @param  variable                   Variables escollides a partir del conductor
#' @param  nou_nom                    Nom NOUS de les variables
#' @param  conductor                  Conductor 
#' @param ...                         Altres funcions
#' @return                            canvi de noms de les variables escollides
#' @export                            canvi_noms_variables
#' @importFrom                        dplyr "%>%"
#' @examples

#'k1<-canvi_noms_variables(dt=dt_plana_test,variable="taula2",
#'nou_nom="var_nou",conductor=conductor1_test)
#'dt_plana_test
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

