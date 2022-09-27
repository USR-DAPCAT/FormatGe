#' @title formula_table1.
#' @description NETEJA NOMS DE VARIABLES DE CARACTERS EXTRANYS 
#' @param dt xxxxx
#' @return NETEJA NOMS DE VARIABLES 
#' @export netejar.noms.variables
#' @importFrom dplyr "%>%"
#' @examples
#' domini="farmacs_prescrits"
#' cod=c("A10BB01","A10BD01","A10BD04","A10BA02","J01DD07")
#' agr_Farmac=c("Sulfonilureas","Biguanidas","Tiazolidinadiones","Biguanidas","Antibioticos")
#' dt_cataleg<-data.frame(domini=domini,cod=cod,agr_Farmac=agr_Farmac)
netejar.noms.variables<-function(dt="LIPOS_EORTEGA"){
  
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
#' @param dt xxxxx
#' @return dt sense accents 
#' @export netejar.accents.variables
#' @importFrom dplyr "%>%"
#' @examples
#' domini="farmacs_prescrits"
#' cod=c("A10BB01","A10BD01","A10BD04","A10BA02","J01DD07")
#' agr_Farmac=c("Sulfonilureas","Biguanidas","Tiazolidinadiones","Biguanidas","Antibioticos")
#' dt_cataleg<-data.frame(domini=domini,cod=cod,agr_Farmac=agr_Farmac)
netejar.accents.variables <- function(dt="LIPOS_EORTEGA"){
  paco<-names(dt) %>%
    iconv(to="ASCII//TRANSLIT")
  names(dt)<-paco
  dt
}


#' @title elimina espais dels noms de les variables.
#' @description elimina espais dels noms de les variables
#' @param dt xxxxx
#' @return dt  elimina espais
#' @export netejar_espais
#' @importFrom dplyr "%>%"
#' @examples
#' domini="farmacs_prescrits"
#' cod=c("A10BB01","A10BD01","A10BD04","A10BA02","J01DD07")
#' agr_Farmac=c("Sulfonilureas","Biguanidas","Tiazolidinadiones","Biguanidas","Antibioticos")
#' dt_cataleg<-data.frame(domini=domini,cod=cod,agr_Farmac=agr_Farmac)
netejar_espais<-function(dt="dades") {
  # dt=dt_total
  dt<-dt %>% dplyr::mutate_if(is.character,stringr::str_trim)
}


