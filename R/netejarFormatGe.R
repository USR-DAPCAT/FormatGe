

#  NETEJA NOMS DE VARIABLES DE CARACTERS EXTRANYS ("/","(".....) ---------------

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


# Funció que elimina accents dels noms de les variables
netejar.accents.variables <- function(dt=LIPOS_EORTEGA){
  paco<-names(dt) %>%
    iconv(to="ASCII//TRANSLIT")
  names(dt)<-paco
  dt
}



netejar_espais<-function(dt=dades) {
  # dt=dt_total
  dt<-dt %>% mutate_if(is.character,stringr::str_trim)
}


