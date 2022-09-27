#' @title factoritzar FormatGe.
#' @description factoritzar una llista de variables donades unes dades i un vector de variables 
#' @param dt xxxxxxxxxx 
#' @param variables xxxxxxxxxx 
#' @return llista de variables
#' @export factoritzar 
#' @importFrom dplyr "%>%"
#' @examples
#' domini="farmacs_prescrits"
#' cod=c("A10BB01","A10BD01","A10BD04","A10BA02","J01DD07")
#' agr_Farmac=c("Sulfonilureas","Biguanidas","Tiazolidinadiones","Biguanidas","Antibioticos")
#' dt_cataleg<-data.frame(domini=domini,cod=cod,agr_Farmac=agr_Farmac)
#'
#' idp=c(1,2,3,4,5)
#' sexe=c("H","H","D","D","H")
#' dnaix=c(19200801,19250201,19370401,19340701,19400101)
#' entrada=c(20060101,20060101,20060101,20060101,20060101)
#' sortida=c(20181029,20090102,20181231,20150911,20181231)
#' situacio=c("A","A","A","D","T")
#' dt_poblacio<-data.frame(idp,sexe,dnaix,situacio,entrada,sortida)

factoritzar<-function(dt="dades",variables=c("grup","situacio")) {
  
  # dt=dades
  # variables=c("grup","situacio","kk","sexe")
  
  # Només si variable existeix la variable en dt
  variables<-variables[variables %in% names(dt)]
  
  factoritzacio<-function(x) {if (!is.factor(x)) x<-as.factor(x) else x<-x}
  
  dt<-dt %>% dplyr::mutate_at(variables,factoritzacio)
}


#' @title factoritzar FormatGe.
#' @description factoritzar NO.Yes llista de variables "factor" situades a la taulavariables camp=factor 
#' @param dt xxxxxxxxxx 
#' @param columna xxxxxxxxxx 
#' @param taulavariables xxxxxxxxxx
#' @param ... altres funcions
#' @return NO.YES  
#' @export factoritzar.NO.YES 
#' @importFrom dplyr "%>%"
#' @examples
#' domini="farmacs_prescrits"
#' cod=c("A10BB01","A10BD01","A10BD04","A10BA02","J01DD07")
#' agr_Farmac=c("Sulfonilureas","Biguanidas","Tiazolidinadiones","Biguanidas","Antibioticos")
#' dt_cataleg<-data.frame(domini=domini,cod=cod,agr_Farmac=agr_Farmac)

factoritzar.NO.YES<-function(dt="dadesDF",columna="factor",taulavariables="variables_FELIPE.xls",...){
  
  # dt=dades
  # columna="factor.YESNO"
  # taulavariables=conductor_variables
  
  # Extreure variables  
  x<-extreure.variables(taula=columna,taulavariables=taulavariables,...) 
  
  # Seleccionar només variables que estan en dt
  if (!x[!x%in%names(dt)] %>% length()<1) {print("No existeixen en dt:");print(x[!x%in%names(dt)])}
  
  # Selecciono nomes les vars en bdades
  x<-x[x%in%names(dt)]
  
  ###   Factoritzar-les
  dt[x]<-lapply(dt[x],function(y) factor(y,levels=c(0,1), labels=c("No","Yes")))
  dt
  
}


#' @title factoritzar FormatGe.
#' @description factoritzar NO.SI llista de variables "factor" situades a la taulavariables camp=factor 
#' @param dt xxxxxxxxxx 
#' @param columna xxxxxxxxxx 
#' @param taulavariables xxxxxxxxxx
#' @return NO.YES  
#' @export factoritzar.NO.SI
#' @importFrom dplyr "%>%"
#' @examples
#' domini="farmacs_prescrits"
#' cod=c("A10BB01","A10BD01","A10BD04","A10BA02","J01DD07")
#' agr_Farmac=c("Sulfonilureas","Biguanidas","Tiazolidinadiones","Biguanidas","Antibioticos")
#' dt_cataleg<-data.frame(domini=domini,cod=cod,agr_Farmac=agr_Farmac)

factoritzar.NO.SI<-function(dt="dadesDF",columna="factor",taulavariables="variables_FELIPE.xls"){
  
  #dt=dt_plana
  #columna="factor"
  #taulavariables=conductor
  
  # Extreure variables  
  x<-extreure.variables(taula=columna,taulavariables=taulavariables) 
  
  # Seleccionar nomÃÂ©s variables que estan en dt
  if (!x[!x%in%names(dt)] %>% length()<1) {print("No existeixen en dt:");print(x[!x%in%names(dt)])}
  
  # Selecciono nomes les vars en bdades
  x<-x[x%in%names(dt)]
  
  ###   Factoritzar-les
  dt[x]<-lapply(dt[x],function(y) factor(y,levels=c(0,1), labels=c("No","Si")))
  dt
  
}
