#' @title              Factoritzar FormatGe
#' @description        Factoritza una llista de variables donades a la teva base de dades
#' @param dt           Base de dades plana
#' @param variables    Vector  a on posem les variables a factoritzar
#' @return             La base de dades amb les variables factoritzades 
#' @export             factoritzar 
#' @importFrom         dplyr "%>%"
#' @examples
#' 
#' class(dt_plana$situacio)
#' dt_plana2<-factoritzar(dt_plana,variables=c("grup","situacio"))
#' class(dt_plana2$situacio)
#' 
factoritzar<-function(dt="dades",variables=c("grup","situacio")) {
  
  # dt=dades
  # variables=c("grup","situacio","kk","sexe")
  
  # Només si variable existeix la variable en dt
  variables<-variables[variables %in% names(dt)]
  
  factoritzacio<-function(x) {if (!is.factor(x)) x<-as.factor(x) else x<-x}
  
  dt<-dt %>% dplyr::mutate_at(variables,factoritzacio)
}


#' @title                 Factoritzar (No/Yes).FormatGe
#' @description           A tots aquells valors dummis, el "0"-->"No" i "1"-->"Yes"  
#' @param dt              Base de dades 
#' @param columna         La columna del conductor a on farem la factoritzacio    
#' @param taulavariables  Conductor
#' @param ...             Altres funcions
#' @return                NO.YES  
#' @export                factoritzar.NO.YES 
#' @importFrom            dplyr "%>%"
#' @examples
#' camp=c("idp","dtindex","sexe","dnaix","situacio","entrada",
#' "sortida", "INCLUSIO.DM2","DG.HTA","DG.IC",
#' "cHDL.valor","cLDL.valor","cT.valor","GLICADA.valor","IMC.valor")
#' factor=c("","","","","","","",1,1,1,"","","","","")
#' dates=c("",1,"",1,"",1,1,"","","","","","","","")
#' conductor<-data.frame(camp,factor,dates)
#'dt_plana2<-dplyr::mutate_at(dt_plana, dplyr::vars(  dplyr::starts_with("DG.") ),  
#'dplyr::funs( dplyr::if_else(.==0  | is.na(.)  ,0,1)))
#'dt_plana2<-dplyr::mutate_at(dt_plana2, dplyr::vars(  dplyr::starts_with("INCLUSIO.") ),
#'dplyr::funs( dplyr::if_else(.==0  | is.na(.)  ,0,1)))
#'dt_plana2
#'dt_plana2<-factoritzar.NO.YES(dt_plana2,"factor",conductor)
#'dt_plana2
#' 
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


#' @title                         Factoritzar (No/Si).FormatGe
#' @description                   A tots aquells valors dummis, el "0"-->"No" i "1"-->"Si"  
#' @param dt                      Base de dades 
#' @param columna                 La columna del conductor a on farem la factoritzacio  
#' @param taulavariables          Conductor
#' @param ...                     Altres funcions
#' @return                        NO.Si  
#' @export                        factoritzar.NO.SI 
#' @importFrom                    dplyr "%>%"
#' @examples
#' 
#' camp=c("idp","dtindex","sexe","dnaix","situacio","entrada","sortida", 
#' "INCLUSIO.DM2","DG.HTA","DG.IC",
#' "cHDL.valor","cLDL.valor","cT.valor",
#' "GLICADA.valor","IMC.valor")
#' factor=c("","","","","","","",1,1,1,"","","","","")
#' dates=c("",1,"",1,"",1,1,"","","","","","","","")
#' conductor<-data.frame(camp,factor,dates)
#'dt_plana2<-dplyr::mutate_at(dt_plana, dplyr::vars(  dplyr::starts_with("DG.") ),  
#'dplyr::funs( dplyr::if_else(.==0  | is.na(.)  ,0,1)))
#'dt_plana2<-dplyr::mutate_at(dt_plana2, dplyr::vars(  dplyr::starts_with("INCLUSIO.") ), 
#' dplyr::funs( dplyr::if_else(.==0  | is.na(.)  ,0,1)))
#'dt_plana2
#'dt_plana2<-factoritzar.NO.SI(dt_plana2,"factor",conductor)
#'dt_plana2
#' 
factoritzar.NO.SI<-function(dt="dadesDF",columna="factor",taulavariables="variables_FELIPE.xls",...){
  
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
