#' To generate a formula with variables according external file
#
#' Open a external file and became a formula in text format: "y ~ x1 + x2 + x3"
#
#' @param x String indicator of field from external file with indicator the position of of formula (1,2....). 

#' @param y string indicator of response (default "."): ". ~ x1 +x2 + x3 "
#'
#' @param eliminar character that indicate if some variable has to be clean
#' 
#' @param a character that indicate if some variable has to be add in the first position
#'
#' @param taulavariables excel file with number indicator field (discrete number 1:Inf)
#' 
#' @param dt data.frame
#'
#' @return Data.frame or tibble with labeled variables
#'
#' @import dplyr
#'
#' @examples
#' 
#' Hmisc::label(iris)
#' 
#' conductor_iris<-data.frame(camp=names(iris),formu=c(1,2,3,4,5))
#' 
#' formula_iris<-formula_text("formu",taulavariables = conductor_iris)
#' 
#' formula_iris
#' 
#' @export

formula_text=function(x="taula1",y="resposta",eliminar=c("IDP"), a="",taulavariables,dt=NA) {
  
  variables<-read_conductor(conductor_iris)
  # variables <- data.frame(readxl::read_excel(taulavariables))
  # variables[is.na(variables)]<- 0
  x_sym<-rlang::sym(x)
  variables<-variables %>% dplyr::filter(!is.na(!!x_sym))
  
  variables<-variables %>% 
    dplyr::filter(!!x_sym>0) %>% 
    dplyr::arrange(!!x_sym)
  
  # Verificar si dades estan en conductor
  if (is.data.frame(dt)) {
    vars_not_dt<-variables %>% anti_join(names(dt) %>% as_tibble(camp=value),by=c("camp"="value"))
    variables<-variables %>% semi_join(names(dt) %>% as_tibble(camp=value),by=c("camp"="value"))
    warning(paste0("Variables not in data ",vars_not_dt["camp"], ". So, it is not included in formula"))}
  
  pepito<-paste("as.vector(variables[variables$",x,">0,]$camp)[!as.vector(variables[variables$",x,">0,]$camp)%in%eliminar]",sep="")
  
  llistataula<-eval(parse(text=pepito))
  if (a!="") llistataula<-c(a,llistataula,a)
  
  y<-paste(y, paste(llistataula, collapse=" + "), sep=" ~ ")
  
}
