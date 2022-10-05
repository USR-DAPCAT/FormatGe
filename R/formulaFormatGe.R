#' @title                Formula FormatGe.
#' @description          Retorna Data.frame or tibble with labeled variables
#' @param x              Indicador de cadena del camp del fitxer extern amb indicador de la posició de la fórmula (1,2....) 
#' @param y              Indicador de resposta de cadena (default "."): ". ~ x1 +x2 + x3 "
#' @param eliminar       Caracter que indica si alguna variable ha de ser eliminada
#' @param a              Caracter , indica si cal afegir alguna variable en la primera posició
#' @param taulavariables Conductor , a on triem l'orde de les variables de la formula
#' @param dt             data.frame o base de dades
#' @return               formula
#' @export               formula_text 
#' @importFrom           dplyr "%>%"
#' @examples
#' 
#' Hmisc::label(iris)
#' conductor_iris<-data.frame(camp=names(iris),formu=c(1,2,3,4,5))
#' formula_iris1<-formula_text("formu",taulavariables = conductor_iris)
#' formula_iris1

formula_text<-function(x="taula1",y="resposta",eliminar=c("IDP"), a="",taulavariables,dt=NA) {
  
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
    vars_not_dt<-variables %>% dplyr::anti_join(names(dt) %>%tibble:: as_tibble(camp=value),by=c("camp"="value"))
    variables<-variables %>%dplyr:: semi_join(names(dt) %>%tibble:: as_tibble(camp=value),by=c("camp"="value"))
    warning(paste0("Variables not in data ",vars_not_dt["camp"], ". So, it is not included in formula"))}
  
  pepito<-paste("as.vector(variables[variables$",x,">0,]$camp)[!as.vector(variables[variables$",x,">0,]$camp)%in%eliminar]",sep="")
  
  llistataula<-eval(parse(text=pepito))
  if (a!="") llistataula<-c(a,llistataula,a)
  
  y<-paste(y, paste(llistataula, collapse=" + "), sep=" ~ ")
  
}







#' @title                   Formula_table1.
#' @description             Retorna formula per table1::table1 segons llista de varibles
#' @param x                 Indicador de cadena del camp del fitxer extern amb indicador de la posició de la fórmula (1,2....) 
#' @param y                 Indicador de resposta de cadena (default "."): ". ~ x1 +x2 + x3 "
#' @param eliminar          Caracter que indica si alguna variable ha de ser eliminada
#' @param a                 Caracter , indica si cal afegir alguna variable en la primera posició
#' @param taulavariables    Conductor , a on triem l'orde de les variables de la formula
#' @param dt                Data.frame o base de dades
#' @param ...               Altres funcions
#' @return                  Formula_table1
#' @export                  formula_table1 
#' @importFrom              dplyr "%>%"
#' @examples
#' Hmisc::label(iris)
#' conductor_iris<-data.frame(camp=names(iris),formu=c(1,2,3,4,5))
#' formula_iris2<-formula_table1("formu",taulavariables = conductor_iris)
#' formula_iris2
formula_table1<-function(x="taula1",
                         y="",
                         eliminar=c("IDP"), 
                         a="",
                         taulavariables='variables.xls',
                         dt=NA,...) {
  
  # x="taula_basal"
  # y="Species"
  # eliminar=c("IDP")
  # a=""
  # taulavariables=etiquetes_iris
  # dt=NA
  
  variables <- read_conductor(taulavariables,...) %>% data.frame()
  
  # variables[is.na(variables)]<- 0
  x_sym<-rlang::sym(x)
  variables<-variables %>% dplyr::filter(!is.na(!!x_sym))
  
  variables<-variables %>% 
    dplyr::filter(!!x_sym>0) %>% 
    dplyr::arrange(!!x_sym)
  
  # Verificar si dades estan en conductor
  if (is.data.frame(dt)) {
    vars_not_dt<-variables %>% dplyr::anti_join(names(dt) %>% tibble::as_tibble(camp=value),by=c("camp"="value")) %>% dplyr::pull("camp")
    variables<-variables %>%dplyr:: semi_join(names(dt) %>% tibble::as_tibble(camp=value),by=c("camp"="value"))
    paste0("Variables not in data: ",paste0(vars_not_dt,collapse = ", "), ". So, it is not included in formula") %>% warning()
  }
  
  pepito<-paste("as.vector(variables[variables$",x,">0,]$camp)[!as.vector(variables[variables$",x,">0,]$camp)%in%eliminar]",sep="")
  
  llistataula<-eval(parse(text=pepito))
  if (a!="") llistataula<-c(a,llistataula,a)
  
  formu<-paste("",paste(llistataula, collapse=" + "), sep=" ~ ") 
  
  if (y!="") formu<-paste0(formu," | ",y)
  
  stats::as.formula(formu)
}



#' @title               Formula_vector.
#' @description         Formula_vector, vector amb les variables ,y, vector caracter a elimina)
#' @param vector        Vector amb les variables
#' @param y             Variable dependents
#' @param logit         Logit
#' @param eliminar      Aquelles variables per eliminar
#' @return              Formula_vector
#' @export              formula_vector 
#' @importFrom          dplyr "%>%"
#' @examples
#' vector=c("sex","age","age")
#' formula_iris3<-formula_vector(vector,y="y")
#' formula_iris3
formula_vector<-function(vector=c("sex","age","age"),y="y",logit=F,eliminar=NA){
  
  vector<-vector [!vector %in% eliminar] %>% unique()
  
  if (!logit) {formula= stats::as.formula(paste(y, paste(vector, collapse=" + "), sep=" ~ "))}
  if (logit) {formula=paste0("as.factor(",y,")~ ", paste(vector, collapse=" + ")) %>%  stats::as.formula()}           
  
  formula
  
}


