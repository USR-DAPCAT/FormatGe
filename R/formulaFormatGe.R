#' @title Formula FormatGe.
#' @description Retorna Data.frame or tibble with labeled variables
#' @param x String indicator of field from external file with indicator the position of of formula (1,2....). 
#' @param y string indicator of response (default "."): ". ~ x1 +x2 + x3 "
#' @param eliminar character that indicate if some variable has to be clean
#' @param a character that indicate if some variable has to be add in the first position
#' @param taulavariables excel file with number indicator field (discrete number 1:Inf)
#' @param dt data.frame
#' @return Data.frame or tibble with labeled variables
#' @export formula_text 
#' @export formula_table1  
#' @export formula_vector  
#' @importFrom dplyr "%>%"
#' @examples
#' 
#' Hmisc::label(iris)
#' conductor_iris<-data.frame(camp=names(iris),formu=c(1,2,3,4,5))
#' formula_iris<-formula_text("formu",taulavariables = conductor_iris)
#' formula_iris

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
    vars_not_dt<-variables %>% anti_join(names(dt) %>% as_tibble(camp=value),by=c("camp"="value"))
    variables<-variables %>% semi_join(names(dt) %>% as_tibble(camp=value),by=c("camp"="value"))
    warning(paste0("Variables not in data ",vars_not_dt["camp"], ". So, it is not included in formula"))}
  
  pepito<-paste("as.vector(variables[variables$",x,">0,]$camp)[!as.vector(variables[variables$",x,">0,]$camp)%in%eliminar]",sep="")
  
  llistataula<-eval(parse(text=pepito))
  if (a!="") llistataula<-c(a,llistataula,a)
  
  y<-paste(y, paste(llistataula, collapse=" + "), sep=" ~ ")
  
}

# Retorna formula per table1::table1 segons llista de varibles
# Columna de variables amb que vull generar la formula 

# x= variables / y = grups (opcional) / eliminar /  a = Avaluar (primera posició no inclosa en condutor)

formula_table1<-function(x="taula1",y="",eliminar=c("IDP"), a="",taulavariables='variables.xls',dt=NA,...) {
  
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
    vars_not_dt<-variables %>% anti_join(names(dt) %>% as_tibble(camp=value),by=c("camp"="value")) %>% pull("camp")
    variables<-variables %>% semi_join(names(dt) %>% as_tibble(camp=value),by=c("camp"="value"))
    paste0("Variables not in data: ",paste0(vars_not_dt,collapse = ", "), ". So, it is not included in formula") %>% warning()
  }
  
  pepito<-paste("as.vector(variables[variables$",x,">0,]$camp)[!as.vector(variables[variables$",x,">0,]$camp)%in%eliminar]",sep="")
  
  llistataula<-eval(parse(text=pepito))
  if (a!="") llistataula<-c(a,llistataula,a)
  
  formu<-paste("",paste(llistataula, collapse=" + "), sep=" ~ ") 
  
  if (y!="") formu<-paste0(formu," | ",y)
  
  as.formula(formu)
}


#  formula_vector(vector,y, vector caracter a elimina) ##########

formula_vector<-function(vector=c("sex","age","age"),y="y",logit=F,eliminar=NA){
  
  vector<-vector [!vector %in% eliminar] %>% unique()
  
  if (!logit) {formula=as.formula(paste(y, paste(vector, collapse=" + "), sep=" ~ "))}
  if (logit) {formula=paste0("as.factor(",y,")~ ", paste(vector, collapse=" + ")) %>% as.formula()}           
  
  formula
  
}


