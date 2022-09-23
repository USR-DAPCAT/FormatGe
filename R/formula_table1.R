
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


