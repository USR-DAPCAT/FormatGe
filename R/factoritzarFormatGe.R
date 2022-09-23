


# factoritzar vector ------------
# factoritzar una llista de variables donades unes dades i un vector de variables 

factoritzar<-function(dt=dades,variables=c("grup","situacio")) {
  
  # dt=dades
  # variables=c("grup","situacio","kk","sexe")
  
  # Només si variable existeix la variable en dt
  variables<-variables[variables %in% names(dt)]
  
  factoritzacio<-function(x) {if (!is.factor(x)) x<-as.factor(x) else x<-x}
  
  dt<-dt %>% mutate_at(variables,factoritzacio)
}


#  factoritzar NO.Yes  ------------------
##########      factoritzar NO.Yes llista de variables "factor" situades a la taulavariables camp=factor
factoritzar.NO.YES<-function(dt=dadesDF,columna="factor",taulavariables="variables_FELIPE.xls",...){
  
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

factoritzar.NO.SI<-function(dt=dadesDF,columna="factor",taulavariables="variables_FELIPE.xls"){
  
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
