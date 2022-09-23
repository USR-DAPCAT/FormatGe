#  Selector de Variables      -------
#
selectorvariables=function(taula="table1",taulavariables="variables_R.xls",dt=dadestotal) {
  
  # taula = "dades_imputacio2" 
  # taulavariables="variables_v2.xls"
  # dt=dades_test 
  
  vector_variables<-extreure.variables(taula=taula,taulavariables = taulavariables)
  
  # Selecciono les que no existeixen en DT 
  variables.no.existeixen<-vector_variables[!is.element(vector_variables,names(dt))]
  
  # Elimino les que no existeixen
  vector_variables<-vector_variables[is.element(vector_variables,names(dt))]
  moco<-dt %>% dplyr::select_at(vector_variables)
  
  message(paste0("Llista de variables que no existeixen en el dataset:",paste0(variables.no.existeixen ,collapse = ", ")))
  
  moco
  
  
}