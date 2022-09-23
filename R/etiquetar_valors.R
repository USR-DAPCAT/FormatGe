#  Etiquetar valors        ------------------------------------------

##    Retorna Data frame etiquetat en funció d'un conductor ##
## dataframe dades, conductor_variables     
etiquetar_valors<-function(dt=dades,variables_factors=conductor_variables,fulla="etiquetes",
                           camp_etiqueta="etiqueta",missings=F, new_vars=F,sufix=".2") {
  
  # dt=dades
  # variables_factors=conductor_variables
  # fulla="etiquetes"
  # camp_etiqueta="etiqueta2"
  # missings=F
  # new_vars=T
  # sufix=".cat"
  
  
  # Llegir conductor#
  variables_factors<-read_conductor(variables_factors,sheet=fulla) %>% 
    filter(!is.na(!!sym(camp_etiqueta))) # Selecciono només variables segons existeix camp etiqueta
  
  # Verifico si alguna variable ja existeix en la base de dades i si existeix comunico que es sobrescriura
  vars_generades<-variables_factors %>% distinct(camp) %>% mutate(camp=paste0(camp,sufix)) %>% pull(camp)
  
  # Llisto la llista de variables que es generara si no sobrescriu
  if (new_vars) message(paste0("Variables generated: ",  paste0(vars_generades,collapse = ", ")))
  
  # printar advertencia si algunes variables ja esta generarada i es sobreescriurà
  vars_sobresescrites<-names(dt) [names(dt) %in% vars_generades]
  if (vars_sobresescrites %>% length()>0) {message(paste0("Variables overwritten: ",paste0(vars_sobresescrites,collapse = ", ")))}
  
  # Les elimino de la base de dades abans de sobreescriurles si nova variable es creada
  if (new_vars) dt<-dt %>% select(-vars_sobresescrites)  #Tinc dubtes si cal eliminar-la
  
  # Split
  camp_etiqueta<-sym(camp_etiqueta)
  
  k<-variables_factors%>%dplyr::select(camp, valor,!!camp_etiqueta)
  pepe<-k %>% base::split(list(.$camp))
  
  #
  noms_variables<-names(pepe)
  num_vars<-length(noms_variables)
  
  # Elimina espais en blanc de les variables factor / character (treu nivells) nomes variables nomenades
  # dt<-dt %>% mutate_at(noms_variables,~ifelse(is.factor(.),trimws(.),.))
  
  dt_original<-dt # Faig copia original
  
  for (i in 1:num_vars) {
    # i<-1
    if (noms_variables[i] %in% colnames(dt)) {
      
      etiquetes_valors<-pepe[[i]] %>% pull(!!camp_etiqueta)
      
      # Transformar a caracter i treure espais en blanc
      if (is.factor(dt[[noms_variables[i]]])) {
        dt<-dt %>% mutate_at(noms_variables[i],~stringr::str_trim(as.factor(.)))}
      
      dt[noms_variables[i]]<-lapply(dt[noms_variables[i]],
                                    function(y) factor(y,levels=pepe[[i]]$valor,labels=etiquetes_valors))
      
      if (missings) dt<-missings_to_level(dt,noms_variables[i])
      
    }
  }
  
  # Si new_vars, selecciono renombro i fusiono a dt original 
  
  if (new_vars) {
    dt_recode<-dt %>% as_tibble() %>% select(noms_variables) %>% rename_at(noms_variables,function(x) paste0(x,sufix)) 
    dt<-cbind(dt_original,dt_recode) %>% as_tibble()}
  
  dt
  
}
#
