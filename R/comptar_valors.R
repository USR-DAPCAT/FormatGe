#  Comptar_valors(dt, vector_variables, valor)  ##################
# en funció de vector de variables, i un valor("Yes")

comptar_valors<-function(dt=dadesevents,variables=c("EV.TER.ARTER_PERIF","EV.TER.AVC"),valor="Yes"){
  
  # dt=dades
  # variables=c("EV1_ULCERES", "EV2_ULCERES", "EV3_ULCERES", "EV4_ULCERES")
  # valor="Yes"
  
  # Concateno valors
  pepito<-paste0("paste0(",paste0(variables,collapse = ","),")")
  
  dt<-dt %>% 
    mutate_("combi_vars"=pepito) %>% 
    mutate(
      num_valors=str_count(combi_vars,valor)) %>% 
    dplyr::select(-combi_vars)
  
}
