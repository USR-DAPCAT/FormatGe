


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
