# Recodificar rangs de valors que cauen fora interva a missings  -----------------
recode_to_missings<-function(dt=dades,taulavariables=conductor_variables,rang="rang_valid", data_long=F,...) {
  
  # dt,taulavariables = conductor_variables,rang="rang_valid")
  # dt=dt
  # taulavariables=conductor_variables
  # rang="rang_valid"
  # data_long=F
  
  # Llegir dades
  variables<-read_conductor(taulavariables,
                            col_types = "text",...) %>% tidyr::as_tibble()
  
  temp<-variables %>% select(c("camp","rang_valid")) %>% filter(!is.na(rang_valid))
  
  # Elimino () i [ ]
  temp <- temp %>% mutate(rang_valid=stringr::str_replace_all(rang_valid,"\\(",""),
                          rang_valid=stringr::str_replace_all(rang_valid,"\\)",""),
                          rang_valid=stringr::str_replace_all(rang_valid,"\\[",""),
                          rang_valid=stringr::str_replace_all(rang_valid,"\\]","")
  )
  
  # Separo limit inferior i limit superior
  rangs<-temp$rang_valid %>% stringr::str_split_fixed("-",2) %>% as_tibble()
  temp<-temp %>% cbind(rangs)
  
  # Inicio blucle
  num_recodes<-length(temp[,1])
  # Assignar a primer element (A partir d'aquÃ? fer un for)
  
  if (data_long==F) {
    
    for (i in 1:num_recodes) {
      # i<-15
      camp<-temp[i,]$camp
      linf<-temp[i,]$V1 %>% as.numeric()
      lsup<-temp[i,]$V2%>% as.numeric()
      
      # Recode missings fora de rang 
      dt<-dt %>% mutate_at(camp,~ifelse(.<linf | .>lsup,NA_real_,.))
    }
    
  }
  
  # En cas de taula long 
  if (data_long) {
    
    for (i in 1:num_recodes) {
      # i<-1
      camp<-temp[i,]$camp
      linf<-temp[i,]$V1 %>% as.numeric()
      lsup<-temp[i,]$V2%>% as.numeric()
      # recodifico/filtro en missings els que estan fora de rang 
      dt<-dt %>% 
        mutate(valor=ifelse((valor<linf | valor>lsup) & cod==camp ,NA,valor)) %>% 
        filter(!is.na(valor))
    }
  }
  
  dt
  
}
