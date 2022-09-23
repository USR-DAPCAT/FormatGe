#  Recodifico EN FUNCIÓ DE de llista de camps  -------------------
### RETORNA DADES AMB RECODIFICACIÓ 
recodificar<-function(dt=dades,taulavariables="VARIABLES.xls",criteris="recode1",missings=F,prefix=NA,...){
  
  # dt=iris
  # taulavariables = etiquetes_iris
  # criteris = "recode"
  # missings=F
  # prefix=NA
  
  ##  Llegeix criteris de variables 
  variables<-read_conductor(taulavariables,...) %>% dplyr::select(camp,!!criteris) %>% mutate_all(as.character)
  criteris_sym<-rlang::sym(criteris)
  variables<-variables %>% dplyr::filter(!is.na(!!criteris_sym) & !!criteris_sym!="")
  
  ##  0. Filtro taula variables només variables implicades en el filtre i el genero 
  caracter_quartil<-"Q"
  
  maco<-variables %>% 
    dplyr::select(camp,criteris) %>% 
    filter(!str_detect(eval(parse(text=criteris)), caracter_quartil))
  
  ## Generar recodificació en base info
  maco_lista<-maco %>% base::split(list(.$camp))
  num_recodes<-length(maco_lista)
  
  # Assignar a primer element (A partir d'aquí fer un for)
  
  for (i in 1:num_recodes) {
    
    # i<-2
    
    maco<-maco_lista[[i]]
    
    mamon<-stringr::str_split(maco[criteris],"/") %>% 
      unlist() %>% 
      as.numeric()
    mamon<-c(-Inf,mamon,Inf)
    
    ##### Fer la recodificació en base el rang generat 
    nomcamp<-maco["camp"] %>% as.character()
    nomrecode<-paste0(nomcamp,".cat",length(mamon))
    
    if (!is.na(prefix)) {nomrecode<-paste0(nomcamp,".cat",prefix,length(mamon)) }
    
    # Si la variables ja existeix la elimino i la sobrescric
    if (nomrecode%in%names(dt)) {dt<-dt %>% select_(paste0("-",nomrecode))}
    
    dt<-dt %>% mutate_(camp=nomcamp)
    dt<-dt %>% mutate(popes=cut(camp,breaks = mamon) %>% as.factor)
    
    # Si missings --> generar a una categoria missing
    if (missings==T) {dt<-missings_to_level(dt,"popes")}
    
    colnames(dt)[colnames(dt)=="popes"] <- nomrecode
    dt<-dt %>% dplyr::select(-camp)
    
    print(paste0("Generada: ",nomrecode))
    # Validació
    dt %>% group_by_at(vars(!!nomrecode)) %>% summarise_at(vars(!!nomcamp),list(min=~min(.,na.rm=T),max=~max(.,na.rm=T),freq=~n())) %>% ungroup() %>% 
      print()
  }
  
  dt
}




#08.05.2020

#############################################################################################
##R E C O D I F I C A C I Ó: # Recodificacions automatiques!

recodificar2<-function(dt=dt_plana,
                       taulavariables =conductor,
                       criteris = "recode",
                       missings=T,
                       prefix=NA,
                       criteris_labels = FALSE,...)
  
  
{
  
  
  #Els  criteris_labels: [N0,AUTO, o els "nostres labels"]
  
  #[NO  : en aquesta variable no hi ha lables]
  #[AUTO: posa els intervals automaticament de manera correlativa posant un Enter (1,2,3,4,5.....)]  
  #["Els nostres labels": si no coincideixen amb els talls+1, tindrem un ERROR, i no farà la funció!]
  
  #Per defecte, actua com la Funcio :  recodificar!.
  
  #Eps: ... heredem mètodes de la "funció cut" :  (right=F, etc)
  
  #cut:...
  
  #-------------------------------------------------------------------------------------------------------------------------------------------------------#
  #        [x]              :   a numeric vector which is to be converted to a factor by cutting.
  
  #        [breaks]         :   either a numeric vector of two or more unique cut points or a single number
  #                             (greater than or equal to 2) giving the number 
  #                             of intervals into which x is to be cut.
  
  #        [labels]         :   labels for the levels of the resulting category. By default, labels are constructed using "(a,b]" interval notation.
  #                             If labels = FALSE, simple integer codes are returned instead of a factor.  
  
  #        [include.lowest] :   logical, indicating if an ‘x[i]’ equal to the lowest 
  #                             (or highest, for right = FALSE) ‘breaks’ value should be included.
  
  #        [right]          :   logical, indicating if the intervals should be closed on the right 
  #                             (and open on the left) or vice versa.
  
  #        [dig.lab]        :   integer which is used when labels are not given. 
  #                             It determines the number of digits used in formatting the break numbers.
  
  #        [ordered_result] :   logical: should the result be an ordered factor?
  #--------------------------------------------------------------------------------------------------------------------------------------------------------#  
  
  #------------------------------#  
  #dt=dt_plana
  #taulavariables =conductor
  #criteris = "recode"
  #missings=T
  #prefix="cat"
  #criteris_labels = "recode_labels"
  #------------------------------#
  
  # si hi ha [criteris_labels], apliquem aquest if.
  
  if (criteris_labels!=is.na(criteris_labels)){
    
    ##  Llegeix criteris de variables 
    
    variables<-read_conductor(taulavariables,...) %>% dplyr::select(camp,!!criteris) %>% mutate_all(as.character)
    criteris_sym<-rlang::sym(criteris)
    variables<-variables %>% dplyr::filter(!is.na(!!criteris_sym))
    
    #variables
    
    ##  0. Filtro taula variables només variables implicades en el filtre i el genero 
    caracter_quartil<-"Q"
    maco<-variables %>% 
      dplyr::select(camp,criteris) %>% 
      filter(!str_detect(eval(parse(text=criteris)), caracter_quartil))
    
    ## Generar recodificació en base info
    maco_lista<-maco %>% base::split(list(.$camp))
    
    
    #8.5.2020#
    ##  Llegeix criteris_labels de variables 
    variables2 <- read_conductor(taulavariables,...) %>% dplyr::select(camp,!!criteris_labels)
    
    criteris_sym2<-rlang::sym(criteris_labels)
    variables2<-variables2 %>% dplyr::filter(!is.na(!!criteris_sym2))
    
    #8.5.2020#
    ##  Filtro taula variables només variables implicades en el filtre i el genero (criteris_labels)
    maco2<-variables2 %>% 
      dplyr::select(camp,criteris_labels) %>% 
      filter(!str_detect(eval(parse(text=criteris_labels)), caracter_quartil))
    
    #8.5.2020#
    ## Generar recodificació en base info
    maco_lista2<-maco2 %>% base::split(list(.$camp))
    
    
    #8.5.2020#
    num_recodes<-length(maco_lista)
    
    # Assignar a primer element (A partir d'aquí fer un for)
    
    for (i in 1:num_recodes) {
      
      #i<-1
      
      maco<-maco_lista[[i]]
      
      mamon<-stringr::str_split(maco[criteris],"/") %>% 
        unlist() %>% 
        as.numeric()
      
      mamon<-c(-Inf,mamon,Inf)
      
      
      #8.5.2020#
      maco2<-maco_lista2[[i]]
      mamon2<-stringr::str_split(maco2[criteris_labels],"/") %>% unlist()
      
      ##### Fer la recodificació en base el rang generat 
      nomcamp<-maco["camp"] %>% as.character()
      
      nomrecode<-paste0(nomcamp,".cat",length(mamon))
      
      #canvi!!#8.5.2020#
      if (!is.na(prefix)) {nomrecode<-paste0(nomcamp,".",prefix) }
      
      # Si la variables ja existeix la elimino i la sobrescric
      if (nomrecode%in%names(dt)) {dt<-dt %>% select_(paste0("-",nomrecode))}
      
      dt<-dt %>% mutate_(camp=nomcamp)
      
      #8.5.2020#
      
      if (mamon2=="NO"){dt<-dt %>% mutate(popes=cut(camp,breaks = mamon,...) %>% as.factor)}
      else if  (mamon2=="AUTO"){dt<-dt %>% mutate(popes=cut(camp,breaks = mamon,labels=FALSE,...) %>% as.factor)}
      else{
        
        if (length(mamon)!=length(mamon2)+1) {return(print(paste0("ERROR!!!,Algun dels talls dels Criteris del recode, no coincideixen amb els Criteris labels, de la variable  : ",nomcamp)))} 
        
        dt<-dt %>% mutate(popes=cut(camp,breaks = mamon,labels = mamon2,...) %>% as.factor) }
      
      # Si missings --> generar a una categoria missing
      if (missings==T) {dt<-missings_to_level(dt,"popes")}
      colnames(dt)[colnames(dt)=="popes"] <- nomrecode
      dt<-dt %>% dplyr::select(-camp)
      
      
      print(paste0("Generada: ",nomrecode))
      #-----------------#
      #
      # Validació :
      #
      #-----------------#
      
      #canvi!!**(6.5.2020)**(data.frame!!!)
      dt%>%group_by_at(vars(!!nomrecode))%>%summarise_at(vars(!!nomcamp),
                                                         list(min=~(min(.,na.rm=T)),max=~(max(.,na.rm=T)),freq=~n()))%>%
        ungroup()%>%
        as.data.frame()%>%print()
      
    }
    
  }
  
  
  # si no hi ha [criteris_labels], apliquem aquest if. (igual que Recode , antic!!!) 
  
  else  
    
  {
    
    
    
    {##  Llegeix criteris de variables 
      
      # variables <- readxl::read_excel(taulavariables) %>% tidyr::as_tibble() %>% dplyr::select(camp,!!criteris)
      variables <- read_conductor(taulavariables,...) %>% dplyr::select(camp,!!criteris) %>% mutate_all(as.character)
      criteris_sym<-rlang::sym(criteris)
      variables<-variables %>% dplyr::filter(!is.na(!!criteris_sym))   }
    
    #variables
    
    ##  0. Filtro taula variables només variables implicades en el filtre i el genero 
    caracter_quartil<-"Q"
    maco<-variables %>% 
      dplyr::select(camp,criteris) %>% 
      filter(!str_detect(eval(parse(text=criteris)), caracter_quartil))
    
    ## Generar recodificació en base info
    maco_lista<-maco %>% base::split(list(.$camp))
    
    #7.5.2020
    num_recodes<-length(maco_lista)
    
    
    for (i in 1:num_recodes) {
      
      #i<-1
      
      maco<-maco_lista[[i]]
      
      mamon<-stringr::str_split(maco[criteris],"/") %>% 
        unlist() %>% 
        as.numeric()
      
      mamon<-c(-Inf,mamon,Inf)
      
      ##### Fer la recodificació en base el rang generat 
      nomcamp<-maco["camp"] %>% as.character()
      
      nomrecode<-paste0(nomcamp,".cat",length(mamon))
      
      #canvi!!**(5.5.2020)**
      if (!is.na(prefix)) {nomrecode<-paste0(nomcamp,".",prefix) }
      
      # Si la variables ja existeix la elimino i la sobrescric
      if (nomrecode%in%names(dt)) {dt<-dt %>% select_(paste0("-",nomrecode))}
      
      dt<-dt %>% mutate_(camp=nomcamp)
      
      dt<-dt %>% mutate(popes=cut(camp,breaks = mamon,...) %>% as.factor)
      
      # Si missings --> generar a una categoria missing
      if (missings==T) {dt<-missings_to_level(dt,"popes")}
      
      colnames(dt)[colnames(dt)=="popes"] <- nomrecode
      
      dt<-dt %>% dplyr::select(-camp)
      
      
      print(paste0("Generada: ",nomrecode))
      
      #-----------------#
      #
      # Validació :
      #
      #-----------------#
      
      #canvi!!**(6.5.2020)**(data.frame!!!)
      dt%>%group_by_at(vars(!!nomrecode))%>%summarise_at(vars(!!nomcamp),
                                                         list(min=~(min(.,na.rm=T)),max=~(max(.,na.rm=T)),freq=~n()))%>%ungroup()%>%as.data.frame()%>%print()
      #-----------------#
    }
    
    
  }
  
  
  
  # la nostra funció té la sortida , la base de dades, amb les noves variables recodificades!
  
  dt 
  
  # fi de la funció!
}  

