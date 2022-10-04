#' @title                       Recodificar les variables que escollim
#' @description                 Recodificar en funció d'un Conductor
#' @param dt                    Base de dades
#' @param taulavariables        Conductor
#' @param criteris              Criteris
#' @param missings              Missing
#' @param prefix                Prefix
#' @param ...                   Altres funcions
#' @return                      Retorna les dades amb la recodificacio 
#' @export                      recodificar
#' @importFrom                  dplyr "%>%"
#' @examples
#'camp=c("idp",
#'       "dtindex",
#'       "sexe",
#'       "dnaix",
#'       "situacio",
#'       "entrada",
#'       "sortida", 
#'       "INCLUSIO.DM2",
#'       "DG.HTA",
#'       "DG.IC",
#'       "cHDL.valor",
#'       "cLDL.valor",
#'       "cT.valor",
#'       "GLICADA.valor",
#'       "IMC.valor")
#'descripcio=c("Identificacio Pacient",
#'             "data Index",
#'             "Sexe",
#'             "data Naixament",
#'             "Situacio",
#'             "Entrada",
#'             "Sortida",
#'             "Inclusio Diabetes Tipus 2",
#'             "Hipertensió arterial",
#'             "Insuficiencia Cardiaca",
#'             "Colesterol HDL(mg/dL)",
#'             "Colesterol LDL(mg/dL)",
#'             "Colesterol Total(mg/dL)",
#'             "HbA1c",
#'             "IMC" )
#'descripcio2=c("Identificacion Paciente",
#'              "data Indice",
#'              "Sexo",
#'              "data Naicimiento",
#'              "Situacion",
#'              "Entrada",
#'              "Salida",
#'              "Inclusion Diabetes Tipus 2",
#'              "Hipertensión arterial",
#'              "Insuficiencia Cardiaca",
#'              "Colesterol HDL(mg/dL)",
#'              "Colesterol LDL(mg/dL)",
#'              "Colesterol Total(mg/dL)",
#'              "HbA1c",
#'              "IMC" )
#'factor=c("","","","","","","",1,1,1,"","","","","")
#'dates=c("",1,"",1,"",1,1,"","","","","","","","")
#'recode=c("","","","","","","","","","","","","","7.0","")
#'conductor1<-data.frame(camp,descripcio,descripcio2,factor,dates,recode)
#'k<-recodificar(dt=dt_plana, 
#'               taulavariables=conductor1,
#'               criteris="recode",
#'               missings=FALSE,
#'               prefix="_recode_")
#'               
#'k
recodificar<-function(dt="dades",
                      taulavariables="VARIABLES.xls",
                      criteris="recode1",
                      missings=F, 
                      prefix=NA,...){
  
  # dt=iris
  # taulavariables = etiquetes_iris
  # criteris = "recode"
  # missings=F
  # prefix=NA
  
  ##  Llegeix criteris de variables 
  variables<-read_conductor(taulavariables,...) %>% dplyr::select(camp,!!criteris) %>% dplyr::mutate_all(as.character)
  criteris_sym<-rlang::sym(criteris)
  variables<-variables %>% dplyr::filter(!is.na(!!criteris_sym) & !!criteris_sym!="")
  
  ##  0. Filtro taula variables només variables implicades en el filtre i el genero 
  caracter_quartil<-"Q"
  
  maco<-variables %>% 
    dplyr::select(camp,criteris) %>% 
    dplyr::filter(!stringr::str_detect(eval(parse(text=criteris)), caracter_quartil))
  
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
    if (nomrecode%in%names(dt)) {dt<-dt %>%dplyr:: select_(paste0("-",nomrecode))}
    
    dt<-dt %>% dplyr::mutate_(camp=nomcamp)
    dt<-dt %>% dplyr::mutate(popes=cut(camp,breaks = mamon) %>% as.factor)
    
    # Si missings --> generar a una categoria missing
    if (missings==T) {dt<-missings_to_level(dt,"popes")}
    
    colnames(dt)[colnames(dt)=="popes"] <- nomrecode
    dt<-dt %>% dplyr::select(-camp)
    
    print(paste0("Generada: ",nomrecode))
    # Validació
    dt %>%dplyr:: group_by_at(dplyr::vars(!!nomrecode)) %>% 
      dplyr::summarise_at(
        dplyr::vars(!!nomcamp),list(min=~min(.,na.rm=T),max=~max(.,na.rm=T),freq=~dplyr::n())) %>%
           dplyr:: ungroup() %>% 
              print()
  }
  
  dt
}




#' @title                       Recodificar les variables que escollim
#' @description                 Recodifico en funció d'un Conductor
#' @param dt                    Base de dades
#' @param taulavariables        Conductor
#' @param criteris              Criteris
#' @param missings              Missing
#' @param prefix                Prefix
#' @param criteris_labels       Criteris labels
#' @param ...                   Altres funcions
#' @return                      Retorna les dades amb la recodificacio
#' @export                      recodificar2
#' @importFrom                  dplyr "%>%"
#' @examples
#'camp=c("idp",
#'       "dtindex",
#'       "sexe",
#'       "dnaix",
#'       "situacio",
#'       "entrada",
#'       "sortida", 
#'       "INCLUSIO.DM2",
#'       "DG.HTA",
#'       "DG.IC",
#'       "cHDL.valor",
#'       "cLDL.valor",
#'       "cT.valor",
#'       "GLICADA.valor",
#'       "IMC.valor")
#'descripcio=c("Identificacio Pacient",
#'             "data Index",
#'             "Sexe",
#'             "data Naixament",
#'             "Situacio",
#'             "Entrada",
#'             "Sortida",
#'             "Inclusio Diabetes Tipus 2",
#'             "Hipertensió arterial",
#'             "Insuficiencia Cardiaca",
#'             "Colesterol HDL(mg/dL)",
#'             "Colesterol LDL(mg/dL)",
#'             "Colesterol Total(mg/dL)",
#'             "HbA1c",
#'             "IMC" )
#'descripcio2=c("Identificacion Paciente",
#'              "data Indice",
#'              "Sexo",
#'              "data Naicimiento",
#'              "Situacion",
#'              "Entrada",
#'              "Salida",
#'              "Inclusion Diabetes Tipus 2",
#'              "Hipertensión arterial",
#'              "Insuficiencia Cardiaca",
#'              "Colesterol HDL(mg/dL)",
#'              "Colesterol LDL(mg/dL)",
#'              "Colesterol Total(mg/dL)",
#'              "HbA1c",
#'              "IMC" )
#'factor=c("","","","","","","",1,1,1,"","","","","")
#'dates=c("",1,"",1,"",1,1,"","","","","","","","")
#'recode=c("","","","","","","","","","","","","","7.0","")
#'conductor1<-data.frame(camp,descripcio,descripcio2,factor,dates,recode)
#'k2<-recodificar(dt=dt_plana, 
#'               taulavariables=conductor1,
#'               criteris="recode",
#'               missings=FALSE,
#'               prefix="_recode_")
#'               
#'k2
recodificar2<-function(dt="dt_plana",
                       taulavariables ="conductor",
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
    
    variables<-read_conductor(taulavariables,...) %>% dplyr::select(camp,!!criteris) %>%dplyr:: mutate_all(as.character)
    criteris_sym<-rlang::sym(criteris)
    variables<-variables %>% dplyr::filter(!is.na(!!criteris_sym))
    
    #variables
    
    ##  0. Filtro taula variables només variables implicades en el filtre i el genero 
    caracter_quartil<-"Q"
    maco<-variables %>% 
      dplyr::select(camp,criteris) %>% 
      dplyr::filter(!stringr::str_detect(eval(parse(text=criteris)), caracter_quartil))
    
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
      dplyr::filter(!stringr::str_detect(eval(parse(text=criteris_labels)), caracter_quartil))
    
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
      if (nomrecode%in%names(dt)) {dt<-dt %>% dplyr::select_(paste0("-",nomrecode))}
      
      dt<-dt %>% dplyr::mutate_(camp=nomcamp)
      
      #8.5.2020#
      
      if (mamon2=="NO"){dt<-dt %>% dplyr::mutate(popes=cut(camp,breaks = mamon,...) %>% as.factor)}
      else if  (mamon2=="AUTO"){dt<-dt %>% dplyr::mutate(popes=cut(camp,breaks = mamon,labels=FALSE,...) %>% as.factor)}
      else{
        
        if (length(mamon)!=length(mamon2)+1) {return(print(paste0("ERROR!!!,Algun dels talls dels Criteris del recode, no coincideixen amb els Criteris labels, de la variable  : ",nomcamp)))} 
        
        dt<-dt %>% dplyr::mutate(popes=cut(camp,breaks = mamon,labels = mamon2,...) %>% as.factor) }
      
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
      dt%>%dplyr::group_by_at( dplyr::vars(!!nomrecode))%>%dplyr::summarise_at( dplyr::vars(!!nomcamp),
                                                         list(min=~(min(.,na.rm=T)),max=~(max(.,na.rm=T)),freq=~n()))%>%
        dplyr::ungroup()%>%
        as.data.frame()%>%print()
      
    }
    
  }
  
  
  # si no hi ha [criteris_labels], apliquem aquest if. (igual que Recode , antic!!!) 
  
  else  
    
  {
    
    
    
    {##  Llegeix criteris de variables 
      
      # variables <- readxl::read_excel(taulavariables) %>% tidyr::as_tibble() %>% dplyr::select(camp,!!criteris)
      variables <- read_conductor(taulavariables,...) %>% dplyr::select(camp,!!criteris) %>%dplyr:: mutate_all(as.character)
      criteris_sym<-rlang::sym(criteris)
      variables<-variables %>% dplyr::filter(!is.na(!!criteris_sym))   }
    
    #variables
    
    ##  0. Filtro taula variables només variables implicades en el filtre i el genero 
    caracter_quartil<-"Q"
    maco<-variables %>% 
      dplyr::select(camp,criteris) %>% 
      dplyr::filter(!stringr::str_detect(eval(parse(text=criteris)), caracter_quartil))
    
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
      if (nomrecode%in%names(dt)) {dt<-dt %>% dplyr::select_(paste0("-",nomrecode))}
      
      dt<-dt %>% dplyr::mutate_(camp=nomcamp)
      
      dt<-dt %>% dplyr::mutate(popes=cut(camp,breaks = mamon,...) %>% as.factor)
      
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
      dt%>%dplyr::group_by_at( dplyr::vars(!!nomrecode))%>%dplyr::summarise_at( dplyr::vars(!!nomcamp),
                                                         list(min=~(min(.,na.rm=T)),max=~(max(.,na.rm=T)),freq=~n()))%>%dplyr::ungroup()%>%as.data.frame()%>%print()
      #-----------------#
    }
    
    
  }
  
  
  
  # la nostra funció té la sortida , la base de dades, amb les noves variables recodificades!
  
  dt 
  
  # fi de la funció!
}  





#' @title                       Recodificar missings
#' @description                 Recodificar rangs de valors que cauen fora interva a missings
#' @param dt                    Base de dades
#' @param taulavariables        Conductor
#' @param rang                  Rang
#' @param data_long             Data
#' @param ...                   Altres funcions
#' @return                      Retorna dades amb la recodificacio 
#' @export                      recode_to_missings
#' @importFrom                  dplyr "%>%"
#' @examples
#'camp=c("idp",
#'       "dtindex",
#'       "sexe",
#'       "dnaix",
#'       "situacio",
#'       "entrada",
#'       "sortida", 
#'       "INCLUSIO.DM2",
#'       "DG.HTA",
#'       "DG.IC",
#'       "cHDL.valor",
#'       "cLDL.valor",
#'       "cT.valor",
#'       "GLICADA.valor",
#'       "IMC.valor")
#'descripcio=c("Identificacio Pacient",
#'             "data Index",
#'             "Sexe",
#'             "data Naixament",
#'             "Situacio",
#'             "Entrada",
#'             "Sortida",
#'             "Inclusio Diabetes Tipus 2",
#'             "Hipertensió arterial",
#'             "Insuficiencia Cardiaca",
#'             "Colesterol HDL(mg/dL)",
#'             "Colesterol LDL(mg/dL)",
#'             "Colesterol Total(mg/dL)",
#'             "HbA1c",
#'             "IMC" )
#'descripcio2=c("Identificacion Paciente",
#'              "data Indice",
#'              "Sexo",
#'              "data Naicimiento",
#'              "Situacion",
#'              "Entrada",
#'              "Salida",
#'              "Inclusion Diabetes Tipus 2",
#'              "Hipertensión arterial",
#'              "Insuficiencia Cardiaca",
#'              "Colesterol HDL(mg/dL)",
#'              "Colesterol LDL(mg/dL)",
#'              "Colesterol Total(mg/dL)",
#'              "HbA1c",
#'              "IMC" )
#'factor= c("","","","","","","",1,1,1,"","","","","")
#'dates=  c("",1,"",1,"",1,1,"","","","","","","","")
#'recode=c("","","","","","","","","","","","","","7.0","")
#'rang_valid=   c("","","","","","","","","","","","","","","15-100")
#'
#'conductor1<-data.frame(camp,
#'descripcio,
#'descripcio2,
#'factor,
#'dates,
#'recode,
#'rang_valid)
#'
#'conductor1
#'dt_plana
#'kk<-recode_to_missings(dt=dt_plana,taulavariables=conductor1,rang="rang_valid")
#'kk
recode_to_missings<-function(dt="dades",
                             taulavariables="conductor_variables",
                             rang="rang_valid", 
                             data_long=FALSE,...) {
  
  
  # dt=dt
  # taulavariables=conductor_variables
  # rang="rang_valid"
  # data_long=F
  
  # Llegir dades
  
  variables<-read_conductor(taulavariables,col_types = "text",...) %>%tibble::as_tibble()
  #variables<-read_conductor(taulavariables,...) %>%tibble::as_tibble()
  
  temp<-variables %>% dplyr::select(c("camp","rang_valid")) %>% dplyr::filter(!is.na(rang_valid))
  
  # Elimino () i [ ]
  temp <- temp %>% dplyr::mutate(rang_valid=stringr::str_replace_all(rang_valid,"\\(",""),
                          rang_valid=stringr::str_replace_all(rang_valid,"\\)",""),
                          rang_valid=stringr::str_replace_all(rang_valid,"\\[",""),
                          rang_valid=stringr::str_replace_all(rang_valid,"\\]","")
  )
  
  # Separo limit inferior i limit superior
  rangs<-temp$rang_valid %>% stringr::str_split_fixed("-",2) %>%tibble::as_tibble()
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
      dt<-dt %>% dplyr::mutate_at(camp,~ifelse(.<linf | .>lsup,NA_real_,.))
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
        dplyr::mutate(valor=ifelse((valor<linf | valor>lsup) & cod==camp ,NA,valor)) %>% 
        dplyr::filter(!is.na(valor))
    }
  }
  
  dt
  
}



#' @title                 Genera dummis
#' @description           Recodificar. Genera dummis (0/1) a partir d'una variable del data frame
#' @param dt              Base de dades
#' @param variable        Variables
#' @param prefix          Prefix
#' @return                Retorna dummis
#' @export                make_dummies
#' @importFrom            dplyr "%>%"
#' @examples
#' 
#' sexe_dummis<-make_dummies(dt_plana,"sexe",prefix="pref.")
#' sexe_dummis
#' 
make_dummies <- function(dt,variable, prefix=" ") {
  
  # dt<-dades
  # variable<-"grup"
  # prefix<-"grup_"
  
  v<-dt %>% dplyr::pull(variable)
  s <- sort(unique(v))
  d <- outer(v, s, function(v, s) 1L * (v == s))
  colnames(d) <- paste0(prefix, s)
  d<-d %>% tibble::as_tibble()
  
  dt<-cbind(dt,d)
}


#' @title                    Comptar_valors
#' @description              Recodificar.Comptar valors
#' @param dt                 Base de dades
#' @param variables          Variables
#' @param valor              Valor
#' @return                   Retorna valors
#' @export                   comptar_valors
#' @importFrom               dplyr "%>%"
#' @examples
#' Comptar_D<-comptar_valors(dt_plana,variables=c("sexe"),valor="D")
#' Comptar_D
comptar_valors<-function(dt="dadesevents",variables=c("EV.TER.ARTER_PERIF","EV.TER.AVC"),valor="Yes"){
  
  # dt=dades
  # variables=c("EV1_ULCERES", "EV2_ULCERES", "EV3_ULCERES", "EV4_ULCERES")
  # valor="Yes"
  
  # Concateno valors
  pepito<-paste0("paste0(",paste0(variables,collapse = ","),")")
  
  dt<-dt %>% 
    dplyr::mutate_("combi_vars"=pepito) %>% 
    dplyr::mutate(
      num_valors=stringr::str_count(combi_vars,valor)) %>% 
    dplyr::select(-combi_vars)
  
}


#' @title                      Missings_to_level
#' @description                Recodificar  missings_to_level
#' @param dades                Base de dades
#' @param variable             Variables
#' @return                     Retorna missings
#' @export                     missings_to_level
#' @importFrom                 dplyr "%>%"
#' @examples
#' missings_to_level(dt_plana,variable="DG.HTA")
missings_to_level<-function(dades,variable="popes") {
  
  # dades=temp
  # variable="val_CKDEPI.cat5"
  
  # Subset columnes de d
  d_temp<-dades %>% dplyr::select_("temp"=variable)
  
  levels_nous <- levels(d_temp$temp)
  levels_nous[length(levels_nous) + 1] <- "None"
  
  d_temp$temp<-factor(d_temp$temp,levels = levels_nous)
  d_temp$temp[is.na(d_temp$temp)]<-"None"
  #
  dades <- dades %>% dplyr::select_(paste0("-",variable))
  # Canviar el nom al origen 
  names(d_temp)[names(d_temp) == "temp"] <- variable
  dades <-cbind(dades,d_temp)
  
}

# 


#' @title                       Generar intervals
#' @description                 Recodificar Generar intervals de valors amb variables continues
#' @param dt                    xxx
#' @param vars                  xxx
#' @param taulavariables        xxx 
#' @param missing               xxx 
#' @param g                     xxx 
#' @return                      Retorna intervals
#' @export                      generar_intervals
#' @importFrom                  dplyr "%>%"
#' @examples
#' 
generar_intervals<-function(dt="dades",vars="ajust4",taulavariables="conductor",missing="Unkown",g=3) {
  
  # dt=dades
  # vars="ajust4"
  # taulavariables=conductor
  # missing="Unkown"
  # g=3
  
  # Actualitzar variables amb dades de quantis a categoritzar (Missings com a categoria)
  if (missing(vars)) {
    vars_fix<-names(dt)
    vars_fix_num<-dt %>% dplyr::select_if(is.numeric) %>% names()
  } else {
    vars_fix<-extreure.variables("ajust4", taulavariables)
    vars_fix_num<-dt %>% dplyr::select(vars_fix) %>% dplyr::select_if(is.numeric) %>% names()
  }
  
  # Generar intervals en categoriques 
  dt_temp<-dt %>% 
    dplyr::mutate_at(vars_fix_num, ~cut2(.,g=g))
  
  # # Falta opcio de recode en funcio de criteris manuals
  # dt %>% dplyr::select(vars_fix_num) %>% recodificar2(taulavariables = conductor,criteris = "recode",prefix="popetes",missings = F) %>% 
  #   dplyr::select(ends_with(".popetes")) %>% rename()
  
  # Reemplaçar missings si cal
  if (!missing(missing)) {
    dt_temp<- dt_temp %>% dplyr::mutate_at(vars_fix,~replace(as.character(.), is.na(.), missing)) %>% 
      dplyr::mutate_at(vars_fix,as.factor)}
  # Etiquetar
  dt_temp %>% etiquetar(taulavariables = taulavariables) 
  
}






