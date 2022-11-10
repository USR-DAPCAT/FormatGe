#' @title                       Recodificar les variables que escollim
#' @description                 Recodificar les variabñes que escollim a partir del parametre "criteris"
#' @param dt                    Base de dades
#' @param taulavariables        Conductor
#' @param criteris              Criteris ex:: 1/3/5
#' @param missings              Missing
#' @param prefix                Posar un prefix a la nova variable
#' @param ...                   Altres funcions
#' @return                      Retorna les dades amb la recodificacio 
#' @export                      recodificar
#' @importFrom                  dplyr "%>%"
#' @examples
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
    
    # if (!is.na(prefix)) {nomrecode<-paste0(nomcamp,".cat",prefix,length(mamon)) }
    #canvi!!#10.11.2022#
    if (!is.na(prefix)) {nomrecode<-paste0(nomcamp,prefix) }
    
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






#' @title                       Recodificar missings
#' @description                 Recodificar rangs de valors que cauen fora de interval es transforman en missings
#' @param dt                    Base de dades
#' @param taulavariables        Conductor
#' @param rang                  Rang
#' @param data_long             Data Long
#' @param ...                   Altres funcions
#' @return                      Retorna dades amb la recodificacio de missings
#' @export                      recode_to_missings
#' @importFrom                  dplyr "%>%"
#' @examples
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


#' @title                    Comptar valors
#' @description              Comptar un valor concret d un conjunt de variables de la nostra B.D
#' @param dt                 Base de dades
#' @param variables          Variables
#' @param valor              Valor
#' @return                   Retorna valors
#' @export                   comptar_valors
#' @importFrom               dplyr "%>%"
#' @examples
#' Comptar_D<-comptar_valors(dt_plana,variables=c("DG.HTA","DG.IC","GLICADA.valor"),valor="NA")
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
#' k1<-missings_to_level(dt_plana,variable="DG.HTA")
#' k1
missings_to_level<-function(dades,variable="popes") {
  
  #dades=dt_plana
  #variable="DG.HTA"
  
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


#' @title                       Generar intervals amb talls a variables continues
#' @description                 Generar intervals a partir d'un conductor i un tall definit
#' @param dt                    Base de dades
#' @param vars                  Variables
#' @param taulavariables        Conductor
#' @param missing               Missing 
#' @param g                     Numero de talls
#' @return                      Retorna intervals
#' @export                      generar_intervals
#' @importFrom                  dplyr "%>%"
#' @examples

#'
#'kk<-generar_intervals(dt=dt_plana,
#'vars="ajust4",
#'taulavariables=conductor1,
#'missing="Unkown",
#'g=2)
#'
#'kk
generar_intervals<-function(dt="dades",
                            vars="ajust4",
                            taulavariables="conductor1",
                            missing="Unkown",
                            g=3) {
  
  # dt=dades
  # vars="ajust4"
  # taulavariables=conductor
  # missing="Unkown"
  # g=3
  
  #vars_sym<-dplyr::sym(vars)
  #!!camp_eval
  #vars=!!vars
  
  
  # Actualitzar variables amb dades de quantis a categoritzar (Missings com a categoria)
  if (missing(vars)) {
    vars_fix<-names(dt)
    vars_fix_num<-dt %>% dplyr::select_if(is.numeric) %>% names()
  } else {
    vars_fix<-extreure.variables(vars, taulavariables)
    vars_fix_num<-dt %>% dplyr::select(vars_fix) %>% dplyr::select_if(is.numeric) %>% names()
  }
  
  ###############################################################################
  # Actualitzar variables amb dades de quantis a categoritzar (Missings com a categoria)
  #if (missing(vars)) {
  #  vars_fix<-names(dt)
  #  vars_fix_num<-dt %>% dplyr::select_if(is.numeric) %>% names()
  #} else {
  #  vars_fix<-extreure.variables("ajust4", taulavariables)
  #  vars_fix_num<-dt %>% dplyr::select(vars_fix) %>% dplyr::select_if(is.numeric) %>% names()
  #}
  ###############################################################################
  #
  #
  # Generar intervals en categoriques 
  dt_temp<-dt %>% 
    dplyr::mutate_at(vars_fix_num, ~Hmisc::cut2(.,g=g))
  #
  # Reemplaçar missings si cal
  if (!missing(missing)) {
    dt_temp<- dt_temp %>% dplyr::mutate_at(vars_fix,~replace(as.character(.), is.na(.), missing)) %>% 
      dplyr::mutate_at(vars_fix,as.factor)}
  #
  # Etiquetar
  dt_temp %>% etiquetar(taulavariables = taulavariables) 
  
}






