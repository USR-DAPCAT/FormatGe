#' @title                 Canviar i definir categoria de referencia 
#' @description           Canviar i definir categoria de referencia en un llistat de variables posades en un conductor. 
#' @param DF              Base de dades
#' @param conductor       conductor
#' @param ref             categoria de referencia
#' @param ...             altres parametres
#' @return                Etiquetar
#' @export                refcat
#' @importFrom            dplyr "%>%"
refcat<-function(DF="iris",conductor="conductor_iris",ref="ref_cat",...){
  # DF=dades_long_total
  # conductor=conductor_matlab
  # ref="ref_cat"
  
  #https://search.r-project.org/CRAN/refmans/forcats/html/fct_relevel.html
  
  #exclusio=c("","","","","","","","","","","","","","",">30")
  #exc_ordre=c("","","","","","","","","","","","","","","1")
  #exc_pre=c("","","","","","","","","","","","","","","")
  #ref_cat=c("","","H","","","","","","","","","","","","")
  #conductor2<-data.frame(conductor1,exclusio,exc_ordre,exc_pre,ref_cat)
  #exclusio1<-c("exclusio1","exclusio1","exclusio1","","","","","","","","","","","1","")
  #conductor2<-rbind(conductor2,exclusio1)
  #
  #
  #DF=Taula_plana
  #conductor=conductor2
  #ref="ref_cat"
  
  
  # Create a factor with the wrong order of levels
  #sizes <- factor(c("small", "large", "large", "small", "medium"))
  #sizes
  #> [1] small  large  large  small  medium
  #> Levels: large medium small
  
  #sizes <- factor(sizes, levels=rev(levels(sizes)))
  #sizes
  #> [1] small  large  large  small  medium
  #> Levels: small medium large
  
  
  
  
    
    ref=rlang::sym(ref)
    # llegeixo conductor informacio de refcats
    
    #conductor_df<-read_conductor(conductor,...) %>%dplyr:: select(camp,!!ref) %>% dplyr::filter(!!ref!="") 
    conductor_df<-read_conductor(conductor) %>%dplyr:: select(camp,!!ref) %>% dplyr::filter(!!ref!="") 
    
    llista_vars<-conductor_df$camp %>% as.character()
    
    # Factoritzar variables i verificar nivells
    DF<-DF %>% dplyr::mutate_at(llista_vars,as.factor)
    
    # Capturar nivells reals com una llista
    nivells_reals<-DF %>% dplyr::select(llista_vars) %>%purrr:: map(~levels(.x))
    
    # Bucle per eliminar nivells no existents
    for (i in 1:length(nivells_reals)) {
      # i<-3
      var<-conductor_df[["camp"]][i]
      
      if (!(conductor_df %>% dplyr::select(!!ref) %>% dplyr::slice(i) %>% as.character()) %in% nivells_reals[[i]]) {
        warning(paste0("Nivell erroni en variable: ",var))
        conductor_df<-conductor_df %>%dplyr:: mutate(!!ref:=dplyr::if_else(camp==var,"",!!ref))
      }
    }
    
    # Torno a filtrar variables sense nivells
    conductor_df<-conductor_df %>% dplyr::filter(!!ref!="") 
    
    # Genero llista de vars
    llista_vars<-conductor_df$camp %>% as.character()
    llista_refcat<-conductor_df %>% dplyr::pull(!!ref) 
    
    # Faig el relevel a les comlumnes seleccionades
    pp<-purrr::map2_df(DF %>% dplyr::select(llista_vars),  llista_refcat, ~ (stats::relevel(.x, .y)),sort)
    
    # Ara intercanviar columnes 2 a 2 de
    DF[llista_vars]<-pp[llista_vars]
    DF
    
    #levels(DF$sexe)
    # ara falta ordenar-ho ....ex::conductor_HTG [HTG_Cohort]
    
    #fct_relevel(f, sort)
  
}


