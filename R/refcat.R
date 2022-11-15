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
  
  
    
    ref=rlang::sym(ref)
    # llegeixo conductor informacio de refcats
    conductor_df<-read_conductor(conductor,...) %>%dplyr:: select(camp,!!ref) %>% dplyr::filter(!!ref!="") 
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
    pp<-purrr::map2_df(DF %>% dplyr::select(llista_vars),  llista_refcat, ~stats::relevel(.x, .y))
    
    # Ara intercanviar columnes 2 a 2 de
    DF[llista_vars]<-pp[llista_vars]
    DF
    
    # ara falta ordenar-ho ....
    
  
}


