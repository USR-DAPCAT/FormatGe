# Generar intervals de valors amb variables continues

# 1. Llegir variables d'ajust + variables que entren en joc (O tota la base de dades)
# 2. Categoritzar variables en grups (g quantils a escollir)
# 3. Assignar valor missing i factoritzar-ho tot 

# Opcio: es pot generar recodificacions predefinides
# Per defecte sobrescriu per shauria de poder generar variables noves

generar_intervals<-function(dt=dades,vars="ajust4",taulavariables=conductor,missing="Unkown",g=3) {
  
  # dt=dades
  # vars="ajust4"
  # taulavariables=conductor
  # missing="Unkown"
  # g=3
  
  # Actualitzar variables amb dades de quantis a categoritzar (Missings com a categoria)
  if (missing(vars)) {
    vars_fix<-names(dt)
    vars_fix_num<-dt %>% select_if(is.numeric) %>% names()
  } else {
    vars_fix<-extreure.variables("ajust4", taulavariables)
    vars_fix_num<-dt %>% select(vars_fix) %>% select_if(is.numeric) %>% names()
  }
  
  # Generar intervals en categoriques 
  dt_temp<-dt %>% 
    mutate_at(vars_fix_num, ~cut2(.,g=g))
  
  # # Falta opcio de recode en funcio de criteris manuals
  # dt %>% select(vars_fix_num) %>% recodificar2(taulavariables = conductor,criteris = "recode",prefix="popetes",missings = F) %>% 
  #   select(ends_with(".popetes")) %>% rename()
  
  # Reemplaçar missings si cal
  if (!missing(missing)) {
    dt_temp<- dt_temp %>% mutate_at(vars_fix,~replace(as.character(.), is.na(.), missing)) %>% 
      mutate_at(vars_fix,as.factor)}
  # Etiquetar
  dt_temp %>% etiquetar(taulavariables = taulavariables) 
  
}


#      FI GENERAR FUNCIONs  



