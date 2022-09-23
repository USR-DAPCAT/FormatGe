## Funcio per canviar noms de variables del data frame

## Entra data frame i cambia nom de variable

canvi_noms_variables<-function(dt=dt_temp,variable="ajust2", nou_nom="descripcio",conductor=conductor_variables,...) {
  
  # dt=dades_TAI
  # variable="camp"
  # nou_nom = "camp2"
  # conductor = conductor_variables
  # sheet="rename_SG"
  
  # dt=dt_temp
  # variable="ajust2"
  # conductor=conductor_variables  
  # etiqueta="descripcio"
  
  # extreure variables
  vars<-extreure.variables(variable,conductor,...)
  
  # Extreure etiquetes 
  etiquetes_noves<-extreure.variables(variable,conductor,variable_camp=nou_nom,...)
  
  # Canviar noms per etiqueta descripcio
  # setnames(dt, old = vars, new = etiquetes_noves)
  dt<-dt %>% rename_at(vars(vars), ~ etiquetes_noves)
  
}

# Retorna taula plana a partir d'un fitxer amb la data index, el fitxer del cataleg i el fitxer de parametres
# Els parametres contenen els seguens camps: 
#     fitxer (nom) ,
#     domini (refereix al tipus d'agregació i codis del cataleg
#     Finestra1 Finestra2 (Periode)
#     camp (Quin camp s'utilitza com agregació)
#     prefix (o suffix de les variables generades)
#     funcio (Funció d'agregacio en variables )
