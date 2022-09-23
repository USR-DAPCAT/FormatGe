# Genera dummis (0/1) a partir d'una variable del data frame   -----------------
# Retorna la variable 

make_dummies <- function(dt,variable, prefix = '') {
  
  # dt<-dades
  # variable<-"grup"
  # prefix<-"grup_"
  
  v<-dt %>% dplyr::pull(variable)
  s <- sort(unique(v))
  d <- outer(v, s, function(v, s) 1L * (v == s))
  colnames(d) <- paste0(prefix, s)
  d<-d %>% as_tibble()
  
  dt<-cbind(dt,d)
}
