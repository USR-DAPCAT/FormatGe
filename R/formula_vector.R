#  formula_vector(vector,y, vector caracter a elimina) ##########

formula_vector<-function(vector=c("sex","age","age"),y="y",logit=F,eliminar=NA){
  
  vector<-vector [!vector %in% eliminar] %>% unique()
  
  if (!logit) {formula=as.formula(paste(y, paste(vector, collapse=" + "), sep=" ~ "))}
  if (logit) {formula=paste0("as.factor(",y,")~ ", paste(vector, collapse=" + ")) %>% as.formula()}           
  
  formula
  
}

