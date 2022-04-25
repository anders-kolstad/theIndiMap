#' Run topic model for 5000 records each iteration
#'
#' @param path Path to RIS files
#' @param N The number of total RIS files 
#' @return saved list of topic models (RDS)

run_topics<-function(path, N){
  tpms=list()
  
iters=seq(1,N,5)
  
for(i in iters){
  file_names <- paste0(path, list.files(path = path))
  data_all <- revtools::read_bibliography(file_names[i:i+4])
  dtm=revtools::make_dtm(data_all)
  tpms[i]=revtools::run_topic_model(dtm)
}

saveRDS(tpms,paste0(here::here(),"/data/tpms.rds"))
}
###############################################################
path<-paste0(here::here(), "/data/RIS_Files/")
run_topics(path = path, N=40)
