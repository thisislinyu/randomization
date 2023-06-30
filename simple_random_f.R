#' @param seed 设置随机数
#' @param trtseq 分组
#' @param group_sample 每组样本量


simple_random_f <- function(seed = 123,
                            trtseq=NULL,
                            group_sample=NULL,
                            ...){
  library(dplyr)

  set.seed(seed)


  temp <- data.frame(patient_id = c(1:sum(group_sample)),
                     random_num <- runif(sum(group_sample),0,1))

  if(length(unique(group_sample))==1){
    assignment <- mapply(rep,trtseq,group_sample) %>% as.vector()
  }else{
    assignment <- mapply(rep,trtseq,group_sample) %>% unlist()
  }

  final_res <- cbind(temp[order(temp$random_num),],assignment)


  colnames(final_res) <- c("patient_id","random_num","assignment")

  return(final_res)
}






