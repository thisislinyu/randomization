#' @param seed 设置随机数
#' @param trtseq 分组名称
#' @param covmat 分层因素矩阵
#' @param ratio 分组比例
#' @param nsample 总样本量
#' @param project_id 项目唯一标识
#' @param assigned 上一次已分组的患者数
#' @param unassigned 二次入组的患者数


stratified_f <- function(seed=123,
                         trtseq=NULL,
                         covmat =NULL,
                         ratio= NULL,
                         nsample=NULL,
                         project_id = NULL,
                         assigned = NULL,
                         unassigned = NULL,
                         deploy_plat="linux",
                         ...){

  library(rio)
  library(dplyr)
  library(devR)
  library(readxl)
  covmat <- as.data.frame(covmat)
  file_path <- '/file/'
  # file_path <- 'E://random_system//random//'
  file_name <- paste0(file_path,project_id,'.xlsx')
  if(!file.exists(file_name)){
    export(covmat,file_name)
  }else{
    data = read_xlsx(file_name)
    covmat = rbind(data,covmat)
    export(covmat,file_name)
  }
  #library(blockrand)
  set.seed(seed)

  ##### total number of strata
  covmat_tmp <- covmat %>% unique()

  ########used to match the
  covmat_tmp1 <- covmat %>% group_by_all(.) %>% mutate(id = row_number())

  final_holder <- NULL

  ### the total number in each strata (equals to or slightly larger than nsample)
  group_sample = ceiling(nsample %>% as.numeric()/sum(ratio))*ratio

  ####
  for(j in 1:nrow(covmat_tmp)){
    assign_tmp <- covmat_tmp[rep(j,each=sum(group_sample)),] %>% data.frame()
    colnames(assign_tmp) <- colnames(covmat_tmp)


    assign_tmp$id <- c(1:sum(group_sample))

    refluction = rnorm(1,100+j,100)
    #这一层进行一个简单随机
    simple_out <-  simple_random_f(seed =seed+refluction,
                                   trtseq=trtseq,
                                   group_sample=group_sample) %>%
      arrange(.,patient_id)

    assign_tmp$assignment = simple_out$assignment
    #%>%
    # mutate(unique_id=paste0(as.vector(temp_res$.)[j],patient_id))
    final_holder <- rbind(final_holder,assign_tmp)
  }

  final_tmp <- left_join(covmat_tmp1,final_holder)

  if((assigned+unassigned)<nsample){
    final <- final_tmp[(assigned+1):(assigned+unassigned),]
  }else{
    final <- final_tmp[(assigned+1):nsample,]
  }
  return(final)
}
