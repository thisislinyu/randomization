#' @param seed 设置随机数
#' @param trtseq 分组名称
#' @param covmat 分层因素矩阵
#' @param block_size 区组长度
#' @param ratio 分组比例
#' @param nsample 总样本量
#' @param project_id 项目唯一标识
#' @param assigned 上一次已分组的患者数
#' @param unassigned 二次入组的患者数
#'

block_random_old_f <- function(seed = 123,
                               trtseq = NULL,
                               ratio = NULL,
                               nsample = NULL,
                               block_size = NULL,
                               deploy_plat="linux",
                               ...) {
  set.seed(seed)
  library(dplyr) # version: v1.0.7


  #nsample = nsample+ceiling(nsample*percent_float)

  min_block = sum(as.numeric(ratio))
  final_res <- NULL

  if (is.null(block_size)) {

    possible_block_size <-sample(c(min_block,min_block*2,min_block*3),
                                 nsample,replace = TRUE, prob = c(0.4, 0.4,0.2))

    block_length = sum(cumsum(possible_block_size)<nsample)+1
    block_size_val = possible_block_size[c(1:block_length)]

    block_before = c(1,cumsum(block_size_val)+1)

    block_after = cumsum(block_size_val)

    for(i in c(1:length(block_size_val))){
      refluction = rnorm(1,100+i,100)
      group_sample = ratio*(block_size_val[i]/sum(ratio))
      simple_out <- simple_random_f(seed = seed+refluction,
                                    trtseq=trtseq,
                                    group_sample = group_sample) %>%
        arrange(., patient_id) %>%
        mutate(patient_id = seq(block_before[i],block_after[i],1)) %>% select(-random_num)
      final_res <- rbind(final_res,simple_out)
    }
  }else{
    if (block_size %% sum(ratio) != 0) {
      #errormessage <-linux_or_win(deploy_plat,'{"ErrorMessage":"区组长度需要是研究分组数的倍数，请重新定义区组长度。"}')
      errormessage <-linux_or_win(deploy_plat,
                                  paste0('[{"ErrorMessage":"区组长度需要是分组比例之和的倍数，请重新定义区组长度。"}]')
      )
      if (length(grep('ErrorMessage',errormessage))==1){
        return(errormessage)
      }
      #print(paste0("区组长度需要是研究分组数（N=", length(trtseq), "", "）的倍数，请重新定义区组长度。"))
    }

    group_sample = ratio*(block_size/sum(ratio))

    for(i in seq(1,nsample,block_size)){
      refluction = rnorm(1,100+i,100)
      #patient_id_new = seq(i,i+block_size-1,1)
      simple_out <- simple_random_f(seed = seed+refluction,
                                    trtseq=trtseq,
                                    group_sample = group_sample) %>%
        arrange(., patient_id) %>%
        mutate(patient_id = seq(i,i+block_size-1,1)) %>% select(-random_num)

      final_res <- rbind(final_res,simple_out)
    }
  }
  final_res <- final_res[1:nsample,]
  return(final_res)
}



stratified_blocked_f <- function(seed=123,
                                 trtseq=NULL,
                                 covmat =NULL,
                                 block_size=NULL,
                                 ratio= NULL,
                                 nsample=NULL,
                                 assigned = NULL,
                                 unassigned = NULL,
                                 project_id = NULL,
                                 deploy_plat="linux",
                                 ...){

  library(rio)
  library(dplyr)
  library(devR)
  library(readxl)
  file_path <- '/file/'
  covmat <- as.data.frame(covmat)
  # file_path <- 'E://random_system//random//'
  file_name <- paste0(file_path,project_id,'.xlsx')
  if(!file.exists(file_name)){
    export(as.data.frame(covmat),file_name)
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
  for(j in 1:nrow(covmat_tmp)){
    assign_tmp <- covmat_tmp[rep(j,each=nsample),] %>% data.frame()
    assign_tmp$id <- c(1:nsample)

    refluction = rnorm(1,100+j,100)
    #这一层进行一个区组随机
    block_out <- block_random_old_f(
      seed = seed+refluction,
      trtseq = trtseq,
      ratio= ratio,
      nsample = nsample,
      block_size = block_size
    )
    assign_tmp$assignment = block_out$assignment
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



#
#
# dat = '{"sex": ["1","2","1","2","1","1","2","1","2","1","2","1"]}'
# library(dplyr)
# library(jsonlite)
# test_dat = fromJSON(dat) %>% data.frame()
#
# stratified_block_out1 <-
#   stratified_blocked_f(
#   seed=123,
#   trtseq=c("a","b") ,
#   covmat =test_dat,
#   block_size=4,
#   assigned= 0,
#   project_id = 'test101',
#   unassigned = 12,
#   ratio= c(1,1),
#   nsample=100
#   )
#
# debug(stratified_blocked_f)
