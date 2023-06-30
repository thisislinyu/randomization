#' @param seed 设置随机数
#' @param trtseq 分组名称
#' @param ratio 分组比例
#' @param nsample 总样本量
#' @param block_size 每个block的样本量
#' @param assigned 上一次已分组的患者数
#' @param unassigned 二次入组的患者数


block_random_f <- function(seed = 123,
                           trtseq = NULL,
                           ratio = NULL,
                           nsample = NULL,
                           block_size = NULL,
                           assigned = NULL,
                           unassigned = NULL,
                           deploy_plat="linux",
                           ...) {
  set.seed(seed)
  library(dplyr) # version: v1.0.7
  library(devR)
  if (nsample == 0) {
    errormessage <-linux_or_win(deploy_plat,
                                paste0('[{"ErrorMessage":"总样本量为0，请重新定义总样本量。"}]')
    )}

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

      #patient_id_new = seq(i,i+block_size-1,1)
      refluction = rnorm(1,100+i,100)
      simple_out <- simple_random_f(seed = seed+refluction,
                                    trtseq=trtseq,
                                    group_sample = group_sample) %>%
        arrange(., patient_id) %>%
        mutate(patient_id = seq(i,i+block_size-1,1)) %>% select(-random_num)


      final_res <- rbind(final_res,simple_out)
    }
  }

  if((assigned+unassigned)<nsample){
    final_res1 = final_res[(assigned+1):(assigned+unassigned),]
  }else{
    final_res1 = final_res[(assigned+1):nsample,]
  }
  return(final_res1)
}
