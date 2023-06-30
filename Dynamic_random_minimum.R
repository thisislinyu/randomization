#' @param seed 设置随机数
#' @param trtseq 分组名称
#' @param covmat 分层因素矩阵
#' @param ratio 分组比例
#' @param nsample 总样本量
#' @param project_id 项目唯一标识
#' @param assigned 上一次已分组的患者数
#' @param unassigned 二次入组的患者数
#'
Minirand_f <- function(seed = 123,
                       trtseq = NULL,
                       covmat = NULL,
                       ratio = NULL,
                       nsample = NULL,
                       deploy_plat="linux",
                       assigned = NULL,
                       unassigned = NULL,
                       project_id = NULL,
                       ...){



  set.seed(seed)
  library(rio)
  library(dplyr)
  library(devR)
  library(readxl)

  # /file/filename.xlsx   初次部署需要给file文件夹777权限
  # docker 内project_id文件及路径
  file_path <- '/file/'
  covmat <- as.data.frame(covmat)
  #file_path <- 'E://random_system//random//'
  file_name <- paste0(file_path,project_id,'.xlsx')
  # 判断此项目文件是否存在  分组指标改变此判断会报错 需要删除内部文件
  if(!file.exists(file_name)){
    export(covmat,file_name)
  }else{
    data = read_xlsx(file_name)
    covmat = rbind(data,covmat)
    export(covmat,file_name)
  }

  method = 'Range'

  p=0.9

  trtseq_out <- trtseq

  trtseq_tmp <- c(1:length(trtseq))

  trtseq <- trtseq_tmp[as.factor(trtseq)]

  nsample <- nrow(covmat) ########改了个啥位置？？

  ntrt <- length(trtseq)

  covwt <- rep(1,length(trtseq))

  #covmat = covmat

  index = which(covmat$assignment==0)

  #covmat_compute = covmat[,-ncol(covmat)] %>% data.frame()

  covmat_compute = covmat %>% select(-assignment)

  #result <- rep(100,nsample)

  result <- c(1:nsample)

  if(index[1]==1){

    # result is the treatment needed from minimization method
    #generate treatment assignment for the 1st subject

    result[1] = sample(trtseq, 1, replace = TRUE, prob = ratio/sum(ratio))
    covmat$assignment[1] = result[1]

    for (j in index[-1])

    {
      # get treatment assignment sequential for all subjects
      matchx = apply(covmat_compute[1:(j - 1), , drop = FALSE], 1,

                     function(x, xrow) {

                       as.numeric(x == xrow)

                     }, covmat_compute[j, ])

      ##if only one cell exists, then
      if(is.null(dim(matchx))){
        matchx = matchx %>% as.matrix() %>% t()
      }

      ### # of covariate * # of trt
      n_matchx <- matrix(0, ncol(covmat_compute), ntrt)

      for (k in 1:ntrt)

      { if(sum(result[1:(j-1)]==trtseq[k])==0){
        n_matchx[,k]=0
      }else{
        ### if only one row/one variable
        if(dim(matchx)[1]==1){
          n_matchx[,k] =  apply(as.matrix(matchx[, result[1:(j-1)]==trtseq[k]]) %>% t(),1,sum)
          ## or
          #n_matchx[,k] =  apply(as.matrix(matchx[, result[1:(j-1)]==trtseq[k]]) ,2,sum)
        }else{
          n_matchx[,k] <- apply(as.matrix(matchx[, result[1:(j-1)]==trtseq[k]]), 1, sum)
        }
        #col_index = which((result[1:(j-1)]==trtseq[k])==TRUE)

      }
      }
      if (method == "Range")

      {

        imbalance <- rep(0, ntrt)

        for (i in 1:ntrt)

        {

          temp <- n_matchx

          temp[,i] <- temp[, i]+1

          num_level <- temp %*% diag(1/ratio)

          range_level <- apply(num_level, 1, range) # 2* # of covariate factors

          imb_margin <- range_level[2, ] - range_level[1, ]

          imbalance[i] <- sum(covwt %*% t(imb_margin))

        }

      }
      if (method == "SD")

      {

        imbalance <- rep(0, ntrt)

        for (i in 1:ntrt)

        {

          temp <- n_matchx

          temp[, i] <- temp[, i]+1

          num_level <- temp %*% diag(1/ratio)

          sd_level <- apply(num_level, 1, sd)

          imb_margin <- sd_level

          imbalance[i] <- sum(covwt %*% t(imb_margin))

        }

      }
      if (method == "Var")

      {

        imbalance <- rep(0, ntrt)

        for (i in 1:ntrt)

        {

          temp <- n_matchx

          temp[,i] <- temp[, i]+1

          num_level <- temp %*% diag(1/ratio)

          var_level <- apply(num_level, 1, var)

          imb_margin <- var_level

          imbalance[i] <- sum(covwt %*% t(imb_margin))

        }

      }



      trt.mini <- trtseq[imbalance == min(imbalance)]

      trt.highprob <- trt.mini

      trt.lowprob <- trtseq[-trt.mini]

      result[j] <- ifelse(length(trt.highprob) < ntrt,

                          sample(c(trt.highprob, trt.lowprob), 1, replace = TRUE,

                                 prob = c(rep(p/length(trt.highprob), length(trt.highprob)),

                                          rep((1-p)/length(trt.lowprob), length(trt.lowprob)))),

                          sample(trtseq, 1, replace = TRUE, prob = rep(1/ntrt, ntrt)))



      covmat$assignment[j] <- result[j]

    }

    assignment1 = trtseq[as.factor(covmat$assignment)]


    covmat1 <-  covmat %>% mutate(assignment = trtseq_out[as.factor(assignment)])

  }




  ######按照现在的算法，我们自己存数据，index一定会等于1
  if(index[1]!=1){



    for (j in index)

    {


      # scenario 2: get treatment assignment sequential for all subjects

      matchx = apply(covmat_compute[1:(j - 1), , drop = FALSE], 1,

                     function(x, xrow) {

                       as.numeric(x == xrow)

                     }, covmat_compute[j, ])

      if(is.null(dim(matchx))){
        matchx = matchx %>% as.matrix() %>% t()
      }

      n_matchx <- matrix(0, ncol(covmat_compute), ntrt)

      for (k in 1:ntrt)

      {
        if(sum(result[1:(j-1)]==trtseq[k])==0){
          n_matchx[,k]=0
        }else{

          ### if only one row/one variable
          if(dim(matchx)[1]==1){
            n_matchx[,k] =  apply(as.matrix(matchx[, result[1:(j-1)]==trtseq[k]]) %>% t(),1,sum)
            ## or
            #n_matchx[,k] =  apply(as.matrix(matchx[, result[1:(j-1)]==trtseq[k]]) ,2,sum)
          }else{
            n_matchx[,k] <- apply(as.matrix(matchx[, result[1:(j-1)]==trtseq[k]]), 1, sum)

          }

        }



        if (method == "Range")

        {

          imbalance <- rep(0, ntrt)

          for (i in 1:ntrt)

          {

            temp <- n_matchx

            temp[,i] <- temp[, i]+1

            num_level <- temp %*% diag(1/ratio)

            range_level <- apply(num_level, 1, range)

            imb_margin <- range_level[2, ] - range_level[1, ]

            imbalance[i] <- sum(covwt %*% t(imb_margin))

          }

        }
        if (method == "SD")
        {
          imbalance <- rep(0, ntrt)
          for (i in 1:ntrt)
          {
            temp <- n_matchx
            temp[, i] <- temp[, i]+1
            num_level <- temp %*% diag(1/ratio)
            sd_level <- apply(num_level, 1, sd)
            imb_margin <- sd_level
            imbalance[i] <- sum(covwt %*% t(imb_margin))
          }
        }
        if (method == "Var")

        {

          imbalance <- rep(0, ntrt)

          for (i in 1:ntrt)

          {

            temp <- n_matchx

            temp[,i] <- temp[, i]+1

            num_level <- temp %*% diag(1/ratio)

            var_level <- apply(num_level, 1, var)

            imb_margin <- var_level

            imbalance[i] <- sum(covwt %*% t(imb_margin))

          }

        }
        trt.mini <- trtseq[imbalance == min(imbalance)]

        trt.highprob <- trt.mini

        trt.lowprob <- trtseq[-trt.mini]

        result[j] <- ifelse(length(trt.highprob) < ntrt,

                            sample(c(trt.highprob, trt.lowprob), 1, replace = TRUE,

                                   prob = c(rep(p/length(trt.highprob), length(trt.highprob)),

                                            rep((1-p)/length(trt.lowprob), length(trt.lowprob)))),

                            sample(trtseq, 1, replace = TRUE, prob = rep(1/ntrt, ntrt)))

        covmat$assignment[j] <- result[j]
      }
      covmat_append <- covmat[index,] %>% mutate(assignment = trtseq_out[as.factor(assignment)])

      covmat1 <- rbind(covmat[-index,],covmat_append)
    }
  }

  ##assigned should be nrow of the previous stored data; unassigned should be
  ## nrow of the current rows

  if((assigned+unassigned)<nsample){
    final <- covmat1[(assigned+1):(assigned+unassigned),]
  }else{
    final <- covmat1[(assigned+1):nsample,]
  }
  return(final)
}
