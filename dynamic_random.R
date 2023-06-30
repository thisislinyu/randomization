#' 生成测试假数据
#' @param param_and_values 指标中文名称以及对应的可能取值个数
#' @param total_num 生成的总病例数
#' @export
#' @examples generate_data_randomly(param_and_values=list('v1'=c("年龄",3), 'v2'=c("性别",2)), total_num=600)
#' generate_data_randomly()


generate_data_randomly <- function(param_and_values, total_num){

  col_data <- list()
  # 每个指标随机生成数据
  for (i in 1:length(param_and_values)){
    col_data[[i]] <- sample(as.numeric(param_and_values[[i]][-1]),
                            size = total_num,
                            replace = T,
                            prob = runif(length(param_and_values[[i]][-1]),min = 0.35, max = 0.65)
    ) # 写死单指标多个取值间的偏置概率在0.35~0.65间
  }
  fake_data <- data.frame(col_data, check.names = F)
  names(fake_data) <- as.vector(vapply(param_and_values, head, n = 1L, FUN.VALUE = character(1)))
  return(fake_data)
}


#' 生成每个层层号与各因子取值对应关系表。
#' @param group_description_info_dataframe 指标中文名称以及对应的包含的所有可能取值
#' @export
#' @examples group_description_info_dataframe=list('f1'=data.frame('年龄'=c('[10,20)','[20,30)','[30,50)')), 'f2'= data.frame('性别'=c('男','女')))
#' generate_block_num()


generate_block_num <- function(group_description_info_dataframe){

  a1 <- group_description_info_dataframe[[1]]
  colnames1 <- c(names(a1))
  for (i in 1:(length(group_description_info_dataframe)-1)){
    a2 <- group_description_info_dataframe[[i+1]]
    colnames1 <- c(colnames1, names(a2))
    if (i==1){
      blocknum_with_variable_value_dataframe <- merge(a1, a2, by=NULL)
    }else {
      blocknum_with_variable_value_dataframe <- merge(blocknum_with_variable_value_dataframe, a2, by=NULL)
    }
  }
  blocknum_with_variable_value_dataframe[['区组号']] <- 1:nrow(blocknum_with_variable_value_dataframe)
  return(blocknum_with_variable_value_dataframe)
  # > blocknum_with_variable_value_dataframe
  #       年龄   性别 是否病1 病2症状   疾病指标1     区组号
  # 1   [10,20)   男      是   症状1    [1.5,7.68)      1
  # 2   [20,30)   男      是   症状1    [1.5,7.68)      2
  # 3   [30,50)   男      是   症状1    [1.5,7.68)      3
  # 4   [10,20)   女      是   症状1    [1.5,7.68)      4
}


#' 首次入组，利用简单随机法
#' @param data 分组前数据
#' @param group_num_ratio 组间比例
#' @export
#' @examples first_time_enter_group(data=data, group_num_ratio=c(1,2,4))
#' first_time_enter_group()


first_time_enter_group <- function(data,
                                   group_num_ratio){

  # 将比例转换成成比例的组号的组合，如c(1,2,3) -> c(1,2,2,3,3,3)
  group_num_ratio <- get_group_distribution_comparison(group_num_ratio)[['实际组号']]
  # 取前5%的患者，做简单随机入组
  patient_num <- nrow(data)
  sample(group_num_ratio, patient_num, replace = T)
  # random_num <- rnorm(patient_num)
  # group_num <- length(group_num_ratio)
  # remainder_num <- patient_num%%group_num
  # if ((patient_num - remainder_num)>0){
  #   group_rep_num <- (patient_num - remainder_num)/group_num
  # }else {
  #   group_rep_num <- 0
  # }
  # # 按顺序排一个组序列
  # group_vector <- c(rep(1:length(group_num_ratio), group_rep_num), sample(group_num_ratio, remainder_num))
  # random_table <- data.frame('随机号'=random_num, '组号'=group_vector)
  # random_table <- random_table[order(random_table[['随机号']]), ]
  data[['所属组号']] <- sample(group_num_ratio, patient_num, replace = T)
  return(data)
}


#' 计算当前已入组患者在各组中各属性拥有的人数
#' @param in_group_data 已入组患者数据
#' @param param_and_values 指标中文名称以及对应的可能取值个数，仅当最小化法入组时才需要
#' @param group_distribution_comparison 基础组号与实际组号，分组按公约数拆分，分组结束再按组间比例合并。
#' @export
#' @examples
#' compute_patient_attr_num_ingroup()


compute_patient_attr_num_ingroup <- function(in_group_data, param_and_values,
                                             group_distribution_comparison){
  library("dplyr")
  library("magrittr")
  patient_attr_num_ingroup_dataframe <- data.frame(check.names = F)
  # 将指标名提取出来
  params_names <- lapply(param_and_values, function(x) x[[1]])
  for (v in names(in_group_data)[1:(ncol(in_group_data)-1)]){
    # 生成所有指标取值和组合的一个完整表，方便将暂未出现样本的特定取值情况纳入不均衡性比较。

    # v的所有取值
    all_values_of_v <- as.numeric(param_and_values[grep(v, params_names)][[1]][-1])
    complete_attr_table <- expand.grid('指标取值'=all_values_of_v,
                                       '所属组号'=group_distribution_comparison[['基础组号']])
    complete_attr_table <- cbind(complete_attr_table,
                                 '对应指标取值所属病人数'=rep(0, nrow(complete_attr_table)))
    patient_attr_num_ingroup_dataframe1 <-
      aggregate(list('对应指标取值所属病人数'=in_group_data[['所属组号']]),
                by = list('指标取值'=in_group_data[[v]],'所属组号'=in_group_data[['所属组号']]),
                FUN = length)
    # browser()
    complete_attr_table <- complete_attr_table %>%
      left_join(. , patient_attr_num_ingroup_dataframe1, by=c('指标取值', '所属组号'))
    # 删掉多出来的“对应指标取值所属病人数.x”列，将“对应指标取值所属病人数.y”列更名回“对应指标取值所属病人数”
    delete_column_index <- grep('.x', names(complete_attr_table))
    complete_attr_table <- subset(complete_attr_table, select = -c(delete_column_index))
    names(complete_attr_table)[3] <- '对应指标取值所属病人数'
    complete_attr_table[is.na(complete_attr_table)] <- 0
    complete_attr_table[['指标名']] <- v
    patient_attr_num_ingroup_dataframe <- rbind(patient_attr_num_ingroup_dataframe,
                                                complete_attr_table)
  }
  return(patient_attr_num_ingroup_dataframe)
}


#' 根据选择入组的某一条患者数据，尝试进入每一组，计算进入每一组时的不均衡度。
#' @param in_group_data 已入组患者数据
#' @param one_patient_data_dataframe_before_ingroup 需要入组的单个患者数据
#' @param variable_probability 指标权重
#' @param group_distribution_comparison 基础组号与实际组号，分组按公约数拆分，分组结束再按组间比例合并。
#' @param param_and_values 指标中文名称以及对应的可能取值个数，仅当最小化法入组时才需要
#' @export
#' @examples
#' compute_unbalance_for_each_group()


compute_unbalance_for_each_group <-
  function(in_group_data,
           one_patient_data_dataframe_before_ingroup,
           variable_probability,
           group_distribution_comparison,
           param_and_values){

    library("devR")
    total_std_of_every_group <- data.frame(check.names = F)
    for (group_num in 1:nrow(group_distribution_comparison)){
      one_patient_data_dataframe_before_ingroup[['所属组号']] <- group_num
      in_group_data_to_compute <-
        rbind(in_group_data,
              one_patient_data_dataframe_before_ingroup)
      # 从in_group_data_to_compute中把id去掉
      if ("id" %in% names(in_group_data_to_compute))
        in_group_data_to_compute <- subset(in_group_data_to_compute, select=-c(id))
      # print(in_group_data_to_compute)
      compute_patient_attr_num_dataframe <-
        compute_patient_attr_num_ingroup(
          in_group_data=in_group_data_to_compute,
          param_and_values=param_and_values,
          group_distribution_comparison=group_distribution_comparison)
      # print(compute_patient_attr_num_dataframe)
      total_std <- 0
      for (v in unique(compute_patient_attr_num_dataframe[['指标名']])){
        # 只取v指标取值等于该待入组患者v指标取值的已入组人数来算不均衡性。
        #     指标取值 所属组号 对应指标取值所属病人数 指标名
        # 1         1        1                      2   年龄
        # 2         2        1                      2   年龄
        # 3         2        2                      1   年龄
        # 4         3        2                      2   年龄
        # 5         1        4                      1   年龄
        # 6         2        4                      1   年龄
        # 7         3        4                      2   年龄
        # 8         1        5                      2   年龄
        # 9         2        5                      2   年龄
        # 10        3        5                      1   年龄
        # 如v=='年龄'时，上表只取'指标取值'为2的所属病人数数据作为v_patient_num
        v_patient_num <- compute_patient_attr_num_dataframe[
          which(
            compute_patient_attr_num_dataframe['指标名']==v &
              compute_patient_attr_num_dataframe['指标取值']==
              one_patient_data_dataframe_before_ingroup[, v]
          ),
          '对应指标取值所属病人数']
        if (length(v_patient_num)<=1){
          std_of_v <- 0
        }else {
          # 不均衡性值
          std_of_v <- sd(v_patient_num)
        }
        # 不均衡度用加权求和
        total_std <- total_std + std_of_v *
          variable_probability[['权重']][which(variable_probability['指标名']==v)] /
          sum(variable_probability[['权重']])
      }
      total_std_of_every_group <-
        rbind(total_std_of_every_group,
              data.frame('尝试进入组号'=group_num,
                         '不均衡性值'=total_std, check.names = F)
        )
    }
    return(total_std_of_every_group)
  }


# 根据进入每一组时的不均衡度，以及对应的进入概率，随机出最终确定进入的组号，返回入组的组号。
# compute_ingroup_num <- function(total_std_of_every_group,
#                                 bias_probability,
#                                 variable_probability,
#                                 deploy_plat='linux'){
#
#   if(bias_probability < 1/nrow(total_std_of_every_group)){
#     linux_or_win(deploy_plat, paste('您选择了', nrow(total_std_of_every_group), '个均衡指标！   ',
#                                     '设置的偏倚分配概率不能小于', 1/nrow(total_std_of_every_group), '！',sep=''))
#   }
#   probability_patient_into_each_group <- as.numeric(c(bias_probability,
#                                            lapply(c(2:nrow(total_std_of_every_group)),
#                                                   function(x) (1-bias_probability)/(x-1))))
#   # [1] 0.850 0.150 0.075
#   complete_probability_patient_into_group <- c(0, cumsum(sort(c(probability_patient_into_each_group))))
#   # [1] 0.075 0.150 0.850 1.075
#   random_ingroup_value <- runif(1,min = 0, max = sum(probability_patient_into_each_group))
#   # 按照不均衡值顺序排序
#   total_std_of_every_group <- total_std_of_every_group[order(total_std_of_every_group[,2]),]
#   #     尝试进入组号 不均衡性值
#   # 1            1   34.25702
#   # 2            2   48.79686
#   # 3            3   58.24041
#   for (i in 2:length(complete_probability_patient_into_group)){
#     if (random_ingroup_value >= complete_probability_patient_into_group[i-1] &
#         random_ingroup_value < complete_probability_patient_into_group[i])
#       # 直接返回组号
#       return(total_std_of_every_group[i-1,1])
#   }
# }


#' 判断系统产生的随机数p1是否<=bias_probability
#' @param in_group_data 已入组患者数据
#' @param one_patient_data_dataframe_before_ingroup 需要入组的单个患者数据
#' @param bias_probability 偏置概率
#' @param allow_diff_between_groups 组间允许例数差值D
#' @param total_std_of_every_group 每个组的不均衡值
#' @param target_group 目标组（只是个待分配目标，并非最后确定分配的组，分配组才是确定分配的组。）
#' @param group_distribution_comparison 基础组号与实际组号，分组按公约数拆分，分组结束再按组间比例合并。
#' @export
#' @examples
#' if_p1_smallerthan_p()


if_p1_smallerthan_p <- function(in_group_data,
                                one_patient_data_dataframe_before_ingroup,
                                bias_probability,
                                allow_diff_between_groups,
                                total_std_of_every_group,
                                target_group,
                                group_distribution_comparison){

  library("devR")
  # Pi<=P
  if(runif(1, min = 0, max = 1) <= bias_probability){
    ingroup_num <- diff_between_groups_after_distribution(in_group_data=in_group_data,
                                                          one_patient_data_dataframe_before_ingroup=one_patient_data_dataframe_before_ingroup,
                                                          target_group=target_group,
                                                          allow_diff_between_groups=allow_diff_between_groups,
                                                          group_distribution_comparison=group_distribution_comparison)
  }
  # p1>p
  else{
    total_std_of_every_group <- total_std_of_every_group[-1, ]
    # 剩余组数!=1
    if (nrow(total_std_of_every_group)!=1){
      # 回到判断最小的不均衡值是否唯一，递归
      ingroup_num <- min_value_unique_loop(in_group_data=in_group_data,
                                           total_std_of_every_group=total_std_of_every_group,
                                           one_patient_data_dataframe_before_ingroup=one_patient_data_dataframe_before_ingroup,
                                           allow_diff_between_groups=allow_diff_between_groups,
                                           bias_probability=bias_probability,
                                           group_distribution_comparison=group_distribution_comparison)
    }
    # 剩余组数==1,剩余组即为分配组
    else {
      target_group <- total_std_of_every_group[['尝试进入组号']]
      ingroup_num <- diff_between_groups_after_distribution(in_group_data=in_group_data,
                                                            one_patient_data_dataframe_before_ingroup=one_patient_data_dataframe_before_ingroup,
                                                            target_group=target_group,
                                                            allow_diff_between_groups=allow_diff_between_groups,
                                                            group_distribution_comparison=group_distribution_comparison)
    }
  }
  return(ingroup_num)
}


#' 判断分配后组间例数差值是否>D
#' @param in_group_data 已入组患者数据
#' @param one_patient_data_dataframe_before_ingroup 需要入组的单个患者数据
#' @param target_group 目标组（只是个待分配目标，并非最后确定分配的组，分配组才是确定分配的组。）
#' @param allow_diff_between_groups 组间允许例数差值D
#' @param group_distribution_comparison 基础组号与实际组号，分组按公约数拆分，分组结束再按组间比例合并。
#' @export
#' @examples
#' diff_between_groups_after_distribution()


diff_between_groups_after_distribution <- function(in_group_data,
                                                   one_patient_data_dataframe_before_ingroup,
                                                   target_group,
                                                   allow_diff_between_groups,
                                                   group_distribution_comparison){

  one_patient_data_dataframe_after_ingroup <- one_patient_data_dataframe_before_ingroup
  one_patient_data_dataframe_after_ingroup['所属组号'] <- target_group
  in_group_data_add_one_patient <- rbind(in_group_data, one_patient_data_dataframe_after_ingroup)
  patient_num_ineachgroup_dataframe_withnewpatient <-
    aggregate(list('对应组拥有病人数'=in_group_data_add_one_patient[['所属组号']]),
              by = list('所属组号'=in_group_data_add_one_patient[['所属组号']]),
              FUN = length)
  # if (nrow(in_group_data)>100)
  # 将暂时没分到病历的组补0记录。
  need_add_zero_group_vector <- which(!group_distribution_comparison[, '基础组号']%in%
                                        patient_num_ineachgroup_dataframe_withnewpatient[, '所属组号'])
  if (length(need_add_zero_group_vector!=0)){
    need_add_zero_group_dataframe <- data.frame('所属组号'=need_add_zero_group_vector,
                                                '对应组拥有病人数'=rep(0,length(need_add_zero_group_vector)),
                                                check.names = F)
    patient_num_ineachgroup_dataframe_withnewpatient <- rbind(patient_num_ineachgroup_dataframe_withnewpatient,
                                                              need_add_zero_group_dataframe)
  }

  diff_between_groups <- max(patient_num_ineachgroup_dataframe_withnewpatient[['对应组拥有病人数']]) -
    min(patient_num_ineachgroup_dataframe_withnewpatient[['对应组拥有病人数']])

  if (diff_between_groups >= allow_diff_between_groups){
    ingroup_num <- patient_num_ineachgroup_dataframe_withnewpatient[which(
      patient_num_ineachgroup_dataframe_withnewpatient[,'对应组拥有病人数']==min(
        patient_num_ineachgroup_dataframe_withnewpatient[['对应组拥有病人数']])),'所属组号']
    # 在分配后组间例数差值>D的情况下，判断此时病例数最少的组是否唯一，若不唯一，则随便分配到一个组;
    # 若唯一，则分配给病例数最少的组。
    if (length(ingroup_num) > 1)
      ingroup_num <- sample(ingroup_num, 1, replace = T)
  }else {
    ingroup_num <- target_group
  }
  return(ingroup_num)
}


#' 判断最小值是否唯一的循环
#' @param in_group_data 已入组患者数据
#' @param total_std_of_every_group 每个组的不均衡值
#' @param one_patient_data_dataframe_before_ingroup 需要入组的单个患者数据
#' @param allow_diff_between_groups 组间允许例数差值D
#' @param bias_probability 偏置概率
#' @param group_distribution_comparison 基础组号与实际组号，分组按公约数拆分，分组结束再按组间比例合并。
#' @export
#' @examples
#' min_value_unique_loop()


min_value_unique_loop <- function(in_group_data,
                                  total_std_of_every_group,
                                  one_patient_data_dataframe_before_ingroup,
                                  allow_diff_between_groups,
                                  bias_probability,
                                  group_distribution_comparison){

  library("devR")

  unbalance_level <- total_std_of_every_group[['不均衡性值']]
  # 最小的不均衡值
  min_value <- length(unbalance_level[which(unbalance_level==min(unbalance_level))])
  # 最小的不均衡值是否唯一
  if (min_value == 1){
    target_group <- total_std_of_every_group[which(total_std_of_every_group[,'不均衡性值'] == min(unbalance_level)),
                                             '尝试进入组号']
    ingroup_num <- if_p1_smallerthan_p(in_group_data=in_group_data,
                                       one_patient_data_dataframe_before_ingroup=one_patient_data_dataframe_before_ingroup,
                                       bias_probability=bias_probability,
                                       allow_diff_between_groups=allow_diff_between_groups,
                                       total_std_of_every_group=total_std_of_every_group,
                                       target_group=target_group,
                                       group_distribution_comparison=group_distribution_comparison)
    return(ingroup_num)
  }
  # 最小的不均衡值不唯一
  else {
    min_unbalance_group <- total_std_of_every_group[which(unbalance_level==min(unbalance_level)), '尝试进入组号']
    # in_group_data中仍可能有部分组空无一人
    patient_num_ineachgroup_dataframe <- aggregate(list('对应组拥有病人数'=in_group_data[['所属组号']]),
                                                   by = list('所属组号'=in_group_data[['所属组号']]),
                                                   FUN = length)
    # 病历数暂时是0的组，添加到patient_num_ineachgroup_dataframe中，使得其中记录所有组的人数，包括人数为0的。
    patient_num_iszero_group <- which(!total_std_of_every_group[, '尝试进入组号']%in%
                                        patient_num_ineachgroup_dataframe[, '所属组号'])
    if (!is.null(patient_num_iszero_group)){
      patient_num_ineachgroup_dataframe  <-
        rbind(patient_num_ineachgroup_dataframe,
              data.frame('所属组号'=patient_num_iszero_group,
                         '对应组拥有病人数'=rep(0,length(patient_num_iszero_group))),
              check.names = F)
    }
    # 不均衡值最小的组
    patient_num_min_unbalance_group_dataframe <- patient_num_ineachgroup_dataframe[which(
      patient_num_ineachgroup_dataframe[,'所属组号']%in%min_unbalance_group), ]
    # 不均衡值最小的组中病例数最少的组
    min_patient_num_in_min_unbalance_group <- patient_num_min_unbalance_group_dataframe[which(
      patient_num_min_unbalance_group_dataframe[,'对应组拥有病人数']==min(
        patient_num_min_unbalance_group_dataframe[,'对应组拥有病人数'])),]
    # 不均衡值最小的组中病例数最少的组是唯一的，回到产生随机数p1
    if (nrow(min_patient_num_in_min_unbalance_group)==1){
      target_group <- min_patient_num_in_min_unbalance_group[['所属组号']]
      ingroup_num <- if_p1_smallerthan_p(in_group_data=in_group_data,
                                         one_patient_data_dataframe_before_ingroup=one_patient_data_dataframe_before_ingroup,
                                         bias_probability=bias_probability,
                                         allow_diff_between_groups=allow_diff_between_groups,
                                         total_std_of_every_group=total_std_of_every_group,
                                         target_group=target_group,
                                         group_distribution_comparison=group_distribution_comparison)
      return(ingroup_num)
    }
    # 不均衡值最小的组中病例数最少的组不唯一,在其中随机分配
    else {
      random_value <- sample(1:length(min_patient_num_in_min_unbalance_group[['所属组号']]), size = 1)
      target_group <- min_patient_num_in_min_unbalance_group[random_value, '所属组号']
      ingroup_num <- diff_between_groups_after_distribution(in_group_data=in_group_data,
                                                            one_patient_data_dataframe_before_ingroup=
                                                              one_patient_data_dataframe_before_ingroup,
                                                            target_group=target_group,
                                                            allow_diff_between_groups=allow_diff_between_groups,
                                                            group_distribution_comparison=group_distribution_comparison)
      return(ingroup_num)
    }
  }
}


#' 操作某个患者入组的函数
#' @param in_group_data 已入组患者数据
#' @param one_patient_data_dataframe_before_ingroup 需要入组的单个患者数据
#' @param variable_probability 指标权重
#' @param bias_probability 偏置概率
#' @param deploy_plat 部署平台
#' @param allow_diff_between_groups 组间允许例数差值D
#' @param group_distribution_comparison 基础组号与实际组号，分组按公约数拆分，分组结束再按组间比例合并。
#' @param param_and_values 指标中文名称以及对应的可能取值个数，仅当最小化法入组时才需要
#' @export
#' @return 返回该患者随机确定的入组号 int
#' @examples
#' one_patient_ingroup()

one_patient_ingroup <- function(in_group_data,
                                one_patient_data_dataframe_before_ingroup,
                                variable_probability,
                                bias_probability,
                                deploy_plat='linux',
                                allow_diff_between_groups,
                                group_distribution_comparison,
                                param_and_values){

  library("devR")
  # 根据选择入组的某一条患者数据，尝试进入每一组，计算进入每一组时的不均衡度。
  in_group_data <- subset(in_group_data, select = c(-实际组号))
  if ('实际组号' %in% names(one_patient_data_dataframe_before_ingroup)){
    one_patient_data_dataframe_before_ingroup <-
      subset(one_patient_data_dataframe_before_ingroup, select = c(-实际组号))
  }

  total_std_of_every_group <-
    compute_unbalance_for_each_group(in_group_data=in_group_data,
                                     one_patient_data_dataframe_before_ingroup=
                                       one_patient_data_dataframe_before_ingroup,
                                     variable_probability=variable_probability,
                                     group_distribution_comparison=group_distribution_comparison,
                                     param_and_values=param_and_values)
  # print(total_std_of_every_group)

  # 走最小的不均衡值是否唯一的loop
  ingroup_num <- min_value_unique_loop(in_group_data=in_group_data,
                                       total_std_of_every_group=total_std_of_every_group,
                                       one_patient_data_dataframe_before_ingroup=
                                         one_patient_data_dataframe_before_ingroup,
                                       allow_diff_between_groups=allow_diff_between_groups,
                                       bias_probability=bias_probability,
                                       group_distribution_comparison=group_distribution_comparison)

  return(ingroup_num)
}


#' 生成该variable的模拟表结果，不包括均衡性检验部分
#' @param variable 针对的变量
#' @param patient_after_group_dataframe 已入组病例dataframe
#' @param group_distribution_comparison 根据组间比例转换出多个子组，以便子组间病例进行合并能达到符
#' 合组间比例的分组情况,比如1:2:3，会转换出6个子组.
#' @export
#' @examples
#' generate_variable_simulation_result()


generate_variable_simulation_result <- function(variable,
                                                patient_after_group_dataframe,
                                                group_distribution_comparison){

  patient_num_in_each_group_dataframe <-
    aggregate(list('对应指标取值所属病人数'=patient_after_group_dataframe[['实际组号']]),
              by = list('指标取值'=patient_after_group_dataframe[[variable]],
                        '实际组号'=patient_after_group_dataframe[['实际组号']]), FUN = length)
  patient_num_in_each_group_dataframe <-
    patient_num_in_each_group_dataframe[order(patient_num_in_each_group_dataframe[, '指标取值'],
                                              patient_num_in_each_group_dataframe[, '实际组号']), ]
  actual_groups <- unique(group_distribution_comparison[['实际组号']])
  # patient_num_in_each_group_dataframe
  #       指标取值 所属组号 对应指标取值所属病人数
  # 1         1        1                     82
  # 4         1        2                     82
  # 7         1        3                     22
  # 10        1        4                     46
  # 13        1        5                     68
  # 2         2        1                     63
  # 5         2        2                     51
  # 8         2        3                     22
  # 11        2        4                     25
  # 14        2        5                     55
  simulation_result_dataframe <- data.frame(check.names = F)
  for (i1 in unique(patient_num_in_each_group_dataframe[['指标取值']])){
    # simulation_result_dataframe <-
    #   check_bug("simulation_result_dataframe <- ds(simulation_result_dataframe=simulation_result_dataframe,
    #                                                patient_num_in_each_group_dataframe=patient_num_in_each_group_dataframe,
    #                                                i1=i1)",
    #             "simulation_result_dataframe")

    # simulation_result_dataframe <-
    #   check_bug("simulation_result_dataframe <- rbind(simulation_result_dataframe,
    #             t(patient_num_in_each_group_dataframe[which(patient_num_in_each_group_dataframe[, '指标取值']==i1),
    #                                                     '对应指标取值所属病人数']))", "simulation_result_dataframe")
    patient_num_in_each_group_dataframe1 <-
      patient_num_in_each_group_dataframe[which(patient_num_in_each_group_dataframe[, '指标取值']==i1),
                                          c('对应指标取值所属病人数', '实际组号')]
    # 如果有某个指标在部分组中人数为0，这里需要人工添加上组人数为0的数据。
    if (nrow(patient_num_in_each_group_dataframe1) <
        length(actual_groups)){
      need_add_zero_group_vector <- which(!actual_groups%in%
                                            patient_num_in_each_group_dataframe1[, '实际组号'])
      need_add_group_dataframe <-
        data.frame('对应指标取值所属病人数'=rep(0,length(need_add_zero_group_vector)),
                   '实际组号'=need_add_zero_group_vector, check.names = F)
      names(need_add_group_dataframe) <- c('对应指标取值所属病人数', '实际组号')
      patient_num_in_each_group_dataframe1 <- rbind(patient_num_in_each_group_dataframe1,
                                                    need_add_group_dataframe)
    }
    patient_num_in_each_group_dataframe1 <-
      patient_num_in_each_group_dataframe1[order(patient_num_in_each_group_dataframe1[['实际组号']]), ]

    # patient_num_in_each_group_dataframe2 <-
    #   patient_num_in_each_group_dataframe1[['对应指标取值所属病人数']]

    patient_num_in_each_group_dataframe3 <- data.frame(t(patient_num_in_each_group_dataframe1), check.names = F)[1, ]
    names(patient_num_in_each_group_dataframe3) <-
      paste('组', patient_num_in_each_group_dataframe1[['实际组号']], sep='')
    patient_num_in_each_group_dataframe3 <- cbind(patient_num_in_each_group_dataframe3,
                                                  data.frame('因子取值'=i1,
                                                             '因子'=variable, check.names = F))
    simulation_result_dataframe <-
      rbind(simulation_result_dataframe, patient_num_in_each_group_dataframe3)
  }
  rownames(simulation_result_dataframe) <- 1:nrow(simulation_result_dataframe)
  # names(simulation_result_dataframe) <-
  #   paste('组', 1:length(unique(patient_num_in_each_group_dataframe[['所属组号']])), sep='')
  # simulation_result_dataframe[, '因子取值'] <- group_description_info_dataframe[variable]
  # simulation_result_dataframe[, '因子'] <- variable
  return(simulation_result_dataframe)
}


#' 试跑中央随机算法每个total_num1跑的程序，单独切出来，为了并行。
#' @param total_num1 试跑中定义的总样本数
#' @param param_and_values 指标中文名称以及对应的可能取值个数
#' @param group_num_ratio 组间比例
#' @param bias_probability 偏置概率
#' @param variable_probability 指标权重
#' @param deploy_plat 部署平台
#' @param allow_diff_between_groups 组间允许例数差值D
#' @export
#' @examples
#' try_dynamic_random_conf_ineach_totalnum()


try_dynamic_random_conf_ineach_totalnum <- function(total_num1,
                                                    param_and_values,
                                                    group_num_ratio,
                                                    bias_probability,
                                                    variable_probability,
                                                    deploy_plat='linux',
                                                    allow_diff_between_groups){

  library("devR")

  check_bug <- function(expression1, return_value){
    x.inv <- try(eval(parse(text=expression1)), silent=TRUE)
    if ('try-error' %in% class(x.inv)) {
      browser()
      eval(parse(text=expression1))
    }else{
      return(eval(parse(text=return_value)))
    }
  }

  patient_recruited_dataframe <- data.frame(check.names = F)
  group_distribution_comparison <- get_group_distribution_comparison(group_num_ratio)
  res <- c()
  # 生成测试假数据
  fake_data <- generate_data_randomly(param_and_values, total_num=total_num1)
  # 首次入组，取前5%进行简单随机
  patient_recruited_this_batch <-
    main_dynamic_random(group_num_ratio = group_num_ratio,
                        total_num = total_num1,
                        bias_probability = bias_probability,
                        variable_probability = variable_probability,
                        recruit_batch_patient_dataframe = fake_data[1:ceiling(nrow(fake_data)*0.04), ],
                        deploy_plat = deploy_plat,
                        allow_diff_between_groups = allow_diff_between_groups,
                        param_and_values=param_and_values)
  patient_recruited_dataframe <- rbind(patient_recruited_dataframe, patient_recruited_this_batch)
  # patient_recruited_dataframe <- simple_random_data
  while (nrow(patient_recruited_dataframe) < total_num1){
    # 每次选择1到10个人入组
    ingroup_patient_num <- sample(1:20, size = 1)
    rownum_upper_limit <- nrow(patient_recruited_dataframe) + ingroup_patient_num
    if (rownum_upper_limit > total_num1)
      rownum_upper_limit <- total_num1
    patient_recruit_this_batch <- fake_data[(nrow(patient_recruited_dataframe)+1):rownum_upper_limit, ]

    patient_recruited_this_batch <- main_dynamic_random(
      recruited_patient_dataframe = patient_recruited_dataframe,
      group_num_ratio = group_num_ratio,
      total_num = total_num1,
      bias_probability = bias_probability,
      variable_probability = variable_probability,
      recruit_batch_patient_dataframe = patient_recruit_this_batch,
      deploy_plat = deploy_plat,
      allow_diff_between_groups = allow_diff_between_groups,
      param_and_values=param_and_values)

    # patient_recruited_this_batch <- check_bug("patient_recruited_this_batch <- main_dynamic_random(
    #                                       recruited_patient_dataframe = patient_recruited_dataframe,
    #                                       group_num_ratio = group_num_ratio,
    #                                       total_num = total_num1,
    #                                       bias_probability = bias_probability,
    #                                       variable_probability = variable_probability,
    #                                       recruit_batch_patient_dataframe = patient_recruit_this_batch,
    #                                       deploy_plat = deploy_plat,
    #                                       allow_diff_between_groups = allow_diff_between_groups,
    #                                       param_and_values=param_and_values)",
    #                                           "patient_recruited_this_batch")
    patient_recruited_dataframe <- rbind(patient_recruited_dataframe, patient_recruited_this_batch)
    print(paste('进行到该项目的第 ', nrow(patient_recruited_dataframe), ' 人'))
  }
  rm(fake_data)
  # group_distribution_comparison如下
  #     基础组号 实际组号
  # 1        1        1
  # 2        2        2
  # 3        3        2
  # 4        4        3

  # 判断组间比例是否非1:1:1:...
  # if (length(unique(group_distribution_comparison[[1]])) !=
  #     length(unique(group_distribution_comparison[[2]]))){
  #   # 用实际组号替换原所属组号。
  #   patient_recruited_dataframe <- merge(patient_recruited_dataframe,
  #                                        group_distribution_comparison,
  #                                        by.x='所属组号', by.y='基础组号')
  #   patient_recruited_dataframe <- subset(patient_recruited_dataframe, select=-c(所属组号))
  #   names(patient_recruited_dataframe)[ncol(patient_recruited_dataframe)] <- '所属组号'
  # }
  simulation_result_dataframe <- generate_variance_analysis(patient_recruited_dataframe,
                                                            group_num_ratio=group_num_ratio,
                                                            deploy_plat=deploy_plat)

  res[['data']] <- patient_recruited_dataframe
  res[['variance_res']] <- simulation_result_dataframe
  res[['组间允许例差']] <- allow_diff_between_groups
  res[['偏倚分配概率']] <- bias_probability
  res[['指标权重']] <- variable_probability
  res[['组间比例']] <- group_distribution_comparison
  return(res)
}


#' 根据组间比例转换出多个子组，以便子组间病例进行合并能达到符合组间比例的分组情况,比如1:2:3，会转换出6个子组.
#' @param group_num_ratio 指标中文名称以及对应的可能取值个数
#' @export
#' @examples
#' get_group_distribution_comparison()


get_group_distribution_comparison <- function(group_num_ratio){
  # 求公因子
  common_factor <- generate_common_factor(group_num_ratio)
  # 原组人数比例约分
  group_num_ratio <- group_num_ratio/common_factor
  total_group_num <- sum(group_num_ratio)
  group_distribution_standard <- c()
  for (num in 1:length(group_num_ratio)){
    group_distribution_standard <- c(group_distribution_standard,
                                     rep(num,group_num_ratio[num]))
  }
  group_distribution_comparison <- data.frame('基础组号'=1:total_group_num,
                                              '实际组号'=group_distribution_standard,
                                              check.names = F)
  return(group_distribution_comparison)
}


#' 根据用户选择的配置试跑中央随机算法。
#' @param param_and_values 指标中文名称以及对应的可能取值个数
#' @param group_num_ratio 组间比例
#' @param total_num 用户定义的总样本数
#' @param bias_probability 偏置概率
#' @param variable_probability 指标权重
#' @param deploy_plat 部署平台
#' @param allow_diff_between_groups 组间允许例数差值D
#' @param run_code_method 运行代码所用的方法，默认用并行，如果无法运行，建议切回非并行调试。
#' @param float_percent 允许上浮百分比，比如0.05表示允许上浮5%，会跑三份，样本量分别取total_num 和 total_num*(1±上浮比例)，
#' @export
#' @examples
#' try_dynamic_random_conf()


try_dynamic_random_conf <- function(param_and_values,
                                    group_num_ratio,
                                    total_num,
                                    bias_probability,
                                    variable_probability,
                                    deploy_plat='linux',
                                    allow_diff_between_groups,
                                    run_code_method='parallel',
                                    float_percent){

  library("devR")
  library("parallel")

  result <- list()
  # 尝试用户指定的范围上下浮动的所有可能性
  # browser()
  all_possible_rate_for_try <- c(1-as.numeric(float_percent), 1, 1+as.numeric(float_percent))
  all_possibility_for_try <- ceiling(all_possible_rate_for_try * total_num)

  group_distribution_comparison <- get_group_distribution_comparison(group_num_ratio)

  if (run_code_method=='parallel'){
    # apply(all_possibility_for_try,1,try_dynamic_random_conf_ineach_totalnum,
    #       param_and_values=param_and_values,
    #       group_num_ratio=group_num_ratio,
    #       bias_probability=bias_probability,
    #       variable_probability=variable_probability,
    #       allow_diff_between_groups=allow_diff_between_groups,
    #       group_distribution_comparison=group_distribution_comparison)

    #1.首先，创建cluster,意味着开启detectCores() - 1个同步线程
    if (length(all_possibility_for_try)>=(detectCores() - 1)){
      no_cores <- detectCores() - 1
    }else{
      no_cores <- length(all_possibility_for_try)
    }
    if (deploy_plat=='win'){
      clus <- makeCluster(no_cores, type="PSOCK") # 共享内存，用了可以大幅减少内存占用，linux用FORK
    }else if (deploy_plat=='linux'){
      clus <- makeCluster(no_cores, type="FORK")
    }

    #2.然后，告诉计算机之后你要在以上3个线程中运行函数，采用clusterExport或clusterEvalQ
    # clusterEvalQ(clus, {
    #   library("devR");
    #   # source('E:/skyroc-exploratory-analysis-r/trunk/R/tp_project/devR/devR/R/dynamic_random.R', encoding = 'UTF-8', echo=TRUE)
    # })
    clusterExport(clus, varlist=c("param_and_values", "group_num_ratio", "bias_probability", "variable_probability",
                                  "allow_diff_between_groups",
                                  "try_dynamic_random_conf_ineach_totalnum", "deploy_plat"), envir = environment())

    #3.类似于apply方法，应用clusterApply得出计算结果
    result1 <- clusterApply(clus, all_possibility_for_try, "try_dynamic_random_conf_ineach_totalnum",
                            param_and_values=param_and_values,
                            group_num_ratio=group_num_ratio,
                            bias_probability=bias_probability,
                            variable_probability=variable_probability,
                            deploy_plat=deploy_plat,
                            allow_diff_between_groups=allow_diff_between_groups)

    # result1 <- clusterApply(clus, all_possibility_for_try, "try_dynamic_random_conf_ineach_totalnum",
    #                         param_and_values,
    #                         group_num_ratio,
    #                         bias_probability,
    #                         variable_probability,
    #                         deploy_plat,
    #                         allow_diff_between_groups,
    #                         group_description_info_dataframe)

    result <- result1

    # 关闭集群
    stopCluster(clus)
  }else{
    result_index <- 1
    for (total_num1 in all_possibility_for_try){
      result1 <- try_dynamic_random_conf_ineach_totalnum(total_num1=total_num1,
                                                         param_and_values=param_and_values,
                                                         group_num_ratio=group_num_ratio,
                                                         bias_probability=bias_probability,
                                                         variable_probability=variable_probability,
                                                         deploy_plat=deploy_plat,
                                                         allow_diff_between_groups=allow_diff_between_groups)
      result[['data']][[as.character(total_num1)]] <- result1
      result_index <- result_index + 1
    }
  }
  return(result)
}


#' 包含精确概率法和卡方检验的方差分析函数
#' @param patient_after_group_dataframe 已入组病例dataframe
#' @param recruit_batch_patient_dataframe_templateId 已入组病例dataframe的数据ID
#' @param group_num_ratio 指标中文名称以及对应的可能取值个数
#' @param mongo_conffile_path mongo数据库连接配置文件路径
#' @param deploy_plat 部署平台
#' @export
#' @examples
#' generate_variance_analysis()


generate_variance_analysis <- function(patient_after_group_dataframe=NULL,
                                       recruit_batch_patient_dataframe_templateId=0,
                                       group_num_ratio,
                                       mongo_conffile_path=NULL,
                                       deploy_plat='linux'){
  library("devR")
  if (is.null(patient_after_group_dataframe))
    patient_after_group_dataframe <- get_data(recruit_batch_patient_dataframe_templateId,
                                              conffile_path = mongo_conffile_path,
                                              deploy_plat = deploy_plat)
  variance_analysis <- c()
  simulation_result_dataframe <- data.frame(check.names = F)
  group_distribution_comparison <- get_group_distribution_comparison(group_num_ratio)
  # 除去"所属组号","实际组号"，其余进行方差检验
  variables <- setdiff(names(patient_after_group_dataframe), c("所属组号","实际组号"))
  # 调用方差检验做均衡性评估
  for (variable in variables){
    # variance_analysis[[variable]] <- variance_result$ks.test_table

    # 生成该variable的模拟表结果，不包括均衡性检验部分
    simulation_result_dataframe1 <-
      generate_variable_simulation_result(variable=variable,
                                          patient_after_group_dataframe=patient_after_group_dataframe,
                                          group_distribution_comparison)
    ncol1 <- ncol(simulation_result_dataframe1)
    # 去掉最后两列，其他进行卡方检验。
    simulation_result_son_matrix <- simulation_result_dataframe1[, c(-ncol1, -(ncol1-1))]
    # simulation_result_son_matrix有小于5的值(卡方检验出现warning)，就要用fisher.test
    test_res = tryCatch({
      #正常的逻辑
      chisq.test(simulation_result_son_matrix)
    }, warning = function(w) {
      #出现warning的处理逻辑
      fisher.test(simulation_result_son_matrix, workspace = 2e8)
    })
    # test_res <- fisher.test(simulation_result_son_matrix, workspace = 2e8)
    # test_res <- chisq.test(simulation_result_son_matrix)
    # asd1 <- chisq.test(matrix(c(2,1,2,3,4,1),nrow=2))
    if (is.null(test_res$statistic)){
      simulation_result_dataframe1[, '统计量'] <- "Fisher's精确概率法"
    }else{
      simulation_result_dataframe1[, '统计量'] <- round(test_res$statistic[[1]], 3)
    }
    simulation_result_dataframe1[, 'P值'] <- round(test_res$p.value,3)
    simulation_result_dataframe1[2:nrow(simulation_result_dataframe1),c('因子', '统计量', 'P值')] <- ''
    # simulation_result_dataframe <- check_bug("simulation_result_dataframe <-
    #                                             rbind(simulation_result_dataframe, simulation_result_dataframe1)",
    #                                          "simulation_result_dataframe")
    simulation_result_dataframe <- rbind(simulation_result_dataframe, simulation_result_dataframe1)
  }
  ncol2 <- ncol(simulation_result_dataframe)
  simulation_result_dataframe <- simulation_result_dataframe[, c(ncol2-2, ncol2-3, 1:(ncol2-4), ncol2-1, ncol2)]
  return(simulation_result_dataframe)
}


#' 使用最小化法进行多个患者的入组操作
#' @param recruited_patient_dataframe 已入组病历数据
#' @param recruit_batch_patient_dataframe 本次执行批量入组的病例数据
#' @param variable_probability 指标权重
#' @param bias_probability 偏置概率
#' @param deploy_plat 部署平台
#' @param allow_diff_between_groups 组间允许例数差值D
#' @param group_distribution_comparison 根据组间比例转换出多个子组，以便子组间病例进行合并能达到符合
#' 组间比例的分组情况,比如1:2:3，会转换出6个子组.
#' @param param_and_values 指标中文名称以及对应的可能取值个数，仅当最小化法入组时才需要
#' @return
#' @export
#' @examples
#' recruit_multi_patients_using_minimize()

recruit_multi_patients_using_minimize <- function(recruited_patient_dataframe,
                                                  recruit_batch_patient_dataframe,
                                                  variable_probability,
                                                  bias_probability,
                                                  deploy_plat,
                                                  allow_diff_between_groups,
                                                  group_distribution_comparison,
                                                  param_and_values){
  library('glue')

  origin_nrow <- nrow(recruited_patient_dataframe)
  # 生成每个区组组号与各因子取值对应关系表。
  # blocknum_with_variable_value_dataframe <- generate_block_num(group_description_info_dataframe)
  for (i in (1:nrow(recruit_batch_patient_dataframe))){
    # 根据选择入组的某一条患者数据，尝试进入每一组，计算进入每一组时的不均衡度。
    recruit_batch_patient_dataframe[i,'所属组号'] <-
      one_patient_ingroup(in_group_data=recruited_patient_dataframe,
                          one_patient_data_dataframe_before_ingroup=recruit_batch_patient_dataframe[i, ],
                          variable_probability=variable_probability,
                          bias_probability=bias_probability,
                          deploy_plat=deploy_plat,
                          allow_diff_between_groups=allow_diff_between_groups,
                          group_distribution_comparison=group_distribution_comparison,
                          param_and_values=param_and_values)
    recruit_batch_patient_dataframe[i,'实际组号'] <- NA
    recruited_patient_dataframe <-
      rbind(recruited_patient_dataframe,
            recruit_batch_patient_dataframe[i, ])
    # recruit_batch_patient_dataframe <- rbind(recruit_batch_patient_dataframe, recruit_batch_patient_dataframe[i, ])

    print(glue("该批病人入组：第{i}人入第{recruit_batch_patient_dataframe[i,'所属组号']}组, 总共{nrow(recruit_batch_patient_dataframe)}人"))
  }
  # if (length(unique(group_distribution_comparison[[1]])) !=
  #     length(unique(group_distribution_comparison[[2]]))){
  #   # 用实际组号替换原所属组号。
  #   recruit_batch_patient_dataframe <- merge(recruit_batch_patient_dataframe,
  #                                            group_distribution_comparison,
  #                                            by.x='所属组号', by.y='基础组号')
  #   # 去掉所属组号
  #   recruit_batch_patient_dataframe <-
  #     recruit_batch_patient_dataframe[, -which(names(recruit_batch_patient_dataframe) %in% c('所属组号'))]
  #   names(recruit_batch_patient_dataframe)[ncol(recruit_batch_patient_dataframe)] <- '所属组号'
  # increment_group_data <- recruit_batch_patient_dataframe[(origin_nrow+1):nrow(recruit_batch_patient_dataframe), ]
  # }
  return(recruit_batch_patient_dataframe)
}


# 参考文献http://www.doc88.com/p-2486357607315.html
#' 最小化法动态随机入组主函数，分为首次和非首次
#' @param recruited_patient_dataframe_templateId 已入组病历数据的数据ID
#' @param recruited_patient_dataframe 已入组病历数据
#' @param group_num_ratio 组间比例
#' @param total_num 用户定义的总样本数
#' @param bias_probability 偏置概率
#' @param variable_probability 指标权重
#' @param recruit_batch_patient_dataframe_templateId 本次执行批量入组的病例数据的数据ID
#' @param recruit_batch_patient_dataframe 本次执行批量入组的病例数据
#' @param deploy_plat 部署平台
#' @param allow_diff_between_groups 组间允许例数差值D
#' @param mongo_conffile_path mongo数据库连接配置文件路径
#' @param param_and_values 指标中文名称以及对应的可能取值个数，仅当最小化法入组时才需要
#' @export
#' @examples
#' main_dynamic_random()


main_dynamic_random <- function(recruited_patient_dataframe_templateId=NULL,
                                recruited_patient_dataframe=NULL,
                                group_num_ratio,
                                total_num,
                                bias_probability,
                                variable_probability,
                                recruit_batch_patient_dataframe_templateId=NULL,
                                recruit_batch_patient_dataframe=NULL,
                                deploy_plat='linux',
                                allow_diff_between_groups,
                                mongo_conffile_path=NULL,
                                param_and_values=NULL){
  library("devR")

  group_distribution_comparison <- get_group_distribution_comparison(group_num_ratio)
  if (is.null(recruit_batch_patient_dataframe))
    recruit_batch_patient_dataframe <-
    get_data(recruit_batch_patient_dataframe_templateId,
             conffile_path = mongo_conffile_path,
             deploy_plat = deploy_plat)
  if (is.null(recruited_patient_dataframe) &&
      !is.null(recruited_patient_dataframe_templateId))
    recruited_patient_dataframe <-
    get_data(recruited_patient_dataframe_templateId,
             conffile_path = mongo_conffile_path,
             deploy_plat = deploy_plat)

  if (!is.null(recruited_patient_dataframe) &&
      nrow(recruited_patient_dataframe) >= total_num){
    errormessage <- linux_or_win(deploy_plat,"{'ErrorMessage':'已入组患者人数已达设置的总样本量'}")
    if (length(grep('ErrorMessage',errormessage))==1){
      return(errormessage)
    }
  }
  # 可用简单随机入组的样本容量
  simple_random_capacity <- ceiling(total_num*0.05)
  # 该次入组完成后，还剩下的可进行简单随机的病例余量（简单随机样本个数不能超过总体的5%）
  remaining_simple_random_capacity_after_recruit <- simple_random_capacity -
    (nrow(recruited_patient_dataframe) + nrow(recruit_batch_patient_dataframe))

  # 入组前，还剩下的可进行简单随机的病例余量
  remaining_simple_random_capacity_before_recruit <- simple_random_capacity -
    nrow(recruited_patient_dataframe)

  # 算上该batch的样本数，剩余可进行简单随机的病例余量还大于0，可以放心进行简单随机入组。
  if (is.null(recruited_patient_dataframe) || remaining_simple_random_capacity_after_recruit >= 0){
    # 前5%的样本按照组间比例使用简单随机算法入组
    data_after_simple_random_recruited <- first_time_enter_group(recruit_batch_patient_dataframe,
                                                                 group_num_ratio)
    # 给简单随机补充一个实际组号列，用于跟最小化法匹配起来
    data_after_simple_random_recruited[['实际组号']] <-
      data_after_simple_random_recruited[['所属组号']]
    return(data_after_simple_random_recruited)
    # 情况①：全部最小化法入组；情况②：一部分简单随机入组，一部分最小化法入组。
  }else if (remaining_simple_random_capacity_before_recruit <= 0){
    # 已入组人数刚好等于可简单随机入组人数的上限，此时剩余样本全部进行最小化入组，执行情况①
    increment_group_data <-
      recruit_multi_patients_using_minimize(recruited_patient_dataframe=recruited_patient_dataframe,
                                            recruit_batch_patient_dataframe=recruit_batch_patient_dataframe,
                                            variable_probability=variable_probability,
                                            bias_probability=bias_probability,
                                            deploy_plat=deploy_plat,
                                            allow_diff_between_groups=allow_diff_between_groups,
                                            group_distribution_comparison=group_distribution_comparison,
                                            param_and_values=param_and_values)
    # 判断组间比例是否非1:1:1:...
    if (length(unique(group_distribution_comparison[[1]])) !=
        length(unique(group_distribution_comparison[[2]]))){
      # 删除掉recruit_multi_patients_using_minimize函数中为了rbind填补缺失的实际组号列
      increment_group_data <- subset(increment_group_data, select = c(-实际组号))

      # 实际组号列才是最终随访人员的真实组号，动态随机算法使用的是所属组号列。
      increment_group_data <- merge(increment_group_data,
                                    group_distribution_comparison,
                                    by.x='所属组号', by.y='基础组号')
    }else {
      increment_group_data[['实际组号']] <- increment_group_data[['所属组号']]
    }
    return(increment_group_data)
  }else{
    # 进入该条件意味着：该次入组前，可简单随机入组数量仍有剩余，但部分样本需通过最小化法入组。
    # browser(condition = (!(remaining_simple_random_capacity_before_recruit > 0 &&
    #           remaining_simple_random_capacity_after_recruit < 0)))

    # assert系检查逻辑用，稳定运行后可删。
    testit::assert("该次入组不满足部分简单随机入组、部分最小化法入组，却进入对应判断条件体中",
                   (remaining_simple_random_capacity_before_recruit > 0 &&
                      remaining_simple_random_capacity_after_recruit < 0))
    # 该batch简单随机入组样本数
    simple_random_num_this_batch <- nrow(recruit_batch_patient_dataframe) -
      remaining_simple_random_capacity_before_recruit
    # 部分样本实行简单随机入组
    data_after_simple_random_recruited <-
      first_time_enter_group(recruit_batch_patient_dataframe[1:simple_random_num_this_batch, ],
                             group_num_ratio)
    # 给简单随机补充一个实际组号列，该实际组号与所属组号相同，用于跟最小化法匹配起来
    data_after_simple_random_recruited[['实际组号']] <-
      data_after_simple_random_recruited[['所属组号']]

    data_after_dynamic_random_recruited <-
      recruit_multi_patients_using_minimize(
        recruited_patient_dataframe=recruited_patient_dataframe,
        recruit_batch_patient_dataframe=
          recruit_batch_patient_dataframe[(simple_random_num_this_batch+1):nrow(recruit_batch_patient_dataframe), ],
        variable_probability=variable_probability,
        bias_probability=bias_probability,
        deploy_plat=deploy_plat,
        allow_diff_between_groups=allow_diff_between_groups,
        group_distribution_comparison=group_distribution_comparison,
        param_and_values=param_and_values)

    # 判断组间比例是否非1:1:1:...，不是就需要合并基础组
    # 如1000：1000：30, 会约分成10：10：3，最终会出现1到23，23个组号，最后按比例合并成1，2，3三个组。
    # 否则则直接用所属组号作为实际组号即可。
    if (length(unique(group_distribution_comparison[[1]])) !=
        length(unique(group_distribution_comparison[[2]]))){
      # 删除掉recruit_multi_patients_using_minimize函数中为了rbind填补缺失的实际组号列
      data_after_dynamic_random_recruited <- subset(data_after_dynamic_random_recruited, select = c(-实际组号))

      # 用实际组号替换原所属组号。
      data_after_dynamic_random_recruited <- merge(data_after_dynamic_random_recruited,
                                                   group_distribution_comparison,
                                                   by.x='所属组号', by.y='基础组号')
      # data_after_dynamic_random_recruited <-
      #   subset(data_after_dynamic_random_recruited, select=-c(所属组号))
      # names(data_after_dynamic_random_recruited)[
      #   ncol(data_after_dynamic_random_recruited)] <- '所属组号'
    }else {
      data_after_dynamic_random_recruited[['实际组号']] <- data_after_dynamic_random_recruited[['所属组号']]
    }
    increment_group_data <- rbind(data_after_simple_random_recruited,
                                  data_after_dynamic_random_recruited)
    return(increment_group_data)
  }
}


# Reference Paper: A. B. Antognini and A. Giovagnoli (2004) A new ’biased coin design’ for the sequential allocation
# of two treatments. Journal of the Royal Statistical Society. Series C (Applied Statistics) 53, No. 4,
# 651-664
#' 动态偏性掷币法入组主函数
#' @param total_num 用户定义的总样本数
#' @param rho 平衡分组的强度，1为最强，0为最弱。介于[0，1]之间
#' @export
#' @examples
#' main_biased_coin_random()


main_biased_coin_random <- function(total_num, rho){
  library("randomizeR")
  random_object <- randomizeR::abcdPar(total_num, rho, groups = LETTERS[1:2])
  seq_object <- genSeq(random_object)
  seq <- getRandList(seq)
  return(seq)
}


# Reference Paper: A. B. Antognini and A. Giovagnoli (2004) A new ’biased coin design’ for the sequential allocation
# of two treatments. Journal of the Royal Statistical Society. Series C (Applied Statistics) 53, No. 4,
# 651-664
#' 动态偏性掷币法入组主函数（临时
#' @param total_num 用户定义的总样本数
#' @param rho 平衡分组的强度，1为最强，0为最弱。介于[0，1]之间
#' @param recruit_batch_patient_dataframe 本批入组病人属性数据带id
#' @param result_last 每次回传的复用历史数据，即上一次的result
#' @export
#' @examples
#' main_biased_coin_random()


main_biased_coin_random_temp <- function(total_num, rho,
                                         recruit_batch_patient_dataframe,
                                         result_last=NULL){
  library("randomizeR")
  result <- c()
  if (is.null(result_last)){
    random_object <- abcdPar(total_num, rho, groups = LETTERS[1:2])
    seq_object <- genSeq(random_object)
    seq <- getRandList(seq_object)
    seq <- as.numeric(as.factor(seq))
    grouping_sequence <- data.frame("所属组号"=seq, "id"=1:length(seq))
  }else{
    history_data <- jsonlite::fromJSON(result_last)
    grouping_sequence <- history_data[['grouping_sequence']]
  }
  selected_group_number_and_id <- grouping_sequence[1:nrow(recruit_batch_patient_dataframe), ]
  recruit_batch_patient_dataframe[, '所属组号'] <-
    selected_group_number_and_id[['所属组号']]
  # 提取出用过的id
  used_id <- selected_group_number_and_id[['id']]
  # 通过id删去grouping_sequence中已用过的组号数据
  grouping_sequence <- grouping_sequence[which(!grouping_sequence[['id']] %in% used_id), ]
  recruit_batch_patient_dataframe
  result[['入组数据']] <- recruit_batch_patient_dataframe
  result[['grouping_sequence']] <- grouping_sequence

  return(result)
}


# Reference Paper: L.J. Wei (1977) A Class of Designs for Sequential
# Clinical Trials. Journal of the American Statistical Association, 72, 382-6.
#' 瓮法入组主函数
#' @param total_num 用户定义的总样本数
#' @param ini 初始瓮中两类各含有的球的数量
#' @param add 每次抽中一个球，就放"add"个属于另一分类的球进瓮。
#' @export
#' @examples
#' main_dynamic_random()


main_urn_random <- function(total_num, ini, add){
  library("randomizeR")
  random_object <- udPar(total_num, ini, add, groups = LETTERS[1:2])
  seq <- genSeq(random_object)
  return(getRandList(seq))
}


# Reference Paper: L.J. Wei (1977) A Class of Designs for Sequential
# Clinical Trials. Journal of the American Statistical Association, 72, 382-6.
#' 瓮法入组主函数(临时)
#' @param total_num 用户定义的总样本数
#' @param ini 初始瓮中两类各含有的球的数量
#' @param add 每次抽中一个球，就放"add"个属于另一分类的球进瓮。
#' @param recruit_batch_patient_dataframe 本批入组病人属性数据带id
#' @param result_last 每次回传的复用历史数据，即上一次的result
#' @export
#' @examples
#' main_urn_random_temp()


main_urn_random_temp <- function(total_num, ini, add,
                                 recruit_batch_patient_dataframe,
                                 result_last=NULL){
  library("randomizeR")
  result <- c()
  if (is.null(result_last)){
    random_object <- udPar(total_num, ini, add, groups = LETTERS[1:2])
    seq_object <- genSeq(random_object)
    seq <- getRandList(seq_object)
    seq <- as.numeric(as.factor(seq))
    grouping_sequence <- data.frame("所属组号"=seq, "id"=1:length(seq))
  }else{
    history_data <- jsonlite::fromJSON(result_last)
    grouping_sequence <- history_data[['grouping_sequence']]
  }
  selected_group_number_and_id <- grouping_sequence[1:nrow(recruit_batch_patient_dataframe), ]
  recruit_batch_patient_dataframe[, '所属组号'] <-
    selected_group_number_and_id[['所属组号']]
  # 提取出用过的id
  used_id <- selected_group_number_and_id[['id']]
  # 通过id删去grouping_sequence中已用过的组号数据
  grouping_sequence <- grouping_sequence[which(!grouping_sequence[['id']] %in% used_id), ]
  recruit_batch_patient_dataframe
  result[['入组数据']] <- recruit_batch_patient_dataframe
  result[['grouping_sequence']] <- grouping_sequence

  return(result)
}


#' 区组随机函数
#' @param group_num_ratio 组间比例
#' @param block_length 用户定义的总样本数
#' @param patient_num_ineach_layer 对应各层所属的病人数，允许只有一层。
#' @param in_patient_dataframe 所有病例数据
#' @param deploy_plat 部署平台
#' @export
#' @examples
#' block_random()

block_random <- function(group_num_ratio,
                         block_length,
                         patient_num_ineach_layer,
                         in_patient_dataframe,
                         deploy_plat){

  # 获取每个小层内随机归属于各个组的随机序列，如group_num_ratio <- c(1,2,3),则son_layer_random_vector=c(1, 2, 2, 3, 3, 3)
  son_layer_random_vector <- c()
  # 生成一个区组块的序列，以便后面直接随机排序可生成各种区组块
  son_layer_random_vector <- rep(1:length(group_num_ratio), group_num_ratio)
  if ((block_length %% sum(group_num_ratio)) != 0){
    errormessage <-linux_or_win(deploy_plat,
                                paste('{"ErrorMessage":"区组长度必须是',
                                      sum(group_num_ratio), '的整数倍"}', sep=''))
    if (length(grep('ErrorMessage',errormessage))==1){
      return(errormessage)
    }
  }

  # 若group_num_ratio <- c(1,2,3),block_length <- 12,那么son_layer_random_vector=rep(c(1, 2, 2, 3, 3, 3), 2),重复两次作为区组长度。
  son_layer_random_vector <- rep(son_layer_random_vector, block_length/sum(group_num_ratio))
  # 随机son_layer_random_vector，用于分配
  for (nrow1 in 1:nrow(patient_num_ineach_layer)){
    # 代表上面62除以c(1, 2, 2, 3, 3, 3)的长度，多出来的病人数，这部分要随机进组。
    excess_patient <- patient_num_ineach_layer[nrow1, 2] %% length(son_layer_random_vector)
    can_loaded_patient_group <- (patient_num_ineach_layer[nrow1, 2] - excess_patient)/
      length(son_layer_random_vector)
    patient_belong_group <- c()
    # 该层内人数比区组长度还小，不需要做下列循环了，直接进行余数处理即可。
    if (patient_num_ineach_layer[nrow1, 2] >= block_length){
      # 生成若干个随机区组，组成随机数
      for (num in 1:can_loaded_patient_group){
        random_num <- sample(son_layer_random_vector, length(son_layer_random_vector))
        patient_belong_group <- c(patient_belong_group, random_num)
      }
    }
    # 层内病例数减去区组长度的n倍，仍有余数才需要对余下的病人做分配。
    if (excess_patient != 0){
      # 余下病人的分组规则按特殊排序规则，具体如group_num_ratio <- c(1,2,4)，则excess_patient_group按照c(3,2,1,3,2,3,3)来取组号
      for_special_group_dataframe <-
        data.frame('组号'=1:length(group_num_ratio),
                   '组对应比例'=group_num_ratio*(block_length/sum(group_num_ratio)),
                   check.names = F)
      for_special_group_dataframe <-
        for_special_group_dataframe[order(for_special_group_dataframe[, '组对应比例'],
                                          decreasing = T), ]
      excess_patient_group <- c()
      for (max_group_ratio in 1:max(for_special_group_dataframe[['组对应比例']])){
        excess_patient_group <- c(excess_patient_group,
                                  for_special_group_dataframe[which(
                                    for_special_group_dataframe[,'组对应比例']>0), '组号'])
        for_special_group_dataframe[, '组对应比例'] = for_special_group_dataframe[, '组对应比例'] - 1
      }
      # 该层所有patient各自所属的组号。
      patient_belong_group <- c(patient_belong_group, excess_patient_group[1:excess_patient])
    }
    in_patient_dataframe[which(in_patient_dataframe[, '所属层号']==
                                 patient_num_ineach_layer[nrow1, 1]), '所属组号'] <-
      patient_belong_group
  }
  return(in_patient_dataframe)
}



#' 分层区组随机主函数,根据设置的区组长度和组间比例，一次性生成所有随机数，供java调取。如果既不指定分层指标也不指定区组长度，即为简单随机。
#' @param group_num_ratio 组间比例
#' @param param_and_values 指定分层指标中文名称以及对应的可能取值个数
#' @param block_length 区组长度，必须是组间比例总和的整数倍，比如三个对照组组间比例是1：2：3，那么区组长度只能是6*n。
#' @param total_num 用户定义的总样本数
#' @param deploy_plat 部署平台
#' @param float_percent 允许上浮百分比，比如0.05表示允许上浮5%，默认是0
#' @param if_use_block 是否使用区组随机
#' @export
#' @examples
#' main_layer_block_random()

main_layer_block_random <- function(group_num_ratio,
                                    param_and_values=NULL,
                                    block_length=NULL,
                                    total_num,
                                    deploy_plat='linux',
                                    float_percent=0,
                                    if_use_block=T){

  # 不设置param_and_values，相当于所有样本都是一个层
  layer_num <- 1
  result <- c()

  # 先将组间比例约分，以缩小区组长度
  group_num_ratio <- group_num_ratio/generate_common_factor(group_num_ratio)

  # 区组长度默认是约分后的组间比例的和
  if(if_use_block && is.null(block_length)) block_length <- sum(group_num_ratio)

  # param_and_values!=NULL,代表至少是分层随机，先连乘算出层数。
  if (!is.null(param_and_values)){
    attr_layer_mapping_table <- data.frame()
    attr_layer_mapping_table_names <- mapply(param_and_values, FUN = function(x) x[1])
    # 属性层号映射表
    attr_layer_mapping_table <- expand.grid(lapply(param_and_values, FUN = function(x) x[-1]))
    names(attr_layer_mapping_table) <- attr_layer_mapping_table_names
    attr_layer_mapping_table[['层号']] <- rownames(attr_layer_mapping_table)
    layer_num <- nrow(attr_layer_mapping_table)
  }
  in_patient_dataframe1 <- data.frame(check.names = F)
  max_sample_nums <- ceiling(total_num*(1+float_percent))
  # 每一个层都要准备max_sample_nums个样本的随机数，以防所有待入组患者全属于某一个层。
  grouping_sequence <-
    data.frame('所属层号'=sample(1:layer_num, size = layer_num*max_sample_nums, replace = T),
               '无用列'=rep(1, max_sample_nums),
               check.names = F) # 无用列为了保持grouping_sequence是一个dataframe

  patient_num_ineach_layer <-
    aggregate(list('对应层所属病人数'=grouping_sequence[['所属层号']]),
              by = list('层号'=grouping_sequence[['所属层号']]), FUN = length)
  #       层号 对应层所属病人数
  # 1      1               62
  # 2      2               69
  # 3      3               46
  # 4      4               52
  if (!is.null(block_length)){
    # browser()
    grouping_sequence <- block_random(group_num_ratio,
                                      block_length,
                                      patient_num_ineach_layer,
                                      grouping_sequence,
                                      deploy_plat)
  }else{
    # 没有设置区组长度，就在每个层内进行简单随机（分层随机）。
    # browser()
    for (layer in patient_num_ineach_layer[['层号']]){
      in_patient_dataframe_specific_layer <-
        grouping_sequence[which(grouping_sequence[,'所属层号']==layer), ]
      in_patient_dataframe1 <- rbind(in_patient_dataframe1,
                                     first_time_enter_group(in_patient_dataframe_specific_layer,
                                                            group_num_ratio))
    }
    grouping_sequence <- in_patient_dataframe1
  }
  # 去掉无用列
  grouping_sequence <- grouping_sequence[order(grouping_sequence[,'所属层号']),
                                         -which(names(grouping_sequence)%in%c("无用列"))]
  grouping_sequence[['id']] <- 1:nrow(grouping_sequence)

  result[['attr_layer_mapping_table']] <- attr_layer_mapping_table
  result[['grouping_sequence']] <- grouping_sequence
  return(result)
}


#' 分层区组随机主函数(java开发完整版中弃用！！！)
#' @param group_num_ratio 组间比例
#' @param param_and_values 指定分层指标中文名称以及对应的可能取值个数
#' @param block_length 区组长度，必须是组间比例总和的整数倍，比如三个对照组组间比例是1：2：3，那么区组长度只能是6*n。
#' @param total_num 用户定义的总样本数
#' @param deploy_plat 部署平台
#' @param float_percent 允许上浮百分比，比如0.05表示允许上浮5%，默认是0
#' @param if_use_block 是否使用区组随机
#' @param recruit_batch_patient_dataframe 本批入组病人属性数据带id
#' @param result_last 每次回传的复用历史数据，即上一次的result
#' @export
#' @examples
#' main_layer_block_random()

main_layer_block_random_temp <- function(group_num_ratio,
                                         param_and_values=NULL,
                                         block_length=NULL,
                                         total_num,
                                         deploy_plat='linux',
                                         float_percent=0,
                                         if_use_block=T,
                                         recruit_batch_patient_dataframe,
                                         result_last=NULL){
  library("devR")
  # colnames in recruit_batch_patient_dataframe must be the same as variable names in param_and_values
  recruit_batch_patient_dataframe_colnames <-
    order(
      as.numeric(
        names(
          subset(recruit_batch_patient_dataframe,
                 select = -c(id))
        )
      )
    )
  variable_names_from_param_and_values <-
    order(
      as.numeric(
        mapply(param_and_values, FUN = function(x) x[1])
      )
    )

  if (!is.null(param_and_values) &&
      !all(recruit_batch_patient_dataframe_colnames ==
           variable_names_from_param_and_values)){
    errormessage <- linux_or_win(deploy_plat,"{'ErrorMessage':'参数 recruit_batch_patient_dataframe
                                 列名与param_and_values 列名对不上'}")
    if (length(grep('ErrorMessage',errormessage))==1){
      return(errormessage)
    }
  }
  # 如果param_and_values是NULL，即不指定分层，则不做上面的列名校验，
  # 以及将param_and_values赋为list(c('1',c(1)))，以表示只有一个层的情况

  if (is.null(param_and_values)){
    param_and_values <- list(c('1',c(1)))
  }

  result <- c()
  # 首次，需要调用算法产生入组序列
  if (is.null(result_last)){

    layer_block_random_result <- main_layer_block_random(group_num_ratio=group_num_ratio,
                                                         param_and_values=param_and_values,
                                                         block_length=block_length,
                                                         total_num=total_num,
                                                         deploy_plat=deploy_plat,
                                                         float_percent=float_percent,
                                                         if_use_block=if_use_block)
    attr_layer_mapping_table <- layer_block_random_result[['attr_layer_mapping_table']]
    grouping_sequence <- layer_block_random_result[['grouping_sequence']]
  }else{
    history_data <- jsonlite::fromJSON(result_last)
    attr_layer_mapping_table <- history_data[['attr_layer_mapping_table']]
    grouping_sequence <- history_data[['grouping_sequence']]
  }

  recruit_batch_patient_dataframe <- merge(recruit_batch_patient_dataframe,
                                           attr_layer_mapping_table)
  recruit_batch_layer_distribution <- table(recruit_batch_patient_dataframe[['层号']])

  # print(nrow(grouping_sequence))
  for (i in 1:length(recruit_batch_layer_distribution)){
    layer <- names(recruit_batch_layer_distribution[i])
    patiens_num <- recruit_batch_layer_distribution[i][[1]]

    # 选中分配给患者的组号
    selected_group_number_and_id <-
      grouping_sequence[which(grouping_sequence[['所属层号']]==layer),
                        c("所属组号","id")][1:patiens_num, ]
    recruit_batch_patient_dataframe[which(recruit_batch_patient_dataframe[['层号']]==layer),
                                    '所属组号'] <-
      selected_group_number_and_id[['所属组号']]
    # 提取出用过的id
    used_id <- selected_group_number_and_id[['id']]
    # 通过id删去grouping_sequence中已用过的组号数据
    grouping_sequence <- grouping_sequence[which(!grouping_sequence[['id']] %in% used_id), ]
  }
  result[['入组数据']] <- recruit_batch_patient_dataframe
  result[['grouping_sequence']] <- grouping_sequence
  result[['attr_layer_mapping_table']] <- attr_layer_mapping_table
  return(result)
}


#' 单元测试动态随机
#' unit_test_dynamic_random()


unit_test_dynamic_random <- function(){

  library("devR")
  # 连续变量转分类变量，分类变量多个区间必须彼此独立，比如这里疾病指标1多个区间彼此独立
  param_and_values <- list(c("年龄", c(1,2,3)),
                           c("性别", c(1,2)),
                           c("是否病1", c(1,2)),
                           c("病2症状", c(1,2,3)),
                           c("疾病指标1", c(1,2,3))) # 从前到后，代表因子的分组顺序，按照年龄最先，疾病指标1最后
  # d1 <- list('字段名'='年龄', '各组具体值'=c('[10,20)','[20,30)','[30,50)'))
  # d2 <- list('字段名'='性别', '各组具体值'=c('男','女'))
  # d3 <- list('字段名'='是否病1', '各组具体值'=c('是','否'))
  # d4 <- list('字段名'='病2症状', '各组具体值'=c('症状1','症状2','症状3'))
  # d5 <- list('字段名'='疾病指标1', '各组具体值'=c('[1.5,7.68)','[7.68,30.12]','(35.68,54.22]'))
  # # 层描述信息
  # group_description_info <- list('d1'=d1, 'd2'=d2, 'd3'=d3, 'd4'=d4, 'd5'=d5)

  # 层描述信息(data.frame版)
  # group_description_info_dataframe <- list('f1'=f1, 'f2'=f2, 'f3'=f3, 'f4'=f4, 'f5'=f5)
  # group_description_info_dataframe <- c(data.frame('年龄'=c('[10,20)','[20,30)','[30,50)')),
  #                                       data.frame('性别'=c('男','女')),
  #                                       data.frame('是否病1'=c('是','否')),
  #                                       data.frame('病2症状'=c('症状1','症状2','症状3')),
  #                                       data.frame('疾病指标1'=c('[1.5,7.68)','[7.68,30.12]','(35.68,54.22]')))


  # length(param_and_values)
  # 组人数比例
  group_num_ratio <- sample(1:4,sample(2:5,1), replace = T)

  # 指标权重
  # variable_probability <- data.frame('指标名'=c('年龄','性别','是否病1','病2症状','疾病指标1'),
  #                                    '权重'=c(1,1,1,1,1))
  variable_probability <- data.frame('指标名'=c('年龄','性别','是否病1','病2症状','疾病指标1'),
                                     '权重'= sample(1:5,5))
  total_num <- 300
  # sample(c(50,100,150,200,250,500,1000,1500),1)
  # test_float_ratio <- 0.05
  bias_probability <- runif(1, min = 0.8, max = 1)
  # runif(1, min = 1/length(group_num_ratio), max = 1)
  # 组间允许例数差值D
  allow_diff_between_groups <- 3

  # print(paste('group_num_ratio:  ', group_num_ratio, sep=''))
  # print(paste('allow_diff_between_groups:  ', allow_diff_between_groups, sep=''))
  # print(paste('bias_probability:  ', bias_probability, sep=''))

  # group_num_ratio <- c(2,4,1,4,1)
  # allow_diff_between_groups <- 15
  # bias_probability <- 0.53309476561844

  # 根据用户选择的配置试跑中央随机算法。
  result <- try_dynamic_random_conf(param_and_values=param_and_values,
                                    group_num_ratio=group_num_ratio,
                                    total_num=total_num,
                                    bias_probability=bias_probability,
                                    variable_probability=variable_probability,
                                    deploy_plat='linux',
                                    allow_diff_between_groups=allow_diff_between_groups,
                                    run_code_method='NUll',
                                    float_percent=0.05)
  return(result)


  # 动态法入组主函数，分为首次和非首次
  # main_dynamic_random(in_group_data=NULL,
  #                     param_and_values,
  #                     group_num_ratio,
  #                     total_num,
  #                     variable_probability,
  #                     in_patient_dataframe,
  #                     if_first=F,
  #                     deploy_plat='linux')

}


#' 单元测试区组随机
#' unit_test_block_random()


unit_test_block_random <- function(){

  library("devR")

  result <- c()
  # 疾病指标1多个区间必须彼此独立
  param_and_values <- list(c("年龄", c(1, 2, 3)),
                           c("性别", c(1, 2))) # 从1到5，代表因子的分组顺序，按照v1先，v5最后
  # d1 <- list('字段名'='年龄', '各组具体值'=c('[10,20)','[20,30)','[30,50)'))
  # d2 <- list('字段名'='性别', '各组具体值'=c('男','女'))
  # d3 <- list('字段名'='是否病1', '各组具体值'=c('是','否'))
  # d4 <- list('字段名'='病2症状', '各组具体值'=c('症状1','症状2','症状3'))
  # d5 <- list('字段名'='疾病指标1', '各组具体值'=c('[1.5,7.68)','[7.68,30.12]','(35.68,54.22]'))
  # # 层描述信息
  # group_description_info <- list('d1'=d1, 'd2'=d2, 'd3'=d3, 'd4'=d4, 'd5'=d5)

  # f1 <- data.frame('年龄'=c('[10,20)','[20,30)','[30,50)'))
  # f2 <- data.frame('性别'=c('男','女'))
  # f3 <- data.frame('是否病1'=c('是','否'))
  # f4 <- data.frame('病2症状'=c('症状1','症状2','症状3'))
  # f5 <- data.frame('疾病指标1'=c('[1.5,7.68)','[7.68,30.12]','(35.68,54.22]'))
  # # 层描述信息(data.frame版)
  # # group_description_info_dataframe <- list('f1'=f1, 'f2'=f2, 'f3'=f3, 'f4'=f4, 'f5'=f5)
  # group_description_info_dataframe <- c(f1, f2, f3, f4, f5)

  # length(param_and_values)
  # 组人数比例
  group_num_ratio <- sample(1:4, sample(2:5, 1), replace = T)

  block_length <- sum(group_num_ratio/generate_common_factor(group_num_ratio))
  total_num <- 500
  float_percent <- 0.05

  # print(paste('group_num_ratio:  ', group_num_ratio, sep=''))
  # print(paste('allow_diff_between_groups:  ', allow_diff_between_groups, sep=''))
  # print(paste('bias_probability:  ', bias_probability, sep=''))

  # group_num_ratio <- c(2,4,1,4,1)
  # allow_diff_between_groups <- 15
  # bias_probability <- 0.53309476561844

  # 根据用户选择的配置试跑区组随机算法。
  for (if_use_block in list(
    # F,
    T)){
    for (param_and_values1 in list(
      # NULL,
      param_and_values)){
      data <- main_layer_block_random(group_num_ratio=group_num_ratio,
                                      param_and_values=param_and_values,
                                      block_length=block,
                                      total_num=total_num,
                                      deploy_plat='win',
                                      float_percent=float_percent,
                                      if_use_block=T)
    }
  }
  result[['data']] <- data
  result[['变量取值情况']] <- param_and_values
  result[['组间比例']] <- group_num_ratio
  return(result)

}

# 区组随机单元测试
# result <- c()
# for (i in 1:1) {
#   result[[as.character(i)]] <- unit_test_block_random()
# }

# 动态随机单元测试
# result <- c()
# for (i in 1:1) {
#   result[[as.character(i)]] <- unit_test_dynamic_random()
# }

# asd <- main_dynamic_random(recruited_patient_dataframe_templateId=NULL,
#                            recruited_patient_dataframe=NULL,
#                            group_num_ratio=c(1000, 1000, 300),
#                            total_num=15,
#                            bias_probability=0.85,
#                            variable_probability=data.frame('指标名'=c('1','2','3','4'),
#                                                            '权重'=c(1,3,3,1000), check.names = F),
#                            recruit_batch_patient_dataframe_templateId=NULL,
#                            recruit_batch_patient_dataframe=
#                              data.frame('1'=c(1524,1523,15235,154,1523,15215),
#                                         '2'=c(1,1,2,1,1,2),'3'=c(1,2,2,2,3,2),'4'=c(1,1,2,1,1,2),
#                                         'id'=c(1124,1131,11214,124,1121,1125414), check.names = F),
#                            deploy_plat='linux',
#                            allow_diff_between_groups=3,
#                            param_and_values=list(c('1',c(1,2,3)),c('2',c(1,2)),c('3',c(1,2,3)),
#                                                  c('4',c(1,2,3,4,5))))


# asd <- unit_test_block_random()

# {"variable_probability":"data.frame('指标名'=c('1','2','3','4'),'权重'=c(1,3,3,1000))",
#   "bias_probability":"0.85","group_num_ratio":"c(1000,1000)",
#   "total_num":"6","param_and_values":"list(c('1',c(1,2,3)),c('2',c(1,2)),c('3',c(1,2,3)),
#   c('4',c(1,2,3,4,5)))","deploy_plat":"win","allow_diff_between_groups":"3",
#   "recruit_batch_patient_dataframe":"data.frame('1'=c(1524,1523,15235,154,1523,15215),
#   '2'=c(1,1,2,1,1,2),'3'=c(1,2,2,2,3,2),'4'=c(1,1,2,1,1,2),'id'=c(1124,1131,11214,124,1121,1125414))"}


# asd <- main_dynamic_random(recruited_patient_dataframe_templateId=NULL,
#                            recruited_patient_dataframe=data.frame('1'=c(1524,1523,15235,154,1523,15215),
#                                                                   '2'=c(1,1,2,1,1,2),'3'=c(1,2,2,2,3,2),
#                                                                   '4'=c(1,1,2,1,1,2),
#                                                                   'id'=c(1124,1131,11214,124,1121,1125414),
#                                                                   '所属组号'=c(1,1,2,2,1,1),
#                                                                   '实际组号'=c(3,1,3,2,1,1), check.names=F),
#                            group_num_ratio=c(1000, 1000, 300),
#                            total_num=15,
#                            bias_probability=0.85,
#                            variable_probability=data.frame('指标名'=c('1','2','3','4'),
#                                                            '权重'=c(1,3,3,1000), check.names = F),
#                            recruit_batch_patient_dataframe_templateId=NULL,
#                            recruit_batch_patient_dataframe=
#                              data.frame('1'=c(1524,1523,15235,154,1523,15215),
#                                         '2'=c(1,1,2,1,1,2),'3'=c(1,2,2,2,3,2),'4'=c(1,1,2,1,1,2),
#                                         'id'=c(1124,1131,11214,124,1121,1125414), check.names = F),
#                            deploy_plat='linux',
#                            allow_diff_between_groups=3,
#                            param_and_values=list(c('1',c(1,2,3)),c('2',c(1,2)),c('3',c(1,2,3)),
#                                 c('4',c(1,2,3,4,5))))

# {"group_num_ratio":"c(30,90,150)","if_use_block":"F","total_num":"270",
#   "param_and_values":"list(c('1',c(1,2)),c('2',c(1,2,3,4,5)))",
#   "deploy_plat":"linux","float_percent":"0",
#   "recruit_batch_patient_dataframe":"data.frame('1'=c(1,2,2,2,1,1,1,1),
#   '2'=c(2,1,1,1,1,1,2,2),'id'=c(4489644,4489641,4489639,4489638,4489629,
#   4489628,4489627,4489626),check.names=F)","block_length":"NULL"}

# asd <- main_layer_block_random_temp(
#   group_num_ratio=c(30,90,150),
#   param_and_values=list(c('1',c(1,2)),c('2',c(1,2,3,4,5))),
#   block_length=NULL,
#   total_num=270,
#   deploy_plat='linux',
#   float_percent=0,
#   if_use_block=F,
#   recruit_batch_patient_dataframe=
#     data.frame('1'=c(1,2,2,2,1,1,1,1),
#                '2'=c(2,1,1,1,1,1,2,2),'id'=c(4489644,4489641,4489639,4489638,4489629,
#                                              4489628,4489627,4489626),check.names=F))

# {"total_num":"100","rho":"0.85","recruit_batch_patient_dataframe":
#     "data.frame('1'=c(1,1,1,1,1,1),'id'=c(1123124,156131,111214,125124,11121,11125414),
#   check.names=F)"}

# asd <- main_biased_coin_random_temp(total_num=100, rho=0.85,
#                                recruit_batch_patient_dataframe=
#                                  data.frame('1'=c(1,1,1,1,1,1),
#                                             'id'=c(1123124,156131,111214,125124,11121,11125414),
#                                             check.names=F),
#                                result_last=NULL)

# {"add":"1","result_last":"'{\"入组数据\":[
#   {\"1\":1,\"所属组号\":1,\"id\":4509106},
#   {\"1\":1,\"所属组号\":1,\"id\":4509103},
#   {\"1\":1,\"所属组号\":1,\"id\":4509094},
#   {\"1\":1,\"所属组号\":1,\"id\":4509093},
#   {\"1\":1,\"所属组号\":1,\"id\":4509083},
#   {\"1\":1,\"所属组号\":1,\"id\":4509075},
#   {\"1\":1,\"所属组号\":1,\"id\":4509074},
#   {\"1\":1,\"所属组号\":1,\"id\":4509073},
#   {\"1\":1,\"所属组号\":2,\"id\":4509056},
#   {\"1\":1,\"所属组号\":1,\"id\":4509054}
#   ],
#   \"grouping_sequence\":[]}'","ini":"1","total_num":"60",
#   "recruit_batch_patient_dataframe":"data.frame('1'=c(1,1,1,1,1,1,1,1,1,1),
#   'id'=c(4509106,4509103,4509094,4509093,4509083,4509075,4509074,
#   4509073,4509056,4509054),check.names=F)"}

# asd <- main_urn_random_temp(total_num=100, ini=1, add=1,
#                             recruit_batch_patient_dataframe=
#                               data.frame('1'=c(1,1,1,1,1,1),
#                                          'id'=c(1123124,156131,111214,125124,11121,11125414),
#                                          check.names=F),
#                             result_last='{\"入组数据\":[
#   {\"1\":1,\"所属组号\":1,\"id\":4509106},
#   {\"1\":1,\"所属组号\":1,\"id\":4509103},
#   {\"1\":1,\"所属组号\":1,\"id\":4509094},
#   {\"1\":1,\"所属组号\":1,\"id\":4509093},
#   {\"1\":1,\"所属组号\":1,\"id\":4509083},
#   {\"1\":1,\"所属组号\":1,\"id\":4509075},
#   {\"1\":1,\"所属组号\":1,\"id\":4509074},
#   {\"1\":1,\"所属组号\":1,\"id\":4509073},
#   {\"1\":1,\"所属组号\":2,\"id\":4509056},
#   {\"1\":1,\"所属组号\":1,\"id\":4509054}
#   ],
#   \"grouping_sequence\":[]}')


