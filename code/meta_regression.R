#multiple meta regresssion 
  library(PerformanceAnalytics)
  library(metafor)
#check for correlation between variables to see check for multicollinearity
  #if correlation occurs (>=.08), you can remove redundant variables
  #none were found
  
  #filter out vanson_2
  smd_nft_fromFirst_f_out <- filter(smd_nft_fromFirst_f, !author_n == "vanSon_2")
  #smd_nft_fromBase_f_out  <- filter(smd_nft_fromBase_f, !author_n == "Maszczyk")
  
  write.csv(smd_nft_fromFirst, file = "results/smd_fromFirst1.csv")
  write.csv(smd_nft_fromBase,  file = "results/smd_fromBase1.csv")
  
  #convert to numeric
  smd_nft_fromFirst_f_out[,c("year", "blinding", "update.timing", "training.dur.min",
                         "device", "instruction_yn", "motivation_yn", "rehearsal", "func.loc",
                         "QA_Score", "ratio_yn", "direction")] <- as.data.frame(lapply(smd_nft_fromFirst_f_out[,c("year", "blinding", "update.timing", "training.dur.min",
                                                                            "device", "instruction_yn", "motivation_yn", "rehearsal", "func.loc",
                                                                            "QA_Score", "ratio_yn", "direction")], as.numeric))
  
  smd_nft_fromFirst_f_out[,c("year", "blinding", "update.timing", "training.dur.min",
                        "device", "instruction_yn", "motivation_yn", "rehearsal", "func.loc",
                        "QA_Score", "ratio_yn", "direction")] %>% cor()
  
  smd_nft_fromFirst_f_out[,c("year", "blinding", "update.timing", "training.dur.min",
                         "device", "instruction_yn", "motivation_yn", "rehearsal", "func.loc",
                         "QA_Score", "ratio_yn", "direction")]%>% 
      chart.Correlation()

#use multi-model inference to select variables that will go in the model
  multimodel.inference(TE = "SMD_first_mean_last", 
                       seTE = "SMD_first_var_last",
                       data = smd_nft_fromFirst_f_out,
                       predictors = c("year", "blinding", "training.dur.min",
                                      "device", "instruction_yn", "rehearsal", "func.loc",
                                      "QA_Score"),
                       interaction = FALSE)
  
  multimodel.inference(TE = "SMD_base_mean_post", 
                       seTE = "SMD_base_var_post",
                       data = smd_nft_fromBase_f_out,
                       predictors = c("year", "blinding", "training.dur.min","QA_Score"),
                       interaction = FALSE)
  
  
#run meta regression on training duration
  #duration
  #from first training
  m.gen.reg <- metareg(m.gen_outX, ~training.dur.min.log)
  bubble(m.gen.reg, studlab = FALSE, xlab = "Total Training Duration (min)", ylab = "Standardized Mean Difference (Hedge's g)")
  
  #from baseline
  m.gen.reg_base <- metareg(m.gen_base, ~training.dur.min.log)
  bubble(m.gen.reg_base, studlab = FALSE, xlab = "Total Training Duration (min)", ylab = "Standardized Mean Difference (Hedge's g)")
  
  #QA
  #from first training
  m.gen.reg_qa <- metareg(m.gen_outX, ~QA_Score)
  bubble(m.gen.reg_qa, studlab = FALSE, xlab = "Quality Score", ylab = "Standardized Mean Difference (Hedge's g)")

  #from first baseline
  m.gen.reg_qa_base <- metareg(m.gen_base_outX, ~QA_Score)
  bubble(m.gen.reg_qa, studlab = FALSE, xlab = "Quality Score", ylab = "Standardized Mean Difference (Hedge's g)")
  
  #age
  #from base
  smd_nft_fromBase_f$age.mean <- as.numeric(smd_nft_fromBase_f$age.mean)
  age.exclude_base <- c(which(is.na(smd_nft_fromBase_f$age.mean)), 9)
  
  m.gen_outX_age_base <- update(m.gen_base, exclude = age.exclude_base) #update model without outlier
  summary(m.gen_outX_age)
  
  m.gen.reg_age_base <- metareg(m.gen_outX_age_base, ~age.mean)
  bubble(m.gen.reg_qa, studlab = FALSE, xlab = "Quality Score", ylab = "Standardized Mean Difference (Hedge's g)")
  
  #from first training
  smd_nft_fromFirst_f$age.mean <- as.numeric(smd_nft_fromFirst_f$age.mean)
  age.exclude_first <- c(which(is.na(smd_nft_fromFirst_f$age.mean)), 33)
  
  m.gen_outX_age_first <- update(m.gen, exclude = age.exclude_first) #update model without outlier
  summary(m.gen_outX_age_first)
  
  m.gen.reg_age <- metareg(m.gen_outX_age_first, ~age.mean)
  bubble(m.gen.reg_qa, studlab = FALSE, xlab = "Quality Score", ylab = "Standardized Mean Difference (Hedge's g)")
  
  
  # time since first and amount of training
  nf_first_long <- read.csv("data/smd_long_first.csv")
  nf_base_long  <- read.csv("data/smd_long_base.csv")
  
  # load short data sets
  nf_first_short <- read.csv("data/short_from_first.csv") 
  nf_base_short  <- read.csv("data/short_from_base.csv") 
  
  #filter out vanSon_2
  nf_first_short <- filter(nf_first_short, author_n != "vanSon_2")
  nf_first_long  <- filter(nf_first_long, author_n != "vanSon_2")
  
  nf_first_long_n$author_n %in%  nf_first_short$author_n #check for match
  
  # order variables
  nf_first_short <- nf_first_short[order(nf_first_short$author_n),]
  nf_first_long  <- nf_first_long[order(nf_first_long$author_n),]
  
  # add other variables
  nf_first_long_n <- nf_first_long %>% #get number of repetitions per paper
  group_by(author_n) %>%
    summarise(n = n())
  
  toRep_first <- nf_first_long_n$n 
  
  #run function to add column
  nf_first_long$roi_chan_num <- repeat_values(nf_first_short, "roi_chan_num", toRep_first)

  # add columns with repeated values
  repeat_values <- function(dat_short, variable, toRep_first) {
    # Ensure that the variable is used as a column name within the dataframe
    rep_col <- rep(dat_short[[variable]], toRep_first)
    return(rep_col)
  }
  
  # add log transformed columns
  nf_base_long <- filter(nf_base_long, !is.na(time_since_first))
  nf_base_long$log.time_since_first          <- log(nf_base_long$time_since_first)
  nf_base_long$log.train_amount_since_first  <- log(nf_base_long$train_amount_since_first)
  
  nf_first_long$log.time_since_first         <- log(nf_first_long$time_since_first)
  nf_first_long$log.train_amount_since_first <- log(nf_first_long$train_amount_since_first)
  
  #filter out post + transfer trials
  trials_to_filter <- c("post", "post_2", "transfer")
  nf_first_long_f  <- filter(nf_first_long, !trial_num %in% trials_to_filter)
  nf_base_long_f   <- filter(nf_base_long, !trial_num %in% trials_to_filter)
  
  #as numeric
  nf_base_long_f$trial_num  <- as.numeric(nf_base_long_f$trial_num)
  nf_first_long_f$trial_num <- as.numeric(nf_first_long_f$trial_num)
  nf_first_long_f$trial_num <- as.numeric(nf_first_long_f$trial_num)
  
  
  #with meta####
  m.gen_first_long <- metagen(TE = SMD,
                   seTE = SE,
                   studlab = author_n,
                   data = nf_first_long_f,
                   sm = "Hedge's g",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   method.random.ci = "HK",
                   prediction = TRUE,
                   cluster = author_n,
                   title = "Neurofeedback")
  summary(m.gen_first_long)
  
  m.gen_base_long <- metagen(TE = SMD,
                              seTE = SE,
                              studlab = author_n,
                              data = nf_base_long_f,
                              sm = "Hedge's g",
                              fixed = FALSE,
                              random = TRUE,
                              method.tau = "REML",
                              method.random.ci = "HK",
                              prediction = TRUE,
                              title = "Neurofeedback")
  summary(m.gen_base_long)
  
  # 3 level meta regression 
  ## since first training trial
  reg.train_since_first_int     <- metareg(m.gen_first_long, ~log.train_amount_since_first * log.time_since_first)
  reg.train_since_first.log <- metareg(m.gen_first_long, ~log.train_amount_since_first)
  reg.t_since_first         <- metareg(m.gen_first_long, ~time_since_first)
  reg.t_since_first.log     <- metareg(m.gen_first_long, ~log.time_since_first)
  
  ##plots
  bubble(reg.train_since_first, studlab = FALSE, xlab    = "Amount of Training Since First Training Trial (min)", ylab = "Standardized Mean Difference (Hedge's g)")
  bubble(reg.train_since_first_int, studlab = FALSE, pch = 1, xlab = "log (Amount of Training Since First Training Trial)", ylab = "Standardized Mean Difference (Hedge's g)")
  bubble(reg.t_since_first, studlab = FALSE, xlab    = "Time Since First Training Trial (days)", ylab = "Standardized Mean Difference (Hedge's g)")
  bubble(reg.t_since_first.log, studlab = FALSE, xlab    = "log (Time Since First Training Trial)", ylab = "Standardized Mean Difference (Hedge's g)")
  
  ## since baseline
  reg.train_since_base_int     <- metareg(m.gen_base_long, ~log.train_amount_since_first * log.time_since_first)
  reg.train_since_base     <- metareg(m.gen_base_long, ~train_amount_since_first)
  reg.train_since_base.log <- metareg(m.gen_base_long, ~log.train_amount_since_first)
  reg.t_since_base         <- metareg(m.gen_base_long, ~time_since_first)
  reg.t_since_base.log     <- metareg(m.gen_base_long, ~log.time_since_first)
  
  bubble(reg.train_since_base, studlab = FALSE, xlab    = "Amount of Training Since Baseline (min)", ylab = "Standardized Mean Difference (Hedge's g)")
  bubble(reg.train_since_base.log, studlab = FALSE, xlab = "log (Amount of Training Since Baseline)", ylab = "Standardized Mean Difference (Hedge's g)")
  bubble(reg.t_since_base, studlab = FALSE, xlab    = "Time Since Baseline (days)", ylab = "Standardized Mean Difference (Hedge's g)")
  bubble(reg.t_since_base.log, studlab = FALSE, xlab    = "log (Time Since Baseline)", ylab = "Standardized Mean Difference (Hedge's g)")
  
  
  # with metafor
  full.model <- rma.mv(yi = SMD, 
                       V = SE, 
                       slab = author_n,
                       data = nf_first_long_f,
                       random = ~ 1 | author_n/es.id, 
                       test = "t", 
                       method = "REML",
                       mods = ~ log.train_amount_since_first)
  summary(full.model)
  
  i2 <- var.comp(full.model)
  summary(i2)
  
  regplot(full.model, xlab="Absolute Latitude")

  
  #histograms
  hist(nf_first_long_f$time_since_first)
  hist(nf_first_long_f$train_amount_since_first)
  
  hist(nf_base_long_f$time_since_first)
  hist(nf_base_long_f$train_amount_since_first)
  
  
  