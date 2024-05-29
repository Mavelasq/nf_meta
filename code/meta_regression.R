#multiple meta regresssion 
  library(PerformanceAnalytics)
  library(metafor)
#check for correlation between variables to see check for multicollinearity
  #if correlation occurs (>=.08), you can remove redundant variables
  #none were found
  
  #filter out vanson_2
  smd_nft_fromFirst_f_out <- filter(smd_nft_fromFirst_f, !author_n == "vanSon_2")
  smd_nft_fromBase_f_out  <- filter(smd_nft_fromBase_f, !author_n == "Maszczyk")
  
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
  m.gen.reg <- metareg(m.gen_outX, ~training.dur.min)
  bubble(m.gen.reg, studlab = FALSE, xlab = "Total Training Duration (min)", ylab = "Standardized Mean Difference (Hedge's g)")
  
  #from baseline
  m.gen.reg_base <- metareg(m.gen_base_outX, ~training.dur.min)
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
  nf_first_long <- read.csv("data/smd_long.csv")
  
  m.gen_first_long <- metagen(TE = SMD,
                   seTE = SE,
                   studlab = author_n,
                   data = nf_first_long,
                   sm = "Hedge's g",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   method.random.ci = "HK",
                   prediction = TRUE,
                   title = "Neurofeedback")
  summary(m.gen)
  
  reg.t_since_first <- metareg(m.gen_first_long, ~train_amount_since_first)
  bubble(reg.t_since_first, studlab = FALSE, xlab = "Time Since First", ylab = "Standardized Mean Difference (Hedge's g)")
  