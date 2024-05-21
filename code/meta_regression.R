#multiple meta regresssion 
  library(PerformanceAnalytics)
  library(metafor)
#check for correlation between variables to see check for multicollinearity
  #if correlation occurs (>=.08), you can remove redundant variables
  #none were found
  
  #filter out vanson_2
  smd_nft_fromFirst_f_out <- filter(smd_nft_fromFirst_f, !author_n == "vanSon_2")
  
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
                       predictors = c("year", "blinding", "update.timing", "training.dur.min",
                                      "device", "instruction_yn", "motivation_yn", "rehearsal", "func.loc",
                                      "QA_Score", "ratio_yn", "direction"),
                       interaction = FALSE)
  
#run meta regression on training duration
  m.gen.reg <- metareg(m.gen_outX, ~training.dur.min)
  bubble(m.gen.reg, studlab = FALSE)
  
  m.gen.reg_qa <- metareg(m.gen_outX, ~QA_Score)
  bubble(m.gen.reg_qa, studlab = FALSE)
