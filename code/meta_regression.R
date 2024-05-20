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
  m.qual.rep <- rma(yi =  SMD_first_mean_last, 
                    sei = SMD_first_var_last, 
                    data = smd_nft_fromFirst_f_out, 
                    method = "ML", 
                    mods = ~ training.dur.min, 
                    test = "z")
  
  m.qual.rep
  
  m.gen.reg <- metareg(m.gen_outX, ~rehearsal)
  bubble(m.gen.reg, studlab = TRUE)
  
  # or use anova to compare between full and reduced models
  levels(MVRegressionData$continent) = c("Europe", "North America")
  
  qual <- rma(yi = yi,
              sei = sei,
              data = MVRegressionData,
              method = "ML",
              mods = ~ quality,
              test = "knha")
  
  m.qual
  
  ##steps
  #verify all variables are complete 
  #change words to numbers, then add factor level labels
  #run multimodel inference to test which variables should stay
  #plot relationships of interest 
  #clean for Samantha
  #what should be done with feedback and ROI variables