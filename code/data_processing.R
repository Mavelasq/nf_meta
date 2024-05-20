## RUN PREPROCESSING OF DATA##
  # Steps:
    # filter out rejected papers
    # convert individual studies' SE to SD
    # calculate standardized mean difference (using Hedge's g since studies overall have low sample sizes) separately for difference between:  
      # last training session and first training session
      # last resting state session (post training) and baseline (pre training) 
    # multiply -1 times the SMD in studies where the task is to "decrease" activity. This is done so that SMD 
      # indicates direction of predicted effect (positive SMD means training resulted in change in the predicted 
      # direction while negative SMD means it occured in the opposite direction)
    # calculate pooled effect using metagen
    # determine outliers that contribute disproportionately to heterogeneity
    

# Load necessary libraries
  library(tidyverse)
  library(readxl)
  library(dmetar)
  library(meta)
  library(metafor)
  library(esc)

# Set working directory
  setwd("D:/Documents/Projects/NF_Meta Analysis/analysis/nf_meta")

# Load data set
  #nf_data <- read_excel("data/nf_data.xlsx", sheet = 1)
  nf_data <- read.csv("data/nf_data.csv")

# Preprocess data: Filter out rejected papers and convert SE to SD
  nf_data_filtered <- nf_data %>%
    filter(reject == 0) %>%
    calc_sd()

# Initialize output dataframes for various comparisons and metrics
  smd_nft_fromBase  <- initialize_dfOut(nf_data_filtered, "base", "nft", "SMD")
  smd_nft_fromFirst <- initialize_dfOut(nf_data_filtered, "first", "nft", "SMD")

# Calculate Standard Mean Differences (SMD) or Percent Signal Change (PSC)
  # Calculate standard error (impute r to .08 according to Gnambs, 2023)
  # Formula for SE SMD as in Borenstein, 2009 (Introduction to Meta Analysis)
  # Formula for SD pooled and Hedge's g as in Harrer, 2021 (Doing Meta-Analysis with R: A Hands-On Guide)
  
  smd_nft_fromBase   <- outcomeCalc(nf_data_filtered, "smd", smd_nft_fromBase, "base", "train") # Customize parameters as needed
  smd_nft_fromFirst  <- outcomeCalc(nf_data_filtered, "smd", smd_nft_fromFirst, "first", "train") # Customize parameters as needed
  #smd_rest_fromBase <- outcomeCalc(nf_data_filtered, "smd", smd_rest_fromBase, "base", "rest") # Customize parameters as needed

# multiply es of "decrease" studies by -1 so that they are pointing in the direction of the prediction
  smd_nft_fromBase   <- switch.direction(smd_nft_fromBase)
  smd_nft_fromFirst  <- switch.direction(smd_nft_fromFirst)
  #smd_rest_fromBase <- switch.direction(smd_rest_fromBase)
  
# add variables and filter out NAs 
  smd_nft_fromFirst  <- cbind(smd_nft_fromFirst, nf_data_filtered[,c(2,3,11,14:17,19,31,37:40,42,49)])
  smd_nft_fromBase   <- cbind(smd_nft_fromBase, nf_data_filtered[,c(2,3,11,14:17,19,31,37:40,42,49)])
  #smd_rest_fromBase <- cbind(smd_nft_fromFirst, nf_data_filtered[,c(2,3,11,14:17,19,31,37:40,42,49)])
  
  smd_nft_fromFirst_f <- filter(smd_nft_fromFirst, !is.na(SMD_first_mean_last))
  smd_nft_fromBase_f  <- filter(smd_nft_fromBase, !is.na(SMD_base_mean_post))
  
# get pooled effect
  m.gen <- metagen(TE = SMD_first_mean_last,
                   seTE = SMD_first_var_last,
                   studlab = author_n,
                   data = smd_nft_fromFirst_f,
                   sm = "Hedge's g",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   method.random.ci = "HK",
                   prediction = TRUE,
                   title = "Neurofeedback")
  summary(m.gen)

# Outlier rejection due to high heterogeneity (I^2 > 50%)
  #study 33 (vanson_2) will be excluded as an outlier
  #sensitivity analysis will be performed with and without 33
  m.rma <- rma(yi = m.gen$TE,
               sei = m.gen$seTE,
               method = m.gen$method.tau,
               test = "knha")
  res.gosh <- gosh(m.rma)
  
  plot(res.gosh, alpha = 0.01)
  
  res.gosh.diag <- gosh.diagnostics(res.gosh, 
                                    km.params = list(centers = 2),
                                    db.params = list(eps = 0.08, 
                                                     MinPts = 50))
  res.gosh.diag
  
  plot(res.gosh.diag)
  
  m.gen_outX <- update(m.gen, exclude = c(33)) #update model without outlier

# Forrest plot of pooled effect size post-outlier removal
  m.gen_outX
  forest(m.gen_outX)
  
  pdf(file='results/nf_plot_outX.pdf', width=10, height=12)
  forest(m.gen_outX, sortvar=author_n, fixed=FALSE, random=TRUE, lty.random=2, layout="meta", leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("g [95% CI]"), print.tau2=FALSE, 
         bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue")
  dev.off()
  
# Functions ####
# Function to convert standard error to standard deviation
  calc_sd <- function(nf_data) {
  idx_start <- which(colnames(nf_data) == "mean_pre")
  idx_end <- ncol(nf_data)
  
  # Convert columns to numeric
  nf_data[idx_start:idx_end] <- as.data.frame(lapply(nf_data[, idx_start:idx_end], as.numeric))
  nf_data$n <- as.numeric(nf_data$n)
  
  se_idxs <- which(nf_data$var_measure == "SE")
  idx_var <- which(grepl("var", colnames(nf_data)[idx_start:idx_end])) + (idx_start - 1)
  
  for (p in se_idxs) {
    for (s in idx_var) {
      nf_data[p, s] <- ifelse(is.na(nf_data[p, s]), NA, nf_data[p, s] * sqrt(nf_data$n[p]))
      nf_data$var_measure[p] <- "SD"
    }
  }
  return(nf_data)
}

# Function to initialize output data frames for SMD or PSC
  initialize_dfOut <- function(nf_data, firstValue, nftOrBase, outcome) {
  idx_start_tr <- ifelse(nftOrBase == "nft" && firstValue == "base", which(colnames(nf_data) == "mean_first"), which(colnames(nf_data) == "mean_last"))
  idx_end_tr <- which(colnames(nf_data) == "var_ses_15")
  idx_start_bl <- which(colnames(nf_data) == "mean_ses_bl_2")
  idx_end_bl <- which(colnames(nf_data) == "var_ses_bl_10")
  
  out <- if (nftOrBase == "nft") {
    nf_data[idx_start_tr:idx_end_tr]
  } else {
    nf_data[idx_start_bl:idx_end_bl]
  }
  
  colnames(out) <- paste(outcome, firstValue, colnames(out), sep = "_")
  out[,] <- NA
  return(out)
}

# multiply SMD of "decrease" studies by -1 so that they are pointing in the direction of the prediction
  switch.direction <- function(dat){
    dat_neg      <- dat
    down_studies <- which(nf_data_filtered$direction ==0) #indices of studies where the task was to decrease activity
    mean_idx     <- grep("mean",colnames(dat))  #indices of "mean" columns
    dat_neg[down_studies, mean_idx] <- dat[down_studies, mean_idx]*-1 #multiple "decrease" studies effect sizes by -1
    return(dat_neg)
  }
  
# Calculate Standard Mean Differences (SMD) or Percent Signal Change (PSC)
  outcomeCalc <- function(nf_data_filtered, outputCalc, out_data, firstValue, trainOrRest){
  # Determine indices for training or rest data
  idx_start_tr <- ifelse(firstValue == "first", which(colnames(nf_data_filtered) == "mean_first"), which(colnames(nf_data_filtered) == "mean_pre"))
  idx_end_tr   <- which(colnames(nf_data_filtered) == "var_ses_15")
  idx_start_bl <- which(colnames(nf_data_filtered) == "mean_ses_bl_2")
  idx_end_bl   <- which(colnames(nf_data_filtered) == "var_ses_bl_10")
  idx_end      <- ncol(nf_data_filtered)
  
  impute_r     <- .8 #correlation coefficient to impute in SE SMD formula
  
  # Convert df columns to numeric
  #nf_data_filtered[, idx_start_tr:idx_end] <- as.data.frame(lapply(nf_data_filtered[, idx_start_tr:idx_end], as.numeric))
  #nf_data_filtered$n <- as.numeric(nf_data_filtered$n)
  
  # Determine paper indices based on input
  ids <- if (firstValue == "first") which(!is.na(nf_data_filtered$mean_first)) else which(!is.na(nf_data_filtered$mean_pre))
  
  firstCol_mean <- if (firstValue == "first") "mean_first" else "mean_pre"
  firstCol_var  <- if (firstValue == "first") "var_first" else "var_pre"
  
  #offset <- ifelse(trainOrRest == "rest", which(colnames(nf_data_filtered) == "mean_ses_bl_2") - 1,
  #                 which(colnames(nf_data_filtered) == "mean_first") - 1)
  
  offset <- if (firstValue == "first") {which(colnames(nf_data_filtered) == "mean_last")-1
  } else if (trainOrRest == "rest" & firstValue == "base"){which(colnames(nf_data_filtered) == "mean_ses_bl_2")-1
  } else if (firstValue == "base"){which(colnames(nf_data_filtered) == "mean_first")-1}
  
  # Determine column indices for means and variances
  idx_mean <- if (trainOrRest == "train") {
    which(grepl("mean", colnames(nf_data_filtered)[(idx_start_tr+2):idx_end_tr])) + (idx_start_tr+1)
  } else {
    which(grepl("mean", colnames(nf_data_filtered)[idx_start_bl:idx_end_bl])) + (idx_start_bl-1)
  }
  
  idx_sd <- if (trainOrRest == "train") {
    which(grepl("var", colnames(nf_data_filtered)[(idx_start_tr+2):idx_end_tr])) + (idx_start_tr+1)
  } else {
    which(grepl("var", colnames(nf_data_filtered)[idx_start_bl:idx_end_bl])) + (idx_start_bl-1)
  }
  
  # Calculate SMD or PSC
  for (p in ids) {
    idxs_mean <- idx_mean[which(!is.na(nf_data_filtered[p, idx_mean]))]
    idxs_sd   <- idx_sd[which(!is.na(nf_data_filtered[p, idx_sd]))]
    
    if (outputCalc == "smd") { #get hedge's g
      for (s in idxs_sd) {
        sd_pooled <- sqrt((nf_data_filtered[p, firstCol_var]^2 + nf_data_filtered[p, s]^2) / 2)
        for (i in idxs_mean) {
          col_index <- (i - offset)  # Ensure this does not exceed the number of columns in out_data
          if (col_index <= ncol(out_data)) {
            d   <- (nf_data_filtered[p, i] - nf_data_filtered[p, firstCol_mean]) / sd_pooled #compute d
            j   <- 1 - (3/(4 * (nf_data_filtered$n[p] - 1)-1)) #compute correction factor for Hedge's g
            out_data[p, col_index]   <- d*j #hedge's g
            v <- ((2*(1-impute_r))/nf_data_filtered$n[p])+(out_data[p, col_index]^2/(2*nf_data_filtered$n[p])) #compute variance. Weights will be computed later on
            out_data[p, col_index+1] <- (j^2)*sqrt(v) #hedge's g variance
          }
        }
      }
    }
    else if (outputCalc == "smd1") { # get glass's delta
      for (s in idxs_sd) {
        for (i in idxs_mean) {
          col_index <- (i - offset)  # Ensure this does not exceed the number of columns in out_data
          if (col_index <= ncol(out_data)) {
            out_data[p, col_index] <- (nf_data_filtered[p, i] - nf_data_filtered[p, firstCol_mean]) / nf_data_filtered[p, firstCol_var]
            out_data[p, col_index+1] <- sqrt(((2*(1-impute_r))/nf_data_filtered$n[p])+(out_data[p, col_index]^2/(2*nf_data_filtered$n[p])))
            
          }
        }
      }
    }
    else if (outputCalc == "psc") {
      for (i in idxs_mean) {
        col_index <- (i - offset)
        if (col_index <= ncol(out_data)) {
          out_data[p, col_index] <- (nf_data_filtered[p, i] - nf_data_filtered[p, firstCol_mean]) / nf_data_filtered[p, firstCol_mean]
        }
      }
      for (s in idxs_sd) {
        col_index <- (s - offset - 1)
        if (col_index <= ncol(out_data)) {
          nf_data_filtered[p, col_index] <- sqrt((nf_data_filtered[p, firstCol_var]^2 + nf_data_filtered[p, s]^2 - 2 * 0.5 * nf_data_filtered[p, firstCol_var] * nf_data_filtered[p, s]))
        }
      }
    }
  }
  return(out_data)
}

  