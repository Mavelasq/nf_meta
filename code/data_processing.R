# Load necessary libraries
  library(tidyverse)
  library(readxl)
  library(dmetar)
  library(meta)
  library(esc)

# Set working directory
  setwd("D:/Documents/Projects/NF_Meta Analysis/analysis/nf_meta")

# Load data set
  nf_data <- read_excel("data/nf_data.xlsx", sheet = 1)

# Preprocess data: Filter out rejected papers and convert SE to SD
  nf_data_filtered <- nf_data %>%
    filter(reject == 0) %>%
    calc_sd()

# Initialize output dataframes for various comparisons and metrics
  smd_nft_fromBase = initialize_dfOut(nf_data_filtered, "base", "nft", "SMD")
  smd_nft_fromFirst = initialize_dfOut(nf_data_filtered, "first", "nft", "SMD")
  smd_rest_fromBase = initialize_dfOut(nf_data_filtered, "base", "rest", "SMD")

# Calculate Standard Mean Differences (SMD) or Percent Signal Change (PSC)
# Calculate standard error (impute r to .08 according to Gnambs, 2023)
# Formula for SE SMD as in Borenstein, 2009 (Introduction to Meta Analysis)
# Formula for SD pooled as in Harrer, 2021 (Doing Meta-Analysis with R: A Hands-On Guide)
  
  smd_nft_fromBase  <- outcomeCalc(nf_data_filtered, "smd", smd_nft_fromBase, "base", "train") # Customize parameters as needed
  smd_nft_fromFirst <- outcomeCalc(nf_data_filtered, "smd", smd_nft_fromFirst, "first", "train") # Customize parameters as needed
  smd_rest_fromBase <- outcomeCalc(nf_data_filtered, "smd", smd_rest_fromBase, "base", "rest") # Customize parameters as needed

# get pooled avg
  m.gen <- metagen(TE = SMD_first_mean_last,
                   seTE = SMD_first_var_last,
                   studlab = nf_data_filtered$author_n,
                   data = smd_nft_fromFirst,
                   sm = "g",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   method.random.ci = "HK",
                   title = "Neurofeedback")
  
# weights first?
# Calculate overall weighted average
  #get weights for each study and multiply times SMD
  #then get average weighted smd (4.2 and 4.3)

# Functions ####
# Function to convert standard error to standard deviation
calc_sd <- function(nf_data) {
  idx_start <- which(colnames(nf_data) == "mean_pre")
  idx_end <- ncol(nf_data)
  
  # Convert columns to numeric
  nf_data[, idx_start:idx_end] <- as.data.frame(lapply(nf_data[, idx_start:idx_end], as.numeric))
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

outcomeCalc <- function(nf_data_filtered, outputCalc, out_data, firstValue, trainOrRest){
  # Determine indices for training or rest data
  idx_start_tr <- ifelse(firstValue == "first", which(colnames(nf_data_filtered) == "mean_first"), which(colnames(nf_data_filtered) == "mean_pre"))
  idx_end_tr   <- which(colnames(nf_data_filtered) == "var_ses_15")
  idx_start_bl <- which(colnames(nf_data_filtered) == "mean_ses_bl_2")
  idx_end_bl   <- which(colnames(nf_data_filtered) == "var_ses_bl_10")
  idx_end      <- ncol(nf_data_filtered)
  
  impute_r     <- .8 #correlation coefficient to impute in SE SMD formula
  
  # Convert df columns to numeric
  nf_data_filtered[, idx_start_tr:idx_end] <- as.data.frame(lapply(nf_data_filtered[, idx_start_tr:idx_end], as.numeric))
  nf_data_filtered$n <- as.numeric(nf_data_filtered$n)
  
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
            v <- ((2*(1-impute_r))/nf_data_filtered$n[p])+(out_data[p, col_index]^2/(2*nf_data_filtered$n[p])) #compute variance. Weights will be computed later on
            out_data[p, col_index]   <- d*j #hedge's g
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
