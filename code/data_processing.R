# Load necessary libraries
library(tidyverse)
library(readxl)

# Set working directory
setwd("D:/Documents/Projects/NF_Meta Analysis/analysis/nf_meta")

# Load data set
nf_data <- read_excel("data/nf_data.xlsx", sheet = 1)

# Preprocess data: Filter out rejected papers and convert SE to SD
nf_data_filtered <- nf_data %>%
  filter(reject == 0) %>%
  calc_sd()

# Initialize output dataframes for various comparisons and metrics
outputs <- list(
  smd_nft_fromBase = initialize_dfOut(nf_data_filtered, "base", "nft", "SMD"),
  smd_nft_fromFirst = initialize_dfOut(nf_data_filtered, "first", "nft", "SMD"),
  smd_rest_fromBase = initialize_dfOut(nf_data_filtered, "base", "rest", "SMD"),
  smd_rest_fromFirst = initialize_dfOut(nf_data_filtered, "first", "rest", "SMD")
)

# Calculate Standard Mean Differences (SMD) or Percent Signal Change (PSC)
outcome_results <- lapply(outputs, function(out_data) {
  outcomeCalc(nf_data_filtered, "smd", out_data, "base", "nft") # Customize parameters as needed
})

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


outcomeCalc <- function(nf_data, outputCalc, out_data, firstValue, nftOrBase){
  # Determine indices for training or rest data
  idx_start_tr <- which(colnames(nf_data) == "mean_pre")
  idx_end_tr   <- which(colnames(nf_data) == "var_ses_15")
  idx_start_bl <- which(colnames(nf_data) == "mean_ses_bl_2")
  idx_end_bl   <- which(colnames(nf_data) == "var_ses_bl_10")
  idx_end      <- ncol(nf_data)
  
  # Convert df columns to numeric
  nf_data[, idx_start_tr:idx_end] <- as.data.frame(lapply(nf_data[, idx_start_tr:idx_end], as.numeric))
  nf_data$n <- as.numeric(nf_data$n)
  
  # Determine participant indices based on input
  ids <- if (firstValue == "first") which(!is.na(nf_data$mean_first)) else which(!is.na(nf_data$mean_pre))
  
  # Determine column indices for means and variances
  idx_mean <- if (nftOrBase == "nft") which(grepl("mean", colnames(nf_data)[idx_start_tr:idx_end_tr])) + (idx_start_tr - 1)
  else which(grepl("mean", colnames(nf_data)[idx_start_bl:idx_end_bl])) + (idx_start_bl - 1)
  
  idx_sd <- if (nftOrBase == "nft") which(grepl("var", colnames(nf_data)[idx_start_tr:idx_end_tr])) + (idx_start_tr - 1)
  else which(grepl("var", colnames(nf_data)[idx_start_bl:idx_end_bl])) + (idx_start_bl - 1)
  
  # Calculate SMD or PSC
  for (p in ids) {
    idxs_mean <- idx_mean[which(!is.na(nf_data[p, idx_mean]))]
    idxs_sd   <- idx_sd[which(!is.na(nf_data[p, idx_sd]))]
    
    if (outputCalc == "smd") {
      for (s in idxs_sd) {
        sd_pooled <- sqrt((nf_data$var_pre[p]^2 + nf_data[p, s]^2) / 2)
        for (i in idxs_mean) {
          out_data[p, (i + 1) - i] <- (nf_data[p, i] - nf_data$mean_pre[p]) / sd_pooled
        }
      }
    } else if (outputCalc == "psc") {
      for (i in idxs_mean) {
        out_data[p, (i + 1) - i] <- (nf_data[p, i] - nf_data$mean_pre[p]) / nf_data$mean_pre[p]
      }
      for (s in idxs_sd) {
        nf_data[p, s + 66] <- sqrt((nf_data$var_pre[p]^2 + nf_data[p, s]^2 - 2 * 0.5 * nf_data$var_pre[p] * nf_data[p, s]))
      }
    }
  }
  return(out_data)
}
