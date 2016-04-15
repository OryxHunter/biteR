#' @title Calculate Body Intake Population Statistics
#' @usage
#' pop.bite(x)
#' @description
#' Returns matrices of population additive consumption or portion size.
#' @details
#' When the type is 'amt', calculates population (NHANES) survey weighted statistics for an 
#' additive by demographic strata.
#' When the type is 'size', calculates raw percentiles of portion size by occasion, broken
#' out by age only.
#' @param x   An object of class "bite" processed by raw.bite, and containing the demo
#' attribute with each NHANES participant's raw average daily consumption of the additive
#' by use and by total.
#' @return A list of 3 matrices of population statistics in the supported demographic strata
#' (see help >package?biteR for supported strata).  The matrix for the two genders combined
#' is called "full", the female matrix is called "female"; the male matrix is called "male".
#' @export
#' 

# Change Control 1 (CC1) Collapse age bands 6-18.
# Change Control 2 (CC2) Percent users of the age band, not of the total.

pop.bite <- function (x) {
  
  requireNamespace("survey")
  
  if (x$type == "amt") {
    # Return to data frames for survey statistics
    Data <- data.frame(x$demo)
    Input <- data.frame(x$use_data)
    # Requirement is for a value for each use and for total use.
    # Hard-coding the following output matrix dimensions for now.
    # 3 output matrices:  total, female, male
    
    # CC1 START
    # num_rows <- (length(levels(Input$Use))+1)*10
    num_rows <- (length(levels(Input$Use))+1)*9
    # CC1 END
    output_matrix <- matrix(, nrow = num_rows, ncol = 13)
    colnames(output_matrix) <- c("Use", "Age", "N", "% Users", "Mean/User", "90%ile/User", "Mean/All", "90%ile/All", "N-BWT", "Mean/kg/User", "90%ile/kg/User", "Mean/kg/All", "90%ile/kg/All")
    
    uses <- c(levels(Input$Use), "Total")
    row <- 1
    for (i in 1:length(uses)) {
      Use <- uses[i]
      output_matrix[row,1] <- Use
      # Reformat these with single apostrophe in front to prevent Excel from converting to a date.
      # Per Microsoft, there is no other way to do this, neither before nor after setting the values.
      # CC1 START Collapse ages 6-18
      # output_matrix[(row+1):(row+9),2] <- cbind("'2+", "'2-5", "'6-12", "'13-18", "'19+", "'19-29", "'30-44", "'45-59", "'60+")
      # row <- row + 10
      output_matrix[(row+1):(row+8),2] <- cbind("'2+", "'2-5", "'6-18", "19+", "'19-29", "'30-44", "'45-59", "'60+")
      row <- row + 9
      # CC1 END
    }
    output_matrix_female <- output_matrix
    output_matrix_male <- output_matrix
    
    # Max_age <- max(Demo$RIDAGEYR) is 80
    # CC1 START
    # Age_bands <- c("2-80", "2-5", "6-12", "13-18", "19-80", "19-29", "30-44", "45-59", "60-80")
    Age_bands <- c("2-80", "2-5", "6-18", "19-80", "19-29", "30-44", "45-59", "60-80")
    # CC1 END
    
    # Create one full design object, from which all subsets will be created/taken
    Full_survey <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~ Cycles_2_WTDR2D, data = Data, nest = T)
    
    ### Results:  Total (male & female together)
    output_row_start <- 1
    for (j in 1:length(uses)) {  
      Use <- uses[j]
      
      for (b in 1:length(Age_bands)) {
        Band <- strsplit(Age_bands[b], "-")
        Min <- as.numeric(Band[[1]][1])
        Max <- as.numeric(Band[[1]][2])
        
        # First with full set, regardless of body weight reporting
        N <- nrow(subset(Data, RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0))
        # CC2 START Percent users of the age band, not of the total
        # Perc_users <- N/nrow(Data)*100
        Perc_users <- N/nrow(subset(Data, RIDAGEYR >= Min & RIDAGEYR <= Max))*100
        # CC2 END                           
        Mean_users <- svyby(~get(Use),~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0), design=Full_survey, svymean)[2,2]
        Ninety_users <- svyby(~get(Use), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0), design=Full_survey, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
        Mean <- svyby(~get(Use),~I(RIDAGEYR >= Min & RIDAGEYR <= Max), design=Full_survey, svymean)[2,2]
        Ninety <- svyby(~get(Use), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max), design=Full_survey, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
        
        # Now with set who reported body weight
        N_bwt <- nrow(subset(Data, RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0 & BMXWT > 0) )
        Use_kg <- paste(Use, "_per_kg", sep = "")  
        Mean_kg_users <- svyby(~get(Use_kg),~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use_kg) > 0 & BMXWT > 0), design=Full_survey, svymean)[2,2]
        Ninety_kg_users <- svyby(~get(Use_kg), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use_kg) > 0 & BMXWT > 0), design=Full_survey, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
        Mean_kg <- svyby(~get(Use_kg),~I(RIDAGEYR >= Min & RIDAGEYR <= Max & BMXWT > 0), design=Full_survey, na.rm = TRUE, svymean)[2,2]
        Ninety_kg <- svyby(~get(Use_kg), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max & BMXWT > 0), design=Full_survey, na.rm = TRUE, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
        # Set these values where they belong in output matrix
        Out <- c(N,
                 format(round(Perc_users, 3), nsmall = 3),
                 format(round(Mean_users, 3), nsmall = 3),
                 format(round(Ninety_users, 3), nsmall = 3),
                 format(round(Mean, 3), nsmall = 3),
                 format(round(Ninety, 3), nsmall = 3),
                 N_bwt,
                 format(round(Mean_kg_users, 3), nsmall = 3),
                 format(round(Ninety_kg_users, 3), nsmall = 3),
                 format(round(Mean_kg, 3), nsmall = 3),
                 format(round(Ninety_kg, 3), nsmall = 3))
        
        output_matrix[output_row_start+b, 3:13] <- Out
      }
      # CC1 START
      # output_row_start <- output_row_start + 10
      output_row_start <- output_row_start + 9
      # CC1 END
    }
    output_matrix[is.na(output_matrix)] <- ""
    
    ### Results:  Female
    output_row_start <- 1
    for (j in 1:length(uses)) {  
      Use <- uses[j]
      
      for (b in 1:length(Age_bands)) {
        Band <- strsplit(Age_bands[b], "-")
        Min <- as.numeric(Band[[1]][1])
        Max <- as.numeric(Band[[1]][2])
        
        # First with full set, regardless of body weight reporting
        N <- nrow(subset(Data, RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0 & RIAGENDR == 2))
        # CC2 START Percent users of the age band, not of the total
        # Perc_users <- N/nrow(Data)*100
        Perc_users <- N/nrow(subset(Data, RIDAGEYR >= Min & RIDAGEYR <= Max & RIAGENDR == 2))*100
        # CC2 END
        Mean_users <- svyby(~get(Use),~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0  & RIAGENDR == 2), design=Full_survey, svymean)[2,2]
        Ninety_users <- svyby(~get(Use), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0  & RIAGENDR == 2), design=Full_survey, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
        Mean <- svyby(~get(Use),~I(RIDAGEYR >= Min & RIDAGEYR <= Max  & RIAGENDR == 2), design=Full_survey, svymean)[2,2]
        Ninety <- svyby(~get(Use), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max  & RIAGENDR == 2), design=Full_survey, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
        
        # Now with set who reported body weight
        N_bwt <- nrow(subset(Data, RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0 & BMXWT > 0  & RIAGENDR == 2) )
        Use_kg <- paste(Use, "_per_kg", sep = "")  
        Mean_kg_users <- svyby(~get(Use_kg),~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use_kg) > 0 & BMXWT > 0  & RIAGENDR == 2), design=Full_survey, svymean)[2,2]
        Ninety_kg_users <- svyby(~get(Use_kg), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use_kg) > 0 & BMXWT > 0  & RIAGENDR == 2), design=Full_survey, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
        Mean_kg <- svyby(~get(Use_kg),~I(RIDAGEYR >= Min & RIDAGEYR <= Max & BMXWT > 0  & RIAGENDR == 2), design=Full_survey, na.rm = TRUE, svymean)[2,2]
        Ninety_kg <- svyby(~get(Use_kg), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max & BMXWT > 0  & RIAGENDR == 2), design=Full_survey, na.rm = TRUE, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
        
        # Set these values where they belong in output matrix
        Out <- c(N,
                 format(round(Perc_users, 3), nsmall = 3),
                 format(round(Mean_users, 3), nsmall = 3),
                 format(round(Ninety_users, 3), nsmall = 3),
                 format(round(Mean, 3), nsmall = 3),
                 format(round(Ninety, 3), nsmall = 3),
                 N_bwt,
                 format(round(Mean_kg_users, 3), nsmall = 3),
                 format(round(Ninety_kg_users, 3), nsmall = 3),
                 format(round(Mean_kg, 3), nsmall = 3),
                 format(round(Ninety_kg, 3), nsmall = 3))
        output_matrix_female[output_row_start+b, 3:13] <- Out
      }
      # CC1 START
      # output_row_start <- output_row_start + 10
      output_row_start <- output_row_start + 9
      # CC1 END
    }
    output_matrix_female[is.na(output_matrix_female)] <- ""
    
    ### Results:  Male
    output_row_start <- 1
    for (j in 1:length(uses)) {  
      Use <- uses[j]
      
      for (b in 1:length(Age_bands)) {
        Band <- strsplit(Age_bands[b], "-")
        Min <- as.numeric(Band[[1]][1])
        Max <- as.numeric(Band[[1]][2])
        
        # First with full set, regardless of body weight reporting
        N <- nrow(subset(Data, RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0 & RIAGENDR == 1))
        # CC2 START Percent users of the age band, not of the total
        # Perc_users <- N/nrow(Data)*100
        Perc_users <- N/nrow(subset(Data, RIDAGEYR >= Min & RIDAGEYR <= Max & RIAGENDR == 1))*100
        # CC2 END
        Mean_users <- svyby(~get(Use),~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0  & RIAGENDR == 1), design=Full_survey, svymean)[2,2]
        Ninety_users <- svyby(~get(Use), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0  & RIAGENDR == 1), design=Full_survey, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
        Mean <- svyby(~get(Use),~I(RIDAGEYR >= Min & RIDAGEYR <= Max  & RIAGENDR == 1), design=Full_survey, svymean)[2,2]
        Ninety <- svyby(~get(Use), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max  & RIAGENDR == 1), design=Full_survey, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
        
        # Now with set who reported body weight
        N_bwt <- nrow(subset(Data, RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0 & BMXWT > 0  & RIAGENDR == 1) )
        Use_kg <- paste(Use, "_per_kg", sep = "")  
        Mean_kg_users <- svyby(~get(Use_kg),~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use_kg) > 0 & BMXWT > 0  & RIAGENDR == 1), design=Full_survey, svymean)[2,2]
        Ninety_kg_users <- svyby(~get(Use_kg), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use_kg) > 0 & BMXWT > 0  & RIAGENDR == 1), design=Full_survey, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
        Mean_kg <- svyby(~get(Use_kg),~I(RIDAGEYR >= Min & RIDAGEYR <= Max & BMXWT > 0  & RIAGENDR == 1), design=Full_survey, na.rm = TRUE, svymean)[2,2]
        Ninety_kg <- svyby(~get(Use_kg), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max & BMXWT > 0  & RIAGENDR == 1), design=Full_survey, na.rm = TRUE, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
        
        # Set these values where they belong in output matrix
        Out <- c(N,
                 format(round(Perc_users, 3), nsmall = 3),
                 format(round(Mean_users, 3), nsmall = 3),
                 format(round(Ninety_users, 3), nsmall = 3),
                 format(round(Mean, 3), nsmall = 3),
                 format(round(Ninety, 3), nsmall = 3),
                 N_bwt,
                 format(round(Mean_kg_users, 3), nsmall = 3),
                 format(round(Ninety_kg_users, 3), nsmall = 3),
                 format(round(Mean_kg, 3), nsmall = 3),
                 format(round(Ninety_kg, 3), nsmall = 3))
        output_matrix_male[output_row_start+b, 3:13] <- Out
      }
      # CC1 START
      # output_row_start <- output_row_start + 10
      output_row_start <- output_row_start + 9
      # CC1 END
    }
    output_matrix_male[is.na(output_matrix_male)] <- ""
    
    output_list <- list(full = output_matrix, female = output_matrix_female, male = output_matrix_male)
    return(output_list)
  } else {
    # Assuming here type = "size"
    # And currently coded to project specifications: no sample weighting, not broken out by gender
    # Files/tables are currently manipulated manually midstream to get gender breakdowns.
    
    Input <- data.frame(x$use_data)
    # Requirement is for an average portion size, with percentiles 5th-95th, every 5.
    # Hard-coding the following output matrix dimensions for now.
    
    uses <- c(levels(Input$Use))
    num_rows <- (length(x$records))*4
    output_matrix <- matrix(, nrow = num_rows, ncol = 23)
    colnames(output_matrix) <- c("Category", "Age", "N", "Mean gr/Occsn", "5 %ile", "10 %ile", "15 %ile", "20 %ile", "25 %ile", "30 %ile", "35 %ile", "40 %ile", 
                                 "45 %ile", "50 %ile", "55 %ile", "60 %ile", "65 %ile", "70 %ile", "75 %ile", "80 %ile", "85 %ile", "90 %ile", "95 %ile")

    row <- 1
    for (i in 1:length(uses)) {
      Use <- uses[i]
      output_matrix[row,1] <- Use
      # Reformat these with single apostrophe in front to prevent Excel from converting to a date.
      # Per Microsoft, there is no other way to do this, neither before nor after setting the values.
      output_matrix[(row+1):(row+3),2] <- cbind("'2-5", "'6-17", "'18+")
      row <- row + 4
    }
    Age_bands <- c("2-5", "6-17", "18-90")
    output_row_start <- 1
    
    for (j in 1:length(uses)) {  
      Use <- uses[j]
      Cat <- x$records[[Use]]
      Cat <- as.data.frame(Cat)
      
      for (b in 1:length(Age_bands)) {
        Band <- strsplit(Age_bands[b], "-")
        Min <- as.numeric(Band[[1]][1])
        Max <- as.numeric(Band[[1]][2])
        
        Set <- subset(Cat, RIDAGEYR >= Min & RIDAGEYR <= Max)
        Out <- c(nrow(Set),
                 format(round(mean(Set$Amt_OCCSN), 2), nsmall = 2),
                 format(round(quantile(Set$Amt_OCCSN, probs = 0.05, type = 8), 2), nsmall = 2),
                 format(round(quantile(Set$Amt_OCCSN, probs = 0.10, type = 8), 2), nsmall = 2),
                 format(round(quantile(Set$Amt_OCCSN, probs = 0.15, type = 8), 2), nsmall = 2),
                 format(round(quantile(Set$Amt_OCCSN, probs = 0.20, type = 8), 2), nsmall = 2),
                 format(round(quantile(Set$Amt_OCCSN, probs = 0.25, type = 8), 2), nsmall = 2),
                 format(round(quantile(Set$Amt_OCCSN, probs = 0.30, type = 8), 2), nsmall = 2),
                 format(round(quantile(Set$Amt_OCCSN, probs = 0.35, type = 8), 2), nsmall = 2),
                 format(round(quantile(Set$Amt_OCCSN, probs = 0.40, type = 8), 2), nsmall = 2),
                 format(round(quantile(Set$Amt_OCCSN, probs = 0.45, type = 8), 2), nsmall = 2),
                 format(round(quantile(Set$Amt_OCCSN, probs = 0.50, type = 8), 2), nsmall = 2),
                 format(round(quantile(Set$Amt_OCCSN, probs = 0.55, type = 8), 2), nsmall = 2),
                 format(round(quantile(Set$Amt_OCCSN, probs = 0.60, type = 8), 2), nsmall = 2),
                 format(round(quantile(Set$Amt_OCCSN, probs = 0.65, type = 8), 2), nsmall = 2),
                 format(round(quantile(Set$Amt_OCCSN, probs = 0.70, type = 8), 2), nsmall = 2),
                 format(round(quantile(Set$Amt_OCCSN, probs = 0.75, type = 8), 2), nsmall = 2),
                 format(round(quantile(Set$Amt_OCCSN, probs = 0.80, type = 8), 2), nsmall = 2),
                 format(round(quantile(Set$Amt_OCCSN, probs = 0.85, type = 8), 2), nsmall = 2),
                 format(round(quantile(Set$Amt_OCCSN, probs = 0.90, type = 8), 2), nsmall = 2),
                 format(round(quantile(Set$Amt_OCCSN, probs = 0.95, type = 8), 2), nsmall = 2))
        output_matrix[output_row_start+b, 3:23] <- Out
      }
      output_row_start <- output_row_start + 4
    }
    output_matrix[is.na(output_matrix)] <- ""
    return(output_matrix)
  }
}
    
