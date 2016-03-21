#' @title Body Intake Estimate
#' @usage
#' bite(file_name = "")
#' @description
#' Given an input file of food codes and additive levels, returns matrices of population intake statistics
#' based on NHANES dietary survey data.
#' @details
#' This function reads an input file of food codes, use, relevant proportion of the food code, and
#' additive use level and produces population intake statistics for strata, currently hard-coded.
#' 
#' See biteR-package help >package?biteR for details of strata and input file format requirements.
#' In subsequent versions it is expected that these strata and features of the input file
#' will be parameterizable by the user.
#' 
#' This function is a wrap-around for 3 underlying steps in the BITE process:
#' param.bite(file_name = ""), raw.bite(x), and pop.bite(x)
#' 
#' This function is for estimating (sub)population average daily intake levels. To estimate
#' average size(gr) consumed of a food in any eating occasion, please see bite.size().
#' @param file_name  Name of input .csv file containing additive uses and use level by food code.  
#' See help >package?biteR for input file requirements.
#' type  "amt" or "size" to define what is being estimated: amount of the food consumed or 
#' size of the portion consumed
#' @return 3 matrices of population stats containing all strata, one for each:  full, female, and male
#' @export

bite <- function(file_name = "") {
  
  Bite <- param.bite(file_name = file_name, type = "amt")
  Raw <- raw.bite(Bite)
  return(pop.bite(Raw))
}
  
# This is the original code in the single code file:
# Expect not to attach required packages from within biteR package code.
#   library(data.table)
#   library(survey)
#   library(utils)
#   library(car)
#   library(plyr)
#   library(devtools)
#   library(testthat)
# Input <- data.table(read.csv("CPF_proportions_final_042215.csv"))
# 
#   Input <- data.table(read.csv(file_name))
#   Input$Food_code <- as.character(Input$Food_code)
#   
#   # Parse out only reliable records and reduce
#   Day1_09_records <- subset(Day1_09, DR1IFDCD %in% Input$Food_code & DR1DRSTZ == 1 )
#   Day1_11_records <- subset(Day1_11, DR1IFDCD %in% Input$Food_code & DR1DRSTZ == 1 )
#   Day1_records <- rbind(Day1_09_records, Day1_11_records)
#   Day1_records$DR1DRSTZ <- NULL
#   colnames(Day1_records)[which(names(Day1_records) == "DR1IFDCD")] <- "FDCD"
#   colnames(Day1_records)[which(names(Day1_records) == "DR1IGRMS")] <- "GRMS"
#   Day1_records$Day <- 1
#   
#   Day2_09_records <- subset(Day2_09, DR2IFDCD %in% Input$Food_code & DR2DRSTZ == 1 )
#   Day2_11_records <- subset(Day2_11, DR2IFDCD %in% Input$Food_code & DR2DRSTZ == 1 )
#   Day2_records <- rbind(Day2_09_records, Day2_11_records)
#   Day2_records$DR2DRSTZ <- NULL
#   colnames(Day2_records)[which(names(Day2_records) == "DR2IFDCD")] <- "FDCD"
#   colnames(Day2_records)[which(names(Day2_records) == "DR2IGRMS")] <- "GRMS"
#   Day2_records$Day <- 2
#   
#   Records <- rbind(Day1_records, Day2_records)
#   Records$FDCD <- as.character(Records$FDCD)
#   
#   Demo <- rbind(Demo_09, Demo_11)
#   
#   Weight <- rbind(Weight_09, Weight_11)
#   Weight <- data.table(Weight)
#   setkey(Weight, SEQN)
#   
#   # Here, create temporary data.tables of records for each use
#   # and one total table, using the highest proportion * use level 
#   # in the case that a food code is associated with more than one.
#   
#   # Remove records with no 2-day sample weight; these are considered incomplete
#   Records <- Records[complete.cases(Records),]
#   Records <- data.table(Records)
#   setkey(Records, FDCD)
#   
#   Uses <- as.factor(levels(Input$Use))
#   Input <- data.table(Input)
#   setkey(Input, Use)
#   
#   Record_tables <- list()
#   
#   for (a in 1:length(Uses)) {   
#     Use_temp <- Uses[a]
#     Foods_temp <- Input[Use == Use_temp]
#     setkey(Foods_temp, Food_code)
#     Records_temp <- Records[FDCD %in% Foods_temp$Food_code]
#     Use <- as.character(Use_temp)
#     Records_temp$Use <- Use
#     Records_temp$Use_level <- Foods_temp$Use_level[1]
#     Records_temp$Prop <- Foods_temp[Food_code == Records_temp$FDCD, Proportion]
#     # Add bodyweight here, for ease of calculation with data.tables later
#     setkey(Records_temp, SEQN)
#     Records_temp$BMXWT <- Weight[SEQN == Records_temp$SEQN, BMXWT]
#     Record_tables[[Use]] <- Records_temp
#   }
#   
#   # And here, we make the final Total data.table, using the maximum proportion * use level
#   # associated with each food code
#   # First create a reduced Input table by
#   # Removing the duplicate food codes with lower use level
#   Input_total <- Input
#   setkey(Input_total, Food_code)
#   
#   repeat {
#     # defaults to duplicates on the data table key (food code) only
#     # checks two at a time (could be more than two duplicates)
#     dupe <- anyDuplicated(Input_total)
#     if (dupe > 0) {
#       amt1 <- Input_total[dupe-1,]$Use_level * Input_total[dupe-1,]$Proportion
#       amt2 <- Input_total[dupe,]$Use_level * Input_total[dupe,]$Proportion
#       if (amt1 >= amt2) {
#         Input_total <- Input_total[-dupe,]
#       } else {
#         Input_total <- Input_total[-(dupe-1),]
#       }
#     } else {break}
#   }
#   
#   # Use the full set of records for the total data.table
#   Records$Use <- "Total"
#   Records$Use_level <- Input_total[Food_code == Records$FDCD]$Use_level
#   Records$Prop <- Input_total[Food_code == Records$FDCD]$Proportion
#   setkey(Records, SEQN)
#   Records$BMXWT <- Weight[SEQN == Records$SEQN, BMXWT]
#   Record_tables[["Total"]] <- Records
#   
#   # Add 2-day dietary sample weights
#   Sample_wts_09 <- Day2_09[, c("SEQN", "WTDR2D")]
#   Sample_wts_09 <- Sample_wts_09[!duplicated(Sample_wts_09[1:2]),]
#   Sample_wts_11 <- Day2_11[, c("SEQN", "WTDR2D")]
#   Sample_wts_11 <- Sample_wts_11[!duplicated(Sample_wts_11[1:2]),]
#   Sample_wts <- rbind(Sample_wts_09, Sample_wts_11)
#   
#   # Per NHANES analytic guidelines, when combining N survey cycles
#   # after 2000, divide sample weight by N.
#   # Future: build name & divisor (number of cycles) dynamically
#   Sample_wts$Cycles_2_WTDR2D <- Sample_wts$WTDR2D/2
#   Demo <- merge(Demo, Sample_wts, by = "SEQN")
#   Demo <- data.table(Demo, key = "SEQN")
#   # It is expected that full Weight table has more participants than eaters:
#   # It is Not expected that adding suppressWarnings causes default print of
#   # this data table!
#   suppressWarnings (Demo[, BMXWT := Weight$BMXWT])
#   
#   # Instantiate bite object.
#   Bite <- list(
#     demo = data.table(Demo),
#     records = Record_tables,
#     use_data = Input)
#   class(Bite) <- append(class(Bite), "bite")
#   
#   ########### CALCULATE RAW INTAKE ###########
#   Demo <- Bite$demo
#   Records <- Bite$records
#   Use_data <- Bite$use_data
#   
#   # For each table of records (one for each use + one for total),
#   # calculate the amount consumed per record
#   for (t in 1:length(Records)) {
#     Records[[t]]$Amt <- Records[[t]]$GRMS * Records[[t]]$Prop * Records[[t]]$Use_level 
#     Records[[t]]$SEQN <- as.character(Records[[t]]$SEQN)
#     setkey(Records[[t]], SEQN)
#   }
#   
#   # Sum consumption for each user by use, divide by 2 days, and store on Demo
#   Demo <- Demo[, SEQN:=as.character(SEQN)]
#   setkey(Demo, SEQN)
#   col_list <- c(levels(Use_data$Use), "Total")
#   
#   # Creating new columns appears to require access by index number in a list; 
#   # and cannot be referenced through a variable.
#   # Adding to Demo table the raw amount per day & raw amount per day / kg body weight
#   for (r in 1:length(col_list)) {
#     Col <- col_list[r]
#     Demo[, col_list[r] := 0]
#     Use_table <- Records[[Col]]
#     setkey(Use_table, SEQN)
#     Use_table[, Per_kg := Amt/BMXWT]
#     # QA:  check why not using format in 159?  Indeed!  Changed.
#     Use_table[, Amt_SEQN := sum(Amt)/2, by = SEQN]
#     # Amt_SEQN <- Use_table[, sum(Amt)/2, by=SEQN]      
#     Demo[SEQN == Use_table$SEQN, col_list[r] := Use_table$Amt_SEQN]
#     # Demo[SEQN == Amt_SEQN$SEQN, col_list[r] := Amt_SEQN$V1]
#     By_kg <- c(paste(col_list[r], "_per_kg", sep = ""))
#     Demo[, By_kg[1] := 0]
#     Use_table[, Amt_kg_SEQN := sum(Per_kg)/2, by = SEQN]
#     Demo[SEQN == Use_table$SEQN, By_kg[1] := Use_table$Amt_kg_SEQN]
#     # Amt_kg_SEQN <- Use_table[, sum(Per_kg)/2, by= SEQN]
#     # Demo[SEQN == Amt_kg_SEQN$SEQN, By_kg[1] := Amt_kg_SEQN$V1]
#   }
#   
#   Bite$demo <- Demo
#   # 0.82 % in this study, 110/13458 of all consumers of this additive in any form
#   # have NAs in Total/per kg body weight, which cause later mean/quantile failures.  
#   # QA:  determine whether omitted set consumes more additive:  they do. (At least, 
#   # before processed meat was added, which more than doubled the users.
#   # Difference was not tested after that, as matrices were in place.)
#   # FARE (Exponent) tool appears to omit these.
#   # We will include them in the totals; not in the /kilo statistics,
#   # and have 2 N columns.
#   # Demo[, BMXWT := Weight$BMXWT]
#   # Data <- data.frame(Demo)
#   #   Data_comp <- Data[complete.cases(Data),]
#   #   Data_inc <- Data[!complete.cases(Data),] 
#   #   Data_comp$Complete <- 1
#   #   Data_inc$Complete <- 0
#   #   Test <- rbind(Data_comp, Data_inc)
#   #   Lm <- lm(Total ~ Complete, data = Test)
#   #   Data_comp_wt <- merge(Data_comp, Weight, all.x = FALSE, by = "SEQN")
#   #   Lm2 <- lm(Total ~ BMXWT, data = Data_comp_wt)
#   
#   ############ CALCULATE POPULATION STATS #############
#   Input <- Bite$use_data
#   Data <- data.frame(Bite$demo)
#   
#   # Requirement is for a value for each use and for total use.
#   # Hard-coding the following output matrix dimensions for now.
#   # 3 output matrices:  total, female, male
#   
#   num_rows <- (length(levels(Input$Use))+1)*10
#   output_matrix <- matrix(, nrow = num_rows, ncol = 13)
#   colnames(output_matrix) <- c("Use", "Age", "N", "% Users", "Mean/User", "90%ile/User", "Mean/All", "90%ile/All", "N-BWT", "Mean/kg/User", "90%ile/kg/User", "Mean/kg/All", "90%ile/kg/All")
#   
#   uses <- c(levels(Input$Use), "Total")
#   row <- 1
#   for (i in 1:length(uses)) {
#     Use <- uses[i]
#     output_matrix[row,1] <- Use
#     # Reformat these with single apostrophe in front to prevent Excel from converting to a date.
#     # Per Microsoft, there is no other way to do this, neither before nor after setting the values.
#     output_matrix[(row+1):(row+9),2] <- cbind("'2+", "'2-5", "'6-12", "'13-18", "'19+", "'19-29", "'30-44", "'45-59", "'60+")
#     row <- row + 10
#   }
#   output_matrix_female <- output_matrix
#   output_matrix_male <- output_matrix
#   
#   # Max_age <- max(Demo$RIDAGEYR) is 80
#   Age_bands <- c("2-80", "2-5", "6-12", "13-18", "19-80", "19-29", "30-44", "45-59", "60-80")
#   
#   # Create one full design object, from which all subsets will be created/taken
#   Full_survey <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~ Cycles_2_WTDR2D, data = Data, nest = T)
# 
#   ### Results:  Total (male & female together)
#   output_row_start <- 1
#   for (j in 1:length(uses)) {  
#     Use <- uses[j]
#     
#     for (b in 1:length(Age_bands)) {
#       Band <- strsplit(Age_bands[b], "-")
#       Min <- as.numeric(Band[[1]][1])
#       Max <- as.numeric(Band[[1]][2])
#        
#       # First with full set, regardless of body weight reporting
#       N <- nrow(subset(Data, RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0))
#       Perc_users <- N/nrow(Data)*100
#       Mean_users <- svyby(~get(Use),~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0), design=Full_survey, svymean)[2,2]
#       Ninety_users <- svyby(~get(Use), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0), design=Full_survey, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
#       Mean <- svyby(~get(Use),~I(RIDAGEYR >= Min & RIDAGEYR <= Max), design=Full_survey, svymean)[2,2]
#       Ninety <- svyby(~get(Use), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max), design=Full_survey, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
#       
#       # Now with set who reported body weight
#       N_bwt <- nrow(subset(Data, RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0 & BMXWT > 0) )
#       Use_kg <- paste(Use, "_per_kg", sep = "")  
#       Mean_kg_users <- svyby(~get(Use_kg),~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use_kg) > 0 & BMXWT > 0), design=Full_survey, svymean)[2,2]
#       Ninety_kg_users <- svyby(~get(Use_kg), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use_kg) > 0 & BMXWT > 0), design=Full_survey, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
#       Mean_kg <- svyby(~get(Use_kg),~I(RIDAGEYR >= Min & RIDAGEYR <= Max & BMXWT > 0), design=Full_survey, na.rm = TRUE, svymean)[2,2]
#       Ninety_kg <- svyby(~get(Use_kg), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max & BMXWT > 0), design=Full_survey, na.rm = TRUE, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
#       # Set these values where they belong in output matrix
#       Out <- c(N,
#                format(round(Perc_users, 3), nsmall = 3),
#                format(round(Mean_users, 3), nsmall = 3),
#                format(round(Ninety_users, 3), nsmall = 3),
#                format(round(Mean, 3), nsmall = 3),
#                format(round(Ninety, 3), nsmall = 3),
#                N_bwt,
#                format(round(Mean_kg_users, 3), nsmall = 3),
#                format(round(Ninety_kg_users, 3), nsmall = 3),
#                format(round(Mean_kg, 3), nsmall = 3),
#                format(round(Ninety_kg, 3), nsmall = 3))
#       
#       output_matrix[output_row_start+b, 3:13] <- Out
#     }
#     output_row_start <- output_row_start + 10
#   }
#   output_matrix[is.na(output_matrix)] <- ""
#   
#   ### Results:  Female
#   output_row_start <- 1
#   for (j in 1:length(uses)) {  
#     Use <- uses[j]
#     
#     for (b in 1:length(Age_bands)) {
#       Band <- strsplit(Age_bands[b], "-")
#       Min <- as.numeric(Band[[1]][1])
#       Max <- as.numeric(Band[[1]][2])
#       
#       # First with full set, regardless of body weight reporting
#       N <- nrow(subset(Data, RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0 & RIAGENDR == 2))
#       Perc_users <- N/nrow(Data)*100
#       Mean_users <- svyby(~get(Use),~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0  & RIAGENDR == 2), design=Full_survey, svymean)[2,2]
#       Ninety_users <- svyby(~get(Use), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0  & RIAGENDR == 2), design=Full_survey, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
#       Mean <- svyby(~get(Use),~I(RIDAGEYR >= Min & RIDAGEYR <= Max  & RIAGENDR == 2), design=Full_survey, svymean)[2,2]
#       Ninety <- svyby(~get(Use), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max  & RIAGENDR == 2), design=Full_survey, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
#       
#       # Now with set who reported body weight
#       N_bwt <- nrow(subset(Data, RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0 & BMXWT > 0  & RIAGENDR == 2) )
#       Use_kg <- paste(Use, "_per_kg", sep = "")  
#       Mean_kg_users <- svyby(~get(Use_kg),~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use_kg) > 0 & BMXWT > 0  & RIAGENDR == 2), design=Full_survey, svymean)[2,2]
#       Ninety_kg_users <- svyby(~get(Use_kg), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use_kg) > 0 & BMXWT > 0  & RIAGENDR == 2), design=Full_survey, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
#       Mean_kg <- svyby(~get(Use_kg),~I(RIDAGEYR >= Min & RIDAGEYR <= Max & BMXWT > 0  & RIAGENDR == 2), design=Full_survey, na.rm = TRUE, svymean)[2,2]
#       Ninety_kg <- svyby(~get(Use_kg), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max & BMXWT > 0  & RIAGENDR == 2), design=Full_survey, na.rm = TRUE, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
#       
#       # Set these values where they belong in output matrix
#       Out <- c(N,
#                format(round(Perc_users, 3), nsmall = 3),
#                format(round(Mean_users, 3), nsmall = 3),
#                format(round(Ninety_users, 3), nsmall = 3),
#                format(round(Mean, 3), nsmall = 3),
#                format(round(Ninety, 3), nsmall = 3),
#                N_bwt,
#                format(round(Mean_kg_users, 3), nsmall = 3),
#                format(round(Ninety_kg_users, 3), nsmall = 3),
#                format(round(Mean_kg, 3), nsmall = 3),
#                format(round(Ninety_kg, 3), nsmall = 3))
#       output_matrix_female[output_row_start+b, 3:13] <- Out
#     }
#     output_row_start <- output_row_start + 10
#   }
#   output_matrix_female[is.na(output_matrix_female)] <- ""
# 
#   ### Results:  Male
#   output_row_start <- 1
#   for (j in 1:length(uses)) {  
#     Use <- uses[j]
#     
#     for (b in 1:length(Age_bands)) {
#       Band <- strsplit(Age_bands[b], "-")
#       Min <- as.numeric(Band[[1]][1])
#       Max <- as.numeric(Band[[1]][2])
#       
#       # First with full set, regardless of body weight reporting
#       N <- nrow(subset(Data, RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0 & RIAGENDR == 1))
#       Perc_users <- N/nrow(Data)*100
#       Mean_users <- svyby(~get(Use),~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0  & RIAGENDR == 1), design=Full_survey, svymean)[2,2]
#       Ninety_users <- svyby(~get(Use), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0  & RIAGENDR == 1), design=Full_survey, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
#       Mean <- svyby(~get(Use),~I(RIDAGEYR >= Min & RIDAGEYR <= Max  & RIAGENDR == 1), design=Full_survey, svymean)[2,2]
#       Ninety <- svyby(~get(Use), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max  & RIAGENDR == 1), design=Full_survey, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
#       
#       # Now with set who reported body weight
#       N_bwt <- nrow(subset(Data, RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use) > 0 & BMXWT > 0  & RIAGENDR == 1) )
#       Use_kg <- paste(Use, "_per_kg", sep = "")  
#       Mean_kg_users <- svyby(~get(Use_kg),~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use_kg) > 0 & BMXWT > 0  & RIAGENDR == 1), design=Full_survey, svymean)[2,2]
#       Ninety_kg_users <- svyby(~get(Use_kg), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max & get(Use_kg) > 0 & BMXWT > 0  & RIAGENDR == 1), design=Full_survey, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
#       Mean_kg <- svyby(~get(Use_kg),~I(RIDAGEYR >= Min & RIDAGEYR <= Max & BMXWT > 0  & RIAGENDR == 1), design=Full_survey, na.rm = TRUE, svymean)[2,2]
#       Ninety_kg <- svyby(~get(Use_kg), ~I(RIDAGEYR >= Min & RIDAGEYR <= Max & BMXWT > 0  & RIAGENDR == 1), design=Full_survey, na.rm = TRUE, svyquantile, quantiles = 0.9, keep.var = FALSE)[2,2]
#       
#       # Set these values where they belong in output matrix
#       Out <- c(N,
#                format(round(Perc_users, 3), nsmall = 3),
#                format(round(Mean_users, 3), nsmall = 3),
#                format(round(Ninety_users, 3), nsmall = 3),
#                format(round(Mean, 3), nsmall = 3),
#                format(round(Ninety, 3), nsmall = 3),
#                N_bwt,
#                format(round(Mean_kg_users, 3), nsmall = 3),
#                format(round(Ninety_kg_users, 3), nsmall = 3),
#                format(round(Mean_kg, 3), nsmall = 3),
#                format(round(Ninety_kg, 3), nsmall = 3))
#       output_matrix_male[output_row_start+b, 3:13] <- Out
#     }
#     output_row_start <- output_row_start + 10
#   }
#   output_matrix_male[is.na(output_matrix_male)] <- ""
#   
#   output_list <- list(full = output_matrix, female = output_matrix_female, male = output_matrix_male)
#   return(output_list)
# }