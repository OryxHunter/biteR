#' @title Define Parameters for body intake estimation (BITE)
#' @usage
#' param.bite(file_name = "", type = "")
#' @description
#' Defines parameters with which to build the BITE object.
#' @details
#' This function uses the input file to parameterize the BITE object, primarily the
#' records attribute, which is a list of data.tables, one for each use and one for the 
#' total use, containing all consumption records for all food codes in the use.
#' @param 
#' file_name  Name of input file containing additive uses and use level by food code.
#' See help >package?biteR for input file requirements.
#' 
#' type "amt" vs. "size" only determines here whether records are kept for participants
#' who did not complete both days of the survey. For size, as we are not concerned
#' with weighting, we do not insist upon it.
#' 
#' @return A BITE object, instantiated with the attributes required to calculate the raw
#' daily intake of the additive for each NHANES participant, using raw.bite, to be passed
#' into raw.bite(x).
#' @export

param.bite <- function (file_name = "", type = "") {
  
  requireNamespace("data.table")
  Input <- data.table(read.csv(file_name))
  Input$Food_code <- as.character(Input$Food_code)
  
  # Parse out only reliable records and reduce
  Day1_09_records <- subset(Day1_09, DR1IFDCD %in% Input$Food_code & DR1DRSTZ == 1 )
  Day1_11_records <- subset(Day1_11, DR1IFDCD %in% Input$Food_code & DR1DRSTZ == 1 )
  Day1_records <- rbind(Day1_09_records, Day1_11_records)
  Day1_records$DR1DRSTZ <- NULL
  colnames(Day1_records)[which(names(Day1_records) == "DR1IFDCD")] <- "FDCD"
  colnames(Day1_records)[which(names(Day1_records) == "DR1IGRMS")] <- "GRMS"
  colnames(Day1_records)[which(names(Day1_records) == "DR1_030Z")] <- "OCCSN"
  Day1_records$Day <- 1
  
  Day2_09_records <- subset(Day2_09, DR2IFDCD %in% Input$Food_code & DR2DRSTZ == 1 )
  Day2_11_records <- subset(Day2_11, DR2IFDCD %in% Input$Food_code & DR2DRSTZ == 1 )
  Day2_records <- rbind(Day2_09_records, Day2_11_records)
  Day2_records$DR2DRSTZ <- NULL
  colnames(Day2_records)[which(names(Day2_records) == "DR2IFDCD")] <- "FDCD"
  colnames(Day2_records)[which(names(Day2_records) == "DR2IGRMS")] <- "GRMS"
  colnames(Day2_records)[which(names(Day2_records) == "DR2_030Z")] <- "OCCSN"
  Day2_records$Day <- 2
  
  Records <- rbind(Day1_records, Day2_records)
  Records$FDCD <- as.character(Records$FDCD)
  
  Demo <- rbind(Demo_09, Demo_11)
  Demo <- data.table(Demo, key = "SEQN")
  
  Weight <- rbind(Weight_09, Weight_11)
  Weight <- data.table(Weight)
  setkey(Weight, SEQN)
  
  # Here, create temporary data.tables of records for each use
  # and one total table, using the highest proportion * use level 
  # for the case that a food code is associated with more than one use.
  
  if (type == "amt") {
  # For population intake estimates, remove records with no 2-day sample weight; these are considered incomplete
  Records <- Records[complete.cases(Records),]
  }
  
  Records <- data.table(Records)
  setkey(Records, FDCD)
  
  Uses <- as.factor(levels(Input$Use))
  colnames(Input)[1] <- "FDCD"
  Input <- data.table(Input)
  setkey(Input, Use)
  
  Record_tables <- list()
  
  for (a in 1:length(Uses)) {   
    Use_temp <- Uses[a]
    Foods_temp <- Input[Use == Use_temp]
    setkey(Foods_temp, FDCD)
    Records_temp <- Records[FDCD %in% Foods_temp$FDCD]
    Use <- as.character(Use_temp)
    Records_temp <- merge(Records_temp, Foods_temp[, c("FDCD", "Proportion", "Use_level"), with = FALSE])
    setkey(Records_temp, SEQN)
    Records_temp <- merge(Records_temp, Weight)
    Records_temp <- merge(Records_temp, Demo)
    Record_tables[[Use]] <- Records_temp
  }
  
  # And here, we make the final Total data.table, using the maximum proportion * use level
  # associated with each food code. First create a reduced Input table by
  # Removing the duplicate food codes with lower use level.
  Input_total <- Input
  setkey(Input_total, FDCD)
  
  repeat {
    # defaults to duplicates on the data table key (food code) only
    # checks two at a time (could be more than two duplicates)
    dupe <- anyDuplicated(Input_total)
    if (dupe > 0) {
      amt1 <- Input_total[dupe-1,]$Use_level * Input_total[dupe-1,]$Proportion
      amt2 <- Input_total[dupe,]$Use_level * Input_total[dupe,]$Proportion
      if (amt1 >= amt2) {
        Input_total <- Input_total[-dupe,]
      } else {
        Input_total <- Input_total[-(dupe-1),]
      }
    } else {break}
  }
  
  # Use the full set of records for the total data.table
  Records$Use <- "Total"
  setkey(Records, "FDCD")
  Input_total$Use <- NULL
  Records <- merge(Records, Input_total)
  setkey(Records, SEQN)
  Records <- merge(Records, Weight)
  Records <- merge(Records, Demo)
  Record_tables[["Total"]] <- Records

  # Add 2-day dietary sample weights
  Sample_wts_09 <- Day2_09[, c("SEQN", "WTDR2D")]
  Sample_wts_09 <- Sample_wts_09[!duplicated(Sample_wts_09[1:2]),]
  Sample_wts_11 <- Day2_11[, c("SEQN", "WTDR2D")]
  Sample_wts_11 <- Sample_wts_11[!duplicated(Sample_wts_11[1:2]),]
  Sample_wts <- rbind(Sample_wts_09, Sample_wts_11)
  
  # Per NHANES analytic guidelines, when combining N survey cycles
  # after 2000, divide sample weight by N.
  Divisor <- 2 # For the last 3 biennials, use biteR 0.1.2
  Sample_wts$Cycles_2_WTDR2D <- Sample_wts$WTDR2D/Divisor
  Demo <- merge(Demo, Sample_wts, by = "SEQN")
  Demo <- data.table(Demo, key = "SEQN")
  Demo <- merge(Demo, Weight)
  
  Bite <- list(
    type = type,
    demo = Demo,
    records = Record_tables,
    use_data = Input)
  class(Bite) <- append(class(Bite), "bite")
  
  return(Bite)
}
