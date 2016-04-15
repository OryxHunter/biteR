#' @title Calculate Raw Body Intake
#' @usage
#' raw.bite(x)
#' @description
#' Calculates raw intake of additive for each NHANES participant.
#' @details
#' This function uses the information on the parameterized BITE object to calculate
#' raw intake values by use and by total for every person who participated in the 
#' NHANES dietary survey.
#' @param x   An object of class "bite" built by param.bite
#' @return Updated bite object, with updated attribute 'demo' now including a calculated
#' amount of additive consumed on average per day for each NHANES dietary survey participant, 
#' by use and total.
#' @export

raw.bite <- function (x) {
  Type <- x$type
  Demo <- x$demo
  Records <- x$records
  Use_data <- x$use_data
  
  # For each table of records calculate the amount consumed per record
  for (t in 1:length(Records)) {
    Records[[t]]$Amt <- Records[[t]]$GRMS * Records[[t]]$Prop * Records[[t]]$Use_level 
    Records[[t]]$Per_kg <- (Records[[t]]$GRMS * Records[[t]]$Prop * Records[[t]]$Use_level)/Records[[t]]$BMXWT
    Records[[t]]$SEQN <- as.character(Records[[t]]$SEQN)
    setkey(Records[[t]], SEQN)
  }
  
  col_list <- c(levels(Use_data$Use), "Total")
  
  # For type = "size":  Here adding functionality to sum by occasion, not by user / day.  
  # The exact same food may be coded (rarely) as having been eaten on the same day, same occasion.
  # Also, different food codes in the same category may be eaten on the same day, same occasion.
  # This loop keeps the records for each such case, but the Amt_OCCSN is the total consumed
  # by the person in this category by day and occasion.
  
  if (Type == "size") {
    for (r in 1:length(col_list)) {
      Col <- col_list[r]
      Use_table <- Records[[Col]]
      keycols = c("SEQN","Day", "OCCSN")
      setkeyv(Use_table, keycols)
      Use_table[, Amt_OCCSN := sum(Amt), by = keycols]    
      # De-dupe. Desire one record per SEQN-day-occasion per use.
      Use_table <- unique(Use_table, by = keycols)
      Records[[Col]] <- Use_table
    }
    x$records <- Records
  } else {
    
    # type = "amt" assumed here:
    # Sum consumption for each user by use, divide by 2 days, and store on Demo
    Demo <- Demo[, SEQN:=as.character(SEQN)]
    setkey(Demo, SEQN)
    
    for (r in 1:length(col_list)) {
      Col <- col_list[r]
      Use_table <- Records[[Col]]
      setkey(Use_table, SEQN)
      Use <- c(paste(col_list[r]))
      By_kg <- c(paste(col_list[r], "_per_kg", sep = ""))
      
      Use_table[, Use[1] := sum(Amt)/2, by = SEQN]  
      Use_table[, By_kg[1] := sum(Per_kg)/2, by = SEQN]

      Demo <- merge(Demo, Use_table[, c("SEQN", Use[1], By_kg[1]), with = FALSE], all.x = TRUE)
      Demo <- unique(Demo) # Removes all duplicated SEQN caused by merge.
      Demo[is.na(get(Use[1])), Use[1] := 0, with=FALSE]
      Demo[is.na(get(By_kg[1])), By_kg[1] := 0, with=FALSE]
    }
    x$demo <- Demo
  }
  return(x)
}