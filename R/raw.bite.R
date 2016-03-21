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
  
  # For each table of records (one for each use + one for total),
  # calculate the amount consumed per record
  for (t in 1:length(Records)) {
    Records[[t]]$Amt <- Records[[t]]$GRMS * Records[[t]]$Prop * Records[[t]]$Use_level 
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
      # De-dupe. Desire one record per SEQN-day-occasion per category.
      Use_table <- unique(Use_table, by = keycols)
      Records[[Col]] <- Use_table
    }
    x$records <- Records
  } else {
    
    # type = "amt" assumed here:
    # Sum consumption for each user by use, divide by 2 days, and store on Demo
    Demo <- Demo[, SEQN:=as.character(SEQN)]
    setkey(Demo, SEQN)
    
    # Creating new columns in a data.table appears to require access by index number 
    # in a list, at least in this release, and cannot be referenced through a variable.
    # Adding to Demo table the raw amount per day & raw amount per day / kg body weight
    for (r in 1:length(col_list)) {
      Col <- col_list[r]
      Demo[, col_list[r] := 0]
      Use_table <- Records[[Col]]
      setkey(Use_table, SEQN)
      Use_table[, Per_kg := Amt/BMXWT]
      Use_table[, Amt_SEQN := sum(Amt)/2, by = SEQN]     
      Demo[SEQN == Use_table$SEQN, col_list[r] := Use_table$Amt_SEQN]
      By_kg <- c(paste(col_list[r], "_per_kg", sep = ""))
      Demo[, By_kg[1] := 0]
      Use_table[, Amt_kg_SEQN := sum(Per_kg)/2, by = SEQN]
      Demo[SEQN == Use_table$SEQN, By_kg[1] := Use_table$Amt_kg_SEQN]
    }
    x$demo <- Demo
  }
  return(x)
}