
# Clean up dataframe, leaving only percentage values.
parse <- function(dfOrFilename) {
  typeof("something")
  if (typeof(dfOrFilename) == "character") {
    df <- read.table(file = dfOrFilename, TRUE, ",", na.strings = "", encoding = "utf-8")
  } else {
    df <- dfOrFilename
  }

    #df <- subset(df, grepl("%", df[, 2]))
  
  # Use first column as row names
  rownames(df) <- df[, 1]

  
  # Trim first column
  df <- df[,-1]
  
  cn <- base::colnames(df)

  # Transpose the data frame
  df <- data.frame(t(df))

  convertPercent <- function(x) {
    return(as.numeric(sub("%", "", x)))
  }

  # Drop the % sign and convert to numeric values in columns if not extract number
  df <- data.frame(lapply(df, convertPercent))

    # Drop the cols with sum less than 0.01
  df <- df[, (colSums(df) >= 0.0001), drop = T]
  base::rownames(df) <- cn

  return(df)
}
