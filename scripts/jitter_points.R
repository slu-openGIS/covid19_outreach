# Jitter Duplicate Points
jitter_points <- function(.data, factor){
  
  dupes <- dplyr::mutate(.data, dupes = paste(x, ", ", y))
  dupes_vector <- duplicated(dupes$dupes)
  dupes_df <- cbind(.data, dupes_vector)
  
  dupes_df <- dplyr::mutate(dupes_df, x = ifelse(dupes_vector == TRUE, jitter(x, factor = factor), x))
  dupes_df <- dplyr::mutate(dupes_df, y = ifelse(dupes_vector == TRUE, jitter(y, factor = factor), y))
  dupes_df <- dplyr::select(dupes_df, -dupes_vector)
  
  return(dupes_df)
  
}
