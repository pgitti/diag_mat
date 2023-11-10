diag_mat <- function(m = matrix(c(0, 1, 3, 4), ncol = 2)){
  
  # guard clauses
  if(nrow(m) != ncol(m)) stop("incorrect dimensions")
  if(det(m) == 0) stop("can't diagonalize")
  det_in <- det(m)
  m_in <- m
  
  # pick row_col_source
  row_cols <- seq(nrow(m))
  for(row_col_source in row_cols){
    
    # swap rows?
    if(m[row_col_source,row_col_source] == 0){
      copy <- m[row_col_source,]
      non_zero <- which(m[,row_col_source] != 0)[1]
      m[row_col_source,] <- m[non_zero,]
      m[non_zero,] <- copy
    }
    
    # subtract from row_target 
    for(row_target in which(!(row_cols %in% row_col_source))){
      if(sum(m[row_target,] != 0)) m[row_target,] <- m[row_target,] - m[row_col_source,] * m[row_target,row_col_source] / m[row_col_source, row_col_source]
    }
  }
  
  if(det_in != det(m)) warning("something went wrong, det(m) shouldn't change")
  return(list(m_in = m_in, det_in = det_in, m_out = m, det_out = det(m)))
}
