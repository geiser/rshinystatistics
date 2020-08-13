#' Set DataSet
#'
#' Creates a new dataset based on a set of dependent variables 
#'
#' @param data a data.frame containing the initial data
#' @param dvs a character vector containing the dependent variables
#' @param wid a character vector containing the unique identifier for each row
#' @param dv.var column with the information to classify observations based on dependent variables
#' @return A data.frame with the created dataset
#' @export
set_dataset <- function(data, dvs, wid = 'row.pos', dv.var = 'var') {
  dat <- as.data.frame(data)
  dataset <- do.call(rbind, lapply(dvs, FUN = function(dv) {
    df <- cbind(var = dv, dat)
    colnames(df) <- c(dv.var, colnames(dat))
    return(df)
  }))
  return(dataset)
}

#' Remove Elements from DataSet
#'
#' Removes elements from a dataset based on a set of dependent variables 
#'
#' @param data a data.frame containing the dataset
#' @param to_remove a vector or list containing the elements to be removed from the dataset
#' @param wid a character vector containing the unique identifier for each row
#' @param dv.var column with the information to classify observations based on dependent variables
#' @return A data.frame with the eliminated dataset
#' @export
remove_from_dataset <- function(data, to_remove, wid = 'row.pos', dv.var = NULL) {
  pos <- c()
  if (is.list(to_remove)) {
    for (dv in names(to_remove)) {
      if (!is.null(dv.var)) {
        pos <- c(which((data[[wid]] %in% to_remove[[dv]]) & (data[[dv.var]] == dv)), pos)
      } else {
        pos <- c(which(data[[wid]] %in% to_remove[[dv]]), pos)
      }
    }
  } else if (!is.null(dv.var)) {
    pos <- c(which((data[[wid]] %in% to_remove) & (data[[dv.var]] == dv)), pos)
  } else {
    pos <- c(which(dat[[wid]] %in% to_remove), pos)  
  }
  return(data[-c(pos),])
}
