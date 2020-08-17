#' Settings Data Table for Dependent Variables
#'
#' Creates a new data table based on a set of dependent variables
#'
#' @param tbl a data.frame containing the initial data
#' @param dvs a character vector containing the dependent variables
#' @param dv.var column with the information to classify observations based on dependent variables
#' @return A data.frame with the created data table
#' @export
set_datatable <- function (tbl, dvs, dv.var = "var") {
  return(do.call(rbind, lapply(dvs, FUN = function(dv) {
    df <- cbind(var = dv, tbl)
    colnames(df) <- c(dv.var, colnames(tbl))
    return(df)
  })))
}

#' Remove Elements from a Data Table for Dependent Variables
#'
#' Removes elements from a data table previously setting based on dependent variables
#'
#' @param tbl a data.frame containing the data table
#' @param to_remove a vector or list containing the elements to be removed from the data table
#' @param wid a character vector containing the unique identifier for each row
#' @param dv.var column with the information to classify observations based on dependent variables
#' @return A data.frame with the eliminated data table
#' @export
remove_from_dataset <- function(tbl, to_remove, wid = 'row.pos', dv.var = NULL) {
  if (length(to_remove) > 1) {
    pos <- c()
    if (is.list(to_remove)) {
      for (dv in names(to_remove)) {
        if (!is.null(dv.var)) {
          pos <- c(which((tbl[[wid]] %in% to_remove[[dv]]) & (tbl[[dv.var]] == dv)), pos)
        } else {
          pos <- c(which(tbl[[wid]] %in% to_remove[[dv]]), pos)
        }
      }
    } else if (!is.null(dv.var)) {
      for (dv in unique(tbl[[dv.var]])) {
        pos <- c(which((tbl[[wid]] %in% to_remove) & (tbl[[dv.var]] == dv)), pos)
      }
    } else {
      pos <- c(which(tbl[[wid]] %in% to_remove), pos)
    }
    return(tbl[-c(pos),])
  } else {
    return(tbl)
  }
}
