
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
#' @param data a data.frame or list containing the data table
#' @param to_remove a vector or list containing the elements to be removed from the data table
#' @param wid a character vector containing the unique identifier for each row
#' @param dv.var column with the information to classify observations based on dependent variables
#' @return A data.frame with the eliminated data table
#' @export
remove_from_datatable <- function(data, to_remove, wid = 'row.pos', dv.var = NULL) {
  if (length(to_remove) > 1) {
    if (is.data.frame(data)) {
      pos <- rep(T, nrow(data))
    } else if (is.list(data)) {
      ldvs <- as.list(names(data))
      names(ldvs) <- names(data)
      pos <- lapply(ldvs, FUN = function(dv) {
        rep(T, nrow(data[[dv]]))
      })
    }

    if (is.list(to_remove)) {
      for (dv in names(to_remove)) {
        if (is.data.frame(data)) {
          if (!is.null(dv.var)) {
            pos <- (!data[[wid]] %in% to_remove[[dv]]) & (data[[dv.var]] == dv) & pos
          } else {
            pos <- (!data[[wid]] %in% to_remove[[dv]]) & pos
          }
        } else if (is.list(data)) {
          pos[[dv]] <- (!data[[dv]][[wid]] %in% to_remove[[dv]]) & pos[[dv]]
        }
      }
    } else if (is.data.frame(data) && !is.null(dv.var)) {
      for (dv in unique(data[[dv.var]])) {
        pos <- (!data[[wid]] %in% to_remove) & (data[[dv.var]] == dv) & pos
      }
    } else if (is.list(data)) {
      for (dv in names(data)) {
        pos[[dv]] <- (!data[[dv]][[wid]] %in% to_remove) & pos[[dv]]
      }
    } else {
      pos <- (!data[[wid]] %in% to_remove) & pos
    }


    if (is.data.frame(data)) {
      toReturn <- data[pos,]
    } else if (is.list(data)) {
      ldvs <- as.list(names(data))
      names(ldvs) <- names(data)
      toReturn <- lapply(ldvs, FUN = function(dv) {
        return(data[[dv]][pos[[dv]],])
      })
    }
    return(toReturn)
  } else {
    return(data)
  }
}

#' Get Formula for ANOVA and ANCOVA
#'
#' @export
as_formula <- function(dv, between = c(), within = c(), covar = NULL, wid = 'row.pos', as.character = F) {
  ivs <- c(between, within)
  if (length(ivs) == 0 && length(covar) == 0) {
    return(NULL)
  }

  sformula <- paste0(paste0('`',ivs,'`'), collapse = '*')
  if (!is.null(covar))
    sformula <- paste0('`', covar, '` + ', sformula)
  sformula <- paste0('`', dv, '` ~ ', sformula)
  if (length(within) > 0) {
    sformula <- paste0(sformula,' + Error(',paste0('`',wid,'`'),'/(',paste0(paste0('`',within,'`'),collapse='*'),'))')
  }
  if (as.character) return(sformula)
  else return(stats::as.formula(sformula))
}


subset_by_tbl <- function(data, tbl, group = intersect(colnames(data), colnames(tbl))) {
  df <- do.call(rbind, lapply(seq(1:nrow(tbl)), FUN = function(i) {
    idx <- rep(T, nrow(data))
    for (g in group)
      idx <- idx & (data[[g]] == tbl[[g]][i])
    return(data[idx,])
  }))
  return(df)
}

