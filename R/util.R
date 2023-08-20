getSkewnessMap <- function(skewness = c(), prefix = 'std.') {
  toReturn <- list()
  for (i in names(skewness))
    if (!is.null(skewness[[i]]) && skewness[[i]]!='none')
      toReturn[[i]] <- paste0(prefix,i)
  return(toReturn)
}


#' Settings Data Table for Dependent Variables
#'
#' Creates a new data table based on a set of dependent variables
#'
#' @param tbl a data.frame containing the initial data
#' @param dvs a character vector containing the dependent variables
#' @param dv.var column with the information to classify observations based on dependent variables
#' @return A data.frame with the created data table
setDataTable <- function (tbl, dvs, dv.var = "var") {
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
removeFromDataTable <- function(data, to_remove, wid = 'row.pos', dv.var = NULL) {
  if (length(to_remove) >= 1) {
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

#' Rounded p-values to a number of digits
#'
#' Removes elements from a data table previously setting based on dependent variables
#'
#' @param tbl a data.frame or data table with p-values to be rounded
#' @param digits number of digits for the p-values
#' @param min minimal value to be displayed as number
#' @param cnames a vector containing the columns names in which the p-values are displayed
#' @return A data.frame with the p-values rounded
#' @export
round.pval <- function(tbl, digits = 3, min = 0.001, cnames = c("p","p.adj")) {
  for (cname in intersect(cnames, colnames(tbl))) {
    tbl[[cname]] <- sapply(tbl[[cname]], function(x) {
      ifelse(is.numeric(x) & x < min, paste0("<",min), round(x, digits = digits))
    })
  }
  return(tbl)
}

#' Remove elements for perforimg statiscits analysis
#'
#' Removes groups of elements from a data table in which we can not perform statistics analysis
#'
#' @param pdat a data.frame or data table with data to be removed
#' @param dv dependent variable
#' @param ivs independent variables
#' @param n.limit minimal value for a group
#' @export
remove_group_data <- function(pdat, dv, ivs, n.limit = 5) {
  ds = data.frame(get.descriptives(pdat, dv, ivs))
  if (length(which(ds$n < n.limit)) < 1) return(pdat)

  ds = ds[which(ds$n < n.limit),]

  toReturn = pdat
  toReturn[[".toRemove."]] = ifelse(length(ivs) > 1, apply(toReturn[,ivs],1,paste0, collapse = ":"), toReturn[[ivs]])
  idx = ifelse(length(ivs) > 1, unique(apply(ds[,ivs],1,paste0, collapse = ":")), unique(ds[[ivs]]))
  toReturn <- toReturn[!toReturn$.toRemove. %in% idx,]
  toReturn <- toReturn[, -which(names(toReturn) == ".toRemove.")]

  for (iv in ivs) {
    if (length(unique(toReturn[[iv]])) < 2) {
      stop(paste0("Column `",iv,"` have only one level, to apply inferencial statistics at least the column should have 2 or more levels!"))
    }
    if (is.factor(toReturn[[iv]])) {
      levs = levels(toReturn[[iv]])
      toReturn[[iv]] <- factor(toReturn[[iv]], levs[levs %in% unique(toReturn[[iv]])])
    }
  }
  return(toReturn)
}

#' Get Formula for ANOVA and ANCOVA
as_formula <- function(dv, between = c(), within = c(), covar = NULL, wid = 'row.pos', as.character = F) {
  ivs <- c(between, within)
  if (length(ivs) == 0 && length(covar) == 0) {
    return(NULL)
  }

  sformula <- paste0(paste0('`',ivs,'`'), collapse = '*')
  if (!is.null(covar))
    sformula <- paste0('`', covar, '`+', sformula)
  sformula <- paste0('`', dv, '`~', sformula)
  if (length(within) > 0) {
    sformula <- paste0(sformula,'+Error(',paste0('`',wid,'`'),'/(',paste0(paste0('`',within,'`'),collapse='*'),'))')
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

getTranslator <- function(lang = 'en') {
  i18n <- shiny.i18n::Translator$new(translation_csvs_path = system.file("i18n", package="rshinystatistics"))
  session <- shiny::getDefaultReactiveDomain()
  if (length(session$userData$lang) > 0)
    lang <- session$userData$lang
  i18n$set_translation_language(lang)
  return(i18n$t)
}
