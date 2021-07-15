
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


#' Winsorization
#'
#' This function performs the replacement of extreme values by less extreme ones
#' in a dataframe for performing parametric tests
#'
#' @param dat a data frame containing the values to be winzorized
#' @param dv column with the dependent variable
#' @param ivs columns with the independent variables
#' @param covar columns with the covariante
#' @param probs numeric vector of probabilities with values in [0,1] as used in quantile
#' @return A data frame containing the column with dv values replaced
#' @export
winzorize <- function(dat, dv, ivs = NULL, covar = NULL, probs = c(0.05, 0.95)) {
  if (dv %in% colnames(dat)) {
    dat[[dv]] <- DescTools::Winsorize(dat[[dv]], probs = probs)
    if (length(ivs) > 0) {
      pdat <- dplyr::group_by_at(dat, dplyr::vars(ivs))
      pdat <- dplyr::group_modify(pdat, function(.x,.y) {
        .x[[dv]] <- DescTools::Winsorize(.x[[dv]], probs = probs)
        if (length(covar) > 0) {
          .x[[covar]] <- DescTools::Winsorize(.x[[covar]], probs = probs)
        }
        return(.x)
      })
      dat <- as.data.frame(pdat)
    }
  }
  return(dat)
}

#' Convert Columns into Categorical Data Based on Quantiles
#'
#' This function ranks numeric data into categories such as high, medium and low based on quantiles.
#' Values lower than q1 (first quantile) are classified as low
#' Values greater than q3 (third quantile) are classified as high
#' Values between q1 and q3 are classified as medium
#'
#' @param data a data.frame with the dataset to be transformed
#' @param vars numeric columns in which perform the ranking numeric data based on quantiles
#' @param qq numeric value {2, 3} to define if the transformation will be carried out in {lower/upper, low/medium/high}
#' @param  params a list with the pairs list(var = var, qq = qq) parameters to be employed for the transformation
#' @return A data.frame containing the ranking data based on quantiles for the columns indicated in the vars argument
#' @export
df2qqs <- function(data, vars = c(), qq = 3, params = NULL) {

  as_qq <- function(values, qq) {
    quantiles <- stats::quantile(values)
    sapply(values, FUN = function(x) {
      if (as.numeric(qq) == 2) {
        if (x < quantiles[[3]]) "lower"
        else if (x > quantiles[[3]]) "upper"
        else NA
      } else {
        if (x <= quantiles[[2]]) "low"
        else if (x >= quantiles[[4]]) "high"
        else "medium"
      }
    })
  }

  ## ... perform function

  if (is.null(params)) {
    lvars <- as.list(vars); names(lvars) <- vars
    params <- lapply(lvars, FUN = function(var) {
      if (is.numeric(data[[var]])) {
        return(list(is.numeric = T, var = var, qq = qq))
      } else {
        return(list(is.numeric = F, var = var))
      }
    })
  }

  data <- data[stats::complete.cases(data[,names(params)]),]
  for (i in seq(1,length(params))) {
    param <- params[[i]]
    if (i == 1 && param$is.numeric) {
      data[[param$var]] <- as_qq(data[[param$var]], param$qq)
    } else if (i > 1 && param$is.numeric) {
      dat <- dplyr::group_by_at(data, dplyr::vars(names(params)[1:i-1]))
      for (j in seq(1, nrow(dplyr::group_data(dat)))) {
        idx <- dplyr::group_data(dat)[['.rows']][[j]]
        data[[param$var]][idx] <- as_qq(as.numeric(data[[param$var]][idx]), param$qq)
      }
    }
    data <- data[stats::complete.cases(data[,names(params)]),]
  }

  for (v in names(params)) {
    if (params[[v]]$is.numeric && as.numeric(params[[v]]$qq) == 2) {
      data[[v]] <- factor(data[[v]], levels=c("lower", "upper"), labels=c("lower", "upper"))
    } else if (params[[v]]$is.numeric && as.numeric(params[[v]]$qq) == 3) {
      data[[v]] <- factor(data[[v]], levels=c("low", "medium", "high"), labels=c("low", "medium", "high"))
    }
  }

  return(data)
}


df2DT <- function(df, pageLength = -1, editable = FALSE, footbuttons = NULL, filename = NULL, prefix = 'table') {
  if (length(dim(df)) > 0) {

    if (is.null(filename)) {
      filename = paste0(prefix, '-', digest::digest(df, algo = "crc32"))
    }

    clengths <- c(25, 50, 100, -1)
    if (!pageLength  %in% clengths) clengths <- c(pageLength, clengths)

    params = list(
      data = df, escape = F, rownames = F, extensions = c("Buttons"),
      class = 'cell-border compact stripe', editable = editable,
      options = list(
        pageLength = pageLength, dom = 'Bfrtip', filter = 'top',
        buttons = list('pageLength','copy','print',
                       list(
                         extend = 'collection',
                         buttons = list(
                           list(extend = 'csv', filename = filename),
                           list(extend = 'excel', filename = filename),
                           list(extend = 'pdf', filename = filename)),
                         text = 'Download')),
        lengthMenu = list(clengths,  paste(clengths, 'rows')),
        columnDefs = list(list(targets = 0:(length(names(df))-1))))
    )

    if (nrow(df) > 100) params$filter = "top"

    if (!is.null(footbuttons) && length(footbuttons) > 0) {
      params$container = tags$table(tableHeader(colnames(df)), tableFooter(c(footbuttons)))
    }

    do.call(DT::datatable, params)
  }
}


