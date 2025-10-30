#' Create Summary Table
#'
#' This function generates summary table given various customizable options.
#'
#' @param data The dataset to be analyzed and summarized.
#' @param by A variable to group the data by; if set to the default, \code{NA}, no analysis will be performed.
#' @param reporting_type A string indicating the type of test to perform. Options are:
#'   \describe{
#'      \item{"parametric"}{Forces the function to perform a parametric test (e.g., t-test) and report the mean ± SD.}
#'      \item{"non_parametric"}{Forces the function to perform a non-parametric test (e.g., Wilcoxon test) and report the median ± IQR.}
#'      \item{"auto"}{Automatically chooses the test based on the data's characteristics
#'      (e.g., a normality test to decide between parametric and non-parametric). If parametric, mean ± SD is reported;
#'      if non-parametric, median ± IQR is reported.}
#'}
#' @param analysis Logical. If \code{TRUE}, performs statistical analysis; if \code{FALSE}, generates descriptive statistics only. Default is \code{TRUE}.
#' @param complete_rows Logical. If \code{TRUE}, and if the feature is a factor with 2 levels, both levels will be shown in the table. Default is \code{TRUE}.
#' @param same_row Logical. If \code{TRUE}, displays the feature name in the same row as its summary data. Default is \code{TRUE}.
#' @param multivariate Logical. If \code{TRUE}, performs multivariate analysis. Default is \code{FALSE}.
#' @param debug Logical. If \code{TRUE}, enables debug mode and disables simplification. Default is \code{FALSE}.
#' @param risk_measure A string indicating the type of association measure to calculate. Options are:
#'  \describe{
#'     \item{\code{"OR"}}{Calculates the Odds Ratio (OR) and its 95-percent confidence interval based on a 2x2 table.}
#'     \item{\code{"RR"}}{Calculates the Relative Risk (RR) and its 95-percent confidence interval based on a 2x2 table.}
#'}
#'
#' @param fmt_num A function applied to each numeric statistic (other than p-values)
#'   before returning the summary table. The function must accept exactly two
#'   arguments \code{(x, context)}, where \code{x} is the raw numeric value and
#'   \code{context} is a named list containing:
#'   \itemize{
#'     \item{\code{param_name}}{(`character`) name of the statistic being
#'           formatted (e.g. `"p"`, or a label describing whether this value is
#'           an "outer" statistic such as mean or n, or an "inner" statistic such
#'           as sd or percent).}
#'     \item{\code{test_name}}{(`character`) name of the statistical test from
#'           which the statistic originates.}
#'     \item{\code{isfeatnum}}{(`logical`) whether the feature is numeric.}
#'     \item{\code{isresnum}}{(`logical`) whether the result is numeric.}
#'   }
#'   The function must return a single formatted character scalar. The default is
#'   the internal formatter \code{fmt_num_default()}.
#'
#' @param fmt_p_default A function applied to each raw numeric p-value before any
#'   user-specified post-processing via \code{fmt_p_after}. The function must
#'   accept at least one argument: a numeric vector of p-values, and may also
#'   accept \code{fmt_num} as an injected argument to control numeric rounding of
#'   p-values before decoration. The default is the internal formatter
#'   \code{handle_ps()}.
#' @param fmt_p_after Optional function applied to each already-formatted
#'   p-value. Must take a single character p-value and return a single character
#'   value. If \code{NULL}, no post-formatting is performed.
#' @param fmt_cd A function used to format a pair of summary
#'   statistics representing central tendency and dispersion (e.g. mean–SD or
#'   median–IQR). The function must accept exactly two arguments
#'   \code{(central, dispersion)},where each argument is already rounded
#'   and formatted as a string, and return a single character scalar. The default is the internal formatter.
#'
#' @return A string matrix with the requested analysis and format.
#' @examples
#' # Basic usage:
#' sumtab(data = my_data)
#' my_data %>% sumtab()
#' my_data %>% sumtab(by = "group")
#'
#' # Example of a custom numeric formatter:
#' # This formatter prints p-values with 3 decimals and all other numbers with 2.
#' fmt_num <- function(x, context) {
#'   if (context$param_name == "p") {
#'     n <- 3
#'   } else {
#'     n <- 2
#'   }
#'   format(round(x, n), nsmall = n, trim = TRUE)
#' }
#' my_data %>% sumtab(fmt_num = fmt_num)
#' @export

sumtab <-  function(data, by=NA, reporting_type = "auto", analysis=TRUE, complete_rows=TRUE, same_row=TRUE, multivariate=FALSE, debug=FALSE, risk_measure="OR", fmt_num = fmt_num_default, fmt_p_default = handle_ps, fmt_p_after = NULL, fmt_cd=default_fmt_cd){
  # Check if the provided reporting_type is valid
  if (!reporting_type %in% c("parametric", "non_parametric", "auto")) {
    stop("Invalid reporting_type. Choose 'parametric', 'non_parametric', or 'auto'.")
  }
  if (!risk_measure %in% c("OR", "RR")) {
    stop("Invalid risk_measure. Choose 'OR' or 'RR'.")
  }

  if (reporting_type == "non_parametric" && multivariate) {
    stop("Only parametric tests are available for multivariate analysis.")
  }

  try_res <- try(fmt_cd("X", "Y"), silent = TRUE)
  if (inherits(try_res, "try-error")) {
    stop("Calling `fmt_cd(\"X\", \"Y\")` failed — check your function body.")
  }

  if (!is.null(fmt_num)) {
    if (length(formals(fmt_num)) != 2) {
      stop("`fmt_num` must accept exactly two arguments: (value, context).")
    }
  }

  if (multivariate) reporting_type <- "parametric"

  if (is.na(by)){
    response <- NULL
  } else {
    response <- data[[by]]
  }
  isresnum <- is.numeric(response)
  numresp <- 1
  numfeat <- 1
  analysis <- analysis & !is.null(response)

  model <- NA
  if (multivariate & analysis){
    model <- doing_multivariate(isresnum, data, by)
  }

  sum <- mapply(function(feature, name){
    if (!is.null(response)) {
      if (name==by){
        return()
      }
    }

    isfeatnum <- is.numeric(feature)
    test <- list()
    ### to prevent rounding exceptions
    test$est <- NA
    test$conf.int <- NA
    test$inner <- NA
    test$outer <- NA


    print(name)

    if( (isresnum | is.null(response)) & isfeatnum){ ### handles by=NA
      if (!is.null(response)) {
        test$lmm <- lm(response ~ feature)
      } else {
        numresp <- 0
      }
      param <- is_numnum_param(feature, test$lmm, reporting_type)
      test[c("outer", "inner")] <- handle_numnum_des(feature, param)
    }

    if(isresnum & !isfeatnum){
      cate <- as.factor(feature)
      param <- is_numcat_param(response, feature, reporting_type)
      test <- handle_numcate_des(response, feature, param)
      numfeat <- test$numcate
    }

    if(!isresnum & isfeatnum & !is.null(response)){
      cate <- as.factor(response)
      param <- is_numcat_param(feature, response, reporting_type)
      test <- handle_numcate_des(feature, response, param)
      numresp <- test$numcate
    }

    if(!isresnum & !isfeatnum){ ### handles by=NA
      response <- as.factor(response)
      feature <- as.factor(feature)
      test <- handle_catecate_des(feature, response)
      numfeat <- test$numfeat
      numresp <- test$numresp
    }


    if (analysis){
      test <- handle_all_inf(test, name, feature, response, isfeatnum, isresnum, numfeat, numresp, param, multivariate, model, risk_measure)

      if ("fmt_num" %in%  names(formals(fmt_p_default))) {
        test$p <- fmt_p_default(test$p, fmt_num = fmt_num) # handled here (not in handle_all_inf), so it's safe to apply format_p now
      } else {
        test$p <- fmt_p_default(test$p)
      }

      if (!is.null(fmt_p_after)){
        test$p <- fmt_p_after(test$p)
      }

    }

    test <- test[names(test) %in% c("inner", "outer", "est", "ci", "p")] ### getting rid of default tests elements

    idx <- !(names(test) %in% c("p", "name"))
    test[idx] <- Map(function(x, nm)
      fmt_num(x, list(param_name = nm,
                      test_name = test$name,
                      isfeatnum = isfeatnum,
                      isresnum = isresnum)),
      test[idx],
      names(test)[idx]
    )

    test$name <- NULL # no longer needed

    ### unifying inner outer
    if (!isresnum & !isfeatnum){
      test$outer <- substr(test$outer, 0, as.numeric(unlist(gregexpr("\\.", test$outer)) - 1)) ###  3.00 -> 3
      test$inner <- ifelse(test$inner=="100.00", "100", test$inner)
      test$inout <- paste0(test$outer, " (", test$inner, "%)")
    }else{
      test$inout <- fmt_cd(test$outer, test$inner)
    }

    if (isresnum) test$inout <- test$inout[-1]
    test$inout <- matrix(test$inout, nrow=numfeat) ## to bring matrix to the beginning
    test[c("inner", "outer")] <- NULL

    #concatenating 95% CI, for multivariate with multiple leves, ci is matrix
    test$ci <- if (is.matrix(test$ci)) {
      apply(test$ci, 1, function(x) paste0(x[1], ", ", x[2]))
    } else {
      paste0(test$ci[1], ", ", test$ci[2])
    }

    test$est <- ifelse(test$est == "NaN", "-", test$est)

    if ((numfeat > 2 | numresp > 2) & !multivariate) {
      test$est <- "-"
      test$ci <- "-"
    }

    test$ci <- ifelse(test$ci == "NA, NA", "-", test$ci) # in the case of kandall

    ### removing redundant row and adding levels or ""
    if(!complete_rows & numfeat==2) {
      test$inout <- test$inout[2,, drop = F]
      test$featlvls <- levels(as.factor(feature))[2]
    }else if (numfeat != 1) {
      test$featlvls <- levels(as.factor(feature))
    }else{
      test$featlvls <- ""
    }

    ### padding "", only for 1: est, 2: ci, 3:p
    cols <- c("featlvls", "inout")
    if (analysis){
      pads <- rep("", nrow(test$inout) - length(test$est))
      test[names(test) %in% c("est", "ci", "p")] <- lapply(test[names(test) %in% c("est", "ci", "p")],
                                                           function(x) if(multivariate) c(pads, x) else c(x, pads))
      cols <- c(cols, "est", "ci", "p")
    }
    if (length(test$inout)==0) test$inout <- "" # for num num case
    test <- test[cols]
    test <- do.call(cbind, test)

    ### handling name column
    if (same_row) {
      test <- cbind(c(name,rep("", nrow(test)-1)), test)
    }else{
      test <- cbind(c(name, rep("", nrow(test)) ), apply(test, 2, function(x) c("", x)))
    }

    ##response variable levels for column name
    if(numresp == 1){
      resplvls <-  by
    }else{
      resplvls <- levels(as.factor(response))
    }



    cols <- c("Variable", "Feature Levels")

    if (!isresnum) cols <- c(cols, "Total")
    if (numresp != 0) {cols <- c(cols, resplvls)}
    if (analysis) {
      cols <- c(cols, "Estimate", "95% CI", "P")
    } else {
        test <- test[,!(colnames(test) %in% c("est", "ci")), drop=F] #while dealing with ci and est, these two are created blank
    }
    colnames(test) <- cols

    return(test)

  }, data, colnames(data), SIMPLIFY = FALSE)

  if (!debug) {
    sum <- do.call(rbind, sum) #simplifying list
  }

  rownames(sum) <- NULL

  return(sum)
}

