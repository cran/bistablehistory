#' Preprocesses time-series data for fitting
#'
#' Performs sanity checks (e.g., whether \code{data} can be used as a data.frame),
#' computes duration of dominance phases (if necessary), assumes a single entry for
#' any missing \code{session}, \code{run}, \code{random_effect}.
#'
#' @param data A table with one or many time-series.
#' @param state String, the name of the column that specifies
#' perceptual state. The column type should be a factor with
#' two or three levels (the third level is assumed to correspond to a
#' transition/mixed phase) or should be convertible to a two level
#' factor (as it would be impossible to infer the identity of transition/
#' mixed phase).
#' @param duration String, name of the column with duration of individual
#' perceptual dominance phases. Optional, you can specify \code{onset}
#' instead.
#' @param onset String, name of the column with onsets of the perceptual
#' dominance states. Optional, used to compute duration of the dominance
#' phases, if these are not provided explicitly via \code{duration}
#' parameter.
#' @param random_effect String, name of the column that identifies random effect,
#' e.g. individual participants, stimuli for a single participant, etc.
#' If omitted, no random effect is assumed. If specified and
#' there is more than one level (participant, stimulus, etc.), it is used
#' in a hierarchical model.
#' @param session String, name of the column that identifies unique
#' experimental session for which a mean dominance phase duration will
#' be computed (see \code{norm_tau} parameter). Code assumes that session
#' IDs are different within a participant but can be the same between them.
#' If omitted, a single mean dominance duration based on the entire time series
#' is used.
#' @param run String, name of the column that identifies unique runs/blocks.
#' If omitted, the data is assumed to belong to a single time series. Code
#' assumes that run IDs are different within an experimental session but
#' can be the same between the session. E.g. session A, runs 1, 2, 3.. and
#' session B, runs 1, 2, 3 but not session A, runs 1, 2, 1.
#'
#' @return A tibble with columns
#'  \itemize{
#'  \item \code{state}
#'  \item \code{duration}
#'  \item \code{random}
#'  \item \code{irandom} - integer, index of \code{random} values,
#'  \item \code{session}
#'  \item \code{run}
#'  \item \code{session_tmean} - numeric, mean duration of clear percepts for every combination of \code{random} and \code{session}.
#'  \item \code{is_used} - integer, whether computed history value needs to be used for linear model fitting.
#'  \item \code{run_start} - integer, 1 for the first row of the run time-series.
#'  }
#'
#' @importFrom dplyr %>% group_by ungroup mutate
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' df <- preprocess_data(br_singleblock, state="State", duration="Duration")
preprocess_data <- function(data,
                            state,
                            duration=NULL,
                            onset=NULL,
                            random_effect=NULL,
                            session=NULL,
                            run=NULL) {
    ## --- 1. Check if data is a data.frame (or something that is convertible to it, e.g. a tibble) ---
    if (!is.data.frame(data)) stop("data parameter is not a data.frame")

    ## --- 2. Check the state column. ---
    # Must be
    # * a factor with two or three levels, or
    # * column with no more than two unique values
    if (!state %in% colnames(data)) stop(sprintf("Column '%s' for state variable is not in the table", state))
    if (sum(is.na(data[[state]]))>0) stop("State column contains NAs")
    if (is.factor(data[[state]])){
      if (length(levels(data[[state]])) < 2) stop("Too few levels for state column, should be either 2 or 3")
      if (length(levels(data[[state]])) > 3) stop("Too many levels for state column, should be either 2 or 3")
    }
    else {
      if (length(unique(data[[state]])) != 2) stop(sprintf("If state column is not a factor, it must have just two unique values, not %s", length(unique(data[[state]]))))

      # converting state column to a factor
      data[[state]] <- as.factor(data[[state]])
    }

    df <- tibble::tibble(istate = as.integer(data[[state]]),
                         state = data[[state]])

    ## --- 3. Check that either duration or onset a supplied ---
    if (is.null(duration) && is.null(onset)) stop("Either duration or onset time must be specified")
    if (is.null(duration)) { # we must infer duration of individual phases from onsets
      # check that column with this name exists
      if (!onset %in% colnames(data)) stop(sprintf("Column '%s' for onset variable is not in the table", onset))

      # NAs (can't handle those for the onset)
      if (sum(is.na(data[[onset]]))>0) stop("Onset column contains NAs")

      # check that onset is has a numeric type
      if (!is.numeric(data[[onset]])) stop("Onset column must be numeric")

      # computing duration, the values at the end of the runs will be trivially wrong
      # (negative) but this will be corrected once we have full participant/session/run data
      df$duration <- c(diff(data[[onset]]), NA)
    }
    else { # duration can be used directly
      # check that column with this name exists
      if (!duration %in% colnames(data)) stop(sprintf("Column '%s' for duration variable is not in the table", duration))

      # check that is has a numeric type
      if (!is.numeric(data[[duration]])) stop("Duration column must be numeric")
      df$duration <- data[[duration]]
    }

    ## --- 4. Check whether we have information about random_effect ---
    if (is.null(random_effect)) {
      # A single random cluster
      df$random <- 1L
      df$irandom <- 1L
    }
    else {
      # check that column with this name exists
      if (!random_effect %in% colnames(data)) stop(sprintf("Column '%s' for random effect variable is not in the table", random_effect))
      if (sum(is.na(data[[random_effect]]))>0) stop("Random effect column contains NAs")
      df$random <- data[[random_effect]]
      df$irandom <- as.integer(as.factor(data[[random_effect]]))
    }

    ## --- 5. Check whether we have information about experimental sessions ---
    if (is.null(session)) {
      # A single experimental session
      df$session <- 1L
    }
    else {
      # check that column with this name exists
      if (!session %in% colnames(data)) stop(sprintf("Column '%s' for session variable is not in the table", session))
      if (sum(is.na(data[[session]]))>0) stop("Session column contains NAs")
      df$session <- data[[session]]
    }

    ## --- 6. Check whether we have information about individual runs ---
    if (is.null(run)) {
      # A single run
      df$run <- 1L
    }
    else {
      # check that column with this name exists
      if (!run %in% colnames(data)) stop(sprintf("Column '%s' for run variable is not in the table", run))
      if (sum(is.na(data[[run]]))>0) stop("Run column contains NAs")
      df$run <- data[[run]]
    }

    ## --- 7. Time series preprocessing (session_tmean, which durations are used for fitting, service flags)
    df <-
      df %>%
      # making sure last duration is 0 (as we won't be using it for fitting  anyhow)
      dplyr::group_by(.data$random, .data$session, .data$run) %>%
      dplyr::mutate(duration = ifelse(dplyr::row_number()==dplyr::n(), 0, .data$duration)) %>%

      # computing average clear percept duration for each experimental session
      dplyr::group_by(.data$random, .data$session) %>%
      dplyr::mutate(session_tmean = mean(.data$duration[.data$istate<3], na.rm=TRUE)) %>%

      # marking out percept that will be used to fit history
      # To this end, we are ignoring
      # * any transition/mixed states (istate==3)
      dplyr::ungroup() %>%
      dplyr::mutate(is_used = .data$istate != 3) %>%

      # * first durations for each state (as they had no chance to be properly history dependent)
      dplyr::group_by(.data$random, .data$session, .data$run, .data$istate) %>%
      dplyr::mutate(is_used = ifelse(dplyr::row_number()==1, FALSE, .data$is_used)) %>%

      # * last duration in each block (as it is not used for predictions)
      dplyr::group_by(.data$random, .data$session, .data$run) %>%
      dplyr::mutate(is_used = ifelse(dplyr::row_number()==dplyr::n(), FALSE, .data$is_used)) %>%

      # replacing any NAs with zeros
      dplyr::mutate(duration = tidyr::replace_na(.data$duration, 0)) %>%

      # add time series start flag (first element for each run)
      dplyr::group_by(.data$random, .data$session, .data$run) %>%
      dplyr::mutate(run_start  = ifelse(dplyr::row_number()==1, 1, 0))   %>%

      dplyr::ungroup()

    ## --- 8. Second durations check ---
    # Check that after all cleaning up used durations are strictly non-negative positive and
    # that durations used for history fitting are strictly positive (only the is_used once, so, ignoring
    # transitions and last durations)
    if (sum(df$duration < 0) > 0) stop("Table contains negative durations.")
    if (sum(df$duration[df$is_used] == 0) > 0) stop("Table contains zero durations for clear states. Check original duration column for 0 and NAs or, perhaps, you forgot to specify participant/session/run variable(s)?")

    df
}
