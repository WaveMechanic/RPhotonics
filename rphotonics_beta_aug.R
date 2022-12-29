library(data.table)
library(plotly)
library(stringr)
library(pracma)
library(assertthat)
# http://adomingues.github.io/2015/09/24/finding-closest-element-to-a-number-in-a-list/

######################

# Keysight Functions #

######################

##################
# Form Factor #
##################

#' @title Get text formatted data
#'
#' @description Retrieve from a Keysight Form Factor file.
#' These tables are formatted wih a space between the colnames
#' and data. This is why `skip = 2` is used.
#'
#' @param text_file file name for data target
#'
#' @importFrom data.table fread
#' @importFrom stringr str_replace_all
#'
#' @return `data.table`
#' @export
#'
#' @note
#'
#' TODO Data Standardization for filenames
#' Keysight system ACT09 is PAS
#' Mapping Function for `get_test_data`
get_txt_data <- function(text_file) {

    # Text files for this function need to have
    ## a break between the colnames and data
    data_colnames <-  colnames(data.table::fread(text_file))
    data <- data.table::fread(text_file, skip = 2)

    # Adjust this if you find any other special characters
    ## inside of the colnames
    colnames(data) <- stringr::str_replace_all(data_colnames, pattern = " ",
                                        replacement = "_")

    return(data)
}


#' @title Retrieve files for a specific structure
#' 
#' @description Retrieve all files matching specific structures
#' such as "coupler", "resonator", or any other photonic device
#' with a scheme similar to: "res****.txt"
#'
#' @param directory directory containing data targets
#' @param struct_type photonic structure prefix
#' @param file_ext file type (eg. txt, csv)
#'
#' @return `list`
#' @export
#'
#' @note
#' Mapping function for `get_test_data`
get_structs_from_folder <- function(directory, struct_type, file_ext = "txt") {

    struct_str <- paste(struct_type, ".*", file_ext, sep = "")
    file_list <- list.files(directory, pattern = struct_str, full.names = TRUE)
    return(file_list)
}

#' @title Get development data from Form Factor
#'
#' @description Create a list of named resonance files
#'
#' @importFrom dplyr %>%
#' @importFrom purrr set_names
#'
#' @param directory Directory containg folders of Keysight Form Factor data
#' @param list_index
#' @param struct_type
#'
#' @return
#' List of data tabels
#'
#' @export
#'
#' @note
#' Currently made only for struct type = "res" (Ring Resonators)
get_test_data <- function(directory, list_index, struct_type) {

    # Target directory needs to contain a list of folders with only files
    # eg. \Data = Targetdirectory
    ## \Data contains directories: \Data\chip1, \Data\chip2, \Data\chip3
    ### where *chip directories contain only data files
    data_dirs <- list.dirs(directory)

    # Preform mapping onto the list of directories
    # TODO generalize struct type
    all_files <- lapply(X = data_dirs, FUN = get_structs_from_folder,
                        struct_type = struct_type)

    # Removes each element of `all_files` that is equal to 0
    # Sets names of each file as their path
    named_files <- all_files[lapply(X = all_files, FUN = length) > 0] %>%
                    unlist() %>%
                        purrr::set_names()

    # `all_tables` contains a list of data tables
    all_tables <- lapply(X = named_files, FUN = get_txt_data)

    # TODO generalize which columns to return
    ## This also renames columns to lambda and power
    lambda_power_cols <- c("Laser1.5_Wavelength", "Logger1_Power")
    test_tables <- all_tables[[list_index]][, lambda_power_cols, with = FALSE]
    colnames(test_tables) <- c("lambda", "power")

    return(test_tables)
}

##############
# ACT 09 PAS #
##############

#' @title data.table fread (with targeting)
#'
#' @description Uses `data.table::fread` with user defined Delimiter Target
#'
#' @importFrom data.table fread
#'
#' @param raw_data_path Data path
#' @param section_delim Section header to be found in raw data
#'
#' @return
#' Data table from section delimiter
#'
#' @export
#'
#' @note
#' Mapping Function
#' "skip = __auto__" for default
#' Updated 7_06_2022
fread_range <- function(section_delim, raw_data_path) {

    partitioned_dt <- data.table::fread(raw_data_path, header = FALSE,
                                skip = section_delim)

    return(partitioned_dt)
}


#' @title Keysight Data Retrieval
#'
#' @description Split data from Keysight data drop
#'
#' @importFrom assertthat assert_that
#'
#' @param raw_data_path Data path containg data straight from Keysight tool
#' @param section_delims List of section headers to be found in raw data
#' @param section_names List of Names you would like to assign to each section
#'
#' @return
#' List of Sections
#'
#' @export
#' 
#' @note
#' For our FAB:
#' Delims
#' c("__auto__", "General:", "Settings:", "Setup:", "Timing")
#' c("=== Mueller Row 1 (TLS0) ===", "=== Average IL (TLS0) ===")
#' c("=== PDL (TLS0) ===", "=== TE")
#' Names
#' c("Fab_Info", "General", "Settings", "Setup", "Timing")
#' c("Mueller", "Average_il_TLS0", "Pdl_TLS0", "TE_TM_TLS0")
#' Updated 7_6_2022
split_keysight_csv <- function(raw_data_path, section_delims, section_names) {

    assertthat::assert_that(length(section_delims) == length(section_names))

    sections <- lapply(X = section_delims, FUN = fread_range, raw_data_path)

    names(sections) <- section_names

    return(sections)

}

#' @title Generate Section Names
#'
#' @description generate names for the TE/TM column of data
#' For example, the TE/TM section is named with the amount
#' of fibers observed and modes
#'
#' @param df_te_tm_names Column names from raw te_tm data table
#'
#' @return
#' `List`
#'
#' @export
#'
#' @note
#' Updated 7_6_22
generate_te_tm_col_names <- function(dt_te_tm_names) {

    new_colnames <- c("lambda")

    fiber_to_colname <- function(fiber_num) {
        return(c(paste("loss_f", fiber_num, "_", "te", sep = ""),
                paste("loss_f", fiber_num, "_", "tm", sep = "")))
    }

    fibers <- 1:((length(dt_te_tm_names) - 1) / 2)

    new_colnames <- c(new_colnames,
                        unlist(lapply(X = fibers, FUN = fiber_to_colname))
                        )

    return(new_colnames)
}

#' @title Get TE and / or TM mode data
#'
#' @description Get data from
#' a dataframe that contains te_tm data.
#' This function allows you to put in specific fibers
#' for analysis, as well as the specific mode(s).
#'
#' @param df_te_tm dataframe containing te_tm data
#' @param fiber_nums Number of optical fibers
#' @param modes Modes to select from data table (Defaults to "te" and "tm")
#'
#' @return `data.table`
#'
#'
#' @export
#'
#' @note
#' Updated 7_06_2022 from "get_te_data"
raw_data_by_fiber <- function(dt_te_tm, fiber_nums = c(), modes = c("te", "tm")) {

    # Finds how many fibers were used in dataframe
    # This does take the lambda coloumn into consideration
    fiber_count <- (length(colnames(dt_te_tm)) - 1) / 2

    # Fiber Selection
    fbr_sel <- c(1)

    # Defualts to all fibers
    if (length(fiber_nums) < 1) {
        fbr_sel <- c(1:(fiber_count * 2 + 1))
    }

    if ("te" %in% modes) {
        fiber_te_colnums <- fiber_nums * 2
        fbr_sel <- c(fbr_sel, fiber_te_colnums)
    }

    if ("tm" %in% modes) {
        fiber_tm_colnums <- fiber_nums * 2 + 1
        fbr_sel <- c(fbr_sel, fiber_tm_colnums)
    }

    dt_by_fiber <- dt_te_tm[, fbr_sel, with = FALSE]
    dt_by_fiber <- dt_by_fiber[, lapply(.SD, as.numeric)]

    return(dt_by_fiber)
}


#' @title Write Keysight Sections
#'
#' @description Creates files for each
#' section contained in the `section body` param
#'
#' @importFrom stringr str_replace
#' @importFrom readr write_csv
#'
#' @param section_bodys list sections from keysight data
#' @param path Directory for files to be saved
#'
#' @return void
#'
#' @export
write_sections_to_files <- function(section_bodys, path) {

    data_dir <- stringr::str_replace(path, ".csv", "")
    dir.create(data_dir)

    lapply(X = section_bodys, )

    section_index <- 1
    for (section in section_bodys) {
        new_file_path <- paste(data_dir, "\\",
                        names(section_bodys)[section_index], ".csv", sep = "")

        readr::write_csv(x = section, file = new_file_path)
        section_index <- section_index + 1
    }

}

########################

# Rphotonics Functions #

########################

#########
# Utils #
#########

#' @title Window Apply
#'
#' @description Apply a given function to "windows" of data.
#' Windows are defined as sections of data between points.
#'
#' @param data_list List of data to be windowed acted upon
#'
#' @param fun Function to act upon windows
#'
#' @param ... any other arguments needed for `fun`
#'
#' @return List of transformed data
#'
#' @export
#'
wapply <- function(data_list, fun, ...) {

  window_list <- list()
  window_count <- 1
  while (window_count < length(data_list)) {
    window_list[[window_count]] <- c(data_list[[window_count]],
                                  data_list[[window_count + 1]])

    window_count <- window_count + 1
  }

  return(lapply(X = window_list, FUN = fun, ...))
}

#' @title Data.table window select
#'
#' @description Select windows by data table indices
#'
#' @param data_table table containg data for windowing
#'
#' @param indices values for start and end of window
#'
#' @return List of each windowed part of `data_table`
#'
#' @export
#'
dt_wselect <- function(data_table, indices) {

  # To track selected values
  data_table$raw_index <- seq_len(nrow(data_table))
  dt_windows <- list()
  window_count <- 1
  while (window_count < (length(indices))) {

    dt_windows[[window_count]] <- data_table[indices[[window_count]]:
                                    indices[[window_count + 1]]]
    window_count <- window_count + 1
  }

  return(dt_windows)
}

#' @title Difference
#'
#' @description Subtracts Second term from first
#'
#' @importFrom assertthat assert_that
#'
#' @param diff_terms List of two numbers to find difference
#'
#' @return diff_terms[[2]] - diff_terms[[1]]
#'
#' @export
#'
difference <- function(diff_terms) {

    assertthat::assert_that(length(diff_terms) == 2)
    return(diff_terms[[2]] - diff_terms[[1]])
}

#' @title Get midpoint of a data.table
#'
#' @description Finds midpoint of data using the median
#'
#' @importFrom data.table setattr
#' @importFrom data.table setkey
#' @importFrom data.table J
#'
#' @param dt_window Data table for finding midpoint
#' @param sort_key basis for taking median 
#' 
#' @return `vector` containing the closest `sort_key` value to the median
#'
#' @export
#'
get_midpoint <- function(dt_window, sort_key = "raw_index") {

  dt_window_copy <- data.table::copy(dt_window)

  data.table::setattr(dt_window_copy, "sorted", sort_key)
  data.table::setkeyv(dt_window_copy, sort_key)

  key_median <- median(unlist(dt_window_copy[, sort_key, with = FALSE]))

  middle_row <- dt_window_copy[J(key_median), roll = "nearest"]

  return(unlist(middle_row[, sort_key, with = FALSE]))
}

#' @title Stack Lists
#'
#' @description Adds elements from all given lists element - wise.
#' This is essentially the same as doing a matrix "transpose" except
#' with sparse data
#'
#' @param lists lists to be combined
#' @param embedded_list_length length of each list (they must all be the same)
#'
#' @return `list`
#'
#' @export
#'
#' @note TODO Change setkey to setkeyv for generization
stack_lists <- function(lists, embedded_list_length) {

  stack_level <- 1
  stack_list <- list()

  while (stack_level <= embedded_list_length) {

    combined_list <- c()

    list_element <- 1
    while (list_element <= length(lists)) {
      combined_list[list_element] <-  lists[[list_element]][stack_level]
      list_element <- list_element + 1
    }

    stack_list[[stack_level]] <- combined_list
    stack_level <- stack_level + 1
  }

  return(stack_list)
}

#' @title Mapping Files to a Dataframe
#'
#' @description
#' Creates a `FileID` column that contains
#' the file path and makes a new dataframe
#' for each file with the new column
#'
#' @importFrom purrr set_names
#' @importFrom purrr map_dfr
#' @importFrom dplyr %>%
#' @importFrom utils read.csv
#'
#' @param directory Files that have been collected from Keysight Software
#'
#' @return `dataframe`
#'
#' @export 
map_files_to_df <- function(directory) {
  dfwg <- directory %>% list.files(full.names = TRUE) %>% purrr::set_names() %>%
          purrr::map_dfr(.f = utils::read.csv, .id = "FileID")

  return(dfwg)
}

#' @title Mapping Files to a Dataframes
#'
#' @description
#' Creates a `FileID` column that contains
#' the file path and makes a new dataframe
#' for each file with the new column
#'
#' @importFrom utils read.csv
#'
#' @param directory Files that have been collected from Keysight Software
#'
#' @return `dataframes`
#'
#' @export
map_files_to_dfs <- function(directory) {
  df_list <- list()
  filenames <- list.files(path = directory, full.names = TRUE)

  # TODO make this a for loop for more concise syntax
  # for loops creates a bunch of 'NA' values...
  file_index <- 1
  while (file_index <= length(filenames)) {
    df_list[[file_index]] <- utils::read.csv(filenames[file_index])
  file_index <- file_index + 1
  }
  names(df_list) <- filenames

  return(df_list)
}

#' @title Number from string
#'
#' @description
#' Converts a string into an int/double
#' by decoding the string
#'
#' @importFrom stringr str_replace_all
#' @importFrom dplyr %>%
#' 
#' @param input_string Waveguide dimensions as a string
#'
#'
#' @return `int` or `double`
#'
#' @export
get_number_from_string <- function(input_string) {

  # first replace "p" with a "."
  num_str <- stringr::str_replace_all(tolower(trimws(input_string)), "p", ".") %>%
          stringr::str_replace_all("[a-z]", "") %>% as.numeric()

  return(num_str)
}

#' @title Smooth data
#'
#' @description
#' Uses `loess` function to find a trend in the data and
#' clear up conested data
#'
#' @importFrom data.table copy
#' @importFrom stats loess
#'
#' @param input_data Spectra in dB
#' @param x_label name of x-axis variable
#' @param y_label name of y-axis variable
#' @param span Intensity of smoothing (higher, the more smoothing)
#'
#' @return `vector` of y-data post-smoothing
#'
#' @export
smooth_data <- function(input_data, x_label = "lambda",
                        y_label = "power", span = 0.02) {

  input_data_copy <- data.table::copy(input_data)

  x_data <- unlist(input_data_copy[, x_label, with = FALSE])
  y_data <- unlist(input_data_copy[, y_label, with = FALSE])

  smoothed_data <- stats::loess(y_data ~ x_data, span = span)$fitted
  return(smoothed_data)

}

remove_peak_oultiers <- function(input_data, y_label, smoothed_label,
                                tolerance = 3) {

  y_data <- unlist(input_data[, y_label, with = FALSE])
  smoothed_data <- unlist(input_data[, smoothed_label, with = FALSE])

  min_smooth <- min(smoothed_data)
  max_smooth <- max(smoothed_data)

  min_anomolees <- y_data[y_data < (min_smooth - tolerance)]
  max_anomalees <- y_data[y_data > (max_smooth + tolerance)]

  anomolous_indices <- which(y_data %in% c(min_anomolees, max_anomalees))
  return(input_data[!anomolous_indices])
}

###################
# Ring Resonators #
###################

#' @title Find peaks from Photonic data
#'
#' @description Given Through port data (Spectral looking data)
#' this function will find resonance peaks.
#'
#' @importFrom pracma findpeaks
#' @importFrom data.table as.data.table
#' @importFrom assertthat assert_that
#' @importFrom data.table melt
#'
#' @param input_data Data table containg values for wavelength and power
#' @param lambda_name Name of column containing wavelengths
#' @param power_name Name of column containg power (dB)
#' @param threshold Minimum value of data (experimental)
#' @param minpeakheight Minimum height for peak detection
#' @param minpeakdistance Minumum number of datapoints between peaks
#' @param npeaks Number of peaks to return
#' @param negative Direction of peaks
#' @return Data table grouped by peak. Each group contains locations
#' where the peak starts, ends, and peak value. The table is ordered by
#' ascending data points.
#'
#' @export
#'
get_peaks <- function(input_data, x_label, y_label, threshold = 0,
              minpeakheight = -Inf, minpeakdistance = 1, npeaks = 0,
              negative = TRUE) {

  input_data_copy <- data.table::copy(input_data)

  # Adjust raw data because `findpeaks` only works with Peaks, no troughs
  if (negative) {
    temp_y_data <- input_data_copy[, y_label, with = FALSE]
    input_data_copy[, (y_label) := -temp_y_data]
  }

  # Data should be a single vector, only need to focus on power
  # TODO Enhance findpeaks with generization and loop for optimiziation
  pre_peak_data <- unlist(input_data_copy[, y_label, with = FALSE])

  peaks <- pracma::findpeaks(pre_peak_data,
                            minpeakheight = minpeakheight,
                            minpeakdistance = minpeakdistance,
                            npeaks = npeaks, threshold = threshold)

  # output from find_peaks need to be converted into a data table
  peak_data <- data.table::as.data.table(peaks)
  peak_count <- nrow(peak_data)

  # Tell the user if no peaks were found
  # Tell them to try to change parameters
  no_peaks_warning <- paste("No peaks found", "try adjusting parameters")
  assertthat::assert_that(ncol(peak_data) > 1, msg = no_peaks_warning)

  colnames(peak_data) <- c(y_label, "peak_max", "peak_start", "peak_end")
  peak_data <- peak_data[order(peak_data$peak_max)]

  peak_num_label <- "peak_num"
  peak_data[, (peak_num_label) := c(1:peak_count)]

  # Group `peak_spec` by peak number
  melted_data <- data.table::melt(peak_data, id.vars = c("peak_num"),
                        measure.vars = c("peak_max", "peak_start", "peak_end"),
                        variable.name = "peak_spec", value.name = "raw_index")

  # Using peak_indices to get lambdas from melt_key
  melt_key <- "raw_index"
  peak_indices <- unlist(melted_data[, melt_key, with = FALSE])
  full_peak_data <- input_data_copy[peak_indices]

  # get power and lambda from full_peak_data
  peak_x_col <- unlist(full_peak_data[, x_label, with = FALSE])
  peak_y_col <- unlist(full_peak_data[, y_label, with = FALSE])

  # Finally merge tables
  peak_table <- melted_data[, (x_label) := peak_x_col]

  if (negative) {
    peak_table <- melted_data[, (y_label) := -peak_y_col]

  } else {
    peak_table <- melted_data[, (y_label) := peak_y_col]
  }

  print(peak_table)
  return(peak_table)

}

#' @title Fit to Lorenzian
#'
#' @description Takes a single peak that looks lorenzian and
#' preforms a non linear fit using the `nls`.
#' This function uses the cauchy equation to fit lorenzian peaks
#' and find: [Amplitude, Center, HWHM] the function will
#' automatically turn HWHM to FWHM for convience.
#'
#' Cauchy equation used:
#' (amp * (wid**2)) / ((x - cen)**2 + wid**2)
#'
#' @importFrom stats as.formula
#' @importFrom stats nls
#'
#' @param raw_lorenz_data Data to use for fitting lorenzian
#' @param guesses Guesses for values of of fitting function
#' @param d_b Is data in decibles? (dB) 
#' @param positive Are the peaks pointing up or down?
#'
#' @return List containg:
#' [1] `raw_lorenz_data` attached with fitted data
#' [2] Fitting params (Amplitutde, Center, FWHM)
#'
#' @export
#'
#' @note Inspiration from the following sites:
#' http://emilygraceripka.com/blog/16
#' https://en.wikipedia.org/wiki/Cauchy_distribution
#'
#' TODO generalize function for different functions
fit_to_lorenzian <- function(raw_lorenz_data, guesses,
                            x_label = "lambda", y_label = "power",
                            d_b = TRUE, positive = FALSE) {

  # Fitting function
  cauchy <- function(x, amp, cen, wid) {
      fit_val <- (amp * (wid**2)) / ((x - cen)**2 + wid**2)
      return(fit_val)
  }

  eq_string <- paste("cauchy(", x_label, ", Amplitude, Center, HWHM)")
  fit <- as.formula(paste(y_label, "~", eq_string))

  ## Fatal Chek - ERROR ##

  # Non Linear Fit (nlf)
  nlf <- stats::nls(data = raw_lorenz_data, formula = fit, start = guesses)

  new_data <- nlf$m
  fit_params <- new_data$getAllPars()

  lorenzian_data <- raw_lorenz_data
  lorenzian_data$fitted_power <- predict(nlf)

  # Used for non - log scale values
  # Automatically overwritten for d_b = TRUE
  fit_params["HWHM"] <- fit_params["HWHM"] * 2
  names(fit_params)[3] <- "FWHM"

  if (d_b) {

    # Made for easy custimization
    x_name <- "lambda"
    x_center <- fit_params["Center"]
    key_name <- "fitted_power_key"
    y_name <- "fitted_power"
    y_peak <- fit_params["Amplitude"]

    fwhm_data <- find_fwhm(lorenzian_data, x_name = x_name, x_center = x_center,
                      key_name = key_name, y_name = y_name, y_peak = y_peak)

    fit_params["FWHM"] <- fwhm_data

  }

  return(list(data = lorenzian_data, params = fit_params))

}


#' @title Retrieve Lorenzian peaks
#'
#' @description Uses `spectral_peak_data` to help determine
#' where each lorenzian is located. Uses `cleaving method`
#' to to decide how to seperate the data.
#' 
#' @importFrom dplyr %>%
#' 
#' @param spectral_data Data containg Spectral peaks (eg. gaussian, lorenzian)
#' @param spectral_peak_data Data containg where each peak starts, ends, and is max
#' @param cleaving_method How to seperate the peaks
#'
#' @return `list` of data tables
#' 
#' @export
#'
#' @note
#' TODO Lots of documentation here
#' TODO Enhance generalization of `cleaving_method`
get_lorenzians <- function(spectral_data, spectral_peak_data, cleaving_method) {

  sort_key <- "raw_index"
  # Raw index scheme #
  peak_data <- spectral_peak_data[order(spectral_peak_data$raw_index)]

  peaks <- unlist(peak_data[peak_spec == "peak_max", sort_key, with = FALSE])

  if (length(peaks) > 1) {
    windowed_tables <- dt_wselect(spectral_data, peaks)

    index_list <- unlist(lapply(windowed_tables,
                        cleaving_method)) %>% as.integer()

    names(index_list) <- seq(length(index_list))

    lorenzians <- dt_wselect(spectral_data,
                            c(1, index_list, nrow(spectral_data)))

    return(lorenzians)

  } else {

    return(spectral_data)

  }
}

normalize_peak <- function(input_data, y_label, outlier_args,
                          y_adjust = TRUE, negative = TRUE) {

  input_data_copy <- data.table::copy(remove_peak_oultiers(input_data,
                                                          y_label,
                                                          outlier_args))

  y_data <- input_data_copy[, y_label, with = FALSE]
  adjust_val <- max(unlist(y_data[y_label > 0]))

  adj_colname <- paste(y_label, "_adj", sep = "")
  #print(generate_loss_plot(y_name = "log_power", data = input_data_copy[, log_power := 10**(power_sub/10)]))
  input_data_copy[, (adj_colname) := y_data - adjust_val]
  #print(generate_loss_plot(y_name = adj_colname, data = input_data_copy))

}

#' @title Create a guess for couchy distribution
#'
#' @description Generates a guess for cauchy parameeters:
#' [1] Amplitude [2] Center [3] HWHM. This function is designed
#' to be used with mapply or lapply (mapping function)
#'
#' @param group Which peak number?
#' @param data Data containg where each peak starts, ends, and is max
#' @param x_label Name of x-axis data
#' @param y_label Name of y-axis data
#' 
#' @return `list` of guesses
#' 
#' @export
#'
get_cauchy_guess <- function(group, data, x_label = "lambda", y_label = "power") {

  peak_group <- data[peak_num == group]

  center <- unlist(peak_group[peak_spec == "peak_max",
                                x_label, with = FALSE])

  amplitude <- unlist(peak_group[peak_spec == "peak_max",
                                y_label, with = FALSE])

  end_lambda <- unlist(peak_group[peak_spec == "peak_end",
                                  x_label, with = FALSE])

  start_lambda <- unlist(peak_group[peak_spec == "peak_start",
                                  x_label, with = FALSE])

  hwhm <- difference(c(start_lambda, end_lambda)) / 2

  return(list(Amplitude = amplitude, Center = center, HWHM = hwhm))
}

dbm_to_mw <- function(input_data_db) {

  return(10**(input_data_db / 10))
}

mw_to_dbm <- function(input_data_mw) {

  return(10 * log(input_data_mw))
}


#' @title Ring Resonator Analysis
#'
#' @description Experimental data can be difficult to interpret,
#' but with this function, those days are no more.
#'
#' @importFrom assertthat assert_that
#' @importFrom data.table as.data.table
#'
#' @param input_data Contains information in the form of the following:
#' Lambda or frequency, through_port and / or cross_port power
#' @param ring_perimeter Single trip length for light through resonator
#' @param units A list of units for `port_data`. This list needs to be
#' the same length of `port_data`. If not given, units are guessed
#' @param labels list of labels for `port_data`. If not given,
#' labels will be guessed
#' @param gc_response grating coupler data or "auto"
#' @param data_smoothing Amount of smoothing of type `double` to raw data or "auto"
#' @return
#' Data table of relevant analysis information regarding integrated photonics
#' in form:
#' list(lorenz_data = lorenzian_data,lorenz_peaks = peak_data,
#' ring_specs = ring_specs)
#'
#' @export
#'
#' @note
#' R will automatically throw an error if the function needs an argument
rr_anlaysis <- function(input_data, ring_perimeter, units,
                        labels, gc_response = "auto", data_smoothing = "auto",
                        minpeakdistance = 1000) {

  # Immediatly convert port_data to data.table
  input_data_copy <- data.table::copy(data.table::as.data.table(input_data))
  # Immediatly convert any list to a char vector
  labels <- unlist(labels)

  # Experimental
  # Keep track of data names
  data_colnames <- c(labels[[2]])

  # Enforce that length of labels matches number of columns
  assertthat::assert_that(length(labels) == length(colnames(input_data_copy)))

  # Experimental
  input_data_copy <- input_data_copy[power < 0]

  # Grating coupler response
  gc_response_colname <- "power"
  if (gc_response == "auto") {
    gc_response_colname <- paste(labels[[2]], "_sub", sep = "")
    input_gc_data <- input_data_copy[, c(labels), with = FALSE]
    input_data_copy <- remove_gc_response(input_gc_data)

    # Experimental
    data_colnames <- c(data_colnames, gc_response_colname, "gc_response_y")
  }

  # Experimental
  # Normalize Data
  #normalized_colname <- paste(gc_response_colname, "_norm", sep = "")
  #flat_data <- unlist(input_data_copy[, gc_response_colname, with = FALSE])
  #norm_constant <- max(flat_data)
  #flat_data <- flat_data / norm_constant
  #input_data_copy[, (normalized_colname) := flat_data]
  #data_colnames <- c(data_colnames, normalized_colname)

  # Reduce noise or "smooth" data
  smoothed_colname <- "power"
  if (data_smoothing == "auto") {
    smoothed_colname <- "power_smoothed"
    smoothed_data <- smooth_data(input_data_copy, y_label = "power_sub")
    input_data_copy[, (smoothed_colname) := smoothed_data]

    data_colnames <- c(data_colnames, smoothed_colname)
  } else if (data_smoothing > 0) {
    smoothed_colname <- "power_smoothed"
    smoothed_data <- smooth_data(input_data_copy, y_label = "power_sub",
                                span = data_smoothing)
    input_data_copy[, (smoothed_colname) := smoothed_data]

    data_colnames <- c(data_colnames, smoothed_colname)
  }

  # Conversion from dBm to mW
  # TODO make units variable in the top level function parameter

  mw_colnames <- unlist(lapply(X = data_colnames, FUN = paste, "_mw", sep = ""))


  temp_mw_data <- input_data_copy[, lapply(.SD, dbm_to_mw),
                                  .SDcols = data_colnames]

  colnames(temp_mw_data) <- mw_colnames

  temp_x_data <- unlist(input_data_copy[, labels[[1]], with = FALSE])
  temp_mw_data[, labels[[1]] := temp_x_data, with = FALSE]

  input_data_copy <- merge(input_data_copy, temp_mw_data, by = labels[[1]])
  ##############################################################

  #print(lapply(X = colnames(input_data_copy)[colnames(input_data_copy) != labels[[1]]],
  #          FUN = generate_loss_plot, data = input_data_copy))

  # Extract peak(s) from resonance data
  peaks_y_label <- smoothed_colname
  peak_data <- get_peaks(input_data_copy, "lambda", peaks_y_label,
                        minpeakdistance = minpeakdistance)

  # Seperates all individual lorenzian peaks into a list
  # Note: `cleaving method` designed for customed methods to do this
  lorenzians <- get_lorenzians(input_data_copy, peak_data, get_midpoint)
  
  # If there are 4 peaks, peak_nums = 4
  peak_nums <- seq(nrow(peak_data[peak_spec == "peak_max"]))

  # When using `nls` to fit a function, starting values are
  # required; this is meant to be custom with more work
  guesses <- lapply(X = peak_nums, FUN = get_cauchy_guess,
                    data = peak_data, y_label = peaks_y_label)
  print(guesses)
  print(lorenzians)
  # Apply fit each lorenzian
  x_y_labels <- list(x_label = "lambda", y_label = "power_sub")
  lorenzian_fits <- mapply(FUN = fit_to_lorenzian, lorenzians, guesses,
                          MoreArgs = x_y_labels)

  # Odds contain data table of data, even are fit values
  lorenzian_data_indices <- seq(1, length(lorenzian_fits), by = 2)
  lorenzian_fit_val_indices <- seq(2, length(lorenzian_fits), by = 2)

  # Merge Data with corresponding fit vals
  lorenzian_data <- lorenzian_fits[lorenzian_data_indices]
  lorenzian_fit_vals <- lorenzian_fits[lorenzian_fit_val_indices]

  # Convert fit params to table for convience
  fit_params <- t(as.data.table(lorenzian_fit_vals))
  fit_attributes <- c("Amplitude", "Center", "FWHM")
  colnames(fit_params) <- fit_attributes

  #######################
  # Find all ring Specs #
  #######################

  ring_resonances <- as.list(fit_params[, "Center"]) 
  fwhm <- as.list(fit_params[, "FWHM"])
  ring_er <- as.list(fit_params[, "Amplitude"])

  ring_fsr <- find_fsr(ring_resonances)
  ring_q <- mapply(FUN = find_q_factor, ring_resonances, fwhm)

  # Uses mean fsr
  ring_ng <- lapply(X = ring_resonances, FUN = find_group_index,
                    fsr = mean(ring_fsr), path_length = ring_perimeter)


  ring_resonances <- unlist(ring_resonances)
  fwhm <- unlist(fwhm)
  ring_fsr <- unlist(ring_fsr)
  ring_q <- unlist(ring_q)
  ring_ng <- unlist(ring_ng)
  ring_er <- unlist(ring_er)

  ring_specs <- list(res = ring_resonances, fwhm = fwhm, fsr = ring_fsr,
                    q = ring_q, ng = ring_ng, er = ring_er)
  ######################

  return(list(lorenz_data = lorenzian_data,
              lorenz_peaks = peak_data,
              ring_specs = ring_specs)
        )
}


#' @title Find Full Width Half Max
#'
#' @description Finds FWHM with given data and key. Key is used
#' to determine how to split peak so the y - value can be found on each
#' side of peak.
#'
#' @importFrom data.table J
#' @importFrom data.table setkeyv
#' @importFrom dplyr %>%
#'
#' @param input_data data table containg a single peak
#' @param x_name Name of x - axis values
#' @param x_center x value of center of peak
#' @param key_name How to split peak in half for calculation
#' @param y_name Name of y - axis values
#' @param y_peak y value of center of peak
#' @param d_b Is data in decibles (dB)
#' @param positive Are peaks facing up or down?
#'
#' @return `double` or int
#'
#' @export
#'
find_fwhm <- function(input_data, x_name, x_center, key_name,
                      y_name, y_peak, d_b = TRUE, positive = FALSE) {

  input_data_copy <- copy(input_data)

  key_vals <- input_data_copy[, y_name, with = FALSE]

  input_data_copy[, (key_name) := key_vals]
  data.table::setkeyv(input_data_copy, key_name)
  # Y value of FWMH
  # For decibals (dB), 3dB is 50% power
  y_fwhm <- 0
  if (positive) {
    y_fwhm <-  y_peak - 3
  } else {
    y_fwhm <- y_peak + 3
  }

  # lhs = left hand side, rhs = right hand side

  lhs <- input_data_copy[get(x_name) <= x_center]
  rhs <- input_data_copy[get(x_name) >= x_center]

  # Find the nearest x value to half max value of peak (y)
  lhs_x_fwhm <- lhs[J(y_fwhm), x_name, with = FALSE, roll = "nearest"]
  rhs_x_fwhm <- rhs[J(y_fwhm), x_name, with = FALSE, roll = "nearest"]
  fwhm <- (rhs_x_fwhm - lhs_x_fwhm) %>% unlist()

  return(fwhm)

}


#' @title Free Spectral Range
#'
#' @description Find FSR given all of resonances
#'
#' @importFrom dplyr %>%
#'
#' @param lambda_res List of resonances
#'
#' @return
#'
#' @export
#'
#' @note TODO assert that there are multipe values
find_fsr <- function(lambda_res) {

    return(wapply(lambda_res, difference) %>% unlist())
}


#' @title Group Index
#' 
#' @description Find group index using the formula(s):
#' [1]: (lambda_res**2) / (fsr * path_length)
#'
#' @param lambda_res list of resonances
#' @param fsr Free Spectral Range of resonances
#' @param path_length Path length (eg. Ring resonator Perimeter)
#'
#' @return
#'
#' @export
#'
find_group_index <- function(lambda_res, fsr, path_length) {
    return((lambda_res**2) / (fsr * path_length))
}


#' @title Q Factor
#' 
#' @description finds q-factor using: lambda_res / fwhm
#'
#' @param lambda_res resonance wavelengths
#' @param fwhm Full Width at Half Max
#'
#' @return
#' Q factor(s)
#' @export
#'
find_q_factor <- function(lambda_res, fwhm) {
    return(lambda_res / fwhm)
}

##############
# Waveguides #
##############

#' @title Fiber Cutback Method
#'
#' @description
#' Fit Waveguide Insertion Loss
#'
#' @importFrom data.table setDT
#' @importFrom assertthat assert_that
#' @importFrom methods is
#' @importFrom data.table .N
#' @importFrom data.table .SD
#'
#' @param wg_df
#' Requires `list` or `data` frame containing columns that describe:
#' Wavelengths used
#' Losses measured
#' Length of Waveguides
#'
#' @param group_sel Variable(s) to group to fit by.
#' This needs to be a list or single variable name
#'
#' @param ... Column names to find required data from 'wg_df'.
#' Must be in the following order:
#' 1: Wavelengths
#' 2: losses
#' 3: Waveguide Lengths
#'
#' @return `data table`
#'
#' @export
cut_back_method <- function(wg_df, group_sel, ...) {

  # Declare local var names to be used by function
    lambda_col <- "lambda"
    loss_col <- "loss"
    wg_len_col <- "wg_length"
    group_names <- group_sel

  ########## ##########
  # Find column names from paramatized values
  # Check for list in case of embedded values
  if (...length() > 1) {
    lambda_col <- ..1
    loss_col <- ..2
    wg_len_col <- ..3
  } else if (is(..., "list")) {
    temp_list <- unlist(...)
    lambda_col <- temp_list[1]
    loss_col <- temp_list[2]
    wg_len_col <- temp_list[3]
  } else {
    lambda_col <- ...[1]
    loss_col <- ...[2]
    wg_len_col <- ...[3]
  }

  # group_names check for list in case of embedded values
  if (is(group_names, "list")) {
    group_names <- unlist(group_sel)
  }
  ########## ##########

  # Combine paramaterized column names
  required_cols <- c(lambda_col, loss_col, wg_len_col, group_names)
  fit_grouping <- c(group_names, lambda_col)
  ########## ##########

  # Create Data table from required fitting columns
  wg_dt <- data.table::setDT(wg_df)[, required_cols, with = FALSE]

  # You can use >with = False< or >..{var}]< to get variable col
  wg_lengths <- wg_dt[, c(unique(.SD[, get(wg_len_col)]), Samples = list(.N)),
                      by = fit_grouping]

  # This statement forces you to use at least 3 data points to fit
  assertthat::assert_that(all(wg_lengths$Samples > 2))
  ########## ##########

  # .N will return how large each group in the fit was
  wg_dt <- wg_dt[, c(lm_metrics(.SD, loss_col, wg_len_col), Samples = list(.N)),
                 by = fit_grouping]
  ########## ##########

  return(wg_dt)
}

#' @title Fitting Linear Models for perfomance
#'
#' @description
#' Linear fitting with term coefficients and r-squared
#'
#' @importFrom stats as.formula
#' @importFrom stats lm
#'
#' @param data
#' `data table` expected
#'
#' @param y_term This varibale will be on LHS of equation in the form: 
#' y = mx + b
#'
#' @param x_term This varibale will be on RHS of equation in the form: 
#' y = mx + b
#'
#' @return `data table`
#'
#' @export
lm_metrics <- function(data, y_term, x_term) {

  fitting_list <- stats::as.formula(paste(y_term, "~", x_term))

  fit <- stats::lm(data, formula = fitting_list)

  fit_coeff <- fit$coefficients
  fit_rsq <- summary(fit)$r.squared

  fit_results <- c(fit_coeff, fit_rsq)
  names(fit_results) <- c("intercept", "coeff1", "residuals")

  return(fit_results)
}

####################
# Grating Couplers #
####################

#' @title Find and remove grating coupler response
#'
#' @description
#' When using a grating coupler, spectra will have a curved offset.
#' This function finds the loss from the grating couple and removes
#' it from the spectra. This will not mutate any input data.
#'
#' @importFrom data.table copy
#' @importFrom data.table as.data.table
#' @importFrom stats poly
#' @importFrom stats lm
#'
#' @param input_data Spectra in dB
#' @param x_label name of x-axis variable
#' @param y_label name of y-axis variable
#' @param degree Degree of Polynomial for fitting grating coupler response
#'
#' @return `data.table` with "gc_response_y" & "y_label_sub" attached
#'
#' @export
remove_gc_response <- function(input_data, x_label = "lambda",
                                y_label = "power", degree = 3) {

  input_data_copy <- data.table::copy(input_data)

  x_data <- unlist(input_data_copy[, x_label, with = FALSE])
  y_data <- unlist(input_data_copy[, y_label, with = FALSE])

  # Make a polynomial fit to account for grating coupler response
  poly_x <- stats::poly(x_data, degree)

  fit_vals <- stats::lm(y_data ~ poly_x)$fitted.values

  gc_response_name <- "gc_response_y"
  fit_vals_name <- paste(y_label, "_sub", sep = "")

  # Experimental Postitive Correction
  corrected_y_data <- y_data - fit_vals

  # TODO Investigate
  corrected_y_data <- corrected_y_data - min(corrected_y_data)

  output_data <- data.table::as.data.table(list(x_data, y_data,
                                  fit_vals, corrected_y_data)
                              )

  colnames(output_data) <- c(x_label, y_label, gc_response_name, fit_vals_name)

  return(output_data)

}

#################
# Visualization #
#################

#' @title Plot waveguide data
#'
#' @description Plot waveguide data:
#' 
#' Waveguide Loss, Waveguide Fit, R-sq
#'
#' @param dfwg_fit Data from the `cut_back_method`
#' @param type Type of plot for the plot_ly function
#' @param plot_median Plot median of you data by group
#' @param ... Any other arguments to pass to `generate_loss_plots`
#'
#'
#' @return A figure containg subplots
#'
#' @export
#' 
#' @note plot_median is experimental and currently set-up for a
#' future update of generization.
#'
plot_waveguide_data <- function(dfwg_fit, type = "box",
                                plot_median = TRUE, ...) {

  y_list <- c("coeff1", "intercept", "residuals")

  fig_list <- lapply(X = y_list, FUN = generate_loss_plot,
                    data = dfwg_fit, type = type, ...)

  # TODO Automate the custimization of additional traces
  # Built to show how to begin building scalable custimization
  # Refer to https://cran.r-project.org/web/packages/data.table/
  ## and read "reference manual"
  if (plot_median) {

    group_var <- "lambda"

    col_names <- c("cal_loss_med", "wg_loss_med", "rsq_med")

    # Set-up for custimizable function use
    f <- median

    dfwg_summary_med <- dfwg_fit[, (col_names) := list(f(coeff1, na.rm = TRUE),
                                                    f(intercept, na.rm = TRUE),
                                                    f(residuals, na.rm = TRUE)),
                                                    by = group_var]

    fig_data <- mapply(list, fig_list, col_names, SIMPLIFY = FALSE)

    fig_list <- lapply(X = fig_data,
                      FUN = add_lines_mapping,
                      data = dfwg_summary_med)
  }

  fig <- plotly::subplot(fig_list, nrows = 2, margin = 0.05)

  annotations <- list(
    list(
      x = 0.2,
      y = 1.0,
      text = "Waveguide Loss by site",
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = "bottom",
      showarrow = FALSE
    ),
    list(
      x = 0.75,
      y = 1.0,
      text = "Waveguide Fit by site",
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = "bottom",
      showarrow = FALSE
    ),
    list(
      x = 0.2,
      y = 0.45,
      text = "R Squared by site",
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = "bottom",
      showarrow = FALSE
    )
  )

  fig <- fig %>% layout(annotations = annotations)
  return(fig)
}


#' @title Generate a loss plot
#'
#' @description This function is meant to be used for mapping onto a list
#' defaults:
#'
#' y_name = "power", x_name = "lambda", type = "scatter"
#'
#' @param y_name Name to be used to plot data on the y-axis
#' @param x_name Name to be used to plot data on the x-axis
#' @param data Input data to be used for plot
#' @param type Type of of plot to create (eg. "scatter", "box")
#' @param ... Any other arguments for plot_ly function
#'
#' @return plot
#'
#' @export
#'
generate_loss_plot <- function(y_name = "power", x_name = "lambda", data,
                              type = "scatter", ...) {

  x_data <- as.formula(paste("~", x_name, sep = ""))
  y_data <- as.formula(paste("~", y_name, sep = ""))

  plot <- plotly::plot_ly(data = data, x = x_data, y = y_data,
                              type = type, ...)

  return(plot)
}

#' @title Add lines to plot
#' 
#' @description This function is meant to be used for mapping onto a list
#' of list of plots and their y_names.
#' 
#'
#'
#' @param fig_data list containing a list in form: list(plot, y_name)
#' @param x_name Name to be used to plot data on the x-axis
#' @param data Input data to be used for plot
#'
#' @return plot with line trace added
#'
#' @export
#' 
#' @note Remember, plot needs to come first in fig data, then y_name
#'
add_lines_mapping <- function(fig_data, x_name = "lambda", data) {

  x_data <- as.formula(paste("~", x_name, sep = ""))
  y_data <- as.formula(paste("~", fig_data[[2]], sep = ""))

  new_plot <- fig_data[[1]] %>% plotly::add_lines(data = data,
                                        y = y_data, x = x_data)

  return(new_plot)
}

#' @title Plotly Faucet Wrap
#'
#' @description Merges plots together
#'
#' @importFrom assertthat assert_that
#' @importFrom plotly subplot
#' @importFrom plotly layout
#' @importFrom dplyr %>%
#'
#' @param plots Plotly graph objects
#' @param titles A list of title coresponding to plots
#'
#' @return plotly plot
#'
#' @export
#'
#' @notes Still requires generalization work
plotly_faucet_wrap <- function(plots, titles) {

    # Expects a vector of length 3  [x_pos, y_pos, title]
    # Mapping Function
    create_annotation <- function(title_pos) {

        item <- list(
                x = title_pos[[1]],
                y = title_pos[[2]],
                text = title_pos[[3]],
                xref = "paper",
                yref = "paper",
                xanchor = "center",
                yanchor = "bottom",
                showarrow = FALSE
                )

        return(item)
    }

    # TODO add coords as function parameter
    coords <- list(c(0.2, 1.0), c(0.75, 1.0), c(0.2, 0.45), c(0.75, 0.45))

    # Enforce equal parameter lenghts
    assertthat::assert_that(length(plots) == length(titles))

    # https://stackoverflow.com/questions/9519543/merge-two-lists-in-r
    title_positions <- mapply(c, coords, titles, SIMPLIFY = FALSE)

    # Create annotations based on how many plots & titles given
    annotations <- lapply(X = title_positions, FUN = create_annotation)

    # TODO Make nrows dynamic
    fig <- plotly::subplot(plots, nrows = 2, margin = 0.05)

    fig <- fig %>% plotly::layout(annotations = annotations)

    return(fig)
}

#' @title Find peaks with data smoothing (With progressive visuals)
#'
#' @description Given Through port data (Spectral looking data)
#' this function will find resonance peaks.
#'
#' @param input_dt Data table containg values for wavelength and power
#' (Needs to be named lambda, power respsectfully)
#'
#' @param threshold Minimum value of data (experimental)
#'
#' @param minpeakheight Minimum height for peak detection
#'
#' @param minpeakdistance Minumum number of datapoints between peaks
#'
#' @param npeaks Number of peaks to return
#'
#' @return Data table grouped by peak. Each group contains locations
#' where the peak starts, ends, and peak value. The table is ordered by
#' ascending data points.
#'
#' [1] t_port data (after being flipped)
#' [2] peak_data
#'
#' @export
#'
extract_peaks_visual <- function(input_dt, threshold = 0,
                                 minpeakheight = -Inf, minpeakdistance = 1,
                                 npeaks = 0) {
  ########## Visualization ##########

  plot_flip <- generate_loss_plot(data = t_port_flip)
  plot_fit <- generate_loss_plot(data = t_port_fit)
  plot_smooth <- generate_loss_plot(data = t_port_smooth)

  plot_peaks <- generate_loss_plot(data = t_port_smooth)
  plot_peaks <- plot_peaks %>% plotly::add_trace(data = peak_data, x = ~lambda,
                            y = ~power, color = ~peak_spec, name = ~peak_spec,
                            type = "scatter", mode = "markers")

  all_plots <- list(plot_flip, plot_fit, plot_smooth, plot_peaks)
  plot_names <- c("Raw Data", "Fit Data", "Smooth Data", "Peak Data")

  return(list(t_port = t_port_smooth, peak_data = peak_data,
              visual = plotly_faucet_wrap(all_plots, plot_names)))
}


#' @title Plot Lorenzians with Fit data
#' 
#' @description Using Peak information, along with fitted data.
#' Specifically, this works with data returned from `rr_analysis`
#'
#' @importFrom plotly plot_ly
#' @importFrom plotly add_trace
#' @importFrom plotly layout
#' 
#' @param lorenzian Data table containing Raw lorenzian data, and fit values
#' @param peak_data Data containg where each peak starts, ends, and is max
#' @param peak_num_param Which peak number to use for plotting
#'
#' @return Labeled Plot
#' 
#' @export
#'
#' @examples
lorenzian_fit_plot <- function(lorenzian, peak_data, peak_num_param) {
  lor_plot <- plotly::plot_ly(data = lorenzian, x = ~lambda)
  lor_plot <- lor_plot %>% plotly::add_trace(y = ~power_sub, name = "Raw Data")
  lor_plot <- lor_plot %>% plotly::add_trace(y = ~fitted_power, mode = "lines", name = "Fitted Data")
  lor_plot <- lor_plot %>% plotly::add_trace(data = peak_data[peak_num == peak_num_param],x = ~lambda, y = ~power_smoothed, color = ~peak_spec)
  
  fig <- lor_plot %>% plotly::layout(title = "Fitting Raw Data to Lorenzian", xaxis = list(title = 'Lambda (nm)'), 
         yaxis = list(title = 'Power (dBm)'), xanchor = 'center', yanchor =  'top')
  return(fig)
}

##################
# Synthetic Data #
##################
