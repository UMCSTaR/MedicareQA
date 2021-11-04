#' Count the number of observations in a set of files inside a target directory
#'
#' @param data_root_or_file Directory path to scan for files OR direct file path if single file
#' @param file.pattern Pattern to use for searching files (default: .csv)
#' @param output_path Path for saving report. Path must include .csv name of report. Default is same directory + new folder called reports/obs_report.csv
#' @return data.frame with two columns where each row contains the file and number of observations in the file
#'
#' @importFrom utils write.csv
#' @importFrom R.utils countLines
#' @export
observation_count <- function(data_root_or_file,
                              output_path = NA,
                              file.pattern = ".csv",
                              save_report = FALSE ) {
  # check if directory or file
  if (R.utils::isDirectory(data_root_or_file)) {
    data_root <- data_root_or_file

    # check if directory exists or not
    if (!dir.exists(data_root)) {
      stop(paste0(
        "directory doesn't exist: ",
        data_root
      ))
    }

    # get the list of csv files inside the directory
    files_found = list.files(data_root, pattern = file.pattern, full.names = T)

    # report error if no matching files are found
    if (length(files_found) < 1) {
      stop(paste0(
        "no files matching pattern found inside: ",
        data_root
      ))
    }
  } else if (R.utils::isFile(data_root_or_file)) {
    data_root <- dirname(data_root_or_file)
    files_found <- data_root_or_file
  } else {
    stop("Something went wrong.")
  }

  message(paste0("Found ", length(files_found), " file(s)."))

  # count the number of observations inside each file
  # NOTE: use an indirect (faster) method to get observations (i.e., number of lines in the file)
  out_df <- as.data.frame(do.call(rbind, lapply(files_found, function(file_path) {
    num_observations <- R.utils::countLines(file_path)
    c(basename(file_path), num_observations - 1) # subtract 1 for the header
  })))

  colnames(out_df) <- c("file", "number_of_observations")

  r_name = "obs"
  if (save_report == TRUE) {
    if (is.na(output_path)) {
      output_path = paste0(data_root, "/reports/",r_name,"_report.csv")
    }
    out_dir <- dirname(output_path)
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

    write.csv(out_df, output_path, quote = F, row.names = F)
  }

  return(out_df)

}

