#' Find percentage missing for each variable
#'
#' @param data_root_or_file Directory path to scan for files OR direct file path if single file
#' @param file.pattern Pattern to use for searching files (default: .csv)
#' @param output_path Path for saving report. Path must include .csv name of report. Default is same directory + new folder called reports/missing_report.csv
#' @return data.frame with three columns: variable, year and missing %
#'
#' @importFrom data.table fread
#' @export
missing_percent <- function(data_root_or_file,
                            output_path = NA,
                            file.pattern = ".csv",
                            save_report = FALSE) {

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

  out_df <- do.call(rbind, lapply(files_found, function(file_path) {
    data.table::fread(file_path) %>%
      dplyr::summarize_all(~sum(is.na(.)) / length(.)) %>% as.data.frame() %>%
      t()  %>% dplyr::as_tibble(rownames = "var") %>%
      dplyr::arrange(-V1) %>%
      dplyr::mutate(missing_percnt = scales::percent(V1), year = tools::file_path_sans_ext(basename(file_path))) %>%
      dplyr::filter(V1 != 0) %>% dplyr::select(-V1)
  }))

  r_name = "missing"
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
