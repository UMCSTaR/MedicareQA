#' Find percentage missing for each variable
#'
#' @param data_root Directory path to scan for files
#' @param file.pattern Pattern to use for searching files (default: .csv)
#' @param output_path Path for saving report. Path must include .csv name of report. Default is same directory + new folder called reports/missing_report.csv
#' @return data.frame with three columns: variable, year and missing %
#'
#' @importFrom data.table fread
#' @export
missing_percent <- function(data_root,
                            output_path = NA,
                            file.pattern = ".csv",
                            save_report = FALSE) {
  # check if directory exists or not
  if (!dir.exists(data_root)) {
    stop(paste0(
      "directory doesn't exist: ",
      data_root
    ))
  }

  # get the list of csv files inside the directory
  files_inside_dir = list.files(data_root, pattern = file.pattern, full.names = T)

  # report error if no matching files are found
  if (length(files_inside_dir) < 1) {
    stop(paste0(
      "no files matching pattern found inside: ",
      data_root
    ))
  }

  message(paste0("Found ", length(files_inside_dir), " files."))

  out_df <- do.call(rbind, lapply(files_inside_dir, function(file_path) {
    data.table::fread(file_path) %>%
      dplyr::summarize_all(~sum(is.na(.)) / length(.)) %>% as.data.frame() %>%
      t()  %>% dplyr::as_tibble(rownames = "var") %>%
      dplyr::arrange(-V1) %>%
      dplyr::mutate(missing_percnt = scales::percent(V1), year = tools::file_path_sans_ext(basename(file_path))) %>%
      dplyr::filter(V1 != 0) %>% dplyr::select(-V1)
  }))

  r_name = "missing"
  if (save_report == TRUE) {
  save_file()
  }

  return(out_df)
}
