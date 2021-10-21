#' Count the number of claims for each procedures each year
#'
#' @param data_root Directory path to scan for files
#' @param file.pattern Pattern to use for searching files (default: .csv)
#' @param output_path Path for saving report. Path must include .csv name of report. Default is same directory + new folder called reports/surgeon_count_report.csv
#' @return data.frame with three columns
#'
#' @importFrom utils write.csv
#' @importFrom data.table fread
#' @export
procedure_count <- function(data_root,
                          output_path = NA,
                          file.pattern = ".csv",
                          save_report = FALSE ) {
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
      dplyr::group_by(facility_clm_yr,e_proc_grp_lbl) %>%
      dplyr::summarise(count_id = n()) %>%
      dplyr::arrange(-count_id) %>%
      as.data.frame()
  }))

  if (is.na(output_path)) {
    output_path = paste0(data_root, "/reports/surgeon_count_report.csv")
  }

  out_dir <- dirname(output_path)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  write.csv(out_df, output_path, quote = F, row.names = F)

  return(out_df)
}
