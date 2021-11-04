#' save file
#'
#' @param data_root_or_file Directory path to scan for files OR direct file path if single file
#' @param output_path Path for saving report. Path must include .csv name of report. Default is same directory + new folder called reports/surgeon_count_report.csv
#'
#' @importFrom utils write.csv
#' @export
save_file <- function(data_root_or_file,
                      output_path = NA) {
  if (is.na(output_path)) {
    output_path = paste0(data_root_or_file, "/reports/",r_name,"_report.csv")
  }

  out_dir <- dirname(output_path)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  write.csv(out_df, output_path, quote = F, row.names = F)
}
