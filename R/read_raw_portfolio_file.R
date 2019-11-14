read_raw_portfolio_file <- function(project_name) {
  out <- NA
  inputs_path <- with_path_in_10_projects(project_name)("20_Raw_Inputs")

  # FIXME: What if `csv_to_read` has length 0 or > 1? (ASK @Clare2D)
  # Now we do nothing. Is this okay?
  csv_path <- fs::dir_ls(inputs_path, regexp = glue("{project_name}_Input.csv"))
  if (identical(length(csv_path), 1L)) {
    out <- readr::read_csv(csv_path)
  }

  # FIXME: If input path has both .csv and .txt files then this function is
  # expensive -- it re-reades the data and overwrites previous data.
  # Should we end as soon as we successfully read the data? ASK @Clare2D

  txt_path <- fs::dir_ls(inputs_path, regexp = glue("{project_name}_Input.txt"))
  # FIXME: What if `csv_to_read` has length 0 or > 1? (ASK @Clare2D)
  # Now we do nothing. Is this okay?
  if (identical(length(txt_path), 1L)) {
    enc <- readr::guess_encoding(txt_path)$encoding[1]
    out <- utils::read.table(
      txt_path, sep = ",", header = TRUE, fileEncoding = enc
    )
  }

  # Reads in Files saved with a ; not a ,
  # FIXME: Re-reading may be expensive. Should we sanitize malformed objects?
  # ASK @Clare2D
  if (identical(ncol(out), 1L) && identical(length(csv_path), 1L)) {
    out <- readr::read_delim(csv_path, delim = ";")
  }

  if (identical(ncol(out), 1L) && identical(length(txt_path), 1L)) {
    out <- utils::read.table(
      txt_path, sep = "\t", header = TRUE, fileEncoding = enc
    )
  }

  if (identical(ncol(out), 1L) && identical(length(txt_path), 1L)) {
    out <- utils::read.table(
      txt_path, sep = ";", header = TRUE, fileEncoding = enc
    )
  }

  if (!is_dataframe_with_some_row(out)) {
    stop("No portfolio Input File")
  }

  out
}
