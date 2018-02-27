
#' Remove File If Present
#'
#' Remove a file if present.
#'
#' @param filename Name of file to remove, if present
#' @export
removeIfPresent <- function(filename){
  if(file.exists(filename)){
    message("Removing file:\n\t", filename)
    file.remove(filename)
  }
}

#' Fetch File If Missing
#'
#' Fetch file if missing, in which case, record timestamp when
#' downloaded, and remove any specified cache files.
#'
#' @param destfile Where to save file, if it's missing
#' @param url URL to use to fetch destfile
#' @param timestamp_file Optional file to record timestamp when new destfile is downloaded
#' @param cached_files Optional files that should be considered out-of-date and removed, if present when new destfile is fetched
#' @param method Method to use when downloading; passed to download.file.
#' @export
fetchIfMissing <- function(
  destfile,
  url,
  timestamp_file,
  cached_files,
  method = "curl"
){
  if(!file.exists(destfile)){
    message("destfile not present:\n\t", destfile)
    message("Downloading destfile from:\n\t", url, "\n")

    if(!is.null(timestamp_file)){
      download.file(url, destfile = destfile, method = method)
      date_downloaded <- now()
      timestamp <- strftime(date_downloaded, usetz = T)
      write(timestamp, file = timestamp_file)
      message("\nTimestamp ", timestamp, " written to:\n\t", timestamp_filename)
    }

    # Remove specified cached files whenever we download new destfile.
    lapply(cached_files, removeIfPresent)
  }
}
