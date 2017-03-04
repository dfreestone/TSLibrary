#' Session information from dates
#'
#' @param date dates for each session
#' @return session numbers
#' @export
#' @examples
#' data = habitest_load_file(file) %>%
#'        mutate(session=habitest_session(date))
session_fromdate = function(date)
{
  as.numeric(factor(date, unique(date)))
}

#' Find all files in a path
#'
#' @param FilePath location and pattern to look for
#' @return all files matching a pattern
#' @export
#' @examples
#'  find_files("~/Dropbox/Data/h/*")
#'  find_files("~Dropbox/Data/h/*.csv")
#'  find_files("~Dropbox/Data/h/*.999")
find_files = function(FilePath)
{
  Sys.glob(FilePath)
}
