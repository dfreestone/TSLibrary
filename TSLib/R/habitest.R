#' Load all habitest files
#'
#' @param files files to load
#' @return data.frame with all files together
#' @export
#' @examples
#' data = find_files_bytype("csv", "./data/") %>%
#'        habitest_load_files()
habitest_load_files = function(files)
{
  bind_rows(lapply(files, habitest_load_file))
}

#' Load a single habitest file
#'
#' @param file file to load
#' @return data.frame of a habitest file
#' @export
#' @examples
#' data = habitest_load_file(file)
habitest_load_file = function(file)
{
  col_types = "ccccccc"
  col_names <- c("index", "time", "input_type", "input_id",
                 "event", "register", "unknown")
  df = read_csv(file, col_names = col_names, col_types = col_types)
  col = 4 #ifelse(is.na(as.integer(df[1,4])), 6, 4)
  df %>%
    mutate(subject=as.integer(.[1,col]),
           protocol=as.character(.[2,col]),
           date=as.character(.[6,col]))  %>%
    tail(-7) %>%
    filter(input_type!="Exit",
           event!="ActivityMonitorOn", event!="ActivityMonitorOff") %>%
    mutate(time=as.double(time),
           register=as.double(register)) %>%
    subset(select=-c(index, input_id, unknown))
}
