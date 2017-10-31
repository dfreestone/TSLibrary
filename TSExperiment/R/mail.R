
#' Email
#'
#' @param to_address List of email addresses
#' @return NULL
#' @export
#' @examples
SendMail <- function(subject, body=""){
  needs(tidyverse, gmailr)
  dropbox = file.path(dirname(DropBoxPaths()$LocalActiveExperimentPath), "system")

  addresses <- file.path(dropbox, "addresses.csv") %>%
    read_csv()

  emails = addresses %>%
    mutate(
      To = sprintf('%s <%s>', name, email),
      From = 'Freestone-lab<freestonelab@gmail.com>',
      Subject = sprintf("[Freestone-lab] %s", subject),
      body = body) %>%
    select(To, From, Subject, body) %>%
    pmap(mime)

  use_secret_file(file.path(dropbox, "mail.json"))

  safe_send_message <- safely(send_message)
  emails %>%
    map(safe_send_message)
}


# NOTE(David): This was the old function, for some reason I can't get rjava to work
# EmailConfirm <- function(subject, body=" ", attachments=NULL){
#   needs(mailR)
#   dropbox = DropBoxPaths()$LocalActiveExperimentPath
#   to_address = readLines(file.path(dropbox, "emails.txt"))
#   send.mail(from = "freestonelab@gmail.com",
#             to = to_address,
#             subject = subject,
#             body = body,
#             attach.files = attachments,
#             smtp = list(host.name = "smtp.gmail.com", port = 465,
#                         user.name = "freestonelab@gmail.com",
#                         passwd = "ForEmailAlerts", ssl = TRUE),
#             authenticate = TRUE,
#             send = TRUE)
#   return(NULL)
# }
