#' Send completion notification
#'
#' Print the elapsed time and send a notification when requested.
#'
#' @param time Numeric value with the elapsed running time in seconds.
#' @param email Logical value indicating whether an email notification should be
#'   sent.
#' @param ntfyAccount Optional email address or ntfy topic used to send the
#'   notification.
#' @param model Model name or expression included in the notification message.
#'
#' @returns `NULL`. The function prints the elapsed time and sends a notification
#'   when requested.
#'
#' @keywords internal
notify <- function(time, email, ntfyAccount, model) {

  if (time < 300) {
    cat(paste0("Elapsed time: ", round(time, 2), " seconds.\n"))
    theMessage <- paste0("Nimble has just finished ",
                         format(Sys.time(), "(%b %d, %X)"),
                         " to run model ", model,
                         " with a total running time of ",
                         round(time, 2), " seconds.")
  } else {
    if (time < 3600) {
      cat(paste0("Elapsed time: ", round(time / 60, 2), " minutes.\n"))
      theMessage <- paste0("Nimble has just finished ",
                           format(Sys.time(), "(%b %d, %X)"),
                           " to run model ", model,
                           " with a total running time of ",
                           round(time / 60, 2), " minutes.")
    } else {
      cat(paste0("Elapsed time: ", round(time / 3600, 2), " hours.\n"))
      theMessage <- paste0("Nimble has just finished ",
                           format(Sys.time(), "(%b %d, %X)"),
                           " to run model ", model,
                           " with a total running time of ",
                           round(time / 3600, 2), " hours.")
    }
  }

  if (is.null(ntfyAccount) & email == TRUE) {
    cat("No email account to notify. No notification has been sent.")
  }

  if (!is.null(ntfyAccount)) {
    if (email) {
      resul <- try(ntfy::ntfy_send(message = theMessage,
                                   title = "Nimble has finished",
                                   topic = "NimbleTools",
                                   email = ntfyAccount))
      if (inherits(resul, "try-error")) {
        cat("Ntfy error. Possibly the email account is invalid or the limit for the daily number of emails has been reached. Try the option email = FALSE and use the ntfy app.")
      }
    } else {
      resul <- try(ntfy::ntfy_send(message = theMessage,
                                   title = "Nimble has finished",
                                   topic = ntfyAccount))
      if (inherits(resul, "try-error")) {
        cat("Ntfy error. Possibly the subscribed topic in Ntfy is invalid or the limit for the daily number of push messages to the ntfy app has been reached. Check these issues.")
      }
    }
  }
}
