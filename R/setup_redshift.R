
#' setup_redshift
#'
#' This function creates a connection object for working with Redshift
#' Does a different thing depending on whether or not this is your
#' first time running the function. If it is, it will prompt you for login
#' information. You can manually control whether you want to re-enter credentials
#' with the `credentials` function argument
#' @return An object named `con` to be used with calls to `DBGetQuery`
#' @export
#'
#' @examples
setup_redshift <- function(credentials = T) {
  if !is.na(keyring::key_get('redshift_user_id')) {
    credentials = F
  }
  if !credentials {
    rstudioapi::askForSecret('redshift_user_id',
                             message = 'Enter your Redshift User ID',
                             title = paste("Redshift Username"))
    rstudioapi::askForSecret('redshift_password')
  }

}
