
#' setup_redshift
#'
#' This function creates a connection object for working with Redshift. You'll need
#' to be on the VPN for this to work.
#'
#' This function does a different thing depending on whether or not this is your
#' first time running the function. If it is, it will prompt you for login
#' information. You can manually control whether you want to re-enter credentials
#' with the `credentials` function argument, for example when you change these.
#'
#'
#' @return An object named `con` to be used with calls to `DBGetQuery`
#' @export
#'
#' @examples
setup_redshift <- function(credentials = T) {
  tryCatch(
    keyring::key_get(
      'RStudio Keyring Secrets',
      'redshift_username'),
    error = function(e) {
     credentials <<- F
    }
  )
  if (!credentials) {
    rstudioapi::askForSecret(
      'redshift_username',
       message = paste('Enter your Redshift User ID',
                       'Click "Remember With Keyring" to save your ID locally',
                       sep = '\n'),
      title = paste("Redshift Username"))
    rstudioapi::askForSecret(
      'redshift_password',
      message = paste('Enter your Redshift Password',
                      'Click "Remember With Keyring" to save your ID locally',
                      sep = '\n'),
      title = paste("Redshift Password"))
  }

  # finally, we setup a con object
  DBI::dbConnect(odbc::odbc(),
            Driver       = "Amazon Redshift (x64)",
            servername   = "dw.data-prod.cars.com",
            database     = "dw",
            UID          = keyring::key_get('RStudio Keyring Secrets', 'redshift_username'),
            PWD          = keyring::key_get('RStudio Keyring Secrets', 'redshift_password'),
            Port         = 5439)
}
