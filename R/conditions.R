# Conditions
#
# Every error, warning and message goes through these, so package conditions
# share the `islh_error`, `islh_warning` and `islh_message` classes.
#
# `call` names the function in the error header. An internal helper that can
# abort takes `call = rlang::caller_env()` and passes it on, so the header
# names the exported function the user called rather than the helper.
# `.envir` is where cli looks up `{values}` in the message.

.islh_abort <- function(
  message,
  ...,
  class = NULL,
  call = rlang::caller_env(),
  .envir = parent.frame()
) {
  cli::cli_abort(
    message,
    ...,
    class = c(class, "islh_error"),
    call = call,
    .envir = .envir
  )
}

.islh_warn <- function(message, ..., class = NULL, .envir = parent.frame()) {
  cli::cli_warn(
    message,
    ...,
    class = c(class, "islh_warning"),
    .envir = .envir
  )
}

.islh_inform <- function(message, ..., class = NULL, .envir = parent.frame()) {
  cli::cli_inform(
    message,
    ...,
    class = c(class, "islh_message"),
    .envir = .envir
  )
}
