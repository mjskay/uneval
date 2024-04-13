#' @returns
#' **NOTE:** Due to the fragility of [`promise`]s, it is often best to keep them
#' in [`arglist`]s. If you attempt to assign the results of this function
#' to a variable instead of passing it directly to another function, it will
#' likely be [forced][force] and no longer be a [`promise`].
