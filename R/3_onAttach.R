#' @import data.table ggplot2
#' @importFrom magrittr %>%
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0(
    "PACKAGE ",
    # utils::packageDescription("PACKAGE")$Version,
    "\n",
    "https://docs.sykdomspulsen.no/PACKAGE"
  ))
}
