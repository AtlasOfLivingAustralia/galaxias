.onLoad <- function(libname, pkgname) {
  if(pkgname == "galaxias") {
    potions::brew(
      directory = "data-publish",
      .pkg = "galaxias")
    galaxias_config()
  }
}