.onLoad <- function(libname, pkgname) {
  if(pkgname == "galaxias") {
    potions::brew(.pkg = "galaxias")
    galaxias_config()
  }
}