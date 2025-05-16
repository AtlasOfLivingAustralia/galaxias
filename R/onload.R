.onLoad <- function(libname, pkgname) {
  if(pkgname == "galaxias") {
    potions::brew(.pkg = "galaxias") # tell `potions` to establish a slot for this pkg
    galaxias_config() # populate that slot with defaults
  }
}