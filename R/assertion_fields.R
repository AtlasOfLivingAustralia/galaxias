#' Internal function to document fields referenced by `assertions` code
#' 
#' Note that these are the fields that are _checked_, not those that are 
#' _required_; this is an important distinction as it is false to interpret
#' this list as mandatory fields.
#' 
#' This may not be critical information for `galaxias`; I'm currently using
#' it to guide which fields are most 'important' in the vignettes, and it
#' has no functional use as yet. One option might be to move it to `tawnydragon`
#' to help rank term importance, e.g. in `view_terms()` 
#' 
#' This information is derived from an ALA spreadsheet called 'Data Quality Checks'
#' 
#' This url is also a useful resource:
#' https://github.com/AtlasOfLivingAustralia/ala-dataquality/wiki
#' @noRd
#' @keywords Internal
asssertion_fields <- function(){

  list(
    spatial = list(
    "continent"                = list("continent_country_mismatch",
                                      "continent_derived_from_coordinates",
                                      "continent_invalid"),
    "coordinatePrecision"      = list("coordinates_centre_of_stateprovince",
                                      "coordinate_precision_invalid",
                                      "uncertainty_not_specified"),
    "coordinateUncertaintyInMetres" = list(
                                      "coordinate_uncertainty_metres_meters_invalid",
                                      "uncertainty_in_precision",
                                      "uncertainty_not_specified"),
    "country"                  = list("country_mismatch",
                                      "country_derived_from_coordinates",
                                      "coordinates_centre_of_country",
                                      "coordinate_country_mismatch",
                                      "country_invalid",
                                      "continent_country_mismatch",
                                      "unknown_country_name"),
    "countryCode"              = list("country_mismatch"),
    "decimalLatitude"          = list("presumed_negated_latitude",
                                      "zero_coordinate",
                                      "coordinate_out_of_range",
                                      "state_coordinate_mismatch",
                                      "country_derived_from_coordinates",
                                      "coordinates_centre_of_stateprovince",
                                      "coordinates_centre_of_country",
                                      "location_not_supplied",
                                      "coordinate_reprojected",
                                      "coordinate_invalid",
                                      "coordinate_rounded",
                                      "coordinate_reprojection_suspicious",
                                      "coordinate_country_mismatch",
                                      "continent_derived_from_coordinates",
                                      "presumed_swapped_coordinates"),
    "decimalLongitude"         = list("presumed_negated_longitude",
                                      "zero_coordinate",
                                      "coordinate_out_of_range",
                                      "state_coordinate_mismatch",
                                      "country_derived_from_coordinates",
                                      "coordinates_centre_of_stateprovince",
                                      "coordinates_centre_of_country",
                                      "location_not_supplied",
                                      "coordinate_reprojected",
                                      "coordinate_invalid",
                                      "coordinate_rounded",
                                      "coordinate_reprojection_suspicious",
                                      "coordinate_country_mismatch",
                                      "continent_derived_from_coordinates",
                                      "presumed_swapped_coordinates"),
    "footprintSRS"             = list("footprint_srs_invalid"),
    "footprintWKT"             = list("location_not_supplied",
                                      "footprint_wkt_invalid"),
    "locality"                 = list("location_not_supplied"),
    "locationID"               = list("location_not_supplied"),
    "geodeticDatum"            = list("missing_geodeticdatum",
                                      "geodetic_datum_assumed_WGS84",
                                      "geodetic_datum_invalid",
                                      "coordinate_reprojected",
                                      "coordinate_reprojection_suspicious"),
    "georeferencedBy"          = list("missing_georeferencedby"),
    "georeferenceProtocol"     = list("missing_georeferencedProtocol"),
    "georeferenceSources"      = list("missing_georeferenceSources"),
    "georeferenceVerificationStatus" = list("missing_georeferenceverificationstatus"),
    "georeferenceDate"         = list("missing_georeference_date"),
    "minimumElevationInMeters" = list("elevation_min_max_swapped"),
    "maximumElevationInMeters" = list("elevation_min_max_swapped"),
    "stateProvince"            = list("state_coordinate_mismatch",
                                      "coordinates_centre_of_stateprovince"),
    "verbatimCoordinates"      = list("location_not_supplied"),
    "verbatimDepth"            = list("depth_not_metric",
                                      "depth_unlikely",
                                      "depth_min_max_swapped",
                                      "depth_non_numeric"),
    "verbatimElevation"        = list("elevation_unlikely", 
                                      "elevation_not_metric", 
                                      "elevation_non_numeric"),
    "verbatimLatitude"         = list("location_not_supplied"),
    "verbatimLongitude"        = list("location_not_supplied"),
    ), # end spatial
  "taxonomic" = list(
    
  ),
  "miscellaneous" = list(
    "basisOfRecord"            = list("basis_of_record_invalid") 
  )
)}