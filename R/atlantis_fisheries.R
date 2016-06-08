atlantis_fisheries <- function (adir) {
    fisheries_doc <- XML::xmlParse(attr(adir, 'xml_fisheries'))
    fisheries_data <- fetch_xml_attributes(fisheries_doc, 'Fishery', c('Code', 'Index', 'Name', 'IsRec', 'NumSubFleets'))
    fisheries_data
}

atlantis_fisheries_catch <- function(adir,
        area_data,
        fishery,
        start_year = attr(adir, 'start_year')) {
    # Read in all (functional_group)_Catch_(fishery)
    nc_out <- ncdf4::nc_open(attr(adir, 'nc_catch'))
    fishery_vars <- list_nc_variables(nc_out, paste0('Catch_FC', fishery$Index, '$'))
    catch_tonnes <- fetch_nc_variables(nc_out, fishery_vars)
    dims <- expand.grid(
        area = as.character(area_data$name),
        time = nc_out$dim$t$vals,
        functional_group = sub('_.*', '', fishery_vars),
        stringsAsFactors = TRUE)
    if (nrow(dims) == 0) return(data.frame())

    # Combine with catch data
    data.frame(
        area = dims$area,
        time = dims$time,
        year = atlantis_time_to_years(dims$time, start_year),
        month = atlantis_time_to_months(dims$time),
        fishery = fishery$Code,
        functional_group = dims$functional_group,
        weight_total = as.numeric(catch_tonnes),
        stringsAsFactors = TRUE)
}
