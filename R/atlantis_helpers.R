# Given an XML document, pull out group_attributes from attribute group_name,
# return as data.frame
fetch_xml_attributes <- function (xml_doc, group_name, group_attributes, stringsAsFactors = default.stringsAsFactors()) {
    xmlAllAttrs <- Vectorize(XML::xmlAttrs)
    attr_xpath <- paste0(
        "./Attribute[contains('|",
        paste(group_attributes, collapse = "|"),
        "|', concat('|', @AttributeName, '|'))]")

    group_nodes <- XML::getNodeSet(xml_doc, paste0("//*[@AttributeGroupName='", group_name, "']"))
    as.data.frame(t(vapply(group_nodes, function (n) {
        # Pull out all attributes we are interested in from the group
        rv <- xmlAllAttrs(XML::getNodeSet(n, attr_xpath))
        structure(rv["AttributeValue",], names = rv["AttributeName",])
    }, rep("", length(group_attributes)))), stringsAsFactors = stringsAsFactors)
}

# Given an ncdf4 file, and any number of vectors (e.g. c('Birds1', Birds2), 'Nums'),
# combine these vectors in all possible ways and return all arrays from the ncdf4 file
fetch_nc_variables <- function(nc_out, ...) {
    ncvar_get_all <- Vectorize(ncdf4::ncvar_get, vectorize.args = 'varid', SIMPLIFY = "array")
    nc_variables <- apply(expand.grid(..., stringsAsFactors = FALSE), 1, function (x) paste(x, collapse="_"))
    ncvar_get_all(nc_out, nc_variables)
}

# List all variable names available, optionally filtering by regex
list_nc_variables <- function(nc_out, filter = "") {
    out <- lapply(nc_out$var, function (v) v$name)
    if (nzchar(filter)) {
        out <- grep(filter, out, value = TRUE)
    }
    return(out)
}

# Convert MgN to grams
mgn_to_grams <- function (mgn) {
    3.65 * as.numeric(mgn) * 5.7 * 20 / 1000
}

# NB: Atlantis treats years as 365 days, no execeptions
atl_secs_to_month <- 60 * 60 * 24 * (365 / 12)

# Year vector from dims$time
atlantis_time_to_years <- function (atl_time, start_year) {
    # NB: The 1year-in-sec steps in the ncout files aren't very smooth due to machine innacuracy,
    # so force to nearest month first, which works for a few years.
    months <- round(as.numeric(atl_time) / atl_secs_to_month)
    return(months %/% 12 + start_year)
}
# month vector from dims$time
atlantis_time_to_months <- function (atl_time, start_year) {
    months <- round(as.numeric(atl_time) / atl_secs_to_month)
    return(months %% 12 + 1)
}
# day vector from dims$time
atlantis_time_to_days <- function (atl_time, start_year) {
    days <- atl_time / (60 * 60 * 24)
    return(days %% (365 / 12) + 1)
}
