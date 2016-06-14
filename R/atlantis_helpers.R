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

# Atlantis time units
atl_day_secs <- 60 * 60 * 24  # Seconds-in-day normal
atl_year_days <- 365  # NB: Atlantis years are 365 days, no leap years
atl_month_days <- 30 + 1/3  # NB: Atlantis months seem to be equally sized, don't quite add up to 365
atl_month_secs <- atl_month_days * atl_day_secs

# Convert vector of atl_time seconds to year
atlantis_time_to_years <- function (atl_time) {
    # NB: The 1year-in-sec steps in the ncout files aren't very smooth due to machine innacuracy,
    # so force to nearest month first, which works for a few years.
    months <- round(as.numeric(atl_time) / atl_month_secs)
    return(months %/% 12)
}
# month vector from dims$time
atlantis_time_to_months <- function (atl_time) {
    months <- round(as.numeric(atl_time) / atl_month_secs)
    return(months %% 12 + 1)
}
# day vector from dims$time
atlantis_time_to_days <- function (atl_time) {
    days <- atl_time / atl_day_secs
    return(days %% atl_month_days + 1)
}
