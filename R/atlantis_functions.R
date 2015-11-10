# Given an XML document, pull out group_attributes from attribute group_name,
# return as data.frame
fetch_xml_attributes <- function (xml_doc, group_name, group_attributes) {
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
    }, rep("", length(group_attributes)))))
}

# Given an ncdf4 file, and any number of vectors (e.g. c('Birds1', Birds2), 'Nums'),
# combine these vectors in all possible ways and return all arrays from the ncdf4 file
fetch_nc_variables <- function(nc_out, ...) {
    ncvar_get_all <- Vectorize(ncdf4::ncvar_get, vectorize.args = 'varid', SIMPLIFY = "array")
    nc_variables <- apply(expand.grid(..., stringsAsFactors = FALSE), 1, function (x) paste(x, collapse="_"))
    ncvar_get_all(nc_out, nc_variables)
}

# Read required details in from bgm file
atlantis_read_areas <- function (adir, bgm_file) {
    if (length(bgm_file) != 1) stop("One bgm file required, not ", length(bgm_file))

    get_box_attribute <- function (bgm_lines, field_name, new_name = field_name) {
        # Extract lines we are interested and break up into key/val
        rv <- grep(paste0("^[A-Za-z0-9]+\\.", field_name), bgm_lines, value = TRUE)
        rv <- strsplit(rv, paste0(".", field_name, "\\s+"))

        # Convert to data.frame & transpose
        rv <- t(as.data.frame(rv))
        rownames(rv) <- NULL
        colnames(rv) <- c("id", new_name)
        return(rv)
    }

    # Extract parts of file we are interested in
    bgm_lines <- readLines(file.path(adir, bgm_file))
    area_data <- merge(
        get_box_attribute(bgm_lines, "label", "name"),
        get_box_attribute(bgm_lines, "area", "size"),
        sort = FALSE)
    area_data$id <- seq_len(nrow(area_data))
    area_data$size <- as.numeric(area_data$size)
    return(area_data)
}

atlantis_functional_groups <- function (adir, fg_file, bio_file) {
    fg_doc <- XML::xmlParse(file.path(adir, fg_file))
    fg_data <- fetch_xml_attributes(fg_doc, 'FunctionalGroup', c('GroupCode', 'Name', 'LongName', 'IsPredator', 'IsTurnedOn', 'NumCohorts', 'NumStages', 'NumAgeClassSize'))

    # Pull out useful flags from biology file and combine
    xmlAllAttrs <- Vectorize(XML::xmlAttrs)
    bio_doc <- XML::xmlParse(file.path(adir, bio_file))
    for (flag in c('FLAG_AGE_MAT', 'FLAG_LI_A', 'FLAG_LI_B')) {
        bio_flags <- xmlAllAttrs(XML::getNodeSet(bio_doc, paste0("//Attribute[@AttributeName='", flag, "']/GroupValue")))
        flag_table <- data.frame(
            GroupCode = bio_flags["GroupName",],
            Value = as.numeric(bio_flags["AttributeValue",]),
            stringsAsFactors = TRUE)
        names(flag_table)[[2]] <- flag
        fg_data <- merge(fg_data, flag_table, all.x = TRUE, sort = FALSE)
    }

    return(fg_data)
}

atlantis_fisheries <- function (adir, fisheries_file) {
    fisheries_doc <- XML::xmlParse(file.path(adir, fisheries_file))
    fisheries_data <- fetch_xml_attributes(fisheries_doc, 'Fishery', c('Code', 'Index', 'Name', 'IsRec', 'NumSubFleets'))
    fisheries_data
}

atlantis_run_options <- function (adir, opt_file) {
    opt_doc <- XML::xmlParse(file.path(adir, opt_file))
    opt_data <- fetch_xml_attributes(opt_doc, "ScenarioOptions", c("dt"))

    return(opt_data)
}

atlantis_tracer <- function (adir,
        nc_file = Sys.glob(file.path(adir, "output*.nc")),
        area_data,
        tracer_name = 'Temp',
        start_year = 1948) {
    nc_out <- ncdf4::nc_open(file.path(adir, nc_file))

    tracer <- ncdf4::ncvar_get(nc_out, tracer_name)
    dims <- expand.grid(
        depth = nc_out$dim$z$vals,
        areacell = as.character(area_data$name),
        time = nc_out$dim$t$vals,
        stringsAsFactors = TRUE)

    year_secs <- 60 * 60 * 24 * 365  # NB: Atlantis treats years as 365 days, no execeptions
    month_secs <- year_secs / 12 # TODO: If month == 30 days is used, this will slip
    data.frame(
        depth = dims$depth,
        areacell = dims$areacell,
        time = factor(dims$time),
        # Add start_year to years
        year = round(as.numeric(dims$time) / year_secs + start_year),
        # Months are remainder from year_secs divided by month_secs
        month = (as.numeric(dims$time) %% year_secs) %/% month_secs + 1,
        value = as.numeric(tracer),
        stringsAsFactors = TRUE)
}

atlantis_fg_count <- function (adir,
        nc_file = Sys.glob(file.path(adir, "output*.nc")),
        area_data,
        fg_group,
        start_year = 1948) {
    nc_out <- ncdf4::nc_open(file.path(adir, nc_file))

    # Read in all StructN and Nums data for the functional group
    fg_Nums <- fetch_nc_variables(nc_out, paste0(fg_group$Name, seq_len(as.character(fg_group$NumCohorts))), 'Nums')
    fg_StructN <- fetch_nc_variables(nc_out, paste0(fg_group$Name, seq_len(as.character(fg_group$NumCohorts))), 'StructN')

    # Populate initial data frame that contains the combinatorial explosions of the axes
    age_class_size <- as.numeric(as.character(fg_group$NumAgeClassSize))
    dims <- expand.grid(
        depth = nc_out$dim$z$vals,
        area = as.character(area_data$name),
        time = nc_out$dim$t$vals,
        # Age is mid-point of sequence of age_class_size values
        age = seq(age_class_size / 2, by = age_class_size, length.out = as.numeric(as.character(fg_group$NumCohorts))),
        stringsAsFactors = TRUE)

    # Add extra values to make this MFDB-compliant
    year_secs <- 60 * 60 * 24 * 365  # NB: Atlantis treats years as 365 days, no execeptions
    month_secs <- year_secs / 12 # TODO: If month == 30 days is used, this will slip
    weight_grams <- 3.65 * as.numeric(fg_StructN) * 5.7 * 20 / 1000  # TODO: Ish?
    data.frame(
        depth = dims$depth,
        area = dims$area,
        time = factor(dims$time),
        species = fg_group$LongName,
        # Add start_year to years
        year = round(as.numeric(dims$time) / year_secs + start_year),
        # Months are remainder from year_secs divided by month_secs
        month = (as.numeric(dims$time) %% year_secs) %/% month_secs + 1,
        age = dims$age,
        # Maturity stage is mature iff ageClass greater than FLAG_AGE_MAT
        maturity_stage = ifelse(dims$age > fg_group$FLAG_AGE_MAT * age_class_size, 5, 1),
        weight = weight_grams,  # TODO: Units?
        length = (weight_grams / fg_group$FLAG_LI_A) ^ (1 / fg_group$FLAG_LI_B),
        count = as.numeric(fg_Nums),
        stringsAsFactors = TRUE)
}

atlantis_fisheries_catch <- function(adir,
        catch_file,
        area_data,
        fishery,
        species,
        start_year = 1948) {
    nc_out <- ncdf4::nc_open(file.path(adir, catch_file))

    catch_tonnes <- fetch_nc_variables(nc_out, species, 'Catch', paste0('FC', fishery$Index))

    # Populate initial data frame that contains the combinatorial explosions of the axes
    dims <- expand.grid(
        area = as.character(area_data$name),
        time = nc_out$dim$t$vals,
        species = species,
        stringsAsFactors = TRUE)

    # Combine with catch data
    year_secs <- 60 * 60 * 24 * 365  # NB: Atlantis treats years as 365 days, no execeptions
    month_secs <- year_secs / 12 # TODO: If month == 30 days is used, this will slip
    data.frame(
        area = dims$area,
        time = dims$time,
        # Add start_year to years
        year = round(as.numeric(dims$time) / year_secs + start_year),
        # Months are remainder from year_secs divided by month_secs
        month = (as.numeric(dims$time) %% year_secs) %/% month_secs + 1,
        fishery = fishery$Code,
        species = dims$species,
        weight_total = as.numeric(catch_tonnes),
        stringsAsFactors = TRUE)
}

