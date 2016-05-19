# Select the first file matching the glob in adir
first_file <- function (...) {
    files <- sort(Sys.glob(file.path(...)))
    if (length(files) == 0) stop("No files found for ", c(...))
    return(files[[1]])
}

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

    data.frame(
        depth = dims$depth,
        areacell = dims$areacell,
        time = factor(dims$time),
        year = atlantis_time_to_years(dims$time, start_year),
        month = atlantis_time_to_months(dims$time),
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
    weight_grams <- mgn_to_grams(fg_StructN)
    data.frame(
        depth = dims$depth,
        area = dims$area,
        time = factor(dims$time),
        species = fg_group$LongName,
        year = atlantis_time_to_years(dims$time, start_year),
        month = atlantis_time_to_months(dims$time),
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
    data.frame(
        area = dims$area,
        time = dims$time,
        year = atlantis_time_to_years(dims$time, start_year),
        month = atlantis_time_to_months(dims$time),
        fishery = fishery$Code,
        species = dims$species,
        weight_total = as.numeric(catch_tonnes),
        stringsAsFactors = TRUE)
}

atlantis_consumption <- function (adir,
        area_data,
        nc_file = first_file(adir, "*.nc"),
        prod_file = first_file(adir, "*PROD.nc"),
        fg_group,
        ingestion_period = c(),
        start_year = 1948) {
    nc_out <- ncdf4::nc_open(nc_file)

    fg_Nums <- fetch_nc_variables(nc_out, paste0(fg_group$Name, seq_len(as.character(fg_group$NumCohorts))), 'Nums')
    dims <- expand.grid(
        depth = nc_out$dim$z$vals,
        area = as.character(area_data$name),
        time = nc_out$dim$t$vals,
        cohort = seq_len(as.character(fg_group$NumCohorts)),
        stringsAsFactors = FALSE)
    df_nums <- data.frame(
        depth = dims$depth,
        area = dims$area,
        year = atlantis_time_to_years(dims$time, start_year),
        month = atlantis_time_to_months(dims$time),
        group = as.character(fg_group$GroupCode),
        cohort = dims$cohort,
        count = as.numeric(fg_Nums),
        stringsAsFactors = TRUE)
    df_nums <- aggregate(count ~ area + year + month + group + cohort, df_nums, sum)

    nc_prod <- ncdf4::nc_open(prod_file)
    fg_Eat <- fetch_nc_variables(nc_prod, paste0(fg_group$Name, seq_len(as.character(fg_group$NumCohorts))), 'Eat')
    dims <- expand.grid(
        area = as.character(area_data$name),
        time = nc_prod$dim$t$vals,
        cohort = seq_len(fg_group$NumCohorts),
        stringsAsFactors = FALSE)
    df_eat <- data.frame(
        area = dims$area,
        year = atlantis_time_to_years(dims$time, start_year),
        month = atlantis_time_to_months(dims$time),
        group = as.character(fg_group$GroupCode),
        cohort = dims$cohort,
        consumption = as.numeric(fg_Eat),
        stringsAsFactors = TRUE)

    df_out <- merge(df_nums, df_eat, by = c('area', 'year', 'month', 'group', 'cohort'))
    # Consumption is mgN/day for the whole cohort. Divide to get rates for individual
    df_out$ind_consumption <- df_out$consumption / df_out$count

    # Add age to data
    age_class_size <- as.numeric(as.character(fg_group$NumAgeClassSize))
    df_out$age <- df_out$cohort * age_class_size + age_class_size / 2

    return(df_out)
}

atlantis_stomach_content <- function (adir,
        consumption,
        predator_map,
        prey_map,
        diet_file = first_file(adir, "*DietCheck.txt"),
        start_year = 1948) {
    repeat_by_col <- function(df, col_name) {
        df[rep(seq_len(nrow(df)), df[, col_name]), names(df) != col_name]
    }
    combine_df_list <- function(df_list) {
        do.call(rbind, df_list)
    }

    # Repeat each row (count) times, and label it
    consumption <- repeat_by_col(consumption, 'count')
    consumption$stomach_name <- seq_len(nrow(consumption))

    diet <- read.table(diet_file, header = TRUE, stringsAsFactors = FALSE)
    diet$Year <- diet$Time %/% 365 + start_year
    diet$Month <- (diet$Time %% 365) %/% (365 / 12) + 1

    predator_data <- data.frame(
        year = consumption$year,
        month = consumption$month,
        areacell = consumption$area,
        species = predator_map[consumption$group],
        age = consumption$age,
        stomach_name = consumption$stomach_name,
        stringsAsFactors = TRUE)

    prey_data <- combine_df_list(lapply(seq_len(nrow(consumption)), function (i) {
        predator <- consumption[i,]
        # Generate a vector of prey quantities
        prey <- diet[
            diet$Year == predator$year &
            diet$Month == predator$month &
            diet$Group == predator$group &
            diet$Cohort == predator$cohort,
            names(prey_map)]
        names(prey) <- prey_map
        if (length(prey) == 0) stop("No Prey for", predator$year, predator$month, predator$group, predator$cohort)
        prey <- prey * predator$ind_consumption # Scale total consumption

        if (length(prey) == 0) {
            return(data.frame())
        }
        data.frame(
            stomach_name = predator$stomach_name,
            species = names(prey),
            weight_total = mgn_to_grams(prey),
            stringsAsFactors = TRUE)
    }))

    list(predator_data = predator_data, prey_data = prey_data)
}
