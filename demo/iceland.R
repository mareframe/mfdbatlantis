library(mfdb)
library(mfdbatlantis)
library(utils)

mdb <- mfdb('Atlantis-Iceland')

# Find all the files in this directory
ice_dir <- 'atlantis-Iceland-2016-05-24/'  # TODO: Shouldn't be in the demo either
attr(ice_dir, 'start_year') <- 1948

ice_run_options <- atlantis_run_options(ice_dir)

# Read in areas / surface temperatures, insert into mfdb
ice_area_data <- atlantis_read_areas(ice_dir)
ice_temp <- atlantis_tracer(ice_dir, ice_area_data, 'Temp')
mfdb_import_area(mdb, ice_area_data)
ice_temp$temperature <- ice_temp$value
mfdb_import_temperature(mdb, ice_temp[ice_temp$depth == 1,])

# Read in all functional groups, assign MFDB shortcodes where possible
ice_functional_groups <- atlantis_functional_groups(ice_dir)
ice_functional_groups$MfdbCode <- vapply(
    mfdb_find_species(ice_functional_groups$LongName)['name',],
    function (x) if (length(x) > 0) x[[1]] else as.character(NA), "")

# Ingest survey data
for (fgName in c("Cod", "Haddock")) {
    fg <- ice_functional_groups[c(ice_functional_groups$Name == fgName),]
    cat("Importing functional group", fg$Name, "\n")

    ice_fg_count <- atlantis_fg_tracer(ice_dir, ice_area_data, fg)
    ice_fg_count$species <- fg$MfdbCode
    ice_fg_count$areacell <- ice_fg_count$area
    # TODO: More selectivity (Look with Bjarki at Rgadget mean length simulat)
    # TODO: Want a length selectivity curve for the gear, areas that you're interested in.
    # TODO: Apply this also to consumption below, maybe combine the two?
    mfdb_import_survey(mdb, ice_fg_count, data_source = paste0('atlantisTracer_', fgName))
}

for (fgName in c("Cod")) {
    fg <- ice_functional_groups[c(ice_functional_groups$Name == fgName),]

    # Fetch consumption and tracer indexes for functional group
    consumption <- atlantis_fg_tracer(
        ice_dir,
        ice_area_data,
        fg_group = fg,
        consumption = TRUE)

    # Reduce area down to a pretend survey time/area
    consumption <- consumption[consumption$year == 1950 & consumption$area %in% c("Box20", "Box21", "Box22"),]
    # Assume we only catch 0.0001% of possible available
    consumption$count <- round(consumption$count * 0.000001)
    # TODO:
    # 2 weeks in spring / autum
    # Sample every nth fish (that the survey takes?)
    #   => Don't bother with this, only generating proportions anyway

    # Convert this into the 2 data.frames import_stomach requires
    stomach <- atlantis_stomach_content(ice_dir, consumption, predator_map = c(
        FCD = 'COD'
    ), prey_map = c(
        # We're only interested in 2 species
        FHE = mfdb_find_species('Clupea Harengus')['name',][[1]],
        FCA = mfdb_find_species('Capelin')['name',][[1]]
    ))
    mfdb_import_stomach(mdb, stomach$predator_data, stomach$prey_data, data_source = "stomach_Cod")
}

ice_fisheries <- atlantis_fisheries(ice_dir)
mfdb_import_vessel_taxonomy(mdb, data.frame(
    id = ice_fisheries$Index,
    name = ice_fisheries$Code,
    full_name = ice_fisheries$Name,
    stringsAsFactors = FALSE))

for (fisheryCode in ice_fisheries$Code) {
    fishery <- ice_fisheries[ice_fisheries$Code == fisheryCode,]
    cat("Importing fishery", fisheryCode, "\n")

    ice_catch <- atlantis_fisheries_catch(ice_dir, ice_area_data, fishery)
    if (nrow(ice_catch) == 0) next

    # Species column that maps to MFDB code
    ice_catch$species <- ice_catch$functional_group
    levels(ice_catch$species) <- ice_functional_groups[match(
        levels(ice_catch$functional_group),
        ice_functional_groups$GroupCode), 'MfdbCode']

    mfdb_import_survey(mdb, data.frame(
        year = ice_catch$year,
        month = ice_catch$month,
        areacell = ice_catch$area,
        vessel = ice_catch$fishery,
        species = ice_catch$species,
        weight_total = ice_catch$weight_total,
        count = c(NA),
        stringsAsFactors = TRUE), data_source = paste0("atlantisFishery_", fisheryCode))
}
