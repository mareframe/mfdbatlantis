library(mfdb)
library(mfdbatlantis)
library(utils)

mdb <- mfdb('Atlantis-LVic')

# Find all the files in this directory
lv_dir <- 'atlantis-L_Vic-OutputFolderTest2'  # TODO: Shouldn't be in the demo either
attr(lv_dir, 'start_year') <- 1948

lv_run_options <- atlantis_run_options(lv_dir, 'LV_run.xml')

# Read in areas / surface temperatures, insert into mfdb
lv_area_data <- atlantis_read_areas(lv_dir, 'LV.bgm')
lv_temp <- atlantis_tracer(lv_dir, 'outputLV.nc', lv_area_data, 'Temp')
mfdb_import_area(mdb, lv_area_data)
lv_temp$temperature <- lv_temp$value
mfdb_import_temperature(mdb, lv_temp[lv_temp$depth == 1,])

# Read in all functional groups, and ingest data in turn
lv_functional_groups <- atlantis_functional_groups(lv_dir, 'LVGroups.xml', 'LV_biol.xml')
for (fgName in c()) {  # TODO: Add some functional groups
    fg <- lv_functional_groups[c(lv_functional_groups$Name == fgName),]
    cat("Importing functional group", as.character(fg$Name), "\n")

    speciesCode <- unlist(mfdb_find_species(as.character(fg$LongName))['name',])
    if (length(speciesCode) != 1) {
        stop("No direct species match: ", speciesCode)
    }
    lv_fg_count <- atlantis_fg_tracer(lv_dir, lv_area_data, fg)
    lv_fg_count$species <- speciesCode
    lv_fg_count$areacell <- lv_fg_count$area
    mfdb_import_survey(mdb, lv_fg_count, data_source = paste0('atlantisTracer_', fgName))
}

lv_fisheries <- atlantis_fisheries(lv_dir, 'LVFisheries_New.xml')
mfdb_import_vessel_taxonomy(mdb, data.frame(
    id = lv_fisheries$Index,
    name = lv_fisheries$Code,
    full_hame = lv_fisheries$Name,
    stringsAsFactors = FALSE))

for (fisheryCode in lv_fisheries$Code) {
    fishery <- lv_fisheries[lv_fisheries$Code == fisheryCode,]

    lv_catch <- atlantis_fisheries_catch(lv_dir, 'outputLVCATCH.nc',
        lv_area_data,
        fishery,
        c('LN', 'CG'))
    mfdb_import_survey(mdb, data.frame(
        year = lv_catch$year,
        month = lv_catch$month,
        areacell = lv_catch$area,
        vessel = lv_catch$fishery,
        species = c('COD'),  # TODO: Mapping
        weight_total = lv_catch$weight_total,
        stringsAsFactors = TRUE), data_source = paste0("atlantisFishery_", fisheryCode))
}
