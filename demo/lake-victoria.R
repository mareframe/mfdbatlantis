library(mfdb)
library(utils)

# TODO: Assign proper case study IDs here
mdb <- mfdb('Test')

# Find all the files in this directory
lv_dir <- 'atlantis-L_Vic-OutputFolderTest2'  # TODO: Shouldn't be in the demo either

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
    lv_fg_count <- atlantis_fg_count(lv_dir, 'outputLV.nc', lv_area_data, fg)
    lv_fg_count$species <- speciesCode
    lv_fg_count$areacell <- lv_fg_count$area
    mfdb_import_survey(mdb, lv_fg_count, data_source = paste0('atlantisTracer_', fgName))
}

lv_fisheries <- atlantis_fisheries(lv_dir, 'LVFisheries_New.xml')
lv_catch <- atlantis_fisheries_catch(lv_dir, 'outputLVCATCH.nc',
    lv_area_data,
    lv_fisheries[lv_fisheries$Code == 'llHooks',],
    c('LN', 'CG'))
