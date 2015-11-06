lv_dir <- 'atlantis-L_Vic-OutputFolderTest2/'
lv_area_data <- atlantis_read_areas(lv_dir, 'LV.bgm')
lv_temp <- atlantis_tracer(lv_dir, 'outputLV.nc', lv_area_data, 'Temp')
lv_functional_groups <- atlantis_functional_groups(lv_dir, 'LVGroups.xml', 'LV_biol.xml')
lv_run_options <- atlantis_run_options(lv_dir, 'LV_run.xml')
lv_fg_count <- atlantis_fg_count(lv_dir, 'outputLV.nc', lv_area_data,
    lv_functional_groups[c(lv_functional_groups$Name == 'Birds'),])
lv_fisheries <- atlantis_fisheries(lv_dir, 'LVFisheries_New.xml')
lv_catch <- atlantis_fisheries_catch(lv_dir, 'outputLVCATCH.nc',
    lv_area_data,
    lv_fisheries[lv_fisheries$Code == 'llHooks',],
    c('LN', 'CG'))
