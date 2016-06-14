atlantis_stomach_content <- function (adir,
        consumption,
        predator_map,
        prey_map,
        start_year = attr(adir, 'start_year')) {
    repeat_by_col <- function(df, col_name) {
        df[rep(seq_len(nrow(df)), df[, col_name]), names(df) != col_name]
    }
    combine_df_list <- function(df_list) {
        do.call(rbind, df_list)
    }

    # Repeat each row (count) times, and label it
    consumption <- repeat_by_col(consumption, 'count')
    consumption$stomach_name <- seq_len(nrow(consumption))

    diet <- read.table(attr(adir, 'txt_diet'), header = TRUE, stringsAsFactors = FALSE)
    diet$Year <- diet$Time %/% atl_year_days + start_year
    diet$Month <- (diet$Time %% atl_year_days) %/% (365 / 12) + 1  # NB: Months aren't 30.333 here(?)

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
