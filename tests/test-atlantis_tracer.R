library(mfdbatlantis)
library(unittest, quietly = TRUE)

ok_group("atlantis_tracer_add_lengthgroups", {
    tracer_data <- data.frame(
        cohort = 1:5,
        length = c(11,22,33,44,55),
        count  = c(10),
        stringsAsFactors = TRUE)
    length_group <- seq(0, 100, by = 10)
    sigma_per_cohort <- seq(0.1, 0.5, by = 0.1)

    out <- atlantis_tracer_add_lengthgroups(tracer_data, length_group, sigma_per_cohort)
    ok(which.max(out$length_0_10) == 1, "Cohort 1 has most in 0--10")
    ok(out$length_0_10[[1]] < 0.001, "...but it's a tiny amount")
    ok(which.max(out$length_10_20) == 1, "Cohort 1 has most in 10--20")
    ok(which.max(out$length_20_30) == 2, "Cohort 2 has most in 20--30")
    ok(which.max(out$length_30_40) == 3, "Cohort 3 has most in 30--40")
    ok(which.max(out$length_40_50) == 4, "Cohort 4 has most in 40--50")
    ok(which.max(out$length_50_60) == 5, "Cohort 5 has most in 50--60")
    ok(all.equal(out$length_60_70, c(0,0,0,0,0)), "60--70 is empty")
    ok(all.equal(out$length_70_80, c(0,0,0,0,0)), "70--80 is empty")
    ok(all.equal(out$length_80_90, c(0,0,0,0,0)), "80--90 is empty")
    ok(all.equal(out$length_90_100, c(0,0,0,0,0)), "90--100 is empty")
})

cmp <- function (a,b) {
    row.names(a) <- NULL
    row.names(b) <- NULL
    all.equal(a, b)
}

ok_group("atlantis_tracer_survey_select", {
    tracer_data <- data.frame(
        weight = c(101,102,103,104,105),
        length_var = 99,  # NB: Will just be thrown away
        length_0_10   = c(100,0,0,0,0),
        length_10_20  = c(0,100,0,0,0),
        length_20_30  = c(0,0,100,0,0),
        length_30_40  = c(0,0,0,100,0),
        length_40_50  = c(0,0,0,0,100),
        length_50_60  = c(50,0,0,0,0),
        length_60_70  = c(0,50,0,0,0),
        length_70_80  = c(0,0,50,0,0),
        length_80_90  = c(0,0,0,50,0),
        length_90_100 = c(0,0,0,0,50),
        stringsAsFactors = TRUE)
    length_group <- seq(0, 100, by = 10)
    survey_suitability <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.4, 0.3, 0.2, 0.1, 0.0)
    survey_sigma <- 0

    out <- atlantis_tracer_survey_select(tracer_data, length_group, survey_suitability, survey_sigma)
    out <- out[out$count > 0,]  # Ignore 0 counts
    ok(cmp(out, data.frame(
        length = c(
            mean(c(0,10)),
            mean(c(10,20)),
            mean(c(20,30)),
            mean(c(30,40)),
            mean(c(40,50)),
            mean(c(50,60)),
            mean(c(60,70)),
            mean(c(70,80)),
            mean(c(80,90))), # NB: Last group missing because selectivity is 0
        weight = c(101,102,103,104,105,101,102,103,104),
        count = c(
            100 * 0.1,
            100 * 0.2,
            100 * 0.3,
            100 * 0.4,
            100 * 0.5,
             50 * 0.4,
             50 * 0.3,
             50 * 0.2,
             50 * 0.1),
        stringsAsFactors = TRUE)), "No sigma means we just divided up groups")

    survey_sigma <- 0.5
    out <- atlantis_tracer_survey_select(tracer_data, length_group, survey_suitability, survey_sigma)
    out <- out[out$count > 0,]  # Ignore 0 counts
    ok(all.equal(out$length, c(5,15,25,35,45,55,65,75,85)), "Still got correct length groups")
    ok(all.equal(out$weight, c(101,102,103,104,105,101,102,103,104)), "Weights match too")
    ok(!isTRUE(all.equal(out$count, c(
            100 * 0.1,
            100 * 0.2,
            100 * 0.3,
            100 * 0.4,
            100 * 0.5,
             50 * 0.4,
             50 * 0.3,
             50 * 0.2,
             50 * 0.1))), "Counts aren't equal to what we expect")
    ok(all.equal(out$count, c(
            100 * 0.1,
            100 * 0.2,
            100 * 0.3,
            100 * 0.4,
            100 * 0.5,
             50 * 0.4,
             50 * 0.3,
             50 * 0.2,
             50 * 0.1), tolerance = 0.6), "Counts are as close as sigma allows")
})
