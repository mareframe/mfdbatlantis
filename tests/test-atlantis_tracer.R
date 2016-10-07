library(mfdbatlantis)
library(unittest, quietly = TRUE)

ok_group("atlantis_tracer_add_lengthgroups", {
    tracer_data <- data.frame(
        cohort = 1:5,
        length = c(11,22,33,44,55),
        count  = c(10),
        stringAsFactors = TRUE)
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
