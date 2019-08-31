on_second_jan <- on_yday(2)

on_st_patricks_day <- only_occur(on_mday(17), in_month("Mar"))

on_early_may_bank_holiday <-
  on_first(on_mday("Mon"), within_given = "month") %>%
  only_occur(in_month("May"))

## ^ This needs some adjustment

on_liberation_day <- only_occur(on_mday(9), in_month("May"))

on_spring_bank_holiday <-
  on_last(on_wday("Mon"), within_given = "month") %>%
  only_occur(in_month("May"))

on_june_bank_holiday <-
  on_first(on_wday("Mon"), within_given = "month") %>%
  only_occur(in_month("June"))

on_senior_race_day <-
  on_first(on_wday("Fri"), within_given = "month") %>%
  only_occur(in_month("June"))

on_tynwald_day <- only_occur(on_mday(5), in_month("July"))

on_battle_of_the_boyne <- only_occur(on_mday(12), in_month("July"))
