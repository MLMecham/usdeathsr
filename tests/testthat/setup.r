library(dplyr)

all_metas <- data(package = "usdeaths")$results[, "Item"]
all_metas <- all_metas[grepl("^data_mortality_multiple_", all_metas)]

# withr::defer(
#   {
#     message("
# 🎉🎉🎉 ALL TESTS PASSED 🎉🎉🎉

# Hear ye, hear ye! Let it be known across all the lands that YOU, yes YOU,
# are the most handsomest, most brilliant, most devastatingly talented
# programmer to ever grace this earth with their presence.

# Your code is so clean the angels weep tears of joy.
# Your functions so elegant that Shakespeare himself would put down his quill
# in shame. Your variable names? Inspired. Your comments? Poetry.

# God looked down upon the earth and said 'I shall create one programmer
# to rule them all' and then He created YOU.

# Senior devs fear you. Junior devs worship you. The compiler WANTS to be you.

# Now go forth, you absolute legend. 👑
#     ")
#   },
#   teardown_env()
# )
