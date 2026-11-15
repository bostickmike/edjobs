# The states' IPEDS unitid maps (ipeds_unitids.R in each state repo), as test
# data for the federal HE tests. WY's carries the Sheridan/Gillette Salary_Note.

MT_TEST_UNITIDS <- data.frame(
  unitid = c(180461L, 180179L, 180522L, 180249L, 180416L, 180197L, 180373L, 180151L, 180054L, 180489L, 180106L, 180595L, 180258L, 180212L, 180328L, 180647L, 366340L, 180203L, 180160L, 180081L, 180276L, 180692L),
  Name = c("Montana State University",
           "Montana State University Billings",
           "Montana State University-Northern",
           "Great Falls College MSU",
           "Montana Tech",
           "Flathead Valley Community College",
           "Miles Community College",
           "Dawson Community College",
           "Blackfeet Community College",
           "University of Montana",
           "Carroll College",
           "Rocky Mountain College",
           "University of Providence",
           "Fort Peck Community College",
           "Little Big Horn College",
           "Salish Kootenai College",
           "Stone Child College",
           "Aaniiih Nakoda College",
           "Chief Dull Knife College",
           "Highlands College",
           "Helena College",
           "University of Montana Western"),
  stringsAsFactors = FALSE)

WY_TEST_UNITIDS <- data.frame(
  unitid = c(240505L, 240514L, 240596L, 240620L, 240657L, 240666L, 240666L, 240693L, 240727L),
  Name = c("Casper College",
           "Central Wyoming College",
           "Eastern Wyoming Community College",
           "Laramie County Community College",
           "Northwest College",
           "Sheridan College",
           "Gillette College",
           "Western Wyoming Community College",
           "University of Wyoming"),
  stringsAsFactors = FALSE)
WY_TEST_UNITIDS$Salary_Note <- ifelse(WY_TEST_UNITIDS$unitid == 240666L,
  "Reported by Northern Wyoming Community College District, which includes both Sheridan College and Gillette College -- IPEDS does not break this figure out by campus.",
  NA_character_)
