library(diyar)

library(tidyverse)

set.seed(1234)

witness <- c("Foobar", "Foo Foo", "Bar Bar Binks", "Baz")

witness <- sample(witness, 100, replace = TRUE)

evidence <- starwars %>% 
  select(name, hair_color, skin_color, gender, species) %>% 
  mutate(source = "Galactic Empire Profiles")

evidence <- evidence %>%
                    select(hair_color, skin_color, gender, species) %>% 
  slice_sample(n=100, replace = TRUE) %>%
  mutate(source = "Eye witness description") %>% 
    cbind(witness) %>%
  plyr::rbind.fill(evidence) %>%
  group_by(hair_color, skin_color, gender, species) %>% 
  mutate(matching_appearance = n()) %>% 
  ungroup() %>% 
  arrange(desc(matching_appearance), hair_color, skin_color, gender, species, name)

dput(eye_witness1)

#YOU may need to iterate through line by line for the registration data to get the best possible match rather than the remaining


data(missing_staff_id)
missing_staff_id

missing_staff_id$by_initials <- links(criteria = missing_staff_id$initials)

arrange(missing_staff_id, by_initials)

missing_staff_id$by_hair_colour <- links(criteria = missing_staff_id$hair_colour)

arrange(missing_staff_id, by_hair_colour)

missing_staff_id$by_initials_then_hair_colour <- links(criteria = as.list(missing_staff_id[c("initials", "hair_colour")]))

arrange(missing_staff_id, by_initials_then_hair_colour)

scri_1 <- sub_criteria(missing_staff_id$hair_colour, 
                       missing_staff_id$branch_office, 
                       operator = "or")

missing_staff_id$by_hair_colour_or_branch_office  <- links(criteria = "place_holder", 
                                                           sub_criteria = list(cr1 = scri_1), 
                                                           recursive = TRUE)

arrange(missing_staff_id, by_hair_colour_or_branch_office)

scri_2 <- sub_criteria(missing_staff_id$hair_colour, 
                       missing_staff_id$branch_office, 
                       operator = "and")

missing_staff_id$by_hair_colour_and_branch_office  <- links(criteria = "place_holder", 
                             sub_criteria = list(cr1 = scri_2), 
                             recursive = TRUE)

arrange(missing_staff_id, by_hair_colour_and_branch_office)

scri_3 <- sub_criteria(scri_1, 
                       sub_criteria(missing_staff_id$initials, 
                                    missing_staff_id$branch_office,
                                    operator = "or"),
                       operator = "and")

missing_staff_id$by_hair_colour_or_branch_and_initials_or_branch <- links(criteria = "place_holder", 
                             sub_criteria = list(cr1 = scri_3), 
                             recursive = TRUE)

arrange(missing_staff_id, by_hair_colour_or_branch_and_initials_or_branch)

# A function to extract the last word in a string
last_word_wf <- function(x) tolower(gsub("^.* ", "", x))
# A logical test using `last_word_wf`.
last_word_cmp <- function(x, y) last_word_wf(x) == last_word_wf(y)

scri_4 <- sub_criteria(missing_staff_id$hair_colour, 
                       missing_staff_id$branch_office,
                       match_funcs = c(last_word_cmp, last_word_cmp),
                       operator = "or")

missing_staff_id$by_hair_colour_or_branch_office_using_functions <- links(criteria = "place_holder", 
                             sub_criteria = list(cr1 = scri_4), 
                             recursive = TRUE)

#arrange(missing_staff_id, by_hair_colour_or_branch_office_using_functions)
missing_staff_id

p8 <- link_records(attribute = list(missing_staff_id$hair_colour, 
                                    missing_staff_id$branch_office), 
                   cmp_func = c(last_word_cmp, last_word_cmp), 
                   probabilistic = FALSE)

p8$pid

p8$pid_weights

p9 <- link_records(attribute = list(missing_staff_id$hair_colour, 
                                    missing_staff_id$branch_office), 
                   cmp_func = c(last_word_cmp, last_word_cmp), 
                   probabilistic = TRUE)

p9$pid

p9$pid_weights

# slower but less memory intensive
p10 <- links_wf_probabilistic(attribute = list(missing_staff_id$hair_colour,
                                               missing_staff_id$branch_office), 
                              cmp_func = c(last_word_cmp, last_word_cmp), 
                              probabilistic = TRUE,
                              recursive = TRUE)
p10$pid

p10$pid_weights

# match only within set

triplicate <- rbind(missing_staff_id[c(4:5, 7)],
                    missing_staff_id[c(4:5, 7)],
                    missing_staff_id[c(4:5, 7)])
triplicate$data_source <- c(rep("set_1", 7), rep("set_2", 7), rep("set_3", 7))

triplicate$p1 <- links(as.list(triplicate[1:2]), 
                       data_source = triplicate$data_source, 
                       strata = triplicate$source_1)

arrange(triplicate, p1)

as.data.frame(triplicate$p1)

triplicate$p2 <- links(as.list(triplicate[1:2]), 
                       strata = triplicate$data_source, 
                       data_source = triplicate$source_1)

arrange(triplicate, p2)

as.data.frame(triplicate$p2)


ep <- episodes(1:8)
unlinked_ep <- delink(ep, ep@sn %in% c(3, 8))
ep; unlinked_ep

pn <- partitions(1:8, length.out = 2, separate = TRUE)
unlinked_pn <- delink(pn, pn@.Data == 5)
pn; unlinked_pn

pd <- links(list(c(1, 1, 1, NA, NA),
                 c(NA, NA, 2, 2, 2)))
unlinked_pd <- delink(pd, pd@pid_cri == 1)
pd; unlinked_pd

# A warning is given if an index record is unlinked as this will lead to seemly impossible links.
ep2 <- episodes(1:8, 2, episode_type = "rolling")
unlinked_ep2 <- delink(ep2, ep2@sn %in% c(3, 5))
schema(ep2, custom_label = decode(ep2@case_nm), seed = 2)
schema(unlinked_ep2, custom_label = decode(unlinked_ep2@case_nm), seed = 2)

make_ids(x_pos = rep(7, 7), y_pos = 1:7)
