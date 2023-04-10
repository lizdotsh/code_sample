library(stringr)
library(haven)
library(tidytable)
library(purrr)
library(forcats)
library(tidystringdist)
freedman_f21 <- read_dta('source_data/complete-freedmen-data-448-f21.dta') 
freedman_init_full <- read_dta('source_data/complete-freedmen-data-448-f22.dta')|> bind_rows.(freedman_f22)
# Sorry I would normally expand my code a lot more but I just slightly above the code line limit and had to compress some stuff ahah
freedman_recoded <- freedman_init_full |> 
    mutate.(
        across.(contains(c('age', 'dependents_number_1865','match_quality')), as.numeric), 
        across.(dependents_yes_or_no_1865, as_factor),
        across.(contains(c('sex', 'gender')), ~ {
            as_factor(.x) |> fct_collapse(
                Female = c('[Female]', '[female]', 'female', 'Female'),
                Male = c('[Male]', '[male]', 'male', 'Male'),
                other_level = 'blank'
            )}
        ),
        across.(contains('match_type'),~{
            as_factor(.x) %>% 
                fct_collapse(
                    good_unique = c('good, unique', 'good and unique', 'goood and unique'),
                    good_nonunique = c('good, nonunique', 'and and nonunique'),
                    bad = c('bad'),
                    other_level = 'blank'
                    )
            }
        ),
        across.(contains('race'),~{
                str_extract(str_to_lower(.x), 'white|black|mulatto|blank') %>% 
                    coalesce(.x) %>% na_if.("") %>% replace_na("other")
            }
        ),
        across.(contains(
            c('cannotread', 'cannotwrite', 'fatherfor', 'attendedschool', 'dependents_yes_or_no')), 
            ~as_factor(.x) %>% fct_collapse(yes = 'Yes',other_level = 'no')
        ),
        across.(everything(), ~{na_if(.x,c("", "blank"))}),
        across.(contains('occupation'), ~str_to_lower(.x)),
        age_original = age,
        match_quality_1865 = 10,
        match_type_1865 = 'good_unique'
    ) %>% 
    select.(-contains(c('url', 'status_', 'deniedvoting', 'foreign'))) %>% 
    rename(
        'age_1865' = age,
        'name_1865' = fullname,
        'gender_1865'= sex,
    ) 
filter_occupation <- function(x){ #extracted this, get_state_name, and remove_state into separate functions 
    case_when.(                   #so it would look a lot cleaned/not all be in one big anonymous function. 
        str_detect(x,paste0('doctor|dressmaker|barber|carpenter|laundress|butcher|nurse',
                             '|physician|police|store\\skeeper|mason|minister|cobbler|shoe|tailor|',
                             '\\w\\smaker|mid-wife|upholsterer|teacher|plasterer')) ~ 'Skilled',
        str_detect(x,'farm|lab(o|ou)r|planter|field|pick') ~ 'Farm Laborer',
        str_detect(x,'factory|\\s(fac|mill|fcty)|mine|miller|wood') ~ 'Factory/Mill/Mine Worker',
        str_detect(x,'school|student') ~ 'Student',
        str_detect(x,'water|oyster|seaman|sailor') ~ 'Oyster/Water Worker',
        str_detect(x,'home|house|domestic|servant|dom|wash|maid|garden|cook') ~ 'Domestic Servant',
        str_detect(x, 'waiter|hotel|janitor|clerk|store|shops|porter|coachman') ~ 'Store/Hotel/Restaurant Worker',
        str_detect(x, 'yes|read') ~ 'Other',
        str_detect(x, '^no|not\\sany|none|n/a|na|child|(no|without)\\soccupation|0|work|\\.') ~ '',
        TRUE ~ x
    )  %>% replace_na('') %>% fct %>% fct_lump_min(10, other_level = 'Other') %>% return()
}
get_state_name <- function(str){ 
    map_vec(
        str_to_title(str) %>% replace_na(''), ~{
            str_split_1(.x, boundary('word')) %>% 
            detect(~.x %in% state.name, .default = NA)
        }
        ) %>% str_to_title() %>% return()
}
remove_state <- function(vec){
    map_vec(replace_na(vec, "") %>% str_to_title(), 
        ~{
            str_split_1(.x, ',') %>% 
            str_remove('^\\s') %>%
            setdiff(c(state.name, 'USA', 'Usa')) %>%
            str_c(collapse = ", ")
        }
    ) %>% return()
}
freedman_pivoted <- freedman_recoded %>% 
    pivot_longer(ends_with(c('1880','1865', '1860', '1900', '1870')),
        names_to = c('.value', 'year'),
        names_pattern = "(\\D+)(\\d+)"
    ) %>%
    rename_with(.cols =  ends_with('_'), ~str_sub(.x, end = -2)) %>%
    mutate.(
        occupation = filter_occupation(occupation),
        across.(contains('residence'), 
            list(state = get_state_name, city = remove_state), 
            .names = '{.col}_{.fn}'
        ),
        across.(contains('_city'), 
            list(
                county = ~str_extract(.x, '(?<=\\,\\s)[a-zA-Z\\s]*$') %>% replace_na(''), 
                town = ~str_extract(.x, '^.*(?=\\,\\s\\w+)') %>% replace_na('')
                )
        ),
        residence_city_county = case_when(
            year == 1865 ~ residence_city,
            residence_city_county == '' ~ residence_city_town, 
            TRUE ~ residence_city_county
         ),
        residence_city_town = if_else(residence_city_county == residence_city_town, '', residence_city_town),
        age_projected = age_original + (as.numeric(year) - 1865),
        match_quality = if_else(match_type == 'bad', 0, match_quality),
        first_name = str_extract(name, '^\\w*'),
        last_name = str_extract(name, '[:alnum:]+(?=($|\\s$|(\\s\\[[\\w\\s\\.\\(\\)]+\\])*$))')
    )%>%
    mutate(
        postoffice = postoffice[[2]],
        dependents = dependents_yes_or_no[[1]],
        married = ifelse(!is.na(marriageyear[[4]]), 1, 0),
        marriageyear = marriageyear[[4]],
        .by = c('idnumber')
    ) %>% 
    arrange(idnumber)%>%
    rename.(
    'age_recorded' = age
    ) %>%
    filter(!is.na(name)) %>% 
    select.(
        idnumber, year,match_type, match_quality,first_name,last_name,
        age_projected, age_recorded, gender,occupation, race, married,postoffice,
        residence_state:residence_city_town,dependents,marriageyear,cannotread,cannotwrite
    ) 
write.csv(freedman_pivoted, 'visualization/outputs/freedman_fully_parsed.csv')

best_matched <- freedman_pivoted %>% 
    slice_max.(order_by = match_quality, n = 2, .by = 'idnumber', with_ties = F) %>% 
    select(idnumber, first_name, last_name, match_quality, occupation, age_projected, race) %>% 
    mutate.(num = row_number(desc(match_quality)),
            .by = 'idnumber') %>% 
    arrange(idnumber, num) %>%
    filter(sum(num) == 3, .by = idnumber) %>% 
    mutate(num = if_else.(num == 1, 'orig', 'best')) %>% 
    pivot_longer(contains(c('first_name', 'last_name')), names_to = 'name_type', values_to = 'name') %>% 
    filter(!is.na(idnumber)) %>% 
    pivot_wider(names_from = c('num'), values_from = c('name', 'occupation', 'match_quality', 'age_projected', 'race')) %>% 
    select(!c(match_quality_orig, race_orig)) %>% 
    rename('match_quality' = match_quality_best) %>% 
    mutate(age_difference = age_projected_best - age_projected_orig) %>% 
    tidy_stringdist(v1 = name_orig, v2 = name_best, method = c('osa', 'lv', 'lcs', 'jw', 'soundex')) 
print('done')
write.csv(best_matched,'visualization/outputs/freedman_best_matched_name_distance.csv')    