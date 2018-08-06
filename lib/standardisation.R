

standardisation_test = function() {

  read.csv('data/standardisation.csv') %>%
  as.tibble() %>%
  mutate( pct = rate/pop) -> df_test

  #df_test %>%
  #  group_by( Age, cat) %>%
  #  summarise( population = sum(pop)) -> df_population_test


  df_test %>%
    inner_join( df_population_test,  by = c("Age", "cat")) %>%
    mutate( pop_rate = pct * population ) %>%
    ungroup() %>%
    group_by( bp ) %>%
    summarise( tot_rate = sum( pop_rate ), 
              gtot_pop = sum( population), 
              gpct = tot_rate / gtot_pop )

#df_test %>% filter( row_number() < 5 ) %>% complete( Age, bp, cat)

  df_test %>%
    group_by( Age, cat, bp ) %>%
    summarise( population = sum(pop)) -> df_population_test

}


get_population_grouped = function ( pop, rollup_level = NA) {
  if (typeof(rollup_level ) == "logical" && is.na( rollup_level )) {
    pop %>% ungroup() %>%
      summarize( population=sum(population)  )
  } else {
    pop %>% ungroup() %>%
      group_by_( .dots=rollup_level ) %>%
      summarize( population=sum(population)  )
  }
}

get_pop_grouped_test = function() {
  get_population_grouped(df_population_test ) 
  get_population_grouped(df_population_test, 'cat')
  get_population_grouped(df_population_test, 'Age')
  get_population_grouped(df_population_test, qw('Age cat'))
  get_population_grouped(df_population_test, qw('bp Age cat'))

  get_population_grouped(df_population, qw('lga seifa'))
  get_population_grouped(df_population, qw('lga supply_year'))
}

# create test population function
f_join_population_test_dataset = function( dataset, 
                             rollup_level = NA,  
                             join_key = rollup_level, 
                             df_population. = df_population_test )  {
  f_join_population( dataset, rollup_level, join_key, df_population. )
}


# expects df_population with variable population
f_join_population = function( dataset, 
                    rollup_level = NA,  
                    join_key = rollup_level, 
                    df_population. = df_population )  {

  pop_names = names( df_population. )
  # can only join and group on what we have available
  available_rollup_vars = intersect( pop_names, rollup_level)
  join_key = intersect(pop_names, join_key )

  if ( length( available_rollup_vars ) == 0 )  {
    # fallback to average total database population if no variables to join with
    dataset %>% 
      mutate(temp=1) %>% 
      inner_join( get_population_grouped( df_population. ) %>% mutate(temp=1)
                  ,by="temp") %>%
      select(-temp) 
  } else {
    dataset %>%
      inner_join( get_population_grouped(  df_population., available_rollup_vars ),  by=join_key)
  }
}


f_join_population_test = function( ) {

  df_patient %>%
    filter( lga=='10050' ) %>%
    f_join_population( )

  df_patient %>%
    filter( lga=='10050' ) %>%
    f_join_population( rollup_level='lga' )

  df_patient %>%
    distinct( lga ) %>%
    f_join_population( rollup_level=qw('lga seifa'), join_key='lga' )

  df_patient %>%
    distinct( lga ) %>%
    f_join_population( rollup_level=qw(' c("lga", "usage_category", "supply_year", "age", "sex") ') )

}



standardise_test = function() {

df_test %>%
  select( Age, bp, cat, rate) %>%
  f_join_population_test_dataset( rollup_level=c( 'Age', 'cat', 'bp') ) %>%
  mutate( ratio = rate / population ) %>%
  select( Age, bp, cat, rate, ratio) %>%
  f_join_population_test_dataset( rollup_level=c( 'Age', 'cat') ) %>%
  mutate( tot_ratio = ratio * population ) %>%
  group_by_(.dots= c('bp' ) )  %>%
  summarise( tot_ratio = sum(tot_ratio)) %>%
  f_join_population_test_dataset( ) %>%
  mutate( ratio = tot_ratio / population ) %>%
  select( bp, ratio )



df_test %>% group_by( bp ) %>% summarise( sum( pop))

  f_join_population_test_dataset( rollup_level=c( 'Age', 'cat', 'bp') ) %>%
  mutate( ratio = rate / population ) %>%
  select( Age, bp, cat, rate, ratio) %>%
  f_join_population_test_dataset( rollup_level=c( 'Age', 'cat') ) %>%
  mutate( tot_ratio = ratio * population ) %>%
  group_by_(.dots= c('bp' ) )  %>%
  summarise( tot_ratio = sum(tot_ratio)) %>%
  f_join_population_test_dataset( ) %>%
  mutate( ratio = tot_ratio / population ) %>%
  select( bp, ratio )

}

simple_standardise_value = function( df, 
                                    standardise_using, 
                                    standardise_across, 
                                    count_var,
                                    f_population. = f_population
                                    ) {

  # 1) sum count_var in df based on c( standardise_using + standardise_across)
  # 2) join summed population on the SAME variables, to get df_test rate for each count
  # 3) join summed population on standardise_using, to multiply rate by population total
  # arriving at population_overall_rate
  # 4) total the population_overall_rate by standardise_across to get grand_total_rate
  # 4) join total population, divide grand_total by total_population

  detailed_vars = c( standardise_using, standardise_across )

  df %>%
    ungroup() %>%
    mutate( count=count_var ) %>%  # create df_test variable for the count field
    group_by_( .dots=detailed_vars ) %>%
      summarise( count = sum( count )) %>%
      select_( .dots=c(detailed_vars, 'count' )) %>%
      f_population.( detailed_vars ) %>%
      mutate( ratio = count / population ) %>%
      select_( .dots=c(detailed_vars, 'ratio')) %>%
      f_population.( standardise_using) %>%
      mutate( population_overall_rate= ratio * population ) %>%
      group_by_(.dots= standardise_across )  %>%
      summarise( grand_total= sum(population_overall_rate)) %>%
      f_population.( ) %>%
      mutate( ratio = grand_total / population ) %>%
      select_(.dots= c(standardise_across, 'ratio') )

}

simple_standardise_value_test = function() {

  df_test %>% simple_standardise_value( standardise_using=qw('Age cat'), 
                                      standardise_across=c('bp'), 
                                      count_var=.$rate, 
                                      f_join_population_test )

  df_patient_dose %>%
    inner_join( df_patient_usage ) %>%
    inner_join( df_patient ) %>%
    ungroup() %>%
    select( lga, usage_category, supply_year, age, sex, n_dose) %>% 
    simple_standardise_value(standardise_using=qw('age sex'), 
                            standardise_across=qw("supply_year lga usage_category"), 
                            .$n_dose,
                            f_join_population)  
}

function () {

  df_patient_dose %>%
    inner_join( df_patient_usage ) %>%
    inner_join( df_patient ) %>%
    ungroup() -> df

  #df = a
  standardise_over=qw('lga usage_category')
  join_with= c('supply_year') 

}

#  -------------------------------------------------
select_and_standardise_ddd <- function( df, standardise_over, join_with, localf_join_population = f_join_population) {
  # select out standardise_over and join_with from df,
  # standardising value based on standardise_using, generally sex and age

  # expects a df containing standardise_over, join_with variables
  # need to standardise the standardise_over
  # returns df containing proportion, standardised based on population figures from 
  # join_population_grouped, along with standardise_over and join_with
  # eg select_and_standardise_variable( standardise_over=qw( "seifa" ), join_with="supply_year", value=.$n_user )
  # join_with are other variables that join with population, eg supply_year, sex, age, 
  # what 


  standardise_using <- qw("age sex supply_year")  %>%   
    setdiff( standardise_over ) %>% 
      setdiff( join_with )

    df %>%  # group by everything we want to group by to get base level number of doses
      ungroup() %>%
        group_by_( .dots=c( standardise_over, join_with, standardise_using )) %>%
        summarise( n_dose=sum(n_dose )) %>%
        # join up with population on the maximal set of the same variables
        localf_join_population ( c( standardise_over, join_with, standardise_using )) %>%
        mutate( proportion = (n_dose * 1000 * 10) / (population * my_year_length( supply_year ) )) %>%  # calculate proportion at this level

        # now that we have the proportion for each subgroup,
        # now we standardise that proportion based on the total population
        # first throw away extraneous variables brought in by previous join, 
        # eg population and person_days
        # all we want is the proportion,and the key variables

        select_( .dots = c(standardise_over, join_with, standardise_using, "proportion") ) %>%

          # get the population grouped by levels we want to standardise on, that is, 
          # the standardisation variables (possibly age and/or sex, as long as they are not
          # included in the non-standard_vars), and join_with, eg. supply_year

          # this next call differentiiates standardise_over and join_with, 
          # we rollup the pupulation by grouping on everything apart from the standardise_over
          # for example, we might put lga or state in standardise_over, and rollup to that
          localf_join_population ( c(standardise_using,  join_with)) %>% 
          mutate( proportion_standardized = proportion * population ) %>%  

          # now, sum up our standardized_proportion, 
          # grouped on age and/or sex as long as we ae not using it elsewhere
          group_by_( .dots=c( standardise_over, join_with )) %>%  # no standardize_vars here
          summarise( proportion_standardized = sum(proportion_standardized )) %>%
            select_( .dots = c( standardise_over, join_with, "proportion_standardized") ) %>% # and cleanup

            # get population completely rolled up, so we can divide
            localf_join_population ( join_with ) %>% 

              # divide by the total population, and standardisation finished
              mutate( ddd = proportion_standardized / population ) %>%  

              # finished standardisation, cleanup by selecting only variables of interest
              select_( .dots = c(standardise_over, join_with, "ddd") ) %>%
              ungroup()
}

select_and_standardise_ddd_test = function() {

  df_patient_dose %>%
    inner_join( df_patient_usage ) %>%
    inner_join( df_patient ) %>%
    ungroup() %>%
    select_and_standardise_ddd(standardise_over=qw('lga usage_category'), 
                               join_with= c('supply_year'), 
                               f_join_population
                               )  



  df_patient_dose %>%
    inner_join( df_patient_usage ) %>%
    inner_join( df_patient ) %>%
    ungroup() %>%
    select( lga, usage_category, supply_year, age, sex, n_dose) %>% 
    f_join_population( rollup_level=c( 'age', 'sex', 'lga', 'supply_year') , df_population.=df_population) %>%
    mutate( ratio = (n_dose * 1000 * 10) / (population * my_year_length( supply_year) )) %>%
    select( -population ) %>%
    f_join_population( rollup_level=c( 'age', 'sex', 'supply_year') , df_population.=df_population) %>%
    mutate( tot_ratio = ratio * population ) %>%
    group_by_(.dots= qw('lga usage_category supply_year' ) )  %>%
    summarise( tot_ratio = sum(tot_ratio)) %>%
    f_join_population( rollup_level=c( 'supply_year') , df_population.=df_population) %>%
    mutate( ratio = tot_ratio / population ) %>%
    select( -population, -tot_ratio )

}

