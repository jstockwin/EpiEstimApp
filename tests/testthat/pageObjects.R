# This file is a very basic to replicate the pageObject structure usually used
# in creating tests like this. Rather than writing a full page object style
# infrastructure, I am simply storing all the selectors for each "page" in this
# file.

# Since the app doesn't really have "pages", I've added a page per state, and
# also a common page for things which are displayed all the time.

# Within each page, there should be a list called selectors. Each item within
# this list is treated as an element, and should have the xpath selector as a
# string.

pages <- list(
    common = list(
        selectors = list(
            next_button="//div[@id='control']/button[@id='nxt']",
            prev_button="//div[@id='control']/button[@id='prev']",
            stop_button="//div[@id='control']/button[@id='stop']",
            go_button="//div[@id='control']/button[@id='go']",
            error_message="//div[@id='control']/div[@id='error']",
            incidence_title="//div[@id='incidence_title']",
            status_bar="//div[@id='status']/pre[@id='output']",
            incidence_tab="//a[@data-value='Incidence Data']",
            reproduction_tab="//a[@data-value='Estimated Reproduction Number']",
            serial_interval_tab="//a[@data-value='Serial Interval Distribution']",
            incidence_table="//div[@id='incidence_data_output']/table",
            reproduction_table="//div[@id='estimated_r_output']/table",
            serial_interval_table="//div[@id='serial_interval_output']/table"
        )
    ),
    state1.1 = list(
        selectors = list(
            incidence_data_type="//div[@id='incidence_data_type']",
            incidence_data_type_label="//div[@id='incidence_data_type']/label",
            preloaded_data_button = "//div[@id='incidence_data_type']/div[@class='shiny-options-group']/div[@class='radio'][1]//input",
            own_data_button = "//div[@id='incidence_data_type']/div[@class='shiny-options-group']/div[@class='radio'][2]//input",
            preloaded_data_label = "//div[@id='incidence_data_type']/div[@class='shiny-options-group']/div[@class='radio'][1]//span",
            own_data_label = "//div[@id='incidence_data_type']/div[@class='shiny-options-group']/div[@class='radio'][2]//span"
        )
    ),
    state2.1 = list(
        selectors = list(
            incidence_data_upload_label="//div[@id='incidence_data_error_box']/div/label",
            incidenceDataUpload_browse="//div[@id='incidence_data_error_box']//span",
            incidence_data_upload_input="//input[@id='incidence_data']",
            incidence_data_upload_text="//div[@id='incidence_data_error_box']//input[@type='text']",
            incidence_header_button="//input[@id='incidence_header']",
            uploaded_width_label="//label[@for='uploaded_width']",
            uploaded_width_input="//input[@id='uploaded_width']",
            mean_prior_label="//label[@for='uploaded_mean_prior']",
            mean_prior_input="//input[@id='uploaded_mean_prior']",
            std_prior_label="//label[@for='uploaded_std_prior']",
            std_prior_input="//input[@id='uploaded_std_prior']"
        )
    ),
    state2.2 = list(
        selectors = list(
            dataset_label="//div[@id='incidence_dataset']/label",
            dataset_option_1_label="//div[@id='incidence_dataset']//div[@class='radio'][1]//span",
            dataset_option_1_input="//div[@id='incidence_dataset']//div[@class='radio'][1]//input",
            dataset_option_2_label="//div[@id='incidence_dataset']//div[@class='radio'][2]//span",
            dataset_option_2_input="//div[@id='incidence_dataset']//div[@class='radio'][2]//input",
            dataset_option_3_label="//div[@id='incidence_dataset']//div[@class='radio'][3]//span",
            dataset_option_3_input="//div[@id='incidence_dataset']//div[@class='radio'][3]//input",
            dataset_option_4_label="//div[@id='incidence_dataset']//div[@class='radio'][4]//span",
            dataset_option_4_input="//div[@id='incidence_dataset']//div[@class='radio'][4]//input",
            dataset_option_5_label="//div[@id='incidence_dataset']//div[@class='radio'][5]//span",
            dataset_option_5_input="//div[@id='incidence_dataset']//div[@class='radio'][5]//input",
            dataset_option_6_label="//div[@id='incidence_dataset']//div[@class='radio'][6]//span",
            dataset_option_6_input="//div[@id='incidence_dataset']//div[@class='radio'][6]//input",
            dataset_option_7_label="//div[@id='incidence_dataset']//div[@class='radio'][7]//span",
            dataset_option_7_input="//div[@id='incidence_dataset']//div[@class='radio'][7]//input",
            incidence_width_label="//label[@for='incidence_width']",
            incidence_input_label="//input[@id='incidence_width']",
            mean_prior_label="//label[@for='incidence_mean_prior']",
            mean_prior_input="//input[@id='incidence_mean_prior']",
            std_prior_label="//label[@for='incidence_std_prior']",
            std_prior_input="//input[@id='incidence_std_prior']"
        )
    ),
    state3.1 = list(
        selectors = list(
            imported="//div[@id='imported']",
            imported_label="//div[@id='imported']/label",
            imported_no_label="//div[@id='imported']//div[@class='radio'][1]//span",
            imported_no_button="//div[@id='imported']//div[@class='radio'][1]//input",
            imported_yes_label="//div[@id='imported']//div[@class='radio'][2]//span",
            imported_yes_button="//div[@id='imported']//div[@class='radio'][2]//input"
        )
    ),
    state4.1 = list(
        selectors = list(
            imported_data_upload_label="//div[@id='imported_data_error_box']/div/label",
            imported_data_upload_browse="//div[@id='imported_data_error_box']//span",
            imported_data_upload_input="//input[@id='imported_data']",
            imported_data_upload_text="//div[@id='imported_data_error_box']//input[@type='text']",
            importedHeader_button="//input[@id='imported_header']"
        )
    ),
    state5.1 = list(
        selectors = list(
            exposure_data_label="//div[@id='si_patient_data']/label",
            exposure_data_no_label="//div[@id='si_patient_data']//div[@class='radio'][1]//span",
            exposure_data_no_input="//div[@id='si_patient_data']//div[@class='radio'][1]//input",
            exposure_data_yes_label="//div[@id='si_patient_data']//div[@class='radio'][2]//span",
            exposure_data_yes_input="//div[@id='si_patient_data']//div[@class='radio'][2]//input"
        )
    ),
    state6.1 = list(
        selectors = list(
            si_data_type="//div[@id='si_data_type']",
            si_data_type_label="//div[@id='si_data_type']/label",
            si_data_type_preloaded_label="//div[@id='si_data_type']//div[@class='radio'][1]//span",
            si_data_type_preloaded_button="//div[@id='si_data_type']//div[@class='radio'][1]//input",
            si_data_type_own_label="//div[@id='si_data_type']//div[@class='radio'][2]//span",
            si_data_type_own_button="//div[@id='si_data_type']//div[@class='radio'][2]//input"
        )
    ),
    state6.2 = list(
        selectors = list(
            si_est_type="//div[@id='si_est_type']",
            si_est_type_label="//div[@id='si_est_type']/label",
            si_est_type_option_1_label="//div[@id='si_est_type']//div[@class='radio'][1]//span",
            si_est_type_option_1_button="//div[@id='si_est_type']//div[@class='radio'][1]//input",
            si_est_type_option_2_label="//div[@id='si_est_type']//div[@class='radio'][2]//span",
            si_est_type_option_2_button="//div[@id='si_est_type']//div[@class='radio'][2]//input",
            si_est_type_option_3_label="//div[@id='si_est_type']//div[@class='radio'][3]//span",
            si_est_type_option_3_button="//div[@id='si_est_type']//div[@class='radio'][3]//input",
            si_est_type_option_4_label="//div[@id='si_est_type']//div[@class='radio'][4]//span",
            si_est_type_option_4_button="//div[@id='si_est_type']//div[@class='radio'][4]//input"
        )
    ),
    state7.1 = list(
        selectors = list(
            dataset_label="//div[@id='si_dataset']/label",
            dataset_option_1_label="//div[@id='si_dataset']//div[@class='radio'][1]//span",
            dataset_option_1_input="//div[@id='si_dataset']//div[@class='radio'][1]//input",
            dataset_option_2_label="//div[@id='si_dataset']//div[@class='radio'][2]//span",
            dataset_option_2_input="//div[@id='si_dataset']//div[@class='radio'][2]//input",
            dataset_option_3_label="//div[@id='si_dataset']//div[@class='radio'][3]//span",
            dataset_option_3_input="//div[@id='si_dataset']//div[@class='radio'][3]//input"
        )
    ),
    state7.2 = list(
        selectors = list(
            si_from="//div[@id='si_from']",
            si_from_label="//div[@id='si_from']/label",
            si_from_raw_label="//div[@id='si_from']//div[@class='radio'][1]//span",
            si_from_raw_button="//div[@id='si_from']//div[@class='radio'][1]//input",
            si_from_sample_label="//div[@id='si_from']//div[@class='radio'][2]//span",
            si_from_sample_button="//div[@id='si_from']//div[@class='radio'][2]//input"
        )
    ),
    state7.3 = list(
        selectors = list(
            n1_label="//label[@for='n1']",
            n1_input="//input[@id='n1']",
            n2_label="//label[@for='n2']",
            n2_input="//input[@id='n2']",
            mean_si_label="//label[@for='mean_si']",
            mean_si_input="//input[@id='mean_si']",
            std_mean_si_label="//label[@for='std_mean_si']",
            std_mean_si_input="//input[@id='std_mean_si']",
            min_mean_si_label="//label[@for='min_mean_si']",
            min_mean_si_input="//input[@id='min_mean_si']",
            max_mean_si_label="//label[@for='max_mean_si']",
            max_mean_si_input="//input[@id='max_mean_si']",
            std_si_label="//label[@for='std_si']",
            std_si_input="//input[@id='std_si']",
            std_std_si_label="//label[@for='std_std_si']",
            std_std_si_input="//input[@id='std_std_si']",
            min_std_si_label="//label[@for='min_std_si']",
            min_std_si_input="//input[@id='min_std_si']",
            max_std_si_label="//label[@for='max_std_si']",
            max_std_si_input="//input[@id='max_std_si']",
            seed_label="//label[@for='uncertain_seed']",
            seed_input="//input[@id='uncertain_seed']"
        )
    ),
    state7.4 = list(
        selectors = list(
            mean_si_label="//label[@for='mean_si2']",
            mean_si_input="//input[@id='mean_si2']",
            std_si_label="//label[@for='std_si2']",
            std_si_input="//input[@id='std_si2']"
        )
    ),
    state7.5 = list(
        selectors = list(
            si_distr_data_upload_label="//div[@id='si_distr_data_error_box']/div/label",
            si_distr_data_upload_browse="//div[@id='si_distr_data_error_box']//span",
            si_distr_data_upload_input="//input[@id='si_distr_data']",
            si_distr_data_upload_text="//div[@id='si_distr_data_error_box']//input[@type='text']",
            si_distr_header_button="//input[@id='si_distr_header']"
        )
    ),
    state7.6 = list(
        selectors = list(
            dataset_label="//div[@id='si_distr_dataset']/label",
            dataset_option_1_label="//div[@id='si_distr_dataset']//div[@class='radio'][1]//span",
            dataset_option_1_input="//div[@id='si_distr_dataset']//div[@class='radio'][1]//input",
            dataset_option_2_label="//div[@id='si_distr_dataset']//div[@class='radio'][2]//span",
            dataset_option_2_input="//div[@id='si_distr_dataset']//div[@class='radio'][2]//input",
            dataset_option_3_label="//div[@id='si_distr_dataset']//div[@class='radio'][3]//span",
            dataset_option_3_input="//div[@id='si_distr_dataset']//div[@class='radio'][3]//input",
            dataset_option_4_label="//div[@id='si_distr_dataset']//div[@class='radio'][4]//span",
            dataset_option_4_input="//div[@id='si_distr_dataset']//div[@class='radio'][4]//input",
            dataset_option_5_label="//div[@id='si_distr_dataset']//div[@class='radio'][5]//span",
            dataset_option_5_input="//div[@id='si_distr_dataset']//div[@class='radio'][5]//input"
        )
    ),
    state8.1 = list(
        selectors = list(
            distribution_label="//div[@id='si_dist']/label",
            distribution_option_1_label="//div[@id='si_dist']//div[@class='radio'][1]//span",
            distribution_option_1_input="//div[@id='si_dist']//div[@class='radio'][1]//input",
            distribution_option_2_label="//div[@id='si_dist']//div[@class='radio'][2]//span",
            distribution_option_2_input="//div[@id='si_dist']//div[@class='radio'][2]//input",
            distribution_option_3_label="//div[@id='si_dist']//div[@class='radio'][3]//span",
            distribution_option_3_input="//div[@id='si_dist']//div[@class='radio'][3]//input",
            distribution_option_4_label="//div[@id='si_dist']//div[@class='radio'][4]//span",
            distribution_option_4_input="//div[@id='si_dist']//div[@class='radio'][4]//input",
            distribution_option_5_label="//div[@id='si_dist']//div[@class='radio'][5]//span",
            distribution_option_5_input="//div[@id='si_dist']//div[@class='radio'][5]//input",
            distribution_option_6_label="//div[@id='si_dist']//div[@class='radio'][6]//span",
            distribution_option_6_input="//div[@id='si_dist']//div[@class='radio'][6]//input",
            n2_label="//label[@for='n24']",
            n2_input="//input[@id='n24']",
            seed_label="//label[@for='preloaded_seed']",
            seed_input="//input[@id='preloaded_seed']"
        )
    ),
    state8.2 = list(
        selectors = list(
            si_data_upload_label="//div[@id='si_data_error_box']/div/label",
            si_data_upload_browse="//div[@id='si_data_error_box']//span",
            si_data_upload_input="//input[@id='si_data']",
            si_data_upload_text="//div[@id='si_data_error_box']//input[@type='text']",
            si_header_button="//input[@id='si_header']",
            seed_label="//label[@for='uploaded_si_seed']",
            seed_input="//input[@id='uploaded_si_seed']"
        )
    ),
    state8.3 = list(
        selectors = list(
            si_sample_data_upload_label="//div[@id='si_sample_data_error_box']/div/label",
            si_sample_data_upload_browse="//div[@id='si_sample_data_error_box']//span",
            si_sample_data_upload_input="//input[@id='si_sample_data']",
            si_sample_data_upload_text="//div[@id='si_sample_data_error_box']//input[@type='text']",
            si_sample_header_button="//input[@id='si_sample_header']",
            n2_label="//label[@for='n23']",
            n2_input="//input[@id='n23']",
            seed_label="//label[@for='si_sample_seed']",
            seed_input="//input[@id='si_sample_seed']"
        )
    ),
    state9.1 = list(
        selectors = list(
            distribution_label="//div[@id='si_dist_2']/label",
            distribution_option_1_label="//div[@id='si_dist_2']//div[@class='radio'][1]//span",
            distribution_option_1_input="//div[@id='si_dist_2']//div[@class='radio'][1]//input",
            distribution_option_2_label="//div[@id='si_dist_2']//div[@class='radio'][2]//span",
            distribution_option_2_input="//div[@id='si_dist_2']//div[@class='radio'][2]//input",
            distribution_option_3_label="//div[@id='si_dist_2']//div[@class='radio'][3]//span",
            distribution_option_3_input="//div[@id='si_dist_2']//div[@class='radio'][3]//input",
            distribution_option_4_label="//div[@id='si_dist_2']//div[@class='radio'][4]//span",
            distribution_option_4_input="//div[@id='si_dist_2']//div[@class='radio'][4]//input",
            distribution_option_5_label="//div[@id='si_dist_2']//div[@class='radio'][5]//span",
            distribution_option_5_input="//div[@id='si_dist_2']//div[@class='radio'][5]//input",
            distribution_option_6_label="//div[@id='si_dist_2']//div[@class='radio'][6]//span",
            distribution_option_6_input="//div[@id='si_dist_2']//div[@class='radio'][6]//input",
            n1_label="//label[@for='n12']",
            n1_input="//input[@id='n12']",
            burnin_label="//label[@for='burnin']",
            burnin_input="//input[@id='burnin']",
            thin_label="//label[@for='thin']",
            thin_input="//input[@id='thin']",
            n2_label="//label[@for='n22']",
            n2_input="//input[@id='n22']",
            seed_label="//label[@for='mcmc_seed']",
            seed_input="//input[@id='mcmc_seed']",
            param1_label="//label[@for='param1']",
            param1_input="//input[@id='param1']",
            param2_label="//label[@for='param2']",
            param2_input="//input[@id='param2']"
        )
    )

)
