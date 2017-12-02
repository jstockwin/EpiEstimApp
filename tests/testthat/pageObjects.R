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
            nextButton="//div[@id='control']/button[@id='nxt']",
            prevButton="//div[@id='control']/button[@id='prev']",
            stopButton="//div[@id='control']/button[@id='stop']",
            goButton="//div[@id='control']/button[@id='go']",
            errorMessage="//div[@id='control']/div[@id='error']",
            incidenceTitle="//div[@id='incidenceTitle']",
            statusBar="//div[@id='status']/pre[@id='output']",
            incidenceTab="//a[@data-value='Incidence Data']",
            reproductionTab="//a[@data-value='Estimated Reproduction Number']",
            serialIntervalTab="//a[@data-value='Serial Interval Distribution']",
            incidenceTable="//div[@id='incidence_data_output']/table",
            reproductionTable="//div[@id='estimated_r_output']/table",
            serialIntervalTable="//div[@id='serial_interval_output']/table"
        )
    ),
    state1.1 = list(
        selectors = list(
            incidenceDataType="//div[@id='incidence_data_type']",
            incidenceDataTypeLabel="//div[@id='incidence_data_type']/label",
            preloadedDataButton = "//div[@id='incidence_data_type']/div[@class='shiny-options-group']/div[@class='radio'][1]//input",
            ownDataButton = "//div[@id='incidence_data_type']/div[@class='shiny-options-group']/div[@class='radio'][2]//input",
            preloadedDataLabel = "//div[@id='incidence_data_type']/div[@class='shiny-options-group']/div[@class='radio'][1]//span",
            ownDataLabel = "//div[@id='incidence_data_type']/div[@class='shiny-options-group']/div[@class='radio'][2]//span"
        )
    ),
    state2.1 = list(
        selectors = list(
            incidenceDataUploadLabel="//div[@id='incidence_data_error_box']/div/label",
            incidenceDataUploadBrowse="//div[@id='incidence_data_error_box']//span",
            incidenceDataUploadInput="//input[@id='incidence_data']",
            incidenceDataUploadText="//div[@id='incidence_data_error_box']//input[@type='text']",
            incidenceHeaderButton="//input[@id='incidenceHeader']",
            uploadedWidthLabel="//label[@for='uploaded_width']",
            uploadedWidthInput="//input[@id='uploaded_width']",
            meanPriorLabel="//label[@for='uploaded_mean_prior']",
            meanPriorInput="//input[@id='uploaded_mean_prior']",
            stdPriorLabel="//label[@for='uploaded_std_prior']",
            stdPriorInput="//input[@id='uploaded_std_prior']"
        )
    ),
    state2.2 = list(
        selectors = list(
            datasetLabel="//div[@id='incidence_dataset']/label",
            datasetOption1Label="//div[@id='incidence_dataset']//div[@class='radio'][1]//span",
            datasetOption1Input="//div[@id='incidence_dataset']//div[@class='radio'][1]//input",
            datasetOption2Label="//div[@id='incidence_dataset']//div[@class='radio'][2]//span",
            datasetOption2Input="//div[@id='incidence_dataset']//div[@class='radio'][2]//input",
            datasetOption3Label="//div[@id='incidence_dataset']//div[@class='radio'][3]//span",
            datasetOption3Input="//div[@id='incidence_dataset']//div[@class='radio'][3]//input",
            datasetOption4Label="//div[@id='incidence_dataset']//div[@class='radio'][4]//span",
            datasetOption4Input="//div[@id='incidence_dataset']//div[@class='radio'][4]//input",
            datasetOption5Label="//div[@id='incidence_dataset']//div[@class='radio'][5]//span",
            datasetOption5Input="//div[@id='incidence_dataset']//div[@class='radio'][5]//input",
            datasetOption6Label="//div[@id='incidence_dataset']//div[@class='radio'][6]//span",
            datasetOption6Input="//div[@id='incidence_dataset']//div[@class='radio'][6]//input",
            datasetOption7Label="//div[@id='incidence_dataset']//div[@class='radio'][7]//span",
            datasetOption7Input="//div[@id='incidence_dataset']//div[@class='radio'][7]//input",
            incidenceWidthLabel="//label[@for='incidence_width']",
            incidenceWidthInput="//input[@id='incidence_width']",
            meanPriorLabel="//label[@for='incidence_mean_prior']",
            meanPriorInput="//input[@id='incidence_mean_prior']",
            stdPriorLabel="//label[@for='incidence_std_prior']",
            stdPriorInput="//input[@id='incidence_std_prior']"
        )
    ),
    state3.1 = list(
        selectors = list(
            imported="//div[@id='imported']",
            importedLabel="//div[@id='imported']/label",
            importedNoLabel="//div[@id='imported']//div[@class='radio'][1]//span",
            importedNoButton="//div[@id='imported']//div[@class='radio'][1]//input",
            importedYesLabel="//div[@id='imported']//div[@class='radio'][2]//span",
            importedYesButton="//div[@id='imported']//div[@class='radio'][2]//input"
        )
    ),
    state4.1 = list(
        selectors = list(
            importedDataUploadLabel="//div[@id='imported_data_error_box']/div/label",
            importedDataUploadBrowse="//div[@id='imported_data_error_box']//span",
            importedDataUploadInput="//input[@id='imported_data']",
            importedDataUploadText="//div[@id='imported_data_error_box']//input[@type='text']",
            importedHeaderButton="//input[@id='imported_header']"
        )
    ),
    state5.1 = list(
        selectors = list(
            exposureDataLabel="//div[@id='si_patient_data']/label",
            exposureDataNoLabel="//div[@id='si_patient_data']//div[@class='radio'][1]//span",
            exposureDataNoInput="//div[@id='si_patient_data']//div[@class='radio'][1]//input",
            exposureDataYesLabel="//div[@id='si_patient_data']//div[@class='radio'][2]//span",
            exposureDataYesInput="//div[@id='si_patient_data']//div[@class='radio'][2]//input"
        )
    ),
    state6.1 = list(
        selectors = list(
            SIDataType="//div[@id='si_data_type']",
            SIDataTypeLabel="//div[@id='si_data_type']/label",
            SIDataTypePreloadedLabel="//div[@id='si_data_type']//div[@class='radio'][1]//span",
            SIDataTypePreloadedButton="//div[@id='si_data_type']//div[@class='radio'][1]//input",
            SIDataTypeOwnLabel="//div[@id='si_data_type']//div[@class='radio'][2]//span",
            SIDataTypeOwnButton="//div[@id='si_data_type']//div[@class='radio'][2]//input"
        )
    ),
    state6.2 = list(
        selectors = list(
            SIEstType="//div[@id='si_est_type']",
            SIEstTypeLabel="//div[@id='si_est_type']/label",
            SIEstTypeOption1Label="//div[@id='si_est_type']//div[@class='radio'][1]//span",
            SIEstTypeOption1Button="//div[@id='si_est_type']//div[@class='radio'][1]//input",
            SIEstTypeOption2Label="//div[@id='si_est_type']//div[@class='radio'][2]//span",
            SIEstTypeOption2Button="//div[@id='si_est_type']//div[@class='radio'][2]//input",
            SIEstTypeOption3Label="//div[@id='si_est_type']//div[@class='radio'][3]//span",
            SIEstTypeOption3Button="//div[@id='si_est_type']//div[@class='radio'][3]//input",
            SIEstTypeOption4Label="//div[@id='si_est_type']//div[@class='radio'][4]//span",
            SIEstTypeOption4Button="//div[@id='si_est_type']//div[@class='radio'][4]//input"
        )
    ),
    state7.1 = list(
        selectors = list(
            datasetLabel="//div[@id='si_dataset']/label",
            datasetOption1Label="//div[@id='si_dataset']//div[@class='radio'][1]//span",
            datasetOption1Input="//div[@id='si_dataset']//div[@class='radio'][1]//input",
            datasetOption2Label="//div[@id='si_dataset']//div[@class='radio'][2]//span",
            datasetOption2Input="//div[@id='si_dataset']//div[@class='radio'][2]//input",
            datasetOption3Label="//div[@id='si_dataset']//div[@class='radio'][3]//span",
            datasetOption3Input="//div[@id='si_dataset']//div[@class='radio'][3]//input"
        )
    ),
    state7.2 = list(
        selectors = list(
            SIFrom="//div[@id='si_from']",
            SIFromLabel="//div[@id='si_from']/label",
            SIFromRawLabel="//div[@id='si_from']//div[@class='radio'][1]//span",
            SIFromRawButton="//div[@id='si_from']//div[@class='radio'][1]//input",
            si_from_sampleLabel="//div[@id='si_from']//div[@class='radio'][2]//span",
            si_from_sampleButton="//div[@id='si_from']//div[@class='radio'][2]//input"
        )
    ),
    state7.3 = list(
        selectors = list(
            n1Label="//label[@for='n1']",
            n1Input="//input[@id='n1']",
            n2Label="//label[@for='n2']",
            n2Input="//input[@id='n2']",
            mean_siLabel="//label[@for='mean_si']",
            mean_siInput="//input[@id='mean_si']",
            std_mean_siLabel="//label[@for='std_mean_si']",
            std_mean_siInput="//input[@id='std_mean_si']",
            min_mean_siLabel="//label[@for='min_mean_si']",
            min_mean_siInput="//input[@id='min_mean_si']",
            max_mean_siLabel="//label[@for='max_mean_si']",
            max_mean_siInput="//input[@id='max_mean_si']",
            std_siLabel="//label[@for='std_si']",
            std_siInput="//input[@id='std_si']",
            std_std_siLabel="//label[@for='std_std_si']",
            std_std_siInput="//input[@id='std_std_si']",
            min_std_siLabel="//label[@for='min_std_si']",
            min_std_siInput="//input[@id='min_std_si']",
            max_std_siLabel="//label[@for='max_std_si']",
            max_std_siInput="//input[@id='max_std_si']",
            seedLabel="//label[@for='uncertain_seed']",
            seedInput="//input[@id='uncertain_seed']"
        )
    ),
    state7.4 = list(
        selectors = list(
            mean_siLabel="//label[@for='mean_si2']",
            mean_siInput="//input[@id='mean_si2']",
            std_siLabel="//label[@for='std_si2']",
            std_siInput="//input[@id='std_si2']"
        )
    ),
    state7.5 = list(
        selectors = list(
            SIDistrDataUploadLabel="//div[@id='si_distr_data_error_box']/div/label",
            SIDistrDataUploadBrowse="//div[@id='si_distr_data_error_box']//span",
            SIDistrDataUploadInput="//input[@id='si_distr_data']",
            SIDistrDataUploadText="//div[@id='si_distr_data_error_box']//input[@type='text']",
            SIDistrHeaderButton="//input[@id='si_distr_header']"
        )
    ),
    state7.6 = list(
        selectors = list(
            datasetLabel="//div[@id='si_distr_dataset']/label",
            datasetOption1Label="//div[@id='si_distr_dataset']//div[@class='radio'][1]//span",
            datasetOption1Input="//div[@id='si_distr_dataset']//div[@class='radio'][1]//input",
            datasetOption2Label="//div[@id='si_distr_dataset']//div[@class='radio'][2]//span",
            datasetOption2Input="//div[@id='si_distr_dataset']//div[@class='radio'][2]//input",
            datasetOption3Label="//div[@id='si_distr_dataset']//div[@class='radio'][3]//span",
            datasetOption3Input="//div[@id='si_distr_dataset']//div[@class='radio'][3]//input",
            datasetOption4Label="//div[@id='si_distr_dataset']//div[@class='radio'][4]//span",
            datasetOption4Input="//div[@id='si_distr_dataset']//div[@class='radio'][4]//input",
            datasetOption5Label="//div[@id='si_distr_dataset']//div[@class='radio'][5]//span",
            datasetOption5Input="//div[@id='si_distr_dataset']//div[@class='radio'][5]//input"
        )
    ),
    state8.1 = list(
        selectors = list(
            distributionLabel="//div[@id='si_dist']/label",
            distributionOption1Label="//div[@id='si_dist']//div[@class='radio'][1]//span",
            distributionOption1Input="//div[@id='si_dist']//div[@class='radio'][1]//input",
            distributionOption2Label="//div[@id='si_dist']//div[@class='radio'][2]//span",
            distributionOption2Input="//div[@id='si_dist']//div[@class='radio'][2]//input",
            distributionOption3Label="//div[@id='si_dist']//div[@class='radio'][3]//span",
            distributionOption3Input="//div[@id='si_dist']//div[@class='radio'][3]//input",
            distributionOption4Label="//div[@id='si_dist']//div[@class='radio'][4]//span",
            distributionOption4Input="//div[@id='si_dist']//div[@class='radio'][4]//input",
            distributionOption5Label="//div[@id='si_dist']//div[@class='radio'][5]//span",
            distributionOption5Input="//div[@id='si_dist']//div[@class='radio'][5]//input",
            distributionOption6Label="//div[@id='si_dist']//div[@class='radio'][6]//span",
            distributionOption6Input="//div[@id='si_dist']//div[@class='radio'][6]//input",
            n2Label="//label[@for='n24']",
            n2Input="//input[@id='n24']",
            seedLabel="//label[@for='preloaded_seed']",
            seedInput="//input[@id='preloaded_seed']"
        )
    ),
    state8.2 = list(
        selectors = list(
            SIDataUploadLabel="//div[@id='si_data_error_box']/div/label",
            SIDataUploadBrowse="//div[@id='si_data_error_box']//span",
            SIDataUploadInput="//input[@id='si_data']",
            SIDataUploadText="//div[@id='si_data_error_box']//input[@type='text']",
            SIHeaderButton="//input[@id='si_header']",
            seedLabel="//label[@for='uploaded_si_seed']",
            seedInput="//input[@id='uploaded_si_seed']"
        )
    ),
    state8.3 = list(
        selectors = list(
            SISampleDataUploadLabel="//div[@id='si_sample_data_error_box']/div/label",
            SISampleDataUploadBrowse="//div[@id='si_sample_data_error_box']//span",
            SISampleDataUploadInput="//input[@id='si_sample_data']",
            SISampleDataUploadText="//div[@id='si_sample_data_error_box']//input[@type='text']",
            SISampleHeaderButton="//input[@id='si_sample_header']",
            n2Label="//label[@for='n23']",
            n2Input="//input[@id='n23']",
            seedLabel="//label[@for='si_sample_seed']",
            seedInput="//input[@id='si_sample_seed']"
        )
    ),
    state9.1 = list(
        selectors = list(
            distributionLabel="//div[@id='si_dist_2']/label",
            distributionOption1Label="//div[@id='si_dist_2']//div[@class='radio'][1]//span",
            distributionOption1Input="//div[@id='si_dist_2']//div[@class='radio'][1]//input",
            distributionOption2Label="//div[@id='si_dist_2']//div[@class='radio'][2]//span",
            distributionOption2Input="//div[@id='si_dist_2']//div[@class='radio'][2]//input",
            distributionOption3Label="//div[@id='si_dist_2']//div[@class='radio'][3]//span",
            distributionOption3Input="//div[@id='si_dist_2']//div[@class='radio'][3]//input",
            distributionOption4Label="//div[@id='si_dist_2']//div[@class='radio'][4]//span",
            distributionOption4Input="//div[@id='si_dist_2']//div[@class='radio'][4]//input",
            distributionOption5Label="//div[@id='si_dist_2']//div[@class='radio'][5]//span",
            distributionOption5Input="//div[@id='si_dist_2']//div[@class='radio'][5]//input",
            distributionOption6Label="//div[@id='si_dist_2']//div[@class='radio'][6]//span",
            distributionOption6Input="//div[@id='si_dist_2']//div[@class='radio'][6]//input",
            n1Label="//label[@for='n12']",
            n1Input="//input[@id='n12']",
            burninLabel="//label[@for='burnin']",
            burninInput="//input[@id='burnin']",
            thinLabel="//label[@for='thin']",
            thinInput="//input[@id='thin']",
            n2Label="//label[@for='n22']",
            n2Input="//input[@id='n22']",
            seedLabel="//label[@for='mcmc_seed']",
            seedInput="//input[@id='mcmc_seed']",
            param1Label="//label[@for='param1']",
            param1Input="//input[@id='param1']",
            param2Label="//label[@for='param2']",
            param2Input="//input[@id='param2']"
        )
    )

)
