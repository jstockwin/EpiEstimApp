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
            incidenceTable="//div[@id='incidenceDataOutput']/table",
            reproductionTable="//div[@id='estimatedROutput']/table",
            serialIntervalTable="//div[@id='serialIntervalOutput']/table"
        )
    ),
    state1.1 = list(
        selectors = list(
            incidenceDataType="//div[@id='incidenceDataType']",
            incidenceDataTypeLabel="//div[@id='incidenceDataType']/label",
            preloadedDataButton = "//div[@id='incidenceDataType']/div[@class='shiny-options-group']/div[@class='radio'][1]//input",
            ownDataButton = "//div[@id='incidenceDataType']/div[@class='shiny-options-group']/div[@class='radio'][2]//input",
            preloadedDataLabel = "//div[@id='incidenceDataType']/div[@class='shiny-options-group']/div[@class='radio'][1]//span",
            ownDataLabel = "//div[@id='incidenceDataType']/div[@class='shiny-options-group']/div[@class='radio'][2]//span"
        )
    ),
    state2.1 = list(
        selectors = list(
            incidenceDataUploadLabel="//div[@id='incidenceDataErrorBox']/div/label",
            incidenceDataUploadBrowse="//div[@id='incidenceDataErrorBox']//span",
            incidenceDataUploadInput="//input[@id='incidenceData']",
            incidenceDataUploadText="//div[@id='incidenceDataErrorBox']//input[@type='text']",
            incidenceHeaderButton="//input[@id='incidenceHeader']",
            uploadedWidthLabel="//label[@for='uploadedWidth']",
            uploadedWidthInput="//input[@id='uploadedWidth']",
            meanPriorLabel="//label[@for='uploadedMeanPrior']",
            meanPriorInput="//input[@id='uploadedMeanPrior']",
            stdPriorLabel="//label[@for='uploadedStdPrior']",
            stdPriorInput="//input[@id='uploadedStdPrior']"
        )
    ),
    state2.2 = list(
        selectors = list(
            datasetLabel="//div[@id='incidenceDataset']/label",
            datasetOption1Label="//div[@id='incidenceDataset']//div[@class='radio'][1]//span",
            datasetOption1Input="//div[@id='incidenceDataset']//div[@class='radio'][1]//input",
            datasetOption2Label="//div[@id='incidenceDataset']//div[@class='radio'][2]//span",
            datasetOption2Input="//div[@id='incidenceDataset']//div[@class='radio'][2]//input",
            datasetOption3Label="//div[@id='incidenceDataset']//div[@class='radio'][3]//span",
            datasetOption3Input="//div[@id='incidenceDataset']//div[@class='radio'][3]//input",
            datasetOption4Label="//div[@id='incidenceDataset']//div[@class='radio'][4]//span",
            datasetOption4Input="//div[@id='incidenceDataset']//div[@class='radio'][4]//input",
            datasetOption5Label="//div[@id='incidenceDataset']//div[@class='radio'][5]//span",
            datasetOption5Input="//div[@id='incidenceDataset']//div[@class='radio'][5]//input",
            datasetOption6Label="//div[@id='incidenceDataset']//div[@class='radio'][6]//span",
            datasetOption6Input="//div[@id='incidenceDataset']//div[@class='radio'][6]//input",
            datasetOption7Label="//div[@id='incidenceDataset']//div[@class='radio'][7]//span",
            datasetOption7Input="//div[@id='incidenceDataset']//div[@class='radio'][7]//input",
            incidenceWidthLabel="//label[@for='incidenceWidth']",
            incidenceWidthInput="//input[@id='incidenceWidth']",
            meanPriorLabel="//label[@for='incidenceMeanPrior']",
            meanPriorInput="//input[@id='incidenceMeanPrior']",
            stdPriorLabel="//label[@for='incidenceStdPrior']",
            stdPriorInput="//input[@id='incidenceStdPrior']"
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
            importedDataUploadLabel="//div[@id='importedDataErrorBox']/div/label",
            importedDataUploadBrowse="//div[@id='importedDataErrorBox']//span",
            importedDataUploadInput="//input[@id='importedData']",
            importedDataUploadText="//div[@id='importedDataErrorBox']//input[@type='text']",
            importedHeaderButton="//input[@id='importedHeader']"
        )
    ),
    state5.1 = list(
        selectors = list(
            exposureDataLabel="//div[@id='SIPatientData']/label",
            exposureDataNoLabel="//div[@id='SIPatientData']//div[@class='radio'][1]//span",
            exposureDataNoInput="//div[@id='SIPatientData']//div[@class='radio'][1]//input",
            exposureDataYesLabel="//div[@id='SIPatientData']//div[@class='radio'][2]//span",
            exposureDataYesInput="//div[@id='SIPatientData']//div[@class='radio'][2]//input"
        )
    ),
    state6.1 = list(
        selectors = list(
            SIDataType="//div[@id='SIDataType']",
            SIDataTypeLabel="//div[@id='SIDataType']/label",
            SIDataTypePreloadedLabel="//div[@id='SIDataType']//div[@class='radio'][1]//span",
            SIDataTypePreloadedButton="//div[@id='SIDataType']//div[@class='radio'][1]//input",
            SIDataTypeOwnLabel="//div[@id='SIDataType']//div[@class='radio'][2]//span",
            SIDataTypeOwnButton="//div[@id='SIDataType']//div[@class='radio'][2]//input"
        )
    ),
    state6.2 = list(
        selectors = list(
            SIEstType="//div[@id='SIEstType']",
            SIEstTypeLabel="//div[@id='SIEstType']/label",
            SIEstTypeOption1Label="//div[@id='SIEstType']//div[@class='radio'][1]//span",
            SIEstTypeOption1Button="//div[@id='SIEstType']//div[@class='radio'][1]//input",
            SIEstTypeOption2Label="//div[@id='SIEstType']//div[@class='radio'][2]//span",
            SIEstTypeOption2Button="//div[@id='SIEstType']//div[@class='radio'][2]//input",
            SIEstTypeOption3Label="//div[@id='SIEstType']//div[@class='radio'][3]//span",
            SIEstTypeOption3Button="//div[@id='SIEstType']//div[@class='radio'][3]//input",
            SIEstTypeOption4Label="//div[@id='SIEstType']//div[@class='radio'][4]//span",
            SIEstTypeOption4Button="//div[@id='SIEstType']//div[@class='radio'][4]//input"
        )
    ),
    state7.1 = list(
        selectors = list(
            datasetLabel="//div[@id='SIDataset']/label",
            datasetOption1Label="//div[@id='SIDataset']//div[@class='radio'][1]//span",
            datasetOption1Input="//div[@id='SIDataset']//div[@class='radio'][1]//input",
            datasetOption2Label="//div[@id='SIDataset']//div[@class='radio'][2]//span",
            datasetOption2Input="//div[@id='SIDataset']//div[@class='radio'][2]//input",
            datasetOption3Label="//div[@id='SIDataset']//div[@class='radio'][3]//span",
            datasetOption3Input="//div[@id='SIDataset']//div[@class='radio'][3]//input"
        )
    ),
    state7.2 = list(
        selectors = list(
            SIFrom="//div[@id='SIFrom']",
            SIFromLabel="//div[@id='SIFrom']/label",
            SIFromRawLabel="//div[@id='SIFrom']//div[@class='radio'][1]//span",
            SIFromRawButton="//div[@id='SIFrom']//div[@class='radio'][1]//input",
            SIFromSampleLabel="//div[@id='SIFrom']//div[@class='radio'][2]//span",
            SIFromSampleButton="//div[@id='SIFrom']//div[@class='radio'][2]//input"
        )
    ),
    state7.3 = list(
        selectors = list(
            n1Label="//label[@for='n1']",
            n1Input="//input[@id='n1']",
            n2Label="//label[@for='n2']",
            n2Input="//input[@id='n2']",
            Mean.SILabel="//label[@for='Mean.SI']",
            Mean.SIInput="//input[@id='Mean.SI']",
            Std.Mean.SILabel="//label[@for='Std.Mean.SI']",
            Std.Mean.SIInput="//input[@id='Std.Mean.SI']",
            Min.Mean.SILabel="//label[@for='Min.Mean.SI']",
            Min.Mean.SIInput="//input[@id='Min.Mean.SI']",
            Max.Mean.SILabel="//label[@for='Max.Mean.SI']",
            Max.Mean.SIInput="//input[@id='Max.Mean.SI']",
            Std.SILabel="//label[@for='Std.SI']",
            Std.SIInput="//input[@id='Std.SI']",
            Std.Std.SILabel="//label[@for='Std.Std.SI']",
            Std.Std.SIInput="//input[@id='Std.Std.SI']",
            Min.Std.SILabel="//label[@for='Min.Std.SI']",
            Min.Std.SIInput="//input[@id='Min.Std.SI']",
            Max.Std.SILabel="//label[@for='Max.Std.SI']",
            Max.Std.SIInput="//input[@id='Max.Std.SI']",
            seedLabel="//label[@for='uncertainSeed']",
            seedInput="//input[@id='uncertainSeed']"
        )
    ),
    state7.4 = list(
        selectors = list(
            Mean.SILabel="//label[@for='Mean.SI2']",
            Mean.SIInput="//input[@id='Mean.SI2']",
            Std.SILabel="//label[@for='Std.SI2']",
            Std.SIInput="//input[@id='Std.SI2']"
        )
    ),
    state7.5 = list(
        selectors = list(
            SIDistrDataUploadLabel="//div[@id='SIDistrDataErrorBox']/div/label",
            SIDistrDataUploadBrowse="//div[@id='SIDistrDataErrorBox']//span",
            SIDistrDataUploadInput="//input[@id='SIDistrData']",
            SIDistrDataUploadText="//div[@id='SIDistrDataErrorBox']//input[@type='text']",
            SIDistrHeaderButton="//input[@id='SIDistrHeader']"
        )
    ),
    state7.6 = list(
        selectors = list(
            datasetLabel="//div[@id='SIDistrDataset']/label",
            datasetOption1Label="//div[@id='SIDistrDataset']//div[@class='radio'][1]//span",
            datasetOption1Input="//div[@id='SIDistrDataset']//div[@class='radio'][1]//input",
            datasetOption2Label="//div[@id='SIDistrDataset']//div[@class='radio'][2]//span",
            datasetOption2Input="//div[@id='SIDistrDataset']//div[@class='radio'][2]//input",
            datasetOption3Label="//div[@id='SIDistrDataset']//div[@class='radio'][3]//span",
            datasetOption3Input="//div[@id='SIDistrDataset']//div[@class='radio'][3]//input",
            datasetOption4Label="//div[@id='SIDistrDataset']//div[@class='radio'][4]//span",
            datasetOption4Input="//div[@id='SIDistrDataset']//div[@class='radio'][4]//input",
            datasetOption5Label="//div[@id='SIDistrDataset']//div[@class='radio'][5]//span",
            datasetOption5Input="//div[@id='SIDistrDataset']//div[@class='radio'][5]//input"
        )
    ),
    state8.1 = list(
        selectors = list(
            distributionLabel="//div[@id='SIDist']/label",
            distributionOption1Label="//div[@id='SIDist']//div[@class='radio'][1]//span",
            distributionOption1Input="//div[@id='SIDist']//div[@class='radio'][1]//input",
            distributionOption2Label="//div[@id='SIDist']//div[@class='radio'][2]//span",
            distributionOption2Input="//div[@id='SIDist']//div[@class='radio'][2]//input",
            distributionOption3Label="//div[@id='SIDist']//div[@class='radio'][3]//span",
            distributionOption3Input="//div[@id='SIDist']//div[@class='radio'][3]//input",
            distributionOption4Label="//div[@id='SIDist']//div[@class='radio'][4]//span",
            distributionOption4Input="//div[@id='SIDist']//div[@class='radio'][4]//input",
            distributionOption5Label="//div[@id='SIDist']//div[@class='radio'][5]//span",
            distributionOption5Input="//div[@id='SIDist']//div[@class='radio'][5]//input",
            distributionOption6Label="//div[@id='SIDist']//div[@class='radio'][6]//span",
            distributionOption6Input="//div[@id='SIDist']//div[@class='radio'][6]//input",
            n2Label="//label[@for='n24']",
            n2Input="//input[@id='n24']",
            seedLabel="//label[@for='preloadedSeed']",
            seedInput="//input[@id='preloadedSeed']"
        )
    ),
    state8.2 = list(
        selectors = list(
            SIDataUploadLabel="//div[@id='SIDataErrorBox']/div/label",
            SIDataUploadBrowse="//div[@id='SIDataErrorBox']//span",
            SIDataUploadInput="//input[@id='SIData']",
            SIDataUploadText="//div[@id='SIDataErrorBox']//input[@type='text']",
            SIHeaderButton="//input[@id='SIHeader']",
            seedLabel="//label[@for='uploadedSISeed']",
            seedInput="//input[@id='uploadedSISeed']"
        )
    ),
    state8.3 = list(
        selectors = list(
            SISampleDataUploadLabel="//div[@id='SISampleDataErrorBox']/div/label",
            SISampleDataUploadBrowse="//div[@id='SISampleDataErrorBox']//span",
            SISampleDataUploadInput="//input[@id='SISampleData']",
            SISampleDataUploadText="//div[@id='SISampleDataErrorBox']//input[@type='text']",
            SISampleHeaderButton="//input[@id='SISampleHeader']",
            n2Label="//label[@for='n23']",
            n2Input="//input[@id='n23']",
            seedLabel="//label[@for='SISampleSeed']",
            seedInput="//input[@id='SISampleSeed']"
        )
    ),
    state9.1 = list(
        selectors = list(
            distributionLabel="//div[@id='SIDist2']/label",
            distributionOption1Label="//div[@id='SIDist2']//div[@class='radio'][1]//span",
            distributionOption1Input="//div[@id='SIDist2']//div[@class='radio'][1]//input",
            distributionOption2Label="//div[@id='SIDist2']//div[@class='radio'][2]//span",
            distributionOption2Input="//div[@id='SIDist2']//div[@class='radio'][2]//input",
            distributionOption3Label="//div[@id='SIDist2']//div[@class='radio'][3]//span",
            distributionOption3Input="//div[@id='SIDist2']//div[@class='radio'][3]//input",
            distributionOption4Label="//div[@id='SIDist2']//div[@class='radio'][4]//span",
            distributionOption4Input="//div[@id='SIDist2']//div[@class='radio'][4]//input",
            distributionOption5Label="//div[@id='SIDist2']//div[@class='radio'][5]//span",
            distributionOption5Input="//div[@id='SIDist2']//div[@class='radio'][5]//input",
            distributionOption6Label="//div[@id='SIDist2']//div[@class='radio'][6]//span",
            distributionOption6Input="//div[@id='SIDist2']//div[@class='radio'][6]//input",
            n1Label="//label[@for='n12']",
            n1Input="//input[@id='n12']",
            burninLabel="//label[@for='burnin']",
            burninInput="//input[@id='burnin']",
            thinLabel="//label[@for='thin']",
            thinInput="//input[@id='thin']",
            n2Label="//label[@for='n22']",
            n2Input="//input[@id='n22']",
            seedLabel="//label[@for='MCMCSeed']",
            seedInput="//input[@id='MCMCSeed']",
            param1Label="//label[@for='param1']",
            param1Input="//input[@id='param1']",
            param2Label="//label[@for='param2']",
            param2Input="//input[@id='param2']"
        )
    )

)
