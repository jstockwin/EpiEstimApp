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
            statusBar="//div[@id='status']"
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
            incidenceSepLabel="//div[@id='incidenceSep']/label",
            incidenceSepCommaButton="//div[@id='incidenceSep']//div[@class='radio'][1]//input",
            incidenceSepCommaLabel="//div[@id='incidenceSep']//div[@class='radio'][1]//span",
            incidenceSepSemiButton="//div[@id='incidenceSep']//div[@class='radio'][2]//input",
            incidenceSepSemiLabel="//div[@id='incidenceSep']//div[@class='radio'][2]//span",
            incidenceSepTabButton="//div[@id='incidenceSep']//div[@class='radio'][3]//input",
            incidenceSepTabLabel="//div[@id='incidenceSep']//div[@class='radio'][3]//span",
            incidenceQuoteLabel="//div[@id='incidenceQuote']/label",
            incidenceQuoteNoneButton="//div[@id='incidenceQuote']//div[@class='radio'][1]//input",
            incidenceQuoteNoneLabel="//div[@id='incidenceQuote']//div[@class='radio'][1]//span",
            incidenceQuoteDoubleButton="//div[@id='incidenceQuote']//div[@class='radio'][2]//input",
            incidenceQuoteDoubleLabel="//div[@id='incidenceQuote']//div[@class='radio'][2]//span",
            incidenceQuoteSingleButton="//div[@id='incidenceQuote']//div[@class='radio'][3]//input",
            incidenceQuoteSingleLabel="//div[@id='incidenceQuote']//div[@class='radio'][3]//span",
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
            datasetOption8Label="//div[@id='incidenceDataset']//div[@class='radio'][8]//span",
            datasetOption8Input="//div[@id='incidenceDataset']//div[@class='radio'][8]//input",
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
            importedHeaderButton="//input[@id='importedHeader']",
            importedSepLabel="//div[@id='importedSep']/label",
            importedSepCommaButton="//div[@id='importedSep']//div[@class='radio'][1]//input",
            importedSepCommaLabel="//div[@id='importedSep']//div[@class='radio'][1]//span",
            importedSepSemiButton="//div[@id='importedSep']//div[@class='radio'][2]//input",
            importedSepSemiLabel="//div[@id='importedSep']//div[@class='radio'][2]//span",
            importedSepTabButton="//div[@id='importedSep']//div[@class='radio'][3]//input",
            importedSepTabLabel="//div[@id='importedSep']//div[@class='radio'][3]//span",
            importedQuoteLabel="//div[@id='importedQuote']/label",
            importedQuoteNoneButton="//div[@id='importedQuote']//div[@class='radio'][1]//input",
            importedQuoteNoneLabel="//div[@id='importedQuote']//div[@class='radio'][1]//span",
            importedQuoteDoubleButton="//div[@id='importedQuote']//div[@class='radio'][2]//input",
            importedQuoteDoubleLabel="//div[@id='importedQuote']//div[@class='radio'][2]//span",
            importedQuoteSingleButton="//div[@id='importedQuote']//div[@class='radio'][3]//input",
            importedQuoteSingleLabel="//div[@id='importedQuote']//div[@class='radio'][3]//span"
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
            uncertainty="//div[@id='uncertainty']",
            uncertaintyLabel="//div[@id='uncertainty']/label",
            uncertaintyNoLabel="//div[@id='uncertainty']//div[@class='radio'][1]//span",
            uncertaintyNoButton="//div[@id='uncertainty']//div[@class='radio'][1]//input",
            uncertaintyYesLabel="//div[@id='uncertainty']//div[@class='radio'][2]//span",
            uncertaintyYesButton="//div[@id='uncertainty']//div[@class='radio'][2]//input"
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
            parametric="//div[@id='parametric']",
            parametricLabel="//div[@id='parametric']/label",
            parametricNoLabel="//div[@id='parametric']//div[@class='radio'][2]//span",
            parametricNoButton="//div[@id='parametric']//div[@class='radio'][2]//input",
            parametricYesLabel="//div[@id='parametric']//div[@class='radio'][1]//span",
            parametricYesButton="//div[@id='parametric']//div[@class='radio'][1]//input"
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
            distributionOption6Input="//div[@id='SIDist']//div[@class='radio'][6]//input"
        )
    ),
    state8.2 = list(
        selectors = list(
            SIDataUploadLabel="//div[@id='SIDataErrorBox']/div/label",
            SIDataUploadBrowse="//div[@id='SIDataErrorBox']//span",
            SIDataUploadInput="//input[@id='SIData']",
            SIDataUploadText="//div[@id='SIDataErrorBox']//input[@type='text']",
            SIHeaderButton="//input[@id='SIHeader']",
            SISepLabel="//div[@id='SISep']/label",
            SISepCommaButton="//div[@id='SISep']//div[@class='radio'][1]//input",
            SISepCommaLabel="//div[@id='SISep']//div[@class='radio'][1]//span",
            SISepSemiButton="//div[@id='SISep']//div[@class='radio'][2]//input",
            SISepSemiLabel="//div[@id='SISep']//div[@class='radio'][2]//span",
            SISepTabButton="//div[@id='SISep']//div[@class='radio'][3]//input",
            SISepTabLabel="//div[@id='SISep']//div[@class='radio'][3]//span",
            SIQuoteLabel="//div[@id='SIQuote']/label",
            SIQuoteNoneButton="//div[@id='SIQuote']//div[@class='radio'][1]//input",
            SIQuoteNoneLabel="//div[@id='SIQuote']//div[@class='radio'][1]//span",
            SIQuoteDoubleButton="//div[@id='SIQuote']//div[@class='radio'][2]//input",
            SIQuoteDoubleLabel="//div[@id='SIQuote']//div[@class='radio'][2]//span",
            SIQuoteSingleButton="//div[@id='SIQuote']//div[@class='radio'][3]//input",
            SIQuoteSingleLabel="//div[@id='SIQuote']//div[@class='radio'][3]//span",
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
            SISampleSepLabel="//div[@id='SISampleSep']/label",
            SISampleSepCommaButton="//div[@id='SISampleSep']//div[@class='radio'][1]//input",
            SISampleSepCommaLabel="//div[@id='SISampleSep']//div[@class='radio'][1]//span",
            SISampleSepSemiButton="//div[@id='SISampleSep']//div[@class='radio'][2]//input",
            SISampleSepSemiLabel="//div[@id='SISampleSep']//div[@class='radio'][2]//span",
            SISampleSepTabButton="//div[@id='SISampleSep']//div[@class='radio'][3]//input",
            SISampleSepTabLabel="//div[@id='SISampleSep']//div[@class='radio'][3]//span",
            SISampleQuoteLabel="//div[@id='SISampleQuote']/label",
            SISampleQuoteNoneButton="//div[@id='SISampleQuote']//div[@class='radio'][1]//input",
            SISampleQuoteNoneLabel="//div[@id='SISampleQuote']//div[@class='radio'][1]//span",
            SISampleQuoteDoubleButton="//div[@id='SISampleQuote']//div[@class='radio'][2]//input",
            SISampleQuoteDoubleLabel="//div[@id='SISampleQuote']//div[@class='radio'][2]//span",
            SISampleQuoteSingleButton="//div[@id='SISampleQuote']//div[@class='radio'][3]//input",
            SISampleQuoteSingleLabel="//div[@id='SISampleQuote']//div[@class='radio'][3]//span",
            n2Label="//label[@for='n23']",
            n2Input="//input[@id='n23']",
            seedLabel="//label[@for='SISampleSeed']",
            seedInput="//input[@id='SISampleSeed']"
        )
    ),
    state8.4 = list(
        selectors = list(
            Mean.SILabel="//label[@for='Mean.SI2']",
            Mean.SIInput="//input[@id='Mean.SI2']",
            Std.SILabel="//label[@for='Std.SI2']",
            Std.SIInput="//input[@id='Std.SI2']"
        )
    ),
    state8.5 = list(
        selectors = list(
            SIDistrDataType="//div[@id='SIDistrDataType']",
            SIDistrDataTypeLabel="//div[@id='SIDistrDataType']/label",
            SIDistrDataTypePreloadedLabel="//div[@id='SIDistrDataType']//div[@class='radio'][1]//span",
            SIDistrDataTypePreloadedButton="//div[@id='SIDistrDataType']//div[@class='radio'][1]//input",
            SIDistrDataTypeOwnLabel="//div[@id='SIDistrDataType']//div[@class='radio'][2]//span",
            SIDistrDataTypeOwnButton="//div[@id='SIDistrDataType']//div[@class='radio'][2]//input"
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
