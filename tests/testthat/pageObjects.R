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
    )

)
