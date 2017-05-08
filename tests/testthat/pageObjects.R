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
            incidenceDataTypeLabel="//div[@id='incidenceDataType']/label"
        )
    )

)
