FROM r-base:3.6.2

RUN apt-get update \
    && apt-get install -y --no-install-recommends --allow-downgrades \
    libcurl4 \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Set R packages to install in site-library so we can cache them
ENV R_LIBS_USER=/site-library
RUN R -e "install.packages('devtools')"

# Copy our code and install test requirements
COPY . /app
RUN R -e "devtools::install_dev_deps('/app')"

# Install the app
RUN R -e "devtools::install('/app')"

# Run the app and expose port 3000
EXPOSE 3000
CMD R -e "EpiEstimApp::runEpiEstimApp(host='0.0.0.0', port=3000)"
