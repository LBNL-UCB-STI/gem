#
# Stage 1 : GAMS download
#
FROM ubuntu:18.04 as gams
# Set GAMS version , GAMS bit architecture, either 'x64_64' or 'x86_32'
ENV GAMS_VERSION=26.1.0
ENV GAMS_HOME=/gams
ENV GAMS_BIT_ARC=x64_64
# Install wget 
RUN apt-get update && apt-get install -y --no-install-recommends wget curl unzip ca-certificates
# Download GAMS R
RUN curl -SL "https://d37drm4t2jghv5.cloudfront.net/distributions/${GAMS_VERSION}/linux/linux_${GAMS_BIT_ARC}_sfx.exe" --create-dirs -o ${GAMS_HOME}-download/gams.exe
# Install GAMS 
RUN mkdir ${GAMS_HOME} && \
    ls ${GAMS_HOME}-download/ &&\
    echo "Dowloaded GAMS.." &&\
    cd ${GAMS_HOME}-download &&\
    chmod +x ./gams.exe; sync &&\
    echo "Installing GAMS.." &&\
    ./gams.exe &&\
    rm -rf gams.exe &&\
    mv -v ${GAMS_HOME}-download/gams${GAMS_VERSION%.*}_linux_${GAMS_BIT_ARC}_sfx/* ${GAMS_HOME} &&\
    rm -rf ${GAMS_HOME}-download

#
# Stage 2 : Running GEM and GAMS
#
FROM r-base:3.5.3
RUN cp -r /usr/bin/Rscript /usr/local/bin/Rscript 
# Install curl and openssl
RUN apt-get update && apt-get install -y --no-install-recommends libcurl4-openssl-dev libssl-dev git
ENV GIT_HOST_URL="github.com"
ENV GIT_REPO_NAME="LBNL-UCB-STI/gem"
# Create required project directories
RUN mkdir /gem && mkdir /gem-raw-inputs
# Set required home paths as env variables
ENV GEM_HOME=/gem
ENV GAMS_HOME=/gams
ENV GEM_RAW_INPUTS_DIR=/gem-raw-inputs
ENV GEM_RUN_SCRIPT=run-gem.sh
# Copy the GEM project directory & GAMS directories from previous stages   
COPY --from=gams ${GAMS_HOME} ${GAMS_HOME}
# Expose GEM_RAW_INPUTS_DIR & GAMS_HOME as volumes to mount required files during runtime
VOLUME ["${GEM_RAW_INPUTS_DIR}","${GAMS_HOME}"]
# Set required paths for GEM and GAMS in Rprofile file
RUN touch /root/.Rprofile &&\
    echo "gem.raw.inputs='${GEM_RAW_INPUTS_DIR}/'" >> /root/.Rprofile &&\
    echo "gem.project.directory='${GEM_HOME}/'" >> /root/.Rprofile &&\
    echo "gams.executable.location='${GAMS_HOME}/'" >> /root/.Rprofile &&\
    echo "export PATH=$PATH:$GAMS_HOME" >> ~/.bashrc
RUN touch ${GEM_RUN_SCRIPT} && chmod +x ${GEM_RUN_SCRIPT} &&\
    echo "git clone https://\${GIT_USERNAME}:\${GIT_PASSWORD}@\${GIT_HOST_URL}/\${GIT_REPO_NAME}.git gem" >> ${GEM_RUN_SCRIPT} &&\
    echo "if [ -n '\$COMMIT_ID' ] ; then cd gem && git checkout \$COMMIT_ID ; fi " >> ${GEM_RUN_SCRIPT} &&\
    echo "chmod +x \$GEM_HOME/src/gem.R"  >> ${GEM_RUN_SCRIPT} &&\
    echo "cd \$GEM_HOME && ./install-dependencies.sh && src/gem.R -e \${INPUT_FILE}" >> ${GEM_RUN_SCRIPT}

# Run GEM
CMD cd $GAMS_HOME && ./gamsinst -a && cd / && ./${GEM_RUN_SCRIPT}