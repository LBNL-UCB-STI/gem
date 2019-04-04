FROM r-base:3.5.3

# Install curl and openssl
RUN apt-get update && apt-get install -y --no-install-recommends libcurl4-openssl-dev \
    && apt-get install -y --no-install-recommends libssl-dev 

# Create required project directories
RUN mkdir /gem && mkdir /gams && mkdir /gem-raw-inputs

# Copy current directory contents on local to the container's project directory    
COPY . ./gem

# Set required home paths as env variables
ENV GEM_HOME=/gem
ENV GAMS_HOME=/gams
ENV GEM_RAW_INPUTS_DIR=/gem-raw-inputs

# Install R dependencies required for GEM
RUN ./gem/install-dependencies.sh

# Set GAMS version 
ENV LATEST=24.8.5
ENV GAMS_VERSION=26.1.0

# Set GAMS bit architecture, either 'x64_64' or 'x86_32'
ENV GAMS_BIT_ARC=x64_64

# Install wget 
RUN apt-get update && apt-get install -y --no-install-recommends wget curl software-properties-common git unzip

# Download GAMS R
RUN curl -SL "https://d37drm4t2jghv5.cloudfront.net/distributions/${GAMS_VERSION}/linux/linux_${GAMS_BIT_ARC}_sfx.exe" --create-dirs -o ${GAMS_HOME}-download/gams.exe

# Install GAMS 
RUN echo ${GAMS_HOME}-download &&\
    cd ${GAMS_HOME}-download &&\
    chmod +x gams.exe; sync &&\
    ./gams.exe &&\
    rm -rf gams.exe &&\
    mv -v ${GAMS_HOME}-download/gams${GAMS_VERSION%.*}_linux_${GAMS_BIT_ARC}_sfx/* ${GAMS_HOME} &&\
    rm -rf ${GAMS_HOME}-download 

# Expose GEM_RAW_INPUTS_DIR & GAMS_HOME as volumes to mount required files during runtime
VOLUME ["${GEM_RAW_INPUTS_DIR}","${GAMS_HOME}"]

# Set required paths for GEM and GAMS in Rprofile file
RUN touch /root/.Rprofile && \
    echo "gem.raw.inputs='${GEM_RAW_INPUTS_DIR}/'" >> /root/.Rprofile &&\
    echo "gem.project.directory='${GEM_HOME}/'" >> /root/.Rprofile &&\
    echo "gams.executable.location='${GAMS_HOME}/'" >> /root/.Rprofile &&\
    echo "export PATH=$PATH:$GAMS_HOME" >> ~/.bashrc

# Run GEM
CMD cd $GAMS_HOME && ./gamsinst -a && cd $GEM_HOME && ./src/gem.R -e /input/experiments/base.yaml