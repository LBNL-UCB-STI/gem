FROM r-base:3.5.3

# Install curl and openssl
RUN apt-get update && apt-get install -y --no-install-recommends libcurl4-openssl-dev \
    && apt-get install -y --no-install-recommends libssl-dev 

# Create project directory
RUN mkdir /gem

# Copy current directory contents on local to the container's project directory    
COPY . ./gem

# Set GEM project home and raw inputs direcotry path as env varaibles
ENV GEM_HOME=/gem
ENV GEM_RAW_INPUTS_DIR=/gem-raw-inputs

# Install R dependencies required for GEM
RUN ./gem/install-dependencies.sh

# Set GAMS version 
ENV LATEST=24.8.5
ENV GAMS_VERSION=24.0.2

# Set GAMS bit architecture, either 'x64_64' or 'x86_32'
ENV GAMS_BIT_ARC=x64_64

# Install wget 
RUN apt-get update && apt-get install -y --no-install-recommends wget curl software-properties-common git unzip

# Download GAMS R
RUN curl -SL "https://d37drm4t2jghv5.cloudfront.net/distributions/${GAMS_VERSION}/linux/linux_${GAMS_BIT_ARC}_sfx.exe" --create-dirs -o /opt/gams/gams.exe

# Install GAMS 
RUN cd /opt/gams &&\
    chmod +x gams.exe; sync &&\
    ./gams.exe &&\
    rm -rf gams.exe 

# Set GAMS home as an env variable
ENV GAMS_HOME=/opt/gams/gams_${GAMS_VERSION}

# Rename GAMS installation folder to GAMS_HOME value
RUN mv /opt/gams/gams${GAMS_VERSION%.*}_linux_${GAMS_BIT_ARC}_sfx ${GAMS_HOME} 

# Expose GEM_RAW_INPUTS_DIR & GAMS_HOME as volumes to mount required files during runtime
VOLUME ["${GEM_RAW_INPUTS_DIR}","${GAMS_HOME}"]

# Set required paths for GEM and GAMS in Rprofile file
RUN touch /root/.Rprofile && \
    echo "gem.raw.inputs=${GEM_RAW_INPUTS_DIR}/'" >> /root/.Rprofile && \
    echo "gem.project.directory='${GEM_HOME}/'" >> /root/.Rprofile && \
    echo "gams.executable.location='${GAMS_HOME}/'" >> /root/.Rprofile

# Run GAMS
RUN echo "export PATH=$PATH:$GAMS_HOME" >> ~/.bashrc && \
    cd $GAMS_HOME && \
    ./gamsinst -a

# Set the working directory
WORKDIR ${GEM_HOME}

# Run GEM
CMD [ "./src/gem.R","-e","/input/experiments/base.yaml" ]