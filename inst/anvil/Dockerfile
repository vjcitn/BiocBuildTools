FROM bioconductor/bioconductor_docker:devel
# intended for AnVIL with Rstudio

# This is to avoid the error
# 'debconf: unable to initialize frontend: Dialog'
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get -y update ; \
    apt-get -y install libharfbuzz-dev libfribidi-dev ; \
    R -e "BiocManager::install('vjcitn/BiocBuildTools', ask=FALSE, update=TRUE)"



# Add back other env vars
RUN echo "TERRA_R_PLATFORM='anvil-rstudio-bioconductor'" >> /usr/local/lib/R/etc/Renviron.site \
    && echo "TERRA_R_PLATFORM_BINARY_VERSION='0.99.1'" >> /usr/local/lib/R/etc/Renviron.site

USER root

# Init command for s6-overlay
CMD ["/init"]

