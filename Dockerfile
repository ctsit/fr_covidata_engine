FROM rocker/tidyverse

WORKDIR /home/rocker

RUN apt update -y && apt install -y libcurl4-openssl-dev

# install necessary libraries
RUN R -e "install.packages(c('sendmailR', 'dotenv', 'REDCapR', 'RCurl', 'checkmate', 'janitor', 'sqldf'))"

# Add sftp support
RUN apt install -y openssh-client
# remove need to verify ssh servers
RUN echo "StrictHostKeyChecking no" >> /etc/ssh/ssh_config

ADD *.R /home/rocker/

# Note where we are and what is there
CMD pwd && ls -AlhF ./
