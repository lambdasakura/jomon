FROM debian:jessie

WORKDIR /tmp
RUN apt-get update && apt-get -y install git build-essential automake libcurl4-openssl-dev
RUN apt-get install locales dialog

# set locale
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    locale-gen
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8


RUN git clone -b release https://github.com/roswell/roswell.git
RUN cd roswell && sh bootstrap && ./configure && make && make install
RUN ros setup
RUN ros install lake
RUN ros install qlot
RUN ros install lambdasakura/tachikoma
RUN ros install lack
RUN ros install clack
RUN ros install ningle
RUN ros install caveman
RUN ros install cl-emb
RUN ros install cl-ppcre
RUN ros install datafly
RUN ros install djula
RUN ros install envy
RUN ros install log4cl
RUN ros install sxql
RUN ros install uuid
RUN ros install ironclad
RUN ros install fukamachi/jose
ENV PATH $PATH:/root/.roswell/bin/
RUN /root/.roswell/bin/lake-tools dump
RUN /root/.roswell/bin/tachikoma-tools dump
RUN mkdir -p /usr/src/app
RUN ln -s /usr/src/app /root/.roswell/local-projects/
WORKDIR /usr/src/app
VOLUME ["/usr/src/app"]
