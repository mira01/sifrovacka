FROM erlang:21-alpine AS sifrovacka_dependencies

RUN apk add --no-cache --virtual .build-deps \
        autoconf \
        automake \
        g++ \
        gawk \
        gcc \
        libtool \
        linux-pam-dev \
        make \
        git \
    && git clone https://github.com/klacke/yaws.git /yaws-src \
    && cd /yaws-src \
    && autoreconf -fi \
    && ./configure --localstatedir=/var --sysconfdir=/etc \
    && make && make install \
    && cd /var \
    && rm -rf /yaws-src \
#    && apk del .build-deps
    && alias cc=gcc

RUN apk add git

RUN rebar3 get-deps

FROM sifrovacka_dependencies

#COPY game /app
#COPY messenger /messenger
#COPY sifrovacka_release /sifrovacka_release
#
#COPY cibulka_game /definitions/cibulka_game
#
#CMD cd /sifrovacka_release; rebar3 compile && rebar3 shell

RUN ls ../

COPY game /erlapp/game
COPY messenger /erlapp/messenger
COPY sifrovacka_release /erlapp/sifrovacka_release

COPY cibulka_game /definitions/cibulka_game

CMD cd /erlapp/sifrovacka_release; rebar3 compile && rebar3 shell
