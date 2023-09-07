FROM haskell:9.4.5-slim AS build

WORKDIR /src
COPY . .
RUN apt update && apt install -y libpq-dev
RUN cabal update
RUN cabal install exe:yaiballprincess --install-method=copy --overwrite-policy=always --installdir=dist

FROM debian:10-slim

VOLUME /app
WORKDIR /app

ENV DB_CONFIG_HOST=localhost
ENV DB_CONFIG_PORT=5432
ENV DB_CONFIG_USER=theroot
ENV DB_CONFIG_PASSWORD=thepassword
ENV DB_CONFIG_DB=yibp_db
ENV JWT_CONFIG_FILEPATH=/app/default-jwt-config.json
ENV SECRET_KEY_FILEPATH=/app/random-key.default
ENV LOG_DIRECTORY=/tmp/yibp

EXPOSE 8080

COPY --from=build /src/dist/yaiballprincess /usr/local/bin/yaiballprincess
RUN chmod +x /usr/local/bin/yaiballprincess
RUN apt update && apt install -y libpq-dev

ENTRYPOINT ["yaiballprincess"]