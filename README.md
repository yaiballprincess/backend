# backend

The application serves 
1. scheduler that periodically creates a poll in a chat on VK.com
2. RESTful API to manage the scheduler

## Installation
1. Install GHC 9.4.5 and Cabal 3.10.1.0 (recommended via [GHCup](https://www.haskell.org/ghcup/))
2. `https://github.com/yaiballprincess/backend && cd backend`
3. `cabal build`
4. Path to executable: `cabal exec which yaiballprincess`

## How to run (Docker)
1. Generate JWK and save it to the file (say in folder `secrets`)
2. Generate a "crypto key": `head -c 32 < /dev/urandom > secrets/random-key.default`
3. **DO NOT** TOUCH `YIBP_BACKEND_PORT`
```
docker run --rm -p 12999:8080 -v ./secrets:/app \
       -e DB_CONFIG_HOST=... -e DB_CONFIG_PORT=5432 \
       -e DB_CONFIG_USER=theroot -e DB_CONFIG_PASSWORD=thepassword \
       -e DB_CONFIG_DB=yibp_db -e LOG_DIRECTORY=/app/logs \
       ghcr.io/yaiballprincess/backend:v1.0.0 migrate
```
4. 
```
docker run --name yaiballprincess-backend -d -p 12999:8080 -v ./secrets:/app \
       -e DB_CONFIG_HOST=... -e DB_CONFIG_PORT=5432 \
       -e DB_CONFIG_USER=theroot -e DB_CONFIG_PASSWORD=thepassword \
       -e DB_CONFIG_DB=yibp_db -e LOG_DIRECTORY=/app/logs \
       ghcr.io/yaiballprincess/backend:v1.0.0
```

## How to run
1. Generate JWK and save it to the file
2. Generate a "crypto key": `head -c 32 < /dev/urandom > extra/random-key.default`
3. Edit [environment variables](#environment-variables)
4. Apply [migrations](./extra/migrations/001.psql) manually or run `./yaiballprincess migrate`
5. `cabal run` or run executable manually

## Help

`./yaiballprincess help` or `cabal run yaiballprincess -- help`


## Environment Variables
| Variable              | Description                     | Example                         |
|-----------------------|---------------------------------|---------------------------------|
| `DB_CONFIG_HOST`      | PostgreSQL hostname             | `localhost`                     |
| `DB_CONFIG_PORT`      | PostgreSQL port                 | `5432`                          |
| `DB_CONFIG_USER`      | PostgreSQL username             | `theroot`                       |
| `DB_CONFIG_PASSWORD`  | PostgreSQL password             | `thepassword`                   |
| `DB_CONFIG_DB`        | PostgreSQL database name        | `yibp_db`                       |
| `JWT_CONFIG_FILEPATH` | JWK                             | `extra/default-jwt-config.json` |
| `SECRET_KEY_FILEPATH` | Secret key filepath             | `extra/random-key.default`      |
| `LOG_DIRECTORY`       | Directory where logs are stored | `/tmp/yibp`                     |
| `YIBP_BACKEND_PORT`   | Application port                | `8080`                          |

See also [example config](./.env.default).