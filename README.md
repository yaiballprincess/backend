# backend

The application serves 
1. scheduler that periodically creates a poll in a chat on VK.com
2. RESTful API to manage the scheduler

## Installation
1. Install GHC 9.4.5 and Cabal 3.10.1.0 (recommended via [GHCup](https://www.haskell.org/ghcup/))
2. `https://github.com/yaiballprincess/backend && cd backend`
3. Apply [migrations](./extra/migrations/001.psql)
4. Generate a "crypto key": `head -c 32 < /dev/urandom > extra/random-key.default`. Do it before compilation, since the key is embedded at compile time
5. `cabal build`
6. Path to executable: `cabal exec which yaiballprincess`

## How to run
1. Generate JWK and save it to the file
2. Edit [environment variables](#environment-variables)
3. `cabal run` or run executable manually

## Environment Variables
| Variable              | Description                     | Example                         |
|-----------------------|---------------------------------|---------------------------------|
| `DB_CONFIG_HOST`      | PostgreSQL hostname             | `localhost`                     |
| `DB_CONFIG_PORT`      | PostgreSQL port                 | `5432`                          |
| `DB_CONFIG_USER`      | PostgreSQL username             | `theroot`                       |
| `DB_CONFIG_PASSWORD`  | PostgreSQL password             | `thepassword`                   |
| `DB_CONFIG_DB`        | PostgreSQL database name        | `yibp_db`                       |
| `JWT_CONFIG_FILEPATH` | JWK                             | `extra/default-jwt-config.json` |
| `LOG_DIRECTORY`       | Directory where logs are stored | `/tmp/yibp`                     |
| `YIBP_BACKEND_PORT`   | Application port                | `8080`                          |
