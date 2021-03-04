# telehaskell
Study project: telegram bot written in haskell

## Running without docker
You need [Telegram bot api](https://github.com/tdlib/telegram-bot-api) and mongodb running on 127.0.0.1 on default ports for this to work.
To run execute `BOT_TOKEN=<your bot token> cabal run`

## Running in docker
Execute `TELEGRAM_API_ID=<yout id> TELEGRAM_API_HASH=<yout token> BOT_TOKEN=<your token> docker-compose up`