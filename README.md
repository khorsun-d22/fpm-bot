Just a bot. You can play with it on [Telegram](https://t.me/fam_bot_test_group).

# Installation

## Ubuntu

```sh
sudo apt-get install chicken-bin libssl-dev
git clone https://github.com/khorsun-d22/fpm-bot
cd fpm-bot
chicken-install -s
```

# Usage

You can configure fpm-bot with environment variables. Here is the full list:

| Name              | Description                                                 | Example value                                  |
| ---               | ---                                                         | ---                                            |
| BOT_TOKEN         | Telegram's API authentication token.                        | `110201543:AAHdqTcvCH1vGWJxfSeofSAs0K5PALDsaw` |
| QUOTES_FILE       | Path of a file that contains quotes, one per line.          | `quotes.txt`                                   |
| QUOTES_CHANNEL_ID | ID of a channel with quotes.                                | `-1887588581`                                  |
| DATABASE_URI      | filename of sqlite3 database where bot stores various info. | `messages.db`                                  |
| PORT              | Port on which webhook reciever listens.                     | `8080`                                         |
| PRODUCTION        | Set this variable to disable sensitive logging info.        | `1`                                            |

```sh 
export BOT_TOKEN='CHANGEME'
export QUOTES_FILE='CHANGEME'
export QUOTES_CHANNEL_ID='CHANGEME'
export DATABASE_URI='CHANGEME'
export PORT='CHANGEME'
export PRODUCTION='CHANGEME'
./fpm-bot
```
