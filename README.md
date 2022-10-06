Just a bot.

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

| Name              | Description                                        | Example value                                  |
| ---               | ---                                                | ---                                            |
| BOT_TOKEN         | Telegram's API authentication token.               | `110201543:AAHdqTcvCH1vGWJxfSeofSAs0K5PALDsaw` |
| QUOTES_FILE       | Path of a file that contains quotes, one per line. | `quotes.txt`                                   |
| QUOTES_CHANNEL_ID | ID of a channel with quotes.                       | `-1887588581`                                  |

```sh 
export BOT_TOKEN='CHANGEME'
export QUOTES_FILE='CHANGEME'
export QUOTES_CHANNEL_ID='CHANGEME'
./fpm-bot
```
