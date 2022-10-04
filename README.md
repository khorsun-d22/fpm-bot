# FPM bot

Just a bot.

## Installation

### Ubuntu

```sh
# Install system dependencies
sudo apt-get install chicken-bin libssl-dev

# Install dependencies
sudo chicken-install

# Run
export BOT_TOKEN=abc12347
./fpm-bot
```

### Nix

```sh
# Enter shell with all dependencies
nix develop

# Build
chicken-install

# Run
export BOT_TOKEN=abc12347
./fpm-bot
```
