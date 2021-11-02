library(tidyverse)
library(telegram.bot)

options(scipen = 999)

bot <- Bot(token = Sys.getenv("TELEGRAM_HILLTOP"))

print(bot$getMe())

updates <- bot$getUpdates()

updates[[27]]$from_chat_id()

bot$sendMessage(chat_id = -1001768573848, text = "Southern")
