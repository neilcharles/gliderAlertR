library(tidyverse)
library(telegram.bot)

bot <- Bot(token = Sys.getenv("TELEGRAM_HILLTOP"))

print(bot$getMe())

updates <- bot$getUpdates()

updates[[5]]$from_chat_id()

bot$sendMessage(chat_id = 96373076, text = "Hello Neil")
