telef <- function(){

  library(tidyverse)
  library(telegram.bot)
  
  options(scipen = 999)
  
  bot <- Bot(token = Sys.getenv("TELEGRAM_HILLTOP"))
  
  print(bot$getMe())
  
  updates <- bot$getUpdates()
  
  updates[[4]]$from_chat_id()
  
  bot$sendMessage(chat_id = 96373076, text = "Summary\n(flying|waiting|gone xc|local max alt)\nDevils Dyke:  3|4|0|800'\nDunstable:  1|2|0|650'")
  
  
  bot$sendMessage(chat_id = 96373076, parse_mode = 'HTML', text = glue("Summary at {format(now(), '%H:%M')}\n<i>flying</i>|<i>waiting</i>|<i>gone xc</i>|<i>avg</i>|<i>max</i>\n\nDevils Dyke 2|6|1|500'|1500'\nDunstable 2|6|1|500'|1500'"))
  
  bot$sendMessage(chat_id = 96373076, text = "No you can't have a phone Evan")
}