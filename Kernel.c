#include <time.h>

#include "Kernel.h"
int Clock(void)
{
/**
  PROCEDURE Clock*(): INTEGER;
  BEGIN RETURN clock
  END Clock;
*/
  struct tm *t;
  time_t now;

  now = time(NULL);
  t = localtime(&now);

  return
    (((t->tm_year-100) % 0x40) * 0x4000000) /* Year */ +
    (((t->tm_mon+1) % 0x10) * 0x400000) /* Month */ +
    (((t->tm_mday) % 0x20) * 0x20000) /* Day of month */ +
    (((t->tm_hour) % 0x20) * 0x1000) /* Hours */ +
    (((t->tm_min) % 0x40) * 0x40) /* Minutes */ +
    (((t->tm_sec) % 0x40)) /* Seconds */ ;
}
