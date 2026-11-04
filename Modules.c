#include "Modules.h"
#include "Files.h"

int OberonFS_fd;

void Set_OberonFS_image(int fd)
{
    OberonFS_fd = fd;
}

void Modules_Init(int fssec)
{
/**
  PROCEDURE Init*;
  BEGIN Files.Init; MTOrg := SYSTEM.REG(MT);
    SYSTEM.GET(16, AllocPtr); SYSTEM.GET(20, root); SYSTEM.GET(24, limit); DEC(limit, 8000H)
  END Init;
 */
    Files_Init(fssec);
}
