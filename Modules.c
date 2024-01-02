#include "Modules.h"
#include "Files.h"

int Modules_OberonFS_fd;
int Modules_OberonFS_offset;

void Modules_Set_OberonFS_image(int fd, int offset)
{
    Modules_OberonFS_fd = fd;
    Modules_OberonFS_offset = offset;
}

void Modules_Init(void)
{
/**
  PROCEDURE Init*;
  BEGIN Files.Init; MTOrg := SYSTEM.REG(MT);
    SYSTEM.GET(16, AllocPtr); SYSTEM.GET(20, root); SYSTEM.GET(24, limit); DEC(limit, 8000H)
  END Init;
 */
    Files_Init();
}
