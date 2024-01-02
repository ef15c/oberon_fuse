#include <stdint.h>
#include <assert.h>
#include <sys/types.h>
#include <unistd.h>
#include <time.h>

#include "Kernel.h"
#include "Modules.h"

#define mapsize 0x10000

int Kernel_NofSectors;
uint32_t sectorMap[mapsize / 32];

void Kernel_MarkSector(int sec)
{
/**
  PROCEDURE MarkSector*(sec: INTEGER);
  BEGIN sec := sec DIV 29; ASSERT(SYSTEM.H(0) = 0);
    INCL(sectorMap[sec DIV 32], sec MOD 32); INC(NofSectors)
  END MarkSector;
*/
    assert (sec % 29 == 0); sec /= 29;
    sectorMap[sec / 32] |= 1 << (sec % 32); Kernel_NofSectors++;
}

void Kernel_AllocSector(int hint, int *sec)
{
/**
  PROCEDURE AllocSector*(hint: INTEGER; VAR sec: INTEGER);
    VAR s: INTEGER;
  BEGIN (*find free sector, starting after hint*)
    hint := hint DIV 29; ASSERT(SYSTEM.H(0) = 0); s := hint;
    REPEAT INC(s);
      IF s = mapsize THEN s := 1 END ;
    UNTIL ~(s MOD 32 IN sectorMap[s DIV 32]);
    INCL(sectorMap[s DIV 32], s MOD 32); INC(NofSectors); sec := s * 29
  END AllocSector;
*/
    int s;

    /*find free sector, starting after hint*/
    assert(hint % 29 == 0); hint /= 29; s = hint;
    do { s++;
        if (s == mapsize) s = 1;
    } while ((1 << (s % 32)) & sectorMap[s / 32]);
    sectorMap[s / 32] |= 1 << (s % 32); Kernel_NofSectors++; *sec = s * 29;
}

void Kernel_FreeSector(int sec)
{
/**
  PROCEDURE FreeSector*(sec: INTEGER);
  BEGIN sec := sec DIV 29; ASSERT(SYSTEM.H(0) = 0);
    EXCL(sectorMap[sec DIV 32], sec MOD 32); DEC(NofSectors)
  END FreeSector;
*/
    assert(sec % 29 == 0); sec /= 29;
    sectorMap[sec / 32] &= ~(1 << (sec % 32)); Kernel_NofSectors--;
}

void Kernel_GetSector(int src, Kernel_Sector dst)
{
/**
  PROCEDURE GetSector*(src: INTEGER; VAR dst: Sector);
  BEGIN src := src DIV 29; ASSERT(SYSTEM.H(0) = 0);
    src := src * 2 + FSoffset;
    ReadSD(src, SYSTEM.ADR(dst)); ReadSD(src+1, SYSTEM.ADR(dst)+512)
  END GetSector;
*/
    off_t pos;
    ssize_t nbRead;

    assert (src % 29 == 0); src /= 29;
    pos = lseek(Modules_OberonFS_fd, src*1024 + Modules_OberonFS_offset, SEEK_SET);
    assert (pos >= 0);
    nbRead = read(Modules_OberonFS_fd, dst, 1024);
    assert (nbRead == 1024);
}

void Kernel_PutSector(int dst, Kernel_Sector src)
{
/**
  PROCEDURE PutSector*(dst: INTEGER; VAR src: Sector);
  BEGIN dst := dst DIV 29; ASSERT(SYSTEM.H(0) =  0);
    dst := dst * 2 + FSoffset;
    WriteSD(dst, SYSTEM.ADR(src)); WriteSD(dst+1, SYSTEM.ADR(src)+512)
  END PutSector;
*/
    off_t pos;
    ssize_t nbWriten;

    assert(dst % 29 == 0); dst /= 29;
    pos = lseek(Modules_OberonFS_fd, dst*1024 + Modules_OberonFS_offset, SEEK_SET);
    assert (pos >= 0);
    nbWriten = write(Modules_OberonFS_fd, src, 1024);
    assert(nbWriten == 1024);
}

void Kernel_InitSecMap(void)
{
/**
  PROCEDURE InitSecMap*;
    VAR i: INTEGER;
  BEGIN NofSectors := 0; sectorMap[0] := {0 .. 31}; sectorMap[1] := {0 .. 31};
    FOR i := 2 TO mapsize DIV 32 - 1 DO sectorMap[i] := {} END
  END InitSecMap;
*/
    int i;

    Kernel_NofSectors = 0; sectorMap[0] = 0xFFFFFFFF; sectorMap[1] = 0xFFFFFFFF;
    for (i = 2; i <= mapsize / 32 - 1; i++) sectorMap[i] = 0;
}

int Kernel_Clock(void)
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

void Kernel_Init(void)
{
/**
  PROCEDURE Init*;
  BEGIN Install(SYSTEM.ADR(Trap), 20H);  (*install temporary trap*)
    SYSTEM.GET(12, MemLim); SYSTEM.GET(24, heapOrg);
    stackOrg := heapOrg; stackSize := 8000H; heapLim := MemLim;
    list1 := 0; list2 := 0; list3 := 0; list0 := heapOrg;
    SYSTEM.PUT(list0, heapLim - heapOrg); SYSTEM.PUT(list0+4, -1); SYSTEM.PUT(list0+8, 0);
    allocated := 0; clock := 0; InitSecMap
  END Init;
*/
    Kernel_InitSecMap();
}
