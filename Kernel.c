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
  PROCEDURE GetSector*(src29: INTEGER; VAR dst: Sector);
    VAR src: INTEGER;
  BEGIN src := src29 DIV 29; ASSERT(src29 = src*29);
    src := src * (SectorLength DIV SDBlockSize) + FSoffset;
    ReadSD(src, SYSTEM.ADR(dst), SectorLength DIV SDBlockSize)
  END GetSector;
*/
    off_t pos;
    ssize_t nbRead;

    assert (src % 29 == 0); src /= 29;
    pos = lseek(Modules_OberonFS_fd, src*Kernel_SectorLength + Modules_OberonFS_offset, SEEK_SET);
    assert (pos >= 0);
    nbRead = read(Modules_OberonFS_fd, dst, Kernel_SectorLength);
    assert (nbRead == Kernel_SectorLength);
}

void Kernel_PutSector(int dst, Kernel_Sector src)
{
/**
  PROCEDURE PutSector*(dst29: INTEGER; VAR src: Sector);
    VAR dst: INTEGER;
  BEGIN dst := dst29 DIV 29; ASSERT(dst29 = dst*29);
    dst := dst * (SectorLength DIV SDBlockSize) + FSoffset;
    WriteSD(dst, SYSTEM.ADR(src), SectorLength DIV SDBlockSize);
  END PutSector;
*/
    off_t pos;
    ssize_t nbWriten;

    assert(dst % 29 == 0); dst /= 29;
    pos = lseek(Modules_OberonFS_fd, dst*Kernel_SectorLength + Modules_OberonFS_offset, SEEK_SET);
    assert (pos >= 0);
    nbWriten = write(Modules_OberonFS_fd, src, Kernel_SectorLength);
    assert(nbWriten == Kernel_SectorLength);
}

void Kernel_InitSecMap(void)
{
/**
  PROCEDURE InitSecMap*;
    CONST NofReservedSectors = (BootReservedSize + SectorLength - 1) DIV SectorLength + 2;
    VAR i, r: INTEGER;
  BEGIN NofSectors := 0; i := 0;
    WHILE i < NofReservedSectors DIV 32 DO sectorMap[i] := {0 .. 31}; INC(i) END;
    r := NofReservedSectors MOD 32; IF r > 0 THEN sectorMap[i] := {0 .. r}; INC(i) END;
    WHILE i < mapsize DIV 32 DO sectorMap[i] := {}; INC(i) END
  END InitSecMap;
*/
    const int NofReservedSectors = (BootReservedSize + Kernel_SectorLength - 1) / Kernel_SectorLength + 2;
    int i, r;

    Kernel_NofSectors = 0; i = 0;
    while (i < NofReservedSectors / 32) { sectorMap[i] = 0xFFFFFFFF; i++; }
    r = NofReservedSectors % 32; if (r > 0) { sectorMap[i] = (1 << r) - 1; i++; }
    while (i < mapsize / 32) { sectorMap[i] = 0; i++; }
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
