/*
MODULE FileSystem; (*CS 17.10.26*)
  (*File system Types and low level procedures*)
  IMPORT SYSTEM, SDCard, Kernel;
*/
#include <assert.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include "Kernel.h"
#include "Modules.h"
#include "FileSystem.h"

/* ---------- Disk storage management ----------*/

/*
  PROCEDURE MarkSector*(VAR fshd: FSHeader; sec29: DiskAdr);
    VAR sec: INTEGER;
  BEGIN sec := sec29 DIV 29; ASSERT(sec29 = sec*29);
    INCL(fshd.sector.map[sec DIV 32], sec MOD 32); INC(fshd.NofSectors);
  END MarkSector;
*/

void MarkSector(FSHeader *fshd, DiskAdr sec29)
{   int sec;

    sec = sec29 / 29; assert (sec29 == sec*29);
    fshd->sector->map[sec / bpi] |= 1 << (sec % bpi);
    fshd->NofUsedSectors++;
}

/*
  PROCEDURE InitSecMap*(VAR fshd: FSHeader);
    CONST NofReservedSectors = (Kernel.BootReservedSize + SectorSize - 1) DIV SectorSize + 2;
    VAR i, r: INTEGER;
  BEGIN NEW(fshd.sector); fshd.NofSectors := 0; i := 0;
    WHILE i < NofReservedSectors DIV 32 DO fshd.sector.map[i] := {0 .. 31}; INC(i) END;
    r := NofReservedSectors MOD 32; IF r > 0 THEN fshd.sector.map[i] := {0 .. r-1}; INC(i) END;
    WHILE i < NofSectors DIV 32 DO fshd.sector.map[i] := {}; INC(i) END
  END InitSecMap;
*/

void InitSecMap(FSHeader *fshd)
#define NofReservedSectors ((BootReservedSize + SectorSize - 1) / SectorSize + 2)
{   int i, r;

    fshd->sector = malloc(sizeof *(fshd->sector)); assert(fshd->sector != NULL);
    fshd->NofUsedSectors = 0; i = 0;
    while (i < NofReservedSectors / bpi) { fshd->sector->map[i] = -1; i++; };
    r = NofReservedSectors % bpi; if (r > 0) { fshd->sector->map[i] = (1 << r)-1; i++; };
    while (i < NofSectors / bpi) { fshd->sector->map[i] = 0; i++; }
}

/*
  PROCEDURE GetSector*(fshd: FSHeader; src29: INTEGER; VAR dst: Sector);
    VAR src: INTEGER;
  BEGIN src := src29 DIV 29; ASSERT(src29 = src*29);
    src := src * (SectorSize DIV SDCard.BLOCKSIZE) + fshd.FS_OFFSET;
    SDCard.Read(src, SYSTEM.ADR(dst), SectorSize DIV SDCard.BLOCKSIZE)
  END GetSector;
*/

void GetSector(const FSHeader *fshd, DiskAdr sec29, Sector dst)
{   int sec;
    off_t pos;
    ssize_t nbRead;

    sec = sec29 / 29; assert(sec29 == sec*29);
    pos = lseek(OberonFS_fd, sec*SectorSize + fshd->FS_OFFSET, SEEK_SET);
    assert (pos >= 0);
    nbRead = read(OberonFS_fd, dst, SectorSize);
    assert (nbRead == SectorSize);
}

/*
  PROCEDURE FreeSector*(VAR fshd: FSHeader; sec29: INTEGER);
    VAR sec: INTEGER;
  BEGIN sec := sec29 DIV 29; ASSERT(sec29 = sec*29);
    EXCL(fshd.sector.map[sec DIV 32], sec MOD 32); DEC(fshd.NofSectors)
  END FreeSector;
*/

void FreeSector(FSHeader *fshd, DiskAdr sec29)
{   int sec;

    sec = sec29 / 29; assert(sec29 == sec*29);
    fshd->sector->map[sec / bpi] &= ~(1 << (sec % bpi)); fshd->NofUsedSectors--;
}

/*
  PROCEDURE AllocSector*(VAR fshd: FSHeader; hint29: INTEGER; VAR sec29: INTEGER);
    VAR hint, s: INTEGER;
  BEGIN (*find free sector, starting after hint*)
    hint := hint29 DIV 29; ASSERT(hint29 = hint*29); s := hint;
    REPEAT INC(s); IF s >= NofSectors THEN s := 1 END
    UNTIL ~(s MOD 32 IN fshd.sector.map[s DIV 32]) OR (s = hint);
    ASSERT (~(s MOD 32 IN fshd.sector.map[s DIV 32]));
    INCL(fshd.sector.map[s DIV 32], s MOD 32); INC(fshd.NofSectors); sec29 := s * 29
  END AllocSector;
*/

void AllocSector(FSHeader *fshd, DiskAdr hint29, DiskAdr *sec29)
{   int hint, s;
    /*find free sector, starting after hint*/
    hint = hint29 / 29; assert(hint29 == hint*29); s = hint;
    do { s++;
        if (s >= NofSectors) s = 1;
    } while (((1 << (s % bpi)) & fshd->sector->map[s / bpi]) && (s != hint));
    assert(!((1 << (s % bpi)) & fshd->sector->map[s / bpi]));
    fshd->sector->map[s / bpi] |= 1 << (s % bpi); fshd->NofUsedSectors++;
    *sec29 = s*29;
}

/*
  PROCEDURE PutSector*(fshd: FSHeader; dst29: INTEGER; VAR src: Sector);
    VAR dst: INTEGER;
  BEGIN dst := dst29 DIV 29; ASSERT(dst29 = dst*29);
    dst := dst * (SectorSize DIV SDCard.BLOCKSIZE) + fshd.FS_OFFSET;
    SDCard.Write(dst, SYSTEM.ADR(src), SectorSize DIV SDCard.BLOCKSIZE);
  END PutSector;
*/
void PutSector(FSHeader *fshd, DiskAdr dst29, Sector src)
{   int dst;
    off_t pos;
    ssize_t nbWriten;

    dst = dst29 / 29; assert(dst29 == dst*29);
    pos = lseek(OberonFS_fd, dst*SectorSize + fshd->FS_OFFSET, SEEK_SET);
    assert (pos >= 0);
    nbWriten = write(OberonFS_fd, src, SectorSize);
    assert(nbWriten == SectorSize);
}

/*
END FileSystem.
*/

