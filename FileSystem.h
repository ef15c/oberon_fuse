#ifndef FILESYSTEM_H_INCLUDED
#define FILESYSTEM_H_INCLUDED
#include <stdint.h>

/*
  CONST
    SectorSize*   = 2048;
    NofSectors* = 10000H; (*64K sectors, 64* 1024*SectorSize bytes*)
    FnLength*    = 32;
    SecTabSize*   = 64;
    ExTabSize*   = 12;
    DirRootAdr*  = 29;
*/

#define SectorSize (2048)
/*64K sectors, 64* 1024*SectorSize bytes*/
#define NofSectors (0x10000)
#define FnLength (32)
#define SecTabSize (64)
#define ExTabSize (12)
#define DirRootAdr (29)

#define bpi (sizeof(unsigned int)*8)

/*
  TYPE
    Sector* = ARRAY SectorSize OF BYTE;
*/
typedef uint8_t Sector[SectorSize];

/*
  (*filesystem*)
    StorageInfo = POINTER TO StorageInfoDesc;
    StorageInfoDesc = RECORD
      map: ARRAY NofSectors DIV 32 OF SET
    END;

    FSHeaderPersistent* = RECORD
      mark*: INTEGER;
      dummy: INTEGER;
      version*: BYTE
    END;

    FSHeader* = RECORD(FSHeaderPersistent)
      FS_OFFSET*: INTEGER;
      sector: StorageInfo;
      NofSectors*: INTEGER;
    END;

    FSHeaderPage* = RECORD(FSHeader)
      fill: ARRAY SectorSize - SYSTEM.SIZE(FSHeader) OF BYTE
    END;

    FSHdP* = POINTER TO FSHeaderPage;
*/
typedef struct {
    unsigned int map[NofSectors / bpi];
} StorageInfoDesc, *StorageInfo;

typedef struct {
  int32_t mark;
  int32_t dummy;
  uint8_t version;
} FSHeaderPersistent;

typedef struct {
  FSHeaderPersistent b;
  int FS_OFFSET;
  StorageInfo sector;
  int NofUsedSectors;
} FSHeader;

typedef struct {
  FSHeader b;
  uint8_t fill[SectorSize - sizeof(FSHeader)];
} FSHeaderPage, *FSHdP;

/*
  (*file*)
    DiskAdr*      = INTEGER;
    FileName*       = ARRAY FnLength OF CHAR;
    SectorTable*    = ARRAY SecTabSize OF DiskAdr;
    ExtensionTable* = ARRAY ExTabSize OF DiskAdr;

    FileHeaderData* =
      RECORD (*first page of each file on disk*)
        mark*: INTEGER;
        name*: FileName;
        aleng*, bleng*, date*: INTEGER;
        ext*:  ExtensionTable;
        sec*: SectorTable;
      END ;
*/
typedef int32_t DiskAdr;
typedef char FileName[FnLength];
typedef DiskAdr SectorTable[SecTabSize];
typedef DiskAdr ExtensionTable[ExTabSize];

typedef struct {
  /*first page of each file on disk*/
  int32_t mark;
  FileName name;
  int32_t aleng, bleng, date;
  ExtensionTable ext;
  SectorTable sec;
} FileHeaderData;

/*
 (*directory*)
    DirEntry* =  (*B-tree node*)
      RECORD
        name*: FileName;
        adr*: DiskAdr; (*sec no of file header*)
        date*, size*: INTEGER;
        p*:    DiskAdr  (*sec no of descendant in directory*)
      END ;

    DirPageHeader* = RECORD
      mark*:  INTEGER;
      m*:     INTEGER;
      p0*:    DiskAdr  (*sec no of left descendant in directory*)
    END;

    DirPageData* = RECORD(DirPageHeader)
      e*:  ARRAY (SectorSize - SYSTEM.SIZE(DirPageHeader)) DIV  SYSTEM.SIZE(DirEntry) DIV 4*4 OF DirEntry
    END;
*/

typedef struct { /*B-tree node*/
  FileName name;
  DiskAdr adr; /*sec no of file header*/
  int32_t date, size;
  DiskAdr p; /*sec no of descendant in directory*/
} DirEntry;

typedef struct {
  int32_t mark;
  int32_t m;
  DiskAdr p0; /*sec no of left descendant in directory*/
} DirPageHeader;

typedef struct {
  DirPageHeader b;
  DirEntry e[(SectorSize - sizeof(DirPageHeader)) / sizeof (DirEntry) / 4*4];
} DirPageData;

/*
  PROCEDURE MarkSector*(VAR fshd: FSHeader; sec29: INTEGER);
  PROCEDURE InitSecMap*(VAR fshd: FSHeader);
  PROCEDURE GetSector*(fshd: FSHeader; sec29: INTEGER; VAR dst: Sector);
  PROCEDURE FreeSector*(VAR fshd: FSHeader; sec29: INTEGER);
  PROCEDURE AllocSector*(VAR fshd: FSHeader; hint29: INTEGER; VAR sec29: INTEGER);
  PROCEDURE PutSector*(fshd: FSHeader; dst29: INTEGER; VAR src: Sector);
*/
void MarkSector(FSHeader *fshd, DiskAdr sec29);
void InitSecMap(FSHeader *fshd);
void GetSector(const FSHeader *fshd, DiskAdr sec29, Sector dst);
void FreeSector(FSHeader *fshd, DiskAdr sec29);
void AllocSector(FSHeader *fshd, DiskAdr hint29, DiskAdr *sec29);
void PutSector(FSHeader *fshd, DiskAdr dst29, Sector src);

#endif // FILESYSTEM_H_INCLUDED
