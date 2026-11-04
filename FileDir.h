#ifndef FILEDIR_H_INCLUDED
#define FILEDIR_H_INCLUDED
#include <stdint.h>
#include <stdbool.h>
#include "FileSystem.h"
#include "Kernel.h"

#define IndexSize (SectorSize / 4)
#define HeaderSize sizeof(FileHeaderData)
#define DirPgSize ((SectorSize - sizeof(DirPageHeader)) / sizeof(DirEntry) / 4*4)
#define DirMark    (0x9B1EA38D)
#define HeaderMark (0x9BA71D86)
#define FSMark     (0x97A1EAD9)

/*
    EntryHandler*   = PROCEDURE (name: FileName; sec: DiskAdr; date, size: INTEGER; VAR continue: BOOLEAN);
*/

typedef void (*EntryHandler) (FileName name, int sec, int date, int size, bool *Continue, void *callerData);

/*
    FileHeader* = RECORD(FileSystem.FileHeaderData)
      fill: ARRAY SectorSize - HeaderSize OF BYTE
    END;

    FileHd* = POINTER TO FileHeader;
*/
typedef struct {
    FileHeaderData b;
    uint8_t fill[SectorSize - HeaderSize];
} FileHeader, *FileHd;

/*
    IndexSector* = ARRAY IndexSize OF DiskAdr;
    DataSector* = ARRAY SectorSize OF BYTE;
*/

typedef DiskAdr IndexSector[IndexSize];
typedef uint8_t DataSector[SectorSize];

/*
    DirPage*  = RECORD(FileSystem.DirPageData)
      fill:  ARRAY SectorSize - SYSTEM.SIZE(FileSystem.DirPageData) OF BYTE;
    END ;
*/

typedef struct {
    DirPageData b;
    uint8_t fill[SectorSize - sizeof(DirPageData)];
} DirPage;

void FileDir_Init(FSHeader *fshd, int fssec);
void Enumerate(const FSHeader *fshd, char *prefix, EntryHandler proc, void *callerData);
void Search(const FSHeader *fshd, const char *name, int *A);
void Insert(FSHeader *fshd, const FileName name, int fad, int date, int size);
void FileDir_Delete(FSHeader *fshd, const FileName name, int *fad);

#endif // FILEDIR_H_INCLUDED
