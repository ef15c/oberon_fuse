#ifndef FILEDIR_H_INCLUDED
#define FILEDIR_H_INCLUDED
#include <stdint.h>
#include <stdbool.h>

#define FileDir_FnLength (32)
#define FileDir_SecTabSize (64)
#define FileDir_ExTabSize (12)
#define FileDir_SectorSize (1024)
#define FileDir_IndexSize (FileDir_SectorSize / 4)
#define FileDir_HeaderSize (352)
#define FileDir_DirRootAdr (29)
#define FileDir_DirPgSize (24)
#define FileDir_DirMark    (0x9B1EA38D)
#define FileDir_HeaderMark (0x9BA71D86)
#define FileDir_FillerSize (52)

typedef char FileDir_FileName[FileDir_FnLength];
typedef int  FileDir_ExtensionTable[FileDir_ExTabSize];
typedef int  FileDir_SectorTable[FileDir_SecTabSize];
typedef int  FileDir_IndexSector[FileDir_IndexSize];
typedef uint8_t FileDir_DataSector[FileDir_SectorSize];

typedef struct {
    /*first page of each file on disk*/
    int mark;
    FileDir_FileName name;
    int aleng, bleng, date;
    FileDir_ExtensionTable ext;
    FileDir_SectorTable sec;
    uint8_t fill[FileDir_SectorSize - FileDir_HeaderSize];
} FileDir_FileHeader;

typedef FileDir_FileHeader *FileDir_FileHd;

typedef struct {
    FileDir_FileName name;
    int adr; /*sec no of file header*/
    int p;   /*sec no of descendant in directory*/
} FileDir_DirEntry; /*B-tree node*/

typedef struct {
    int mark;
    int m;
    int p0;  /*sec no of left descendant in directory*/
    uint8_t fill[FileDir_FillerSize];
    FileDir_DirEntry e[FileDir_DirPgSize];
} FileDir_DirPage;

typedef void (*FileDir_EntryHandler) (FileDir_FileName name, int sec, bool *Continue, void *callerData);

void FileDir_Init(void);
void FileDir_Enumerate(char *prefix, FileDir_EntryHandler proc, void *callerData);
void FileDir_Search(const char *name, int *A);
void FileDir_Insert(const FileDir_FileName name, int fad);
void FileDir_Delete(const FileDir_FileName name, int *fad);

#endif // FILEDIR_H_INCLUDED
