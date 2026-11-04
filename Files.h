#ifndef FILES_H_INCLUDED
#define FILES_H_INCLUDED
#include <stdbool.h>
#include "FileDir.h"

typedef struct FILES_FILEDESC *File;
typedef struct FILES_BUFFERRECORD *Buffer;

typedef struct {
    bool eof;
    int res;
    File file;
    int apos, bpos;
    Buffer buf;
} Rider;

extern FSHeader fshd; /*filesystem instance*/

void Files_Init(int fssec);
File OldByAdr(DiskAdr header);
File Old(const char *name);
File New(const char *name);
void Set(Rider *r, File f, int pos);
void Read(Rider *r, char *ch);
void Write(Rider *r, const char ch);
void Register(File f);
void Close(File f);
void RestoreList(void);
File Base(Rider *r);
void Delete(const char *name, int *res);
void Rename(const char *old, const char *New, int *res);
int Length(File f);

/* Not in Oberon original code */
void Truncate(File f, int length, int *res);
/* helps to count files references */
void Unset(Rider *r);
void Reset(Rider *r, int pos);

#endif // FILES_H_INCLUDED
