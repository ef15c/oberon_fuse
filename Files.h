#ifndef FILES_H_INCLUDED
#define FILES_H_INCLUDED
#include <stdbool.h>

typedef struct FILES_FILEDESC *Files_File;
typedef struct FILES_BUFFERRECORD *Files_Buffer;

typedef struct {
    bool eof;
    int res;
    Files_File file;
    int apos, bpos;
    Files_Buffer buf;
} Files_Rider;

void Files_Init(void);
Files_File Files_Old(const char *name);
Files_File Files_New(const char *name);
void Files_Set(Files_Rider *r, Files_File f, int pos);
void Files_Read(Files_Rider *r, char *ch);
void Files_Write(Files_Rider *r, const char ch);
void Files_Register(Files_File f);
void Files_Close(Files_File f);
void Files_RestoreList(void);
Files_File Files_Base(Files_Rider *r);
void Files_Delete(const char *name, int *res);
void Files_Rename(const char *old, const char *New, int *res);
void Files_Truncate(Files_File f, int length, int *res);
int Files_Length(Files_File f);

/* Not in Oberon original code : helps to count files references */
void Files_Unset(Files_Rider *r);
void Files_Reset(Files_Rider *r, int pos);

#endif // FILES_H_INCLUDED
