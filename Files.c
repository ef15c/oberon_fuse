#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "Files.h"
#include "Kernel.h"
#include "FileDir.h"

#define MaxBufs (4)
#define HS  (FileDir_HeaderSize)
#define SS (FileDir_SectorSize)
#define STS (FileDir_SecTabSize)
#define XS (FileDir_IndexSize)

typedef int DiskAdr;

typedef struct {
    DiskAdr adr;
    bool mod;
    FileDir_IndexSector sec;
} IndexRecord;

typedef IndexRecord *Index;

typedef struct FILES_BUFFERRECORD {
    int apos, lim;
    bool mod;
    Files_Buffer next;
    FileDir_DataSector data;
} BufferRecord;


typedef struct FILES_FILEDESC {
    uint64_t next; /*list of files invisible to the GC*/
    int nofbufs, aleng, bleng;
    bool modH, registered;
    Files_Buffer firstbuf;
    DiskAdr sechint;
    FileDir_FileName name;
    int date;
    Index ext[FileDir_ExTabSize];
    FileDir_SectorTable sec;
    int noRiders;
} FileDesc;

static uint64_t root /*File*/;  /*list of open files*/

static void Check(const char *s, FileDir_FileName name, int *res)
{
/**
  PROCEDURE Check(s: ARRAY OF CHAR;
        VAR name: FileDir.FileName; VAR res: INTEGER);
    VAR i: INTEGER; ch: CHAR;
  BEGIN ch := s[0]; i := 0;
    IF (ch >= "A") & (ch <= "Z") OR (ch >= "a") & (ch <= "z") THEN
      REPEAT name[i] := ch; INC(i); ch := s[i]
      UNTIL ~((ch >= "0") & (ch <= "9") OR (ch >= "A") & (ch <= "Z")
        OR (ch >= "a") & (ch <= "z") OR (ch = ".")) OR (i = FileDir.FnLength);
      IF i = FileDir.FnLength THEN res := 4
      ELSIF ch = 0X THEN res := 0;
        WHILE i < FileDir.FnLength DO name[i] := 0X; INC(i) END
      ELSE res := 5
      END
    ELSIF ch = 0X THEN name[0] := 0X; res := -1
    ELSE res := 3
    END
  END Check;
*/
    int i; char ch;

    ch = s[0]; i = 0;
    if ((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z')) {
        do { name[i] = ch; i++; ch = s[i]; }
        while (!(!((ch >= '0' && ch <= '9') || (ch >= 'A' && ch <= 'Z')
        || (ch >= 'a' && ch <= 'z') || ch == '.') || i == FileDir_FnLength ));
        if (i == FileDir_FnLength) *res = 4;
        else if (ch == 0) { *res = 0;
            while (i < FileDir_FnLength) { name[i] = 0; i++; }
        } else *res = 5;
    } else if (ch == 0) { name[0] = 0; *res = -1;
    } else *res = 3;
}

Files_File Files_Old(const char *name)
{
/**
  PROCEDURE Old*(name: ARRAY OF CHAR): File;
    VAR i, k, res: INTEGER;
      f: File;
      header: DiskAdr;
      buf: Buffer;
      F: FileDir.FileHd;
      namebuf: FileDir.FileName;
      inxpg: Index;
  BEGIN f := NIL; Check(name, namebuf, res);
    IF res = 0 THEN
      FileDir.Search(namebuf, header);
      IF header # 0 THEN
        f := SYSTEM.VAL(File, root);
        WHILE (f # NIL) & (f.sec[0] # header) DO f := SYSTEM.VAL(File, f.next) END ;
        IF f = NIL THEN (*file not yet present*)
          NEW(buf); buf.apos := 0; buf.next := buf; buf.mod := FALSE;
          F := SYSTEM.VAL(FileDir.FileHd, SYSTEM.ADR(buf.data));
          Kernel.GetSector(header, buf.data); ASSERT(F.mark = FileDir.HeaderMark);
          NEW(f); f.aleng := F.aleng; f.bleng := F.bleng; f.date := F.date;
          IF f.aleng = 0 THEN buf.lim := f.bleng ELSE buf.lim := SS END ;
          f.firstbuf := buf; f.nofbufs := 1; f.name := namebuf; f.registered := TRUE;
          f.sec := F.sec;
          k := (f.aleng + (XS-STS)) DIV XS; i := 0;
          WHILE i < k DO
            NEW(inxpg); inxpg.adr := F.ext[i]; inxpg.mod := FALSE;
            Kernel.GetSector(inxpg.adr, inxpg.sec); f.ext[i] := inxpg; INC(i)
          END ;
          WHILE i < FileDir.ExTabSize DO f.ext[i] := NIL; INC(i) END ;
          f.sechint := header; f.modH := FALSE; f.next := root; root := SYSTEM.VAL(INTEGER, f)
        END
      END
    END ;
    RETURN f
  END Old;
*/
    int i, k, res;
    Files_File f;
    DiskAdr header;
    Files_Buffer buf;
    FileDir_FileHd F;
    FileDir_FileName namebuf;
    Index inxpg;

    f = NULL; Check(name, namebuf, &res);
    if (res == 0) {
        FileDir_Search(namebuf, &header);
        if (header != 0) {
            f = (Files_File) root;
            while (f != NULL && f->sec[0] != header) { f = (Files_File) f->next; }
            if (f == NULL) { /*file not yet present*/
                buf = malloc(sizeof *buf); assert(buf); buf->apos = 0; buf->next = buf; buf->mod = false;
                F = (FileDir_FileHd) &buf->data;
                Kernel_GetSector(header, (uint8_t *) buf->data); assert(F->mark == FileDir_HeaderMark);
                f = malloc(sizeof *f); assert(f); f->aleng = F->aleng; f->bleng = F->bleng; f->date = F->date;
                f->noRiders = 0;
                if (f->aleng == 0) buf->lim = f->bleng; else buf->lim = SS;
                f->firstbuf = buf; f->nofbufs = 1; memcpy(f->name, namebuf, sizeof f->name); f->registered = true;
                memcpy(f->sec, F->sec, sizeof f->sec);
                k = (f->aleng + (XS-STS)) / XS; i = 0;
                while (i < k) {
                    inxpg = malloc(sizeof *inxpg); assert(inxpg); inxpg->adr = F->ext[i]; inxpg->mod = false;
                    Kernel_GetSector(inxpg->adr, (uint8_t *) inxpg->sec); f->ext[i] = inxpg; i++;
                }
                while (i < FileDir_ExTabSize) { f->ext[i] = NULL; i++; }
                f->sechint = header; f->modH = false; f->next = root; root = (uint64_t) f;
            }
        }
    }
    return f;
}

Files_File Files_New(const char *name)
{
/**
  PROCEDURE New*(name: ARRAY OF CHAR): File;
    VAR i, res: INTEGER;
      f: File;
      buf: Buffer;
      F: FileDir.FileHd;
      namebuf: FileDir.FileName;
  BEGIN f := NIL; Check(name, namebuf, res);
    IF res <= 0 THEN
      NEW(buf); buf.apos := 0; buf.mod := TRUE; buf.lim := HS; buf.next := buf;
      F := SYSTEM.VAL(FileDir.FileHd, SYSTEM.ADR(buf.data));
      F.mark := FileDir.HeaderMark;
      F.aleng := 0; F.bleng := HS; F.name := namebuf;
      F.date := Kernel.Clock();
      NEW(f); f.aleng := 0; f.bleng := HS; f.modH := TRUE;
      f.registered := FALSE; f.date := F.date;
      f.firstbuf := buf; f.nofbufs := 1; f.name := namebuf; f.sechint := 0;
      i := 0;
      REPEAT f.ext[i] := NIL; F.ext[i] := 0; INC(i) UNTIL i = FileDir.ExTabSize;
      i := 0;
      REPEAT f.sec[i] := 0; F.sec[i] := 0; INC(i) UNTIL i = STS
    END ;
    RETURN f
  END New;
*/
    int i, res;
    Files_File f;
    Files_Buffer buf;
    FileDir_FileHd F;
    FileDir_FileName namebuf;

    f = NULL; Check(name, namebuf, &res);
    if (res <= 0) {
        buf = malloc(sizeof *buf); assert(buf); buf->apos = 0; buf->mod = true; buf->lim = HS; buf->next = buf;
        F = (FileDir_FileHd) &buf->data;
        F->mark = FileDir_HeaderMark;
        F->aleng = 0; F->bleng = HS; memcpy(F->name, namebuf, sizeof F->name);
        F->date = Kernel_Clock();
        f = malloc(sizeof *f); assert(f); f->aleng = 0; f->bleng = HS; f->modH = true;
        f->registered = false; f->date = F->date;
        f->firstbuf = buf; f->nofbufs = 1; memcpy(f->name, namebuf, sizeof f->name); f->sechint = 0;
        f->noRiders = 0;
        i = 0;
        do { f->ext[i] = NULL; F->ext[i] = 0; i++; } while (!(i == FileDir_ExTabSize));
        i = 0;
        do { f->sec[i] = 0; F->sec[i] = 0; i++; } while (!(i == STS));
    }
    return f;
}

void Files_Set(Files_Rider *r, Files_File f, int pos)
{
/**
  PROCEDURE Set*(VAR r: Rider; f: File; pos: INTEGER);
    VAR a, b: INTEGER;
  BEGIN  r.eof := FALSE; r.res := 0;
    IF f # NIL THEN
      IF pos < 0 THEN a := 0; b := HS
      ELSIF pos < f.aleng * SS + f.bleng - HS THEN
        a := (pos + HS) DIV SS; b := (pos + HS) MOD SS;
      ELSE a := f.aleng; b := f.bleng
      END ;
      r.file := f; r.apos := a; r.bpos := b; r.buf := f.firstbuf
    ELSE r.file:= NIL
    END
  END Set;
*/
    r->eof = false; r->res = 0;
    if (f != NULL) {
        r->file = f; f->noRiders++;
        Files_Reset(r, pos);
    } else  r->file = NULL;
}

void Files_Unset(Files_Rider *r)
{
    assert(r->file);
    assert(r->file->noRiders > 0);
    r->file->noRiders--;

    if (r->file->noRiders == 0) {
        Files_RestoreList();
    }
}

static void PosToABLeng(Files_File f, int pos, int *a, int *b)
{
/**
    PROCEDURE PosToABLeng(f: File; pos: INTEGER; VAR a, b: INTEGER);
    BEGIN
      IF pos < 0 THEN a := 0; b := HS
      ELSIF pos < f->aleng * SS + f->bleng - HS THEN
        a := (pos + HS) DIV SS; b := (pos + HS) MOD SS
      ELSE a := f->aleng; *b := f->bleng
      END
    END PosToABLeng;
*/
    if (pos < 0) { *a = 0; *b = HS;
    } else if (pos < f->aleng * SS + f->bleng - HS) {
        *a = (pos + HS) / SS; *b = (pos + HS) % SS;
    } else { *a = f->aleng; *b = f->bleng;
    }
}

void Files_Reset(Files_Rider *r, int pos)
{
    int a, b;
    Files_File f;

    f = r->file;
    assert(f);
    assert(f->noRiders > 0);

    PosToABLeng(f, pos, &a, &b);
    r->apos = a; r->bpos = b; r->buf = f->firstbuf;
}

void ReadBuf(Files_File f, Files_Buffer buf, int pos)
{
/**
  PROCEDURE ReadBuf(f: File; buf: Buffer; pos: INTEGER);
    VAR sec: DiskAdr;
  BEGIN
    IF pos < STS THEN sec := f.sec[pos]
    ELSE sec := f.ext[(pos-STS) DIV XS].sec[(pos-STS) MOD XS]
    END ;
    Kernel.GetSector(sec, buf.data);
    IF pos < f.aleng THEN buf.lim := SS ELSE buf.lim := f.bleng END ;
    buf.apos := pos; buf.mod := FALSE
  END ReadBuf;
*/
    DiskAdr sec;

    if (pos < STS) sec = f->sec[pos];
    else sec = f->ext[(pos-STS) / XS]->sec[(pos-STS) % XS];
    Kernel_GetSector(sec, (uint8_t *) buf->data);
    if (pos < f->aleng) buf->lim = SS; else buf->lim = f->bleng;
    buf->apos = pos; buf->mod = false;
}

static void UpdateHeader(Files_File f, FileDir_FileHeader *F)
{
/**
  PROCEDURE UpdateHeader(f: File; VAR F: FileDir.FileHeader);
    VAR k: INTEGER;
  BEGIN F.aleng := f.aleng; F.bleng := f.bleng;
    F.sec := f.sec; k := (f.aleng + (XS-STS)) DIV XS;
    WHILE k > 0 DO DEC(k); F.ext[k] := f.ext[k].adr END
  END UpdateHeader;
*/
    int k;

    F->aleng = f->aleng; F->bleng = f->bleng; F->date = f->date;
    memcpy(F->sec, f->sec, sizeof F->sec); k = (f->aleng + (XS-STS)) / XS;
    while (k > 0) { k--; F->ext[k] = f->ext[k]->adr; }
}


static void WriteBuf(Files_File f, Files_Buffer buf)
{
/**
  PROCEDURE WriteBuf(f: File; buf: Buffer);
    VAR i, k: INTEGER;
      secadr: DiskAdr; inx: Index;
  BEGIN
    IF buf.apos < STS THEN
      secadr := f.sec[buf.apos];
      IF secadr = 0 THEN
        Kernel.AllocSector(f.sechint, secadr);
        f.modH := TRUE; f.sec[buf.apos] := secadr; f.sechint := secadr
      END ;
      IF buf.apos = 0 THEN
        UpdateHeader(f, SYSTEM.VAL(FileDir.FileHeader, buf.data)); f.modH := FALSE
      END
    ELSE i := (buf.apos - STS) DIV XS; inx := f.ext[i];
      IF inx = NIL THEN
        NEW(inx); inx.adr := 0; inx.sec[0] := 0; f.ext[i] := inx; f.modH := TRUE
      END ;
      k := (buf.apos - STS) MOD XS; secadr := inx.sec[k];
      IF secadr = 0 THEN
        Kernel.AllocSector(f.sechint, secadr);
        f.modH := TRUE; inx.mod := TRUE; inx.sec[k] := secadr; f.sechint := secadr
      END
    END ;
    Kernel.PutSector(secadr, buf.data); buf.mod := FALSE
  END WriteBuf;
*/
    int i, k;
    DiskAdr secadr; Index inx;

    if (buf->apos < STS) {
        secadr = f->sec[buf->apos];
        if (secadr == 0) {
            Kernel_AllocSector(f->sechint, &secadr);
            f->modH = true; f->sec[buf->apos] = secadr; f->sechint = secadr;
        }
        if (buf->apos == 0) {
            UpdateHeader(f, (FileDir_FileHeader *) buf->data); f->modH = false;
        }
    } else { i = (buf->apos - STS) / XS; inx = f->ext[i];
        if (inx == NULL) {
            inx = malloc(sizeof *inx); assert(inx); inx->adr = 0; inx->sec[0] = 0; f->ext[i] = inx; f->modH = true;
        }
        k = (buf->apos - STS) % XS; secadr = inx->sec[k];
        if (secadr == 0) {
            Kernel_AllocSector(f->sechint, &secadr);
            f->modH = true; inx->mod = true; inx->sec[k] = secadr; f->sechint = secadr;
        }
    }
    Kernel_PutSector(secadr, buf->data); buf->mod = false;
}

static Files_Buffer GetBuf(Files_File f, int pos)
{
/**
  PROCEDURE GetBuf(f: File; pos: INTEGER): Buffer;
    VAR buf: Buffer;
  BEGIN buf := f.firstbuf;
    WHILE (buf.apos # pos) & (buf.next # f.firstbuf) DO buf := buf.next END ;
    IF buf.apos # pos THEN
      IF f.nofbufs < MaxBufs THEN  (*allocate new buffer*)
        NEW(buf); buf.next := f.firstbuf.next; f.firstbuf.next := buf; INC(f.nofbufs)
      ELSE (*reuse a buffer*) f.firstbuf := buf;
        IF buf.mod THEN WriteBuf(f, buf) END
      END ;
      IF pos <= f.aleng THEN ReadBuf(f, buf, pos) ELSE buf.apos := pos; buf.lim := 0; buf.mod := FALSE END
    END ;
    RETURN buf
  END GetBuf;
*/
    Files_Buffer buf;

    buf = f->firstbuf;
    while (buf->apos != pos && buf->next != f->firstbuf) buf = buf->next;
    if (buf->apos != pos) {
        if (f->nofbufs < MaxBufs) { /*allocate new buffer*/
            buf = malloc(sizeof *buf); assert(buf); buf->next = f->firstbuf->next; f->firstbuf->next = buf; f->nofbufs++;
        } else { /*reuse a buffer*/ f->firstbuf = buf;
            if (buf->mod) WriteBuf(f, buf);
        }
        if (pos <= f->aleng) ReadBuf(f, buf, pos); else { buf->apos = pos; buf->lim = 0; buf->mod = false; }
    }
    return buf;
}

static Files_Buffer Buf(Files_File f, int pos)
{
/**
  PROCEDURE Buf(f: File; pos: INTEGER): Buffer;
    VAR buf: Buffer;
  BEGIN buf := f.firstbuf;
    WHILE (buf.apos # pos) & (buf.next # f.firstbuf) DO buf := buf.next END ;
    IF buf.apos # pos THEN buf := NIL END ;
    RETURN buf
  END Buf;
*/
    Files_Buffer buf;

    buf = f->firstbuf;
    while (buf->apos != pos && buf->next != f->firstbuf) buf = buf->next;
    if (buf->apos != pos) buf = NULL;
    return buf;
}

static void Unbuffer(Files_File f)
{
/**
  PROCEDURE Unbuffer(f: File);
    VAR i, k: INTEGER;
      buf: Buffer;
      inx: Index;
      head: FileDir.FileHeader;
  BEGIN buf := f.firstbuf;
    REPEAT
      IF buf.mod THEN WriteBuf(f, buf) END ;
      buf := buf.next
    UNTIL buf = f.firstbuf;
    k := (f.aleng + (XS-STS)) DIV XS; i := 0;
    WHILE i < k DO
      inx := f.ext[i]; INC(i);
      IF inx.mod THEN
        IF inx.adr = 0 THEN
          Kernel.AllocSector(f.sechint, inx.adr); f.sechint := inx.adr; f.modH := TRUE
        END ;
        Kernel.PutSector(inx.adr, inx.sec); inx.mod := FALSE
      END
    END ;
    IF f.modH THEN
      Kernel.GetSector(f.sec[0], head); UpdateHeader(f, head);
      Kernel.PutSector(f.sec[0], head); f.modH := FALSE
    END
  END Unbuffer;
*/
    int i, k;
    Files_Buffer buf;
    Index inx;
    FileDir_FileHeader head;

    buf = f->firstbuf;
    do {
        if (buf->mod) WriteBuf(f, buf);
        buf = buf->next;
    } while (!(buf == f->firstbuf));
    k = (f->aleng + (XS-STS)) / XS; i = 0;
    while (i < k) {
        inx = f->ext[i]; i++;
        if (inx->mod) {
            if (inx->adr == 0) {
                Kernel_AllocSector(f->sechint, &inx->adr); f->sechint = inx->adr; f->modH = true;
            }
            Kernel_PutSector(inx->adr, (uint8_t *) inx->sec); inx->mod = false;
        }
    }
    if (f->modH) {
      Kernel_GetSector(f->sec[0], (uint8_t *) &head); UpdateHeader(f, &head);
      Kernel_PutSector(f->sec[0], (uint8_t *) &head); f->modH = false;
    }
}

void Files_Register(Files_File f)
{
/**
  PROCEDURE Register*(f: File);
  BEGIN
    IF (f # NIL) & (f.name[0] # 0X) THEN
      Unbuffer(f);
      IF ~f.registered THEN
        FileDir.Insert(f.name, f.sec[0]); f.registered := TRUE; f.next := root; root := SYSTEM.VAL(INTEGER, f)
      END
    END
  END Register;
*/
    if (f != NULL && f->name[0] != 0) {
        Unbuffer(f);
        if (!f->registered) {
            FileDir_Insert(f->name, f->sec[0]); f->registered = true; f->next = root; root = (uint64_t) f;
        }
    }
}

void Files_Close(Files_File f)
{
/**
  PROCEDURE Close*(f: File);
  BEGIN
    IF f # NIL THEN Unbuffer(f) END
  END Close;
*/
    if (f != NULL) Unbuffer(f);
}

void Files_Delete(const char *name, int *res)
{
/**
  PROCEDURE Delete*(name: ARRAY OF CHAR; VAR res: INTEGER);
    VAR adr: DiskAdr;
        namebuf: FileDir.FileName;
  BEGIN Check(name, namebuf, res);
    IF res = 0 THEN
      FileDir.Delete(namebuf, adr);
      IF adr = 0 THEN res := 2 END
    END
  END Delete;
*/
    DiskAdr adr;
    FileDir_FileName namebuf;

    Check(name, namebuf, res);
    if (*res == 0) {
        FileDir_Delete(namebuf, &adr);
        if (adr == 0) *res = 2;
    }
}

void Files_Truncate(Files_File f, int length, int *res)
{
/**
  (* Christian Schoffit 05/10/2023 *)
  PROCEDURE Truncate*(f: File; length: INTEGER; VAR res: INTEGER);
    VAR a, j, k, cl: INTEGER;
      ind: FileDir.IndexSector;
      r: Rider;
  BEGIN
    IF f # NIL THEN
      Unbuffer(f);
      f.date := Kernel.Clock();
      f.modH := TRUE;
      cl := Length(f);
      IF cl < length THEN (*The file must be extended*)
        Set(r, f, cl);
        WHILE cl < length DO Write(r, 0X); INC(cl) END;
      ELSE
        a := f.aleng; PosToABLeng(f, length, f.aleng, f.bleng);
        IF a > f.aleng THEN (*The file must be shrink*)
          IF a < STS THEN
            WHILE a > f.aleng DO Kernel.FreeSector(f.sec[a]);
              f.sec[a] := 0; DEC(a) END
          ELSE j := (a-STS) MOD XS + 1; k := (a-STS) DIV XS;
            WHILE a > f.aleng DO
              Kernel.GetSector(f.ext[k].adr, ind);
              WHILE (a > f.aleng) & (j > 0) DO DEC(j); Kernel.FreeSector(ind[j]); ind[j] := 0; DEC(a) END;
              IF j = 0 THEN Kernel.FreeSector(f.ext[k].adr); j := XS; DEC(k)
              ELSE Kernel.PutSector(f.ext[k].adr, ind) END;
              DEC(k)
            END
          END
        END
      END;
      Unbuffer(f);
    END
  END Truncate;
*/
    int a, j, k, cl;
    FileDir_IndexSector ind;
    Files_Rider r;

    if (f != NULL) {
        Unbuffer(f);
        f->date = Kernel_Clock();
        f->modH = true;
        cl = Files_Length(f);
        if (cl < length) { /*The file must be extended*/
            Files_Set(&r, f, cl);
            while (cl < length) { Files_Write(&r, 0); cl++; }
            Unbuffer(f);
            Files_Unset(&r);
        } else {
            a = f->aleng; PosToABLeng(f, length, &f->aleng, &f->bleng);
            if (a > f->aleng) { /*The file must be shrink*/
                if (a < STS) {
                    while (a > f->aleng) { Kernel_FreeSector(f->sec[a]);
                        f->sec[a] = 0; a--;
                    }
                } else { j = (a-STS) % XS + 1; k = (a-STS) / XS;
                    while (a > f->aleng) {
                        Kernel_GetSector(f->ext[k]->adr, (uint8_t *) ind);
                        while (a > f->aleng && j > 0) { j--; Kernel_FreeSector(ind[j]); ind[j] = 0; a--; }
                        if (j == 0) { Kernel_FreeSector(f->ext[k]->adr); j = XS; k--;
                        } else Kernel_PutSector(f->ext[k]->adr, (uint8_t *) ind);
                        k--;
                    }
                }
            }
            Unbuffer(f);
        }
    }
}

int Files_Length(Files_File f)
{
/**
  PROCEDURE Length*(f: File): INTEGER;
  BEGIN RETURN f.aleng * SS + f.bleng - HS
  END Length;
*/
  return f->aleng * SS + f->bleng - HS;
}

void Files_Rename(const char *old, const char *New, int *res)
{
/**
  PROCEDURE Rename*(old, new: ARRAY OF CHAR; VAR res: INTEGER);
    VAR adr: DiskAdr;
        oldbuf, newbuf: FileDir.FileName;
        head: FileDir.FileHeader;
  BEGIN Check(old, oldbuf, res);
    IF res = 0 THEN
      Check(new, newbuf, res);
      IF res = 0 THEN
        FileDir.Delete(oldbuf, adr);
        IF adr # 0 THEN
          FileDir.Insert(newbuf, adr);
          Kernel.GetSector(adr, head); head.name := newbuf; Kernel.PutSector(adr, head)
        ELSE res := 2
        END
      END
    END
  END Rename;
*/
    DiskAdr adr;
    FileDir_FileName oldbuf, newbuf;
    FileDir_FileHeader head;

    Check(old+1, oldbuf, res);
    if (*res == 0) {
        Check(New+1, newbuf, res);
        if (*res == 0) {
            FileDir_Delete(oldbuf, &adr);
            if (adr != 0) {
                FileDir_Insert(newbuf, adr);
                Kernel_GetSector(adr, (uint8_t *) &head); memcpy(head.name, newbuf, sizeof head.name); Kernel_PutSector(adr, (uint8_t *) &head);
            } else *res = 2;
        }
    }
}

void Files_Read(Files_Rider *r, char *ch)
{
/**
  PROCEDURE Read*(VAR r: Rider; VAR ch: CHAR);
    VAR buf: Buffer;  (*same as ReadByte*)
  BEGIN
    IF r.apos # r.buf.apos THEN r.buf := GetBuf(r.file, r.apos) END ;
    IF r.bpos < r.buf.lim THEN ch := CHR(r.buf.data[r.bpos]); INC(r.bpos)
    ELSIF r.apos < r.file.aleng THEN
      INC(r.apos); buf := Buf(r.file, r.apos);
      IF buf = NIL THEN
        IF r.buf.mod THEN WriteBuf(r.file, r.buf) END ;
        ReadBuf(r.file, r.buf, r.apos)
      ELSE r.buf := buf
      END ;
      ch := CHR(r.buf.data[0]); r.bpos := 1
    ELSE ch := 0X; r.eof := TRUE
    END
  END Read;
*/
    Files_Buffer buf;  /*same as ReadByte*/

    if (r->apos != r->buf->apos) r->buf = GetBuf(r->file, r->apos);
    if (r->bpos < r->buf->lim) { *ch = r->buf->data[r->bpos]; r->bpos++; }
    else if (r->apos < r->file->aleng) {
        r->apos++; buf = Buf(r->file, r->apos);
        if (buf == NULL) {
            if (r->buf->mod) WriteBuf(r->file, r->buf);
            ReadBuf(r->file, r->buf, r->apos);
        } else r->buf = buf;
        *ch = r->buf->data[0]; r->bpos = 1;
    } else { ch = 0; r->eof = true;
    }
}

static void NewExt(Files_File f)
{
/**
  PROCEDURE NewExt(f: File);
    VAR i, k: INTEGER; ext: Index;
  BEGIN k := (f.aleng - STS) DIV XS;
    NEW(ext); ext.adr := 0; ext.mod := TRUE; f.ext[k] := ext; i := XS;
    REPEAT DEC(i); ext.sec[i] := 0 UNTIL i = 0
  END NewExt;
*/
    int i, k; Index ext;

    k = (f->aleng - STS) / XS;
    assert(k<FileDir_ExTabSize);
    ext = malloc(sizeof *ext); assert(ext); ext->adr = 0; ext->mod = true; f->ext[k] = ext; i = XS;
    do { i--; ext->sec[i] = 0; } while (!(i == 0));
}

void Files_Write(Files_Rider *r, const char ch)
{
/**
  PROCEDURE Write*(VAR r: Rider; ch: CHAR);
    VAR f: File; buf: Buffer;
  BEGIN (*same as WriteByte*)
    IF r.apos # r.buf.apos THEN r.buf := GetBuf(r.file, r.apos); END ;
    IF r.bpos >= r.buf.lim THEN
      IF r.bpos < SS THEN
        INC(r.buf.lim); INC(r.file.bleng); r.file.modH := TRUE
      ELSE f := r.file; WriteBuf(f, r.buf); INC(r.apos); buf := Buf(r.file, r.apos);
        IF buf = NIL THEN
          IF r.apos <= f.aleng THEN ReadBuf(f, r.buf, r.apos)
          ELSE r.buf.apos := r.apos; r.buf.lim := 1; f.aleng := f.aleng + 1; f.bleng := 1; f.modH := TRUE;
            IF (f.aleng - STS) MOD XS = 0 THEN NewExt(f) END
          END
        ELSE r.buf := buf
        END ;
        r.bpos := 0
      END
    END ;
    r.buf.data[r.bpos] := ORD(ch); INC(r.bpos); r.buf.mod := TRUE
  END Write;
*/
    Files_File f; Files_Buffer buf;

    /*same as WriteByte*/
    if (r->apos != r->buf->apos) { r->buf = GetBuf(r->file, r->apos); }
    if (r->bpos >= r->buf->lim) {
        if (r->bpos < SS) {
            r->buf->lim++; r->file->bleng++; r->file->modH = true;
        } else { f = r->file; WriteBuf(f, r->buf); r->apos++; buf = Buf(r->file, r->apos);
            if (buf == NULL) {
                if (r->apos <= f->aleng) ReadBuf(f, r->buf, r->apos);
                else { r->buf->apos = r->apos; r->buf->lim = 1; f->aleng = f->aleng + 1; f->bleng = 1; f->modH = true;
                    if ((f->aleng - STS) % XS == 0) { NewExt(f); }
                }
            } else r->buf = buf;
            r->bpos = 0;
        }
    }
    r->buf->data[r->bpos] = ch; r->bpos++; r->buf->mod = true;
}

static int mark(uint64_t f, uint64_t *nxt) {
/**
    PROCEDURE mark(f: INTEGER): INTEGER;
      VAR m: INTEGER;
    BEGIN
      IF f = 0 THEN m := -1 ELSE SYSTEM.GET(f-4, m) END ;
      RETURN m
    END mark;
*/
    int m;
    Files_File f1 = (Files_File) f;

    *nxt = f;

    if (f == 0) m = -1;
    else m = f1->noRiders;

    if (m == 0) {
        *nxt = f1->next;
        /* Restituer au système la mémoire allouée au descripteur de fichiers */
        Files_Buffer buf;
        int i;

        buf = f1->firstbuf;
        do {
            Files_Buffer next;

            next = buf->next;
            free(buf);
            buf = next;
        } while (buf != f1->firstbuf);

        i = 0;
        while (i < FileDir_ExTabSize && f1->ext[i] != NULL) { free(f1->ext[i]); i++; }

        free(f1);
    }

    return m;
}

void Files_RestoreList(void)
{
/**
  PROCEDURE RestoreList*; (*after mark phase of garbage collection*)
    VAR f, f0: INTEGER;

    PROCEDURE mark(f: INTEGER): INTEGER;
      VAR m: INTEGER;
    BEGIN
      IF f = 0 THEN m := -1 ELSE SYSTEM.GET(f-4, m) END ;
      RETURN m
    END mark;

  BEGIN (*field "next" has offset 0*)
    WHILE mark(root) = 0 DO SYSTEM.GET(root, root) END ;
    f := root;
    WHILE f # 0 DO
      f0 := f;
      REPEAT SYSTEM.GET(f0, f0) UNTIL mark(f0) # 0;
      SYSTEM.PUT(f, f0); f := f0
    END
  END RestoreList;
*/
    uint64_t f, f0, next;

    while (mark(root, &next) == 0) root = next;
    f = root;
    while (f != 0) {
        f0 = ((Files_File) f)->next;
        while (mark(f0, &next) == 0) f0 = next;
        ((Files_File) f)->next = f0; f = f0;
    }
}

Files_File Files_Base(Files_Rider *r)
{
/**
  PROCEDURE Base*(VAR r: Rider): File;
  BEGIN RETURN r.file
  END Base;
*/
  return r->file;
}

void Files_Init(void)
{
/**
  PROCEDURE Init*;
  BEGIN root := 0; Kernel.Init; FileDir.Init
  END Init;
*/
    root = 0;
    Kernel_Init();
    FileDir_Init();
}
