#include <sys/types.h>
#include <unistd.h>
#include <assert.h>
#include <string.h>

#include "Kernel.h"
#include "Modules.h"
#include "FileDir.h"

#define N (DirPgSize / 2)

void Search(const FSHeader *fshd, const char *name, int *A)
{
/**
   PROCEDURE Search*(fshd: FileSystem.FSHeader; name: FileName; VAR A: DiskAdr);
    VAR i, L, R: INTEGER; dadr: DiskAdr;
      a: DirPage;
  BEGIN dadr := DirRootAdr; A := 0;
    REPEAT FileSystem.GetSector(fshd, dadr, a); ASSERT(a.mark = DirMark);
      L := 0; R := a.m; (*binary search*)
      WHILE L < R DO
        i := (L+R) DIV 2;
        IF name <= a.e[i].name THEN R := i ELSE L := i+1 END
      END ;
      IF (R < a.m) & (name = a.e[R].name) THEN A := a.e[R].adr (*found*)
      ELSIF R = 0 THEN dadr := a.p0
      ELSE dadr := a.e[R-1].p
      END ;
    UNTIL (dadr = 0) OR (A # 0)
  END Search;
*/

    int i, L, R; DiskAdr dadr;
        DirPage a;

    dadr = DirRootAdr; *A = 0;
    do { GetSector(fshd, dadr, (uint8_t *) &a); assert(a.b.b.mark == DirMark);
      L = 0; R = a.b.b.m; /*binary search*/
      while (L < R) {
        i = (L+R) / 2;
        if (strcmp(name, a.b.e[i].name) <= 0) R = i; else L = i+1;
      }
      if (R < a.b.b.m && strcmp(name, a.b.e[R].name) == 0) *A = a.b.e[R].adr; /*found*/
      else if (R == 0) dadr = a.b.b.p0;
      else dadr = a.b.e[R-1].p;
    } while (!(dadr == 0 || *A != 0));
}

static void enumerate(const FSHeader *fshd, char *prefix, DiskAdr dpg, EntryHandler proc, bool *Continue, void *callerData)
{
/**
  PROCEDURE enumerate(fshd: FileSystem.FSHeader;
                      prefix:   ARRAY OF CHAR;
                      dpg:          DiskAdr;
                      proc:         EntryHandler;
                      VAR continue: BOOLEAN);
    VAR i, j: INTEGER; pfx, nmx: CHAR;
      dpg1: DiskAdr; a: DirPage;
  BEGIN FileSystem.GetSector(fshd, dpg, a); ASSERT(a.mark = DirMark); i := 0;
    WHILE (i < a.m) & continue DO
      j := 0;
      REPEAT pfx := prefix[j]; nmx := a.e[i].name[j]; INC(j)
      UNTIL (nmx # pfx) OR (pfx = 0X);
      IF nmx >= pfx THEN
        IF i = 0 THEN dpg1 := a.p0 ELSE dpg1 := a.e[i-1].p END ;
        IF dpg1 # 0 THEN enumerate(fshd, prefix, dpg1, proc, continue) END ;
        IF pfx = 0X THEN
          IF continue THEN proc(a.e[i].name, a.e[i].adr, a.e[i].date, a.e[i].size, continue) END
        ELSE continue := FALSE
        END
      END ;
      INC(i)
    END ;
    IF continue & (i > 0) & (a.e[i-1].p # 0) THEN
      enumerate(fshd, prefix, a.e[i-1].p, proc, continue)
    END
  END enumerate;
*/
    int i, j; char pfx, nmx;
    DiskAdr dpg1; DirPage a;

    GetSector(fshd, dpg, (uint8_t *) &a); assert(a.b.b.mark == DirMark); i = 0;
    while (i < a.b.b.m && *Continue) {
        j = 0;
        do { pfx = prefix[j]; nmx = a.b.e[i].name[j]; j++;
        } while (!(nmx != pfx || pfx == 0));
        if (nmx >= pfx) {
            if (i == 0) dpg1 = a.b.b.p0; else dpg1 = a.b.e[i-1].p;
            if (dpg1 != 0) enumerate(fshd, prefix, dpg1, proc, Continue, callerData);
            if (pfx == 0) {
                if (*Continue) proc(a.b.e[i].name, a.b.e[i].adr, a.b.e[i].date, a.b.e[i].size, Continue, callerData);
            } else *Continue = false;
        }
        i++;
    }
    if (*Continue && i > 0 && a.b.e[i-1].p != 0) {
      enumerate(fshd, prefix, a.b.e[i-1].p, proc, Continue, callerData);
    }
}

void Enumerate(const FSHeader *fshd, char *prefix, EntryHandler proc, void *callerData)
{
/**
  PROCEDURE Enumerate*(fshd: FileSystem.FSHeader; prefix: ARRAY OF CHAR; proc: EntryHandler);
    VAR b: BOOLEAN;
  BEGIN b := TRUE; enumerate(fshd, prefix, DirRootAdr, proc, b)
  END Enumerate;
*/
    bool b;

    b = true; enumerate(fshd, prefix, DirRootAdr, proc, &b, callerData);
}

static void insert(FSHeader *fshd,
                   const FileName name,
                   DiskAdr dpg0,
                   bool *h,
                   DirEntry *v,
                   DiskAdr fad,
                   int date,
                   int size)
{
/**
  PROCEDURE insert(VAR fshd: FileSystem.FSHeader;
                   name: FileName;
                   dpg0:  DiskAdr;
                   VAR h: BOOLEAN;
                   VAR v: DirEntry;
                   fad:     DiskAdr;
                   date,
                   size: INTEGER);
    (*h = "tree has become higher and v is ascending element"*)
    VAR ch: CHAR;
      i, j, L, R: INTEGER;
      dpg1: DiskAdr;
      u: DirEntry;
      a: DirPage;

  BEGIN (*~h*) FileSystem.GetSector(fshd, dpg0, a); ASSERT(a.mark = DirMark);
    L := 0; R := a.m; (*binary search*)
    WHILE L < R DO
      i := (L+R) DIV 2;
      IF name <= a.e[i].name THEN R := i ELSE L := i+1 END
    END ;
    IF (R < a.m) & (name = a.e[R].name) THEN
      a.e[R].adr := fad; a.e[R].date := date; a.e[R].size := size; FileSystem.PutSector(fshd, dpg0, a)  (*replace*)
    ELSE (*not on this page*)
      IF R = 0 THEN dpg1 := a.p0 ELSE dpg1 := a.e[R-1].p END ;
      IF dpg1 = 0 THEN (*not in tree, insert*)
        u.adr := fad; u.date := date; u.size := size; u.p := 0; h := TRUE; j := 0;
        REPEAT ch := name[j]; u.name[j] := ch; INC(j)
        UNTIL ch = 0X;
        WHILE j < FnLength DO u.name[j] := 0X; INC(j) END ;
      ELSE
        insert(fshd, name, dpg1, h, u, fad, date, size)
      END ;
      IF h THEN (*insert u to the left of e[R]*)
        IF a.m < DirPgSize THEN
          h := FALSE; i := a.m;
          WHILE i > R DO DEC(i); a.e[i+1] := a.e[i] END ;
          a.e[R] := u; INC(a.m)
        ELSE (*split page and assign the middle element to v*)
          a.m := N; a.mark := DirMark;
          IF R < N THEN (*insert in left half*)
            v := a.e[N-1]; i := N-1;
            WHILE i > R DO DEC(i); a.e[i+1] := a.e[i] END ;
            a.e[R] := u; FileSystem.PutSector(fshd, dpg0, a);
            FileSystem.AllocSector(fshd, dpg0, dpg0); i := 0;
            WHILE i < N DO a.e[i] := a.e[i+N]; INC(i) END
          ELSE (*insert in right half*)
            FileSystem.PutSector(fshd, dpg0, a);
            FileSystem.AllocSector(fshd, dpg0, dpg0); DEC(R, N); i := 0;
            IF R = 0 THEN v := u
            ELSE v := a.e[N];
              WHILE i < R-1 DO a.e[i] := a.e[N+1+i]; INC(i) END ;
              a.e[i] := u; INC(i)
            END ;
            WHILE i < N DO a.e[i] := a.e[N+i]; INC(i) END
          END ;
          a.p0 := v.p; v.p := dpg0
        END ;
        FileSystem.PutSector(fshd, dpg0, a)
      END
    END
  END insert;
*/
    /*h = "tree has become higher and v is ascending element"*/
    char ch;
    int i, j, L, R;
    DiskAdr dpg1;
    DirEntry u;
    DirPage a;

    /*~h*/ GetSector(fshd, dpg0, (uint8_t *) &a); assert(a.b.b.mark == DirMark);
    L = 0; R = a.b.b.m; /*binary search*/
    while (L < R) {
        i = (L+R) / 2;
        if (strcmp(name, a.b.e[i].name) <= 0) R = i; else L = i+1;
    }
    if (R < a.b.b.m && strcmp(name, a.b.e[R].name) == 0) {
        a.b.e[R].adr = fad; PutSector(fshd, dpg0, (uint8_t *) &a);  /*replace*/
    } else { /*not on this page*/
        if (R == 0) dpg1 = a.b.b.p0; else dpg1 = a.b.e[R-1].p;
        if (dpg1 == 0) { /*not in tree, insert*/
            u.adr = fad; u.date = date; u.size = size; u.p = 0; *h = true; j = 0;
            do { ch = name[j]; u.name[j] = ch; j++;
            } while (!(ch == 0));
            while (j < FnLength) { u.name[j] = 0; j++; }
        } else
            insert(fshd, name, dpg1, h, &u, fad, date, size);
        if (*h) { /*insert u to the left of e[R]*/
            if (a.b.b.m < DirPgSize) {
                *h = false; i = a.b.b.m;
                while (i > R) { i--; a.b.e[i+1] = a.b.e[i]; }
                a.b.e[R] = u; a.b.b.m++;
            } else { /*split page and assign the middle element to v*/
                a.b.b.m = N; a.b.b.mark = DirMark;
                if (R < N) { /*insert in left half*/
                    memcpy(v, &a.b.e[N-1], sizeof *v); i = N-1;
                    while (i > R) { i--; a.b.e[i+1] = a.b.e[i]; }
                    a.b.e[R] = u; PutSector(fshd, dpg0, (uint8_t * ) &a);
                    AllocSector(fshd, dpg0, &dpg0); i = 0;
                    while (i < N) { a.b.e[i] = a.b.e[i+N]; i++; }
                } else { /*insert in right half*/
                    PutSector(fshd, dpg0, (uint8_t *) &a);
                    AllocSector(fshd, dpg0, &dpg0); R -= N; i = 0;
                    if (R == 0) memcpy(v, &u, sizeof *v);
                    else { memcpy(v, &a.b.e[N], sizeof *v);
                        while (i < R-1) { a.b.e[i] = a.b.e[N+1+i]; i++; }
                        a.b.e[i] = u; i++;
                    }
                    while (i < N) { a.b.e[i] = a.b.e[N+i]; i++; }
                 }
                 a.b.b.p0 = v->p; v->p = dpg0;
            }
            PutSector(fshd, dpg0, (uint8_t *) &a);
        }
    }
}

void Insert(FSHeader *fshd, const FileName name, int fad, int date, int size)
{
/**
  PROCEDURE Insert*(VAR fshd: FileSystem.FSHeader; name: FileName; fad: DiskAdr; date, size: INTEGER);
    VAR  oldroot: DiskAdr;
      h: BOOLEAN; U: DirEntry;
      a: DirPage;
  BEGIN h := FALSE; Kernel.DisableAbortHandler;
    insert(fshd, name, DirRootAdr, h, U, fad, date, size);
    IF h THEN (*root overflow*)
      FileSystem.GetSector(fshd, DirRootAdr, a); ASSERT(a.mark = DirMark);
      FileSystem.AllocSector(fshd, DirRootAdr, oldroot); FileSystem.PutSector(fshd, oldroot, a);
      a.mark := DirMark; a.m := 1; a.p0 := oldroot; a.e[0] := U;
      FileSystem.PutSector(fshd, DirRootAdr, a)
    END; Kernel.EnableAbortHandler
  END Insert;
*/
    DiskAdr oldroot;
    bool h; DirEntry U;
    DirPage a;

    h = false;
    insert(fshd, name, DirRootAdr, &h, &U, fad, date, size);
    if (h) { /*root overflow*/
        GetSector(fshd, DirRootAdr, (uint8_t *) &a); assert(a.b.b.mark == DirMark);
        AllocSector(fshd, DirRootAdr, &oldroot); PutSector(fshd, oldroot, (uint8_t *) &a);
        a.b.b.mark = DirMark; a.b.b.m = 1; a.b.b.p0 = oldroot; a.b.e[0] = U;
        PutSector(fshd, DirRootAdr, (uint8_t *) &a);
    }
}

static void underflow(FSHeader *fshd,
                      DirPage *c, /*ancestor page*/
                      DiskAdr dpg0,
                      int s,              /*insertion point in c*/
                      bool *h)            /*c undersize*/
{
/**
  PROCEDURE underflow(VAR fshd: FileSystem.FSHeader; VAR c: DirPage;  (*ancestor page*)
                      dpg0:  DiskAdr;
                      s:     INTEGER;  (*insertion point in c*)
                      VAR h: BOOLEAN); (*c undersize*)
    VAR i, k: INTEGER;
        dpg1: DiskAdr;
        a, b: DirPage;  (*a := underflowing page, b := neighbouring page*)
  BEGIN FileSystem.GetSector(fshd, dpg0, a); ASSERT(a.mark = DirMark);
    (*h & a.m = N-1 & dpg0 = c.e[s-1].p*)
    IF s < c.m THEN (*b := page to the right of a*)
      dpg1 := c.e[s].p; FileSystem.GetSector(fshd, dpg1, b); ASSERT(b.mark = DirMark);
      k := (b.m-(N-1)) DIV 2; (*k = no. of items available on page b*)
      a.e[N-1] := c.e[s]; a.e[N-1].p := b.p0;
      IF k > 0 THEN
        (*move k-1 items from b to a, one to c*) i := 0;
        WHILE i < k-1 DO a.e[i+N] := b.e[i]; INC(i) END ;
        c.e[s] := b.e[i]; b.p0 := c.e[s].p;
        c.e[s].p := dpg1; b.m := b.m - k; i := 0;
        WHILE i < b.m DO b.e[i] := b.e[i+k]; INC(i) END ;
        FileSystem.PutSector(fshd, dpg1, b); a.m := N-1+k; h := FALSE
      ELSE (*merge pages a and b, discard b*) i := 0;
        WHILE i < N DO a.e[i+N] := b.e[i]; INC(i) END ;
        i := s; DEC(c.m);
        WHILE i < c.m DO c.e[i] := c.e[i+1]; INC(i) END ;
        a.m := 2*N; h := c.m < N
      END ;
      FileSystem.PutSector(fshd, dpg0, a)
    ELSE (*b := page to the left of a*) DEC(s);
      IF s = 0 THEN dpg1 := c.p0 ELSE dpg1 := c.e[s-1].p END ;
      FileSystem.GetSector(fshd, dpg1, b); ASSERT(b.mark = DirMark);
      k := (b.m-(N-1)) DIV 2; (*k = no. of items available on page b*)
      IF k > 0 THEN
        i := N-1;
        WHILE i > 0 DO DEC(i); a.e[i+k] := a.e[i] END ;
        i := k-1; a.e[i] := c.e[s]; a.e[i].p := a.p0;
        (*move k-1 items from b to a, one to c*) b.m := b.m - k;
        WHILE i > 0 DO DEC(i); a.e[i] := b.e[i+b.m+1] END ;
        c.e[s] := b.e[b.m]; a.p0 := c.e[s].p;
        c.e[s].p := dpg0; a.m := N-1+k; h := FALSE;
        FileSystem.PutSector(fshd, dpg0, a)
      ELSE (*merge pages a and b, discard a*)
        c.e[s].p := a.p0; b.e[N] := c.e[s]; i := 0;
        WHILE i < N-1 DO b.e[i+(N+1)] := a.e[i]; INC(i) END ;
        b.m := 2*N; DEC(c.m); h := c.m < N
      END ;
      FileSystem.PutSector(fshd, dpg1, b)
    END
  END underflow;
*/
    int i, k;
    DiskAdr dpg1;
    DirPage a, b;  /*a := underflowing page, b := neighbouring page*/

    GetSector(fshd, dpg0, (uint8_t *) &a); assert(a.b.b.mark == DirMark);
    /*h & a.m = N-1 & dpg0 = c.e[s-1].p*/
    if (s < c->b.b.m) { /*b := page to the right of a*/
        dpg1 = c->b.e[s].p; GetSector(fshd, dpg1, (uint8_t *) &b); assert(b.b.b.mark == DirMark);
        k = (b.b.b.m-N+1) / 2; /*k = no. of items available on page b*/
        a.b.e[N-1] = c->b.e[s]; a.b.e[N-1].p = b.b.b.p0;
        if (k > 0) {
            /*move k-1 items from b to a, one to c*/ i = 0;
            while (i < k-1) { a.b.e[i+N] = b.b.e[i]; i++; }
            c->b.e[s] = b.b.e[i]; b.b.b.p0 = c->b.e[s].p;
            c->b.e[s].p = dpg1; b.b.b.m = b.b.b.m - k; i = 0;
            while (i < b.b.b.m) { b.b.e[i] = b.b.e[i+k]; i++; }
            PutSector(fshd, dpg1, (uint8_t *) &b); a.b.b.m = N-1+k; *h = false;
        } else { /*merge pages a and b, discard b*/ i = 0;
            while (i < N) { a.b.e[i+N] = b.b.e[i]; i++; }
            i = s; c->b.b.m--;
            while (i < c->b.b.m) { c->b.e[i] = c->b.e[i+1]; i++; }
            a.b.b.m = 2*N; *h = c->b.b.m < N;
        }
        PutSector(fshd, dpg0, (uint8_t *) &a);
    } else { /*b := page to the left of a*/ s--;
        if (s == 0) dpg1 = c->b.b.p0; else dpg1 = c->b.e[s-1].p;
        GetSector(fshd, dpg1, (uint8_t *) &b); assert(b.b.b.mark == DirMark);
        k = (b.b.b.m-N+1) / 2; /*k = no. of items available on page b*/
        if (k > 0) {
            i = N-1;
            while (i > 0) { i--; a.b.e[i+k] = a.b.e[i]; }
            i = k-1; a.b.e[i] = c->b.e[s]; a.b.e[i].p = a.b.b.p0;
            /*move k-1 items from b to a, one to c*/ b.b.b.m -= k;
            while (i > 0) { i--; a.b.e[i] = b.b.e[i+b.b.b.m+1]; }
            c->b.e[s] = b.b.e[b.b.b.m]; a.b.b.p0 = c->b.e[s].p;
            c->b.e[s].p = dpg0; a.b.b.m = N-1+k; *h = false;
            PutSector(fshd, dpg0, (uint8_t *) &a);
        } else { /*merge pages a and b, discard a*/
            c->b.e[s].p = a.b.b.p0; b.b.e[N] = c->b.e[s]; i = 0;
            while (i < N-1) { b.b.e[i+N+1] = a.b.e[i]; i++; }
            b.b.b.m = 2*N; c->b.b.m--; *h = c->b.b.m < N;
        }
        PutSector(fshd, dpg1, (uint8_t *) &b);
    }
}

static void del(FSHeader *fshd, DirPage *a, int R, DiskAdr dpg1, bool *h)
{
/**
    PROCEDURE del(VAR fshd: FileSystem.FSHeader; VAR a: DirPage; R: INTEGER; dpg1: DiskAdr; VAR h: BOOLEAN);
      VAR dpg2: DiskAdr;  (*global: a, R*)
          b: DirPage;
    BEGIN FileSystem.GetSector(fshd, dpg1, b); ASSERT(b.mark = DirMark); dpg2 := b.e[b.m-1].p;
      IF dpg2 # 0 THEN del(fshd, a, R, dpg2, h);
        IF h THEN underflow(fshd, b, dpg2, b.m, h); FileSystem.PutSector(fshd, dpg1, b) END
      ELSE
        b.e[b.m-1].p := a.e[R].p; a.e[R] := b.e[b.m-1];
        DEC(b.m); h := b.m < N; FileSystem.PutSector(fshd, dpg1, b)
      END
    END del;
*/
    DiskAdr dpg2;  /*global: a, R*/
    DirPage b;

    GetSector(fshd, dpg1, (uint8_t *) &b); assert(b.b.b.mark == DirMark); dpg2 = b.b.e[b.b.b.m-1].p;
    if (dpg2 != 0) { del(fshd, a, R, dpg2, h);
        if (*h) { underflow(fshd, &b, dpg2, b.b.b.m, h); PutSector(fshd, dpg1, (uint8_t *) &b); }
    } else {
        b.b.e[b.b.b.m-1].p = a->b.e[R].p; a->b.e[R] = b.b.e[b.b.b.m-1];
        b.b.b.m--; *h = b.b.b.m < N; PutSector(fshd, dpg1, (uint8_t *) &b);
    }
}

static void delete0(FSHeader *fshd, const FileName name,
                   DiskAdr dpg0,
                   bool *h,
                   DiskAdr *fad)
{
/**
  PROCEDURE delete(VAR fshd: FileSystem.FSHeader; name: FileName;
                   dpg0: DiskAdr;
                   VAR h: BOOLEAN;
                   VAR fad: DiskAdr);
  (*search and delete entry with key name; if a page underflow arises,
    balance with adjacent page or merge; h := "page dpg0 is undersize"*)

    VAR i, L, R: INTEGER;
      dpg1: DiskAdr;
      a: DirPage;

    PROCEDURE del(VAR fshd: FileSystem.FSHeader; VAR a: DirPage; R: INTEGER; dpg1: DiskAdr; VAR h: BOOLEAN);
      VAR dpg2: DiskAdr;  (*global: a, R*)
          b: DirPage;
    BEGIN FileSystem.GetSector(fshd, dpg1, b); ASSERT(b.mark = DirMark); dpg2 := b.e[b.m-1].p;
      IF dpg2 # 0 THEN del(fshd, a, R, dpg2, h);
        IF h THEN underflow(fshd, b, dpg2, b.m, h); FileSystem.PutSector(fshd, dpg1, b) END
      ELSE
        b.e[b.m-1].p := a.e[R].p; a.e[R] := b.e[b.m-1];
        DEC(b.m); h := b.m < N; FileSystem.PutSector(fshd, dpg1, b)
      END
    END del;

  BEGIN (*~h*) FileSystem.GetSector(fshd, dpg0, a); ASSERT(a.mark = DirMark);
    L := 0; R := a.m; (*binary search*)
    WHILE L < R DO
      i := (L+R) DIV 2;
      IF name <= a.e[i].name THEN R := i ELSE L := i+1 END
    END ;
    IF R = 0 THEN dpg1 := a.p0 ELSE dpg1 := a.e[R-1].p END ;
    IF (R < a.m) & (name = a.e[R].name) THEN
      (*found, now delete*) fad := a.e[R].adr;
      IF dpg1 = 0 THEN  (*a is a leaf page*)
        DEC(a.m); h := a.m < N; i := R;
        WHILE i < a.m DO a.e[i] := a.e[i+1]; INC(i) END
      ELSE del(fshd, a, R, dpg1, h);
        IF h THEN underflow(fshd, a, dpg1, R, h) END
      END ;
      FileSystem.PutSector(fshd, dpg0, a)
    ELSIF dpg1 # 0 THEN
      delete(fshd, name, dpg1, h, fad);
      IF h THEN underflow(fshd, a, dpg1, R, h); FileSystem.PutSector(fshd, dpg0, a) END
    ELSE (*not in tree*) fad := 0
    END
  END delete;
*/
  /*search and delete entry with key name; if a page underflow arises,
    balance with adjacent page or merge; h := "page dpg0 is undersize"*/

    int i, L, R;
    DiskAdr dpg1;
    DirPage a;

    /*~h*/ GetSector(fshd, dpg0, (uint8_t *) &a); assert(a.b.b.mark == DirMark);
    L = 0; R = a.b.b.m; /*binary search*/
    while (L < R) {
        i = (L+R) / 2;
        if (strcmp(name, a.b.e[i].name) <= 0) R = i; else L = i+1;
    }
    if (R == 0) dpg1 = a.b.b.p0; else dpg1 = a.b.e[R-1].p;
    if (R < a.b.b.m && strcmp(name, a.b.e[R].name) == 0) {
        /*found, now delete*/ *fad = a.b.e[R].adr;
        if (dpg1 == 0) { /*a is a leaf page*/
            a.b.b.m--; *h = a.b.b.m < N; i = R;
            while (i < a.b.b.m) { a.b.e[i] = a.b.e[i+1]; i++; }
        } else { del(fshd, &a, R, dpg1, h);
            if (*h) underflow(fshd, &a, dpg1, R, h);
        }
        PutSector(fshd, dpg0, (uint8_t *) &a);
    } else if (dpg1 != 0) {
        delete0(fshd, name, dpg1, h, fad);
        if (*h) { underflow(fshd, &a, dpg1, R, h); PutSector(fshd,dpg0, (uint8_t *) &a); }
    } else /*not in tree*/ *fad = 0;
}

void FileDir_Delete(FSHeader *fshd, const FileName name, int *fad)
{
/**
  PROCEDURE Delete*(VAR fshd: FileSystem.FSHeader; name: FileName; VAR fad: DiskAdr);
    VAR h: BOOLEAN; newroot: DiskAdr;
      a: DirPage;
  BEGIN h := FALSE; Kernel.DisableAbortHandler;
    delete(fshd, name, DirRootAdr, h, fad);
    IF h THEN (*root underflow*)
      FileSystem.GetSector(fshd, DirRootAdr, a); ASSERT(a.mark = DirMark);
      IF (a.m = 0) & (a.p0 # 0) THEN
        newroot := a.p0; FileSystem.GetSector(fshd, newroot, a); ASSERT(a.mark = DirMark);
        FileSystem.PutSector(fshd, DirRootAdr, a) (*discard newroot*)
      END
    END; Kernel.EnableAbortHandler
  END Delete;
*/
    bool h; DiskAdr newroot;
    DirPage a;

    h = false;
    delete0(fshd, name, DirRootAdr, &h, fad);
    if (h) { /*root underflow*/
        GetSector(fshd, DirRootAdr, (uint8_t *) &a); assert(a.b.b.mark == DirMark);
        if (a.b.b.m == 0 && a.b.b.p0 != 0) {
            newroot = a.b.b.p0; GetSector(fshd, newroot, (uint8_t *) &a); assert(a.b.b.mark == DirMark);
            PutSector(fshd, DirRootAdr, (uint8_t *) &a); /*discard newroot*/
        }
    }
}

static void sift(DiskAdr *A, int L, int R)
{
/**
      PROCEDURE sift(VAR A: ARRAY OF DiskAdr; L, R: INTEGER);
        VAR i, j: INTEGER; x: DiskAdr;
      BEGIN j := L; x := A[j];
        REPEAT i := j; j := 2*j + 1;
          IF (j+1 < R) & (A[j] < A[j+1]) THEN INC(j) END ;
          IF (j < R) & (x <= A[j]) THEN A[i] := A[j] END
        UNTIL (j >= R) OR (x > A[j]);
        A[i] := x
      END sift;
*/
    int i, j; DiskAdr x;

    j = L; x = A[j];
    do { i = j; j = 2*j + 1;
        if (j+1 < R && A[j] < A[j+1]) j++;
        if (j < R && x <= A[j]) A[i] = A[j];
    } while (!(j >= R || x > A[j]));
    A[i] = x;
}

static void MarkSectors(FSHeader *fshd, DiskAdr *A, int k)
{
/**
    PROCEDURE MarkSectors(VAR fshd: FileSystem.FSHeader; VAR A: ARRAY OF DiskAdr; k: INTEGER);
      VAR L, R, i, j, n: INTEGER; x: DiskAdr;
        fhd: FileHeader;
        B: IndexSector;

    BEGIN L := k DIV 2; R := k; (*heapsort*)
      WHILE L > 0 DO DEC(L); sift(fshd, A, L, R) END ;
      WHILE R > 0 DO
        DEC(R); x := A[0]; A[0] := A[R]; A[R] := x; sift(fshd, A, L, R)
      END ;
      WHILE L < k DO
        FileSystem.GetSector(fshd, A[L], fhd); ASSERT(fhd.mark = HeaderMark);
        IF fhd.aleng < SecTabSize THEN j := fhd.aleng + 1;
          REPEAT DEC(j); FileSystem.MarkSector(fshd, fhd.sec[j]) UNTIL j = 0
        ELSE j := SecTabSize;
          REPEAT DEC(j); FileSystem.MarkSector(fshd, fhd.sec[j]) UNTIL j = 0;
          n := (fhd.aleng - SecTabSize) DIV IndexSize; i := 0;
          WHILE i <= n DO
            FileSystem.MarkSector(fshd, fhd.ext[i]);
            FileSystem.GetSector(fshd, fhd.ext[i], B); (*index sector*)
            IF i < n THEN j := IndexSize ELSE j := (fhd.aleng - SecTabSize) MOD IndexSize + 1 END ;
            REPEAT DEC(j); FileSystem.MarkSector(fshd, B[j]) UNTIL j = 0;
            INC(i)
          END
        END ;
        INC(L)
      END
    END MarkSectors;
*/
    int L, R, i, j, n; DiskAdr x;
    FileHeader hd;
    IndexSector B;

    L = k / 2; R = k; /*heapsort*/
    while (L > 0) { L--; sift(A, L, R); }
    while (R > 0) {
        R--; x = A[0]; A[0] = A[R]; A[R] = x; sift(A, L, R);
    }
    while (L < k) {
        GetSector(fshd, A[L], (uint8_t *) &hd); assert(hd.b.mark == HeaderMark);
        if (hd.b.aleng < SecTabSize) { j = hd.b.aleng + 1;
            do { j--; MarkSector(fshd, hd.b.sec[j]); } while (!(j == 0));
        } else { j = SecTabSize;
            do { j--; MarkSector(fshd, hd.b.sec[j]); } while (!(j == 0));
            n = (hd.b.aleng - SecTabSize) / IndexSize; i = 0;
            while (i <= n) {
                MarkSector(fshd, hd.b.ext[i]);
                GetSector(fshd, hd.b.ext[i], (uint8_t *) B); /*index sector*/
                if (i < n) j = IndexSize; else j = (hd.b.aleng - SecTabSize) % IndexSize + 1;
                do { j--; MarkSector(fshd, B[j]); } while (!(j == 0));
                i++;
            }
        }
        L++;
    }
}

static void TraverseDir(FSHeader *fshd, DiskAdr *A, int *k, DiskAdr dpg)
{
/**
    PROCEDURE TraverseDir(VAR fshd: FileSystem.FSHeader; VAR A: ARRAY OF DiskAdr; VAR k: INTEGER; dpg: DiskAdr);
      VAR i: INTEGER; a: DirPage;
    BEGIN FileSystem.GetSector(fshd, dpg, a); ASSERT(a.mark = DirMark); FileSystem.MarkSector(fshd, dpg); i := 0;
      WHILE i < a.m DO
        A[k] := a.e[i].adr; INC(k); INC(i);
        IF k = 2000 THEN MarkSectors(fshd, A, k); k := 0 END
      END ;
      IF a.p0 # 0 THEN
        TraverseDir(fshd, A, k, a.p0); i := 0;
        WHILE i < a.m DO TraverseDir(fshd, A, k, a.e[i].p); INC(i) END
      END
    END TraverseDir;
*/
    int i; DirPage a;

    GetSector(fshd, dpg, (uint8_t *) &a); assert(a.b.b.mark == DirMark); MarkSector(fshd, dpg); i = 0;
    while (i < a.b.b.m) {
        A[*k] = a.b.e[i].adr; (*k)++; i++;
        if (*k == 2000) { MarkSectors(fshd, A, *k); *k = 0; }
    }
    if (a.b.b.p0 != 0) {
        TraverseDir(fshd, A, k, a.b.b.p0); i = 0;
        while (i < a.b.b.m) {
            TraverseDir(fshd, A, k, a.b.e[i].p); i++;
        }
    }
}

void FileDir_Init(FSHeader *fshd, int fssec)
{
/**
  PROCEDURE Init*(VAR fshd: FileSystem.FSHeader; fssec: INTEGER);
    VAR k: INTEGER;
      A: ARRAY 2000 OF DiskAdr;
      fshdp!: FileSystem.FSHeaderPage; (*header mapped on sector fssec*)

  BEGIN SDCard.Read(fssec, SYSTEM.ADR(fshdp), SectorSize DIV SDCard.BLOCKSIZE); LED(1*2);
    ASSERT(fshdp.mark = FSMark); ASSERT(fshdp.version = 2); LED(2*2);
    CLEAR(fshd); SYSTEM.COPY(SYSTEM.ADR(fshdp), SYSTEM.ADR(fshd), SYSTEM.SIZE(FileSystem.FSHeaderPersistent) DIV 4); LED(3*2);
    fshd.FS_OFFSET := fssec; FileSystem.InitSecMap(fshd); LED(4*2);
    k := 0; TraverseDir(fshd, A, k, DirRootAdr); MarkSectors(fshd, A, k); LED(5*2)
  END Init;
*/
    int k;
    DiskAdr A[2000];
    FSHeaderPage fshdp;
    off_t pos;
    ssize_t nbRead;

    pos = lseek(OberonFS_fd, fssec, SEEK_SET);
    assert (pos >= 0);
    nbRead = read(OberonFS_fd, &fshdp, SectorSize);
    assert (nbRead == SectorSize);
    assert(fshdp.b.b.mark == FSMark); assert(fshdp.b.b.version == 2);

    memset(fshd, 0, sizeof *fshd); memcpy(fshd, &fshdp, sizeof(FSHeaderPersistent));
    fshd->FS_OFFSET = fssec; InitSecMap(fshd);
    k = 0; TraverseDir(fshd, A, &k, DirRootAdr); MarkSectors(fshd, A, k);
}
