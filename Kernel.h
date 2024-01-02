#ifndef KERNEL_H_INCLUDED
#define KERNEL_H_INCLUDED

#include <stdint.h>

#define Kernel_SectorLength (1024)

extern int Kernel_NofSectors;

typedef uint8_t Kernel_Sector[Kernel_SectorLength];

void Kernel_Init(void);
void Kernel_InitSecMap(void);
void Kernel_GetSector(int src, Kernel_Sector dst);
void Kernel_PutSector(int dst, Kernel_Sector src);
void Kernel_MarkSector(int sec);
void Kernel_AllocSector(int hint, int *sec);
void Kernel_FreeSector(int sec);
int Kernel_Clock(void);

#endif // KERNEL_H_INCLUDED
