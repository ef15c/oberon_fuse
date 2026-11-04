#ifndef KERNEL_H_INCLUDED
#define KERNEL_H_INCLUDED

#include <stdint.h>

#define BootReservedSize 61440

void Kernel_Init(void);
int Kernel_Clock(void);

#endif // KERNEL_H_INCLUDED
