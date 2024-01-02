#ifndef MODULES_H_INCLUDED
#define MODULES_H_INCLUDED

extern int Modules_OberonFS_fd;
extern int Modules_OberonFS_offset;

void Modules_Set_OberonFS_image(int fd, int offset);

void Modules_Init(void);

#endif // MODULES_H_INCLUDED
