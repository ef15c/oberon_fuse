#ifndef MODULES_H_INCLUDED
#define MODULES_H_INCLUDED

extern int OberonFS_fd;
extern int OberonFS_offset;

void Set_OberonFS_image(int fd, int offset);

void Modules_Init(void);

#endif // MODULES_H_INCLUDED
