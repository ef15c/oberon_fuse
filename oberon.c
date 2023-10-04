/*
  FUSE: Filesystem in Userspace
  Copyright (C) 2001-2007  Miklos Szeredi <miklos@szeredi.hu>

  This program can be distributed under the terms of the GNU GPLv2.
  See the file COPYING.
*/

/** @file
 *
 * minimal oberon filesystem using high-level API
 *
 * Compile with:
 *
 *     gcc -Wall oberon.c `pkg-config fuse3 --cflags --libs` -o oberon
 *
 * ## Source code ##
 * \include oberon.c
 */


#define FUSE_USE_VERSION 31

#include <fuse.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <assert.h>
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>

#include "Modules.h"
#include "FileDir.h"
#include "Kernel.h"
#include "Files.h"

/*
 * Command line options
 *
 * We can't set default values for the char* fields here because
 * fuse_opt_parse would attempt to free() them when the user specifies
 * different values on the command line.
 */
static struct options {
	int show_help;
	const char *oberon_image;
	int oberon_image_offset;
} options;

int oberon_fs_file_desc = -1;

#define OPTION(t, p)                           \
    { t, offsetof(struct options, p), 1 }
static const struct fuse_opt option_spec[] = {
    OPTION("--image=%s", oberon_image),
    OPTION("--offset=%d", oberon_image_offset),
	OPTION("-h", show_help),
	OPTION("--help", show_help),
	FUSE_OPT_END
};

static void *oberon_init(struct fuse_conn_info *conn,
			struct fuse_config *cfg)
{
	(void) conn;

	cfg->kernel_cache = 1;

	Modules_Set_OberonFS_image(oberon_fs_file_desc, options.oberon_image_offset);

    Modules_Init();

	return NULL;
}

static time_t ClockToTime(int t)
{
/**
    WritePair(W, " ", d DIV 20000H MOD 20H);   (*day*)
    WritePair(W, ".", d DIV 400000H MOD 10H); (*month*)
    WritePair(W, ".", d DIV 4000000H MOD 40H);   (*year*)
    WritePair(W, " ", d DIV 1000H MOD 20H);   (*hour*)
    WritePair(W, ":", d DIV 40H MOD 40H);  (*min*)
    WritePair(W, ":", d MOD 40H)  (*sec*)

    ex. : 11.12.18 17:32:24
*/
    struct tm date;

    date.tm_mday = t / 0x20000 % 0x20; /* day */
    date.tm_mon  = t / 0x400000 % 0x10 - 1; /* month - 1 */
    date.tm_year = t / 0x4000000 % 0x40 + 100; /* year - 1900 */
    date.tm_hour = t / 0x1000 % 0x20; /* hour */
    date.tm_min  = t / 0x40 % 0x40; /* min */
    date.tm_sec  = t % 0x40; /* sec */

    return mktime(&date);
}

static int oberon_getattr(const char *path, struct stat *stbuf,
			 struct fuse_file_info *fi)
{
	(void) fi;
	int res = 0;

	int adr;

	memset(stbuf, 0, sizeof(struct stat));
	if (strcmp(path, "/") == 0) {
		stbuf->st_mode = S_IFDIR | 0777;
		stbuf->st_nlink = 2;
	} else  {
        FileDir_Search(path+1, &adr);
        if (adr) {
            FileDir_FileHeader hp;

            Kernel_GetSector(adr, (uint8_t *) &hp);

            stbuf->st_mode = S_IFREG | 0666;
            stbuf->st_nlink = 1;
            stbuf->st_size = hp.aleng*FileDir_SectorSize + hp.bleng - FileDir_HeaderSize /* length */;
            stbuf->st_mtim.tv_sec = ClockToTime(hp.date);
        } else {
            res = -ENOENT;
        }
    }
	return res;
}

struct readdir_parameters {
    fuse_fill_dir_t filler;
    void *buf;
};

void List(FileDir_FileName name, int sec, bool *Continue, void *callerData)
{
    struct readdir_parameters *par = callerData;
	par->filler(par->buf, name, NULL, 0, 0);
}


static int oberon_readdir(const char *path, void *buf, fuse_fill_dir_t filler,
			 off_t offset, struct fuse_file_info *fi,
			 enum fuse_readdir_flags flags)
{
	struct readdir_parameters par = {
        .filler = filler,
        .buf = buf,
	};

	(void) offset;
	(void) fi;
	(void) flags;

	if (strcmp(path, "/") != 0)
		return -ENOENT;

	filler(buf, ".", NULL, 0, 0);
	filler(buf, "..", NULL, 0, 0);
    FileDir_Enumerate("", List, &par);

	return 0;
}

static int oberon_open(const char *path, struct fuse_file_info *fi)
{
    Files_File f;
    Files_Rider *Rf;

    f = Files_Old(path+1);

    if (f == NULL) {
		return -ENOENT;
    }

    Rf = malloc(sizeof *Rf);
    assert(Rf);

    Files_Set(Rf, f, 0);

    fi->fh = (uint64_t) Rf;

	return 0;
}

static int oberon_read(const char *path, char *buf, size_t size, off_t offset,
		      struct fuse_file_info *fi)
{
	int i;
    Files_Rider *Rf;
    char ch;

    assert(fi);
    Rf = (Files_Rider *) fi->fh;
    assert(Rf);

    Files_Reset(Rf, offset);
    Files_Read(Rf, &ch);
    i = 0;
    while (!Rf->eof && i<size) { buf[i++] = ch; Files_Read(Rf, &ch); }

	return i;
}

static int oberon_write(const char *path, const char *buf, size_t size,
		     off_t offset, struct fuse_file_info *fi)
{
    int i;
    Files_Rider *Rf;

    assert(fi);
    Rf = (Files_Rider *) fi->fh;
    assert(Rf);

    Files_Reset(Rf, offset);
    i = 0;
    while (i<size) Files_Write(Rf, buf[i++]);

    return i;
}

int oberon_release (const char *path, struct fuse_file_info *fi)
{
    Files_Rider *Rf;
    Files_File f;

    assert(fi);
    Rf = (Files_Rider *) fi->fh;
    assert(Rf);

    f = Files_Base(Rf);
    assert(f);

    Files_Close(f);

    Files_Unset(Rf);

    return 0;
}


static int oberon_create(const char *path, mode_t mode,
		      struct fuse_file_info *fi)
{
    Files_File f;
    Files_Rider *Rf;

    f = Files_New(path+1);

    if (f == NULL) {
		return -ENOENT;
    }

    Files_Register(f);

    Rf = malloc(sizeof *Rf);
    assert(Rf);

    Files_Set(Rf, f, 0);

    fi->fh = (uint64_t) Rf;

	return 0;
}

static int oberon_unlink(const char *path)
{
	int res;

	Files_Delete(path+1, &res);
	if (res != 0)
		return -ENOENT;

	return 0;
}

static const struct fuse_operations oberon_oper = {
	.init       = oberon_init,
	.getattr	= oberon_getattr,
	.readdir	= oberon_readdir,
	.open		= oberon_open,
	.read		= oberon_read,
	.write      = oberon_write,
	.release    = oberon_release,
	.create     = oberon_create,
	.unlink     = oberon_unlink,
};

static void show_help(const char *progname)
{
	printf("usage: %s [options] --image=<oberon image> <mountpoint>\n\n", progname);
	printf("File-system specific options:\n"
	       "    --image=<file name>      Name of the oberon image file\n"
	       "    --offset=<signed value>  Number of bytes of offset to the\n"
	       "                             beginning of the file system.\n"
	       "\n"
	       "                             0 means that the directory root\n"
	       "                             is located ad address 0x400.\n"
	       "\n"
	       "                             The RISCW32 emulator images have\n"
	       "                             their directory root located at 0x0\n"
	       "                             so offset equal to -1024 must be\n"
	       "                             specified\n"
	       "\n");
}

int main(int argc, char *argv[])
{
	int ret;
	struct fuse_args args = FUSE_ARGS_INIT(argc, argv);

	/* Parse options */
	if (fuse_opt_parse(&args, &options, option_spec, NULL) == -1)
		return 1;

	/* When --help is specified, first print our own file-system
	   specific help text, then signal fuse_main to show
	   additional help (by adding `--help` to the options again)
	   without usage: line (by setting argv[0] to the empty
	   string) */
	if (options.show_help) {
		show_help(argv[0]);
		assert(fuse_opt_add_arg(&args, "--help") == 0);
		args.argv[0][0] = '\0';
	} else {
        if (!options.oberon_image) {
            printf("Missing mandatory --image=<oberon image> parameter\n");
            return 2;
        }

        oberon_fs_file_desc = open(options.oberon_image, O_RDWR);
        if (oberon_fs_file_desc < 0) {
            printf("Unable to open file %s for read\n", options.oberon_image);
            return 3;
        }
	}


	ret = fuse_main(args.argc, args.argv, &oberon_oper, NULL);
	fuse_opt_free_args(&args);

    if (oberon_fs_file_desc >= 0) {
        close(oberon_fs_file_desc);
    }

	return ret;
}
