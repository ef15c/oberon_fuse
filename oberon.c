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

#include "Modules.h"
#include "FileDir.h"

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
} options;

int oberon_fs_file_desc = -1;

#define OPTION(t, p)                           \
    { t, offsetof(struct options, p), 1 }
static const struct fuse_opt option_spec[] = {
    OPTION("--image=%s", oberon_image),
	OPTION("-h", show_help),
	OPTION("--help", show_help),
	FUSE_OPT_END
};

static void *oberon_init(struct fuse_conn_info *conn,
			struct fuse_config *cfg)
{
	(void) conn;

	cfg->kernel_cache = 1;

	Modules_Set_OberonFS_fd(oberon_fs_file_desc);
    Modules_Init();

	return NULL;
}

static int oberon_getattr(const char *path, struct stat *stbuf,
			 struct fuse_file_info *fi)
{
	(void) fi;
	int res = 0;

	memset(stbuf, 0, sizeof(struct stat));
	if (strcmp(path, "/") == 0) {
		stbuf->st_mode = S_IFDIR | 0755;
		stbuf->st_nlink = 2;
#ifdef TODO
	} else if (strcmp(path+1, options.filename) == 0) {
		stbuf->st_mode = S_IFREG | 0444;
		stbuf->st_nlink = 1;
		stbuf->st_size = strlen(options.contents);
#endif // TODO
	} else
		res = -ENOENT;

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

#ifdef CONTENT_SUPPORT
static int oberon_open(const char *path, struct fuse_file_info *fi)
{
	if (strcmp(path+1, options.filename) != 0)
		return -ENOENT;

	if ((fi->flags & O_ACCMODE) != O_RDONLY)
		return -EACCES;

	return 0;
}

static int oberon_read(const char *path, char *buf, size_t size, off_t offset,
		      struct fuse_file_info *fi)
{
	size_t len;
	(void) fi;
	if(strcmp(path+1, options.filename) != 0)
		return -ENOENT;

	len = strlen(options.contents);
	if (offset < len) {
		if (offset + size > len)
			size = len - offset;
		memcpy(buf, options.contents + offset, size);
	} else
		size = 0;

	return size;
}
#endif // CONTENT_SUPPORT

static const struct fuse_operations oberon_oper = {
	.init       = oberon_init,
	.getattr	= oberon_getattr,
	.readdir	= oberon_readdir,
#ifdef CONTENT_SUPPORT
	.open		= oberon_open,
	.read		= oberon_read,
#endif // CONTENT_SUPPORT
};

static void show_help(const char *progname)
{
	printf("usage: %s [options] --image=<oberon image> <mountpoint>\n\n", progname);
	printf("File-system specific options:\n"
	       "    --image=<file name>    Name of the oberon image file\n"
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
	}

	oberon_fs_file_desc = open(options.oberon_image, O_RDONLY);
    if (oberon_fs_file_desc < 0) {
        printf("Unable to open file %s for read\n", options.oberon_image);
        return 3;
    }

	ret = fuse_main(args.argc, args.argv, &oberon_oper, NULL);
	fuse_opt_free_args(&args);

    if (oberon_fs_file_desc >= 0) {
        close(oberon_fs_file_desc);
    }

	return ret;
}
