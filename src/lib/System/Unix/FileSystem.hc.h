/* hand-coded interface part of FileSystem */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date$ ($Revision$)
*/

/* import mode_t, ino_t, dev_t, struct stat... */
#include <unixconfig.h>


/* DATA inode == */
typedef struct sINODE {
  struct sCELL header;
  ino_t        value;
} * INODE;

#define size_inode          sizeof_small(struct sINODE)
#define alloc_inode(i)      alloc_small_flat(size_inode,i)
#define make_inode(v,i)     {alloc_inode(i);((INODE)(i))->value=v;}
#define copy_inode(i,n)     copy_structured(i,n)
#define free_inode(i,n)     free_structured(i,n)
#define excl_inode(i,n)     excl_structured(i,n)
#define decr_inode(i,n)     decr_structured(i,n)

/* DATA device == */

typedef struct sDEVICE {
  struct sCELL header;
  dev_t        value;
} * DEVICE;

#define size_device          sizeof_small(struct sDEVICE)
#define alloc_device(d)      alloc_small_flat(size_device,d)
#define make_device(v,d)     {alloc_device(d);((DEVICE)(d))->value=v;}
#define copy_device(d,n)     copy_structured(d,n)
#define free_device(d,n)     free_structured(d,n)
#define excl_device(d,n)     excl_structured(d,n)
#define decr_device(d,n)     decr_structured(d,n)

/* DATA filemode == */
/* NOTE: It is ESSENTIAL that the value of a filemode does ONLY contain */
/*       ACCESS PERMISSION BITS! Several system calls rely on this. */
typedef struct sFILEMODE {
  struct sCELL header;
  mode_t       value;
} * FILEMODE;

#define size_filemode          sizeof_small(struct sFILEMODE)
#define alloc_filemode(f)      alloc_small_flat(size_filemode,f)
#define make_filemode(v,f)     {alloc_filemode(f);((FILEMODE)(f))->value=v;}
#define copy_filemode(f,n)     copy_structured(f,n)
#define free_filemode(f,n)     free_structured(f,n)
#define excl_filemode(f,n)     excl_structured(f,n)
#define decr_filemode(f,n)     decr_structured(f,n)

/* macro based implementations */

/* < : inode ** inode -> bool */
#define AFileSystem_Sl(i1,i2,r) {\
  r=pack_clean_bool(((INODE)(i1))->value < ((INODE)(i2))->value);\
  free_inode(i1,1); free_inode(i2,1);\
}
/* = : inode ** inode -> bool */
#define AFileSystem_Se(i1,i2,r) {\
  r=pack_clean_bool(((INODE)(i1))->value == ((INODE)(i2))->value);\
  free_inode(i1,1); free_inode(i2,1);\
}

/* < : device ** device -> bool */
#define AFileSystem_Sl_O1(d1,d2,r) {\
  r=pack_clean_bool(((DEVICE)(d1))->value < ((DEVICE)(d2))->value);\
  free_device(d1,1); free_device(d2,1);\
}
/* = : device ** device -> bool */
#define AFileSystem_Se_O1(d1,d2,r) {\
  r=pack_clean_bool(((DEVICE)(d1))->value == ((DEVICE)(d2))->value);\
  free_device(d1,1); free_device(d2,1);\
}

/* < : filemode ** filemode -> bool */
#define AFileSystem_Sl_O3(m1,m2,r) {\
  r=pack_clean_bool(((FILEMODE)(m1))->value < ((FILEMODE)(m2))->value);\
  free_filemode(m1,1); free_filemode(m2,1);\
}
/* = : filemode ** filemode -> bool */
#define AFileSystem_Se_O3(m1,m2,r) {\
  r=pack_clean_bool(((FILEMODE)(m1))->value == ((FILEMODE)(m2))->value);\
  free_filemode(m1,1); free_filemode(m2,1);\
}

/* hc_emptymode */
/* constant -- no macro expansion entry */

/* hc_nonemode */
/* no macro expansion entry */

/* hc_addmode */
/* no macro expansion entry */

/* hc_delmode */
/* no macro expansion entry */

/* hc_ormode */
/* no macro expansion entry */

/* hc_andnegmode */
/* no macro expansion entry */

/* hc_andmode */
/* no macro expansion entry */

/* hc_inmode */
/* no macro expansion entry */

/* hc_stat */
/* no macro expansion entry */

/* hc_link */
/* no macro expansion entry */

/* hc_unlink */
/* no macro expansion entry */

/* hc_rename */
/* no macro expansion entry */

/* hc_utime */
/* no macro expansion entry */

/* hc_chmod */
/* no macro expansion entry */

/* hc_chown */
/* no macro expansion entry */

/* hc_mkdir */
/* no macro expansion entry */

/* hc_rmdir */
/* no macro expansion entry */

/* hc_readdir */
/* no macro expansion entry */

/* hc_symlink */
/* no macro expansion entry */

/* hc_readlink */
/* no macro expansion entry */

/* hc_mkfifo */
/* no macro expansion entry */

/* hc_tmpnam */
/* no macro expansion entry */
